library(tidyverse)
library(lubridate)
setwd("~/Case Study Competition")
crimes <- vroom::vroom("data/Crimes.csv")
moon <- vroom::vroom("data/full_moon.csv")
holiday <- vroom::vroom("data/holidays.csv")
weather <- vroom::vroom("data/weather.csv")
income <- vroom::vroom("data/median-household-income.csv", 
                       skip = 3,
                       col_names = c("Year", "Income"))

theme_set(theme_bw())

# Rename all of the columns to remove spaces
crimes <- crimes %>%
  rename(Case_Num = `Case Number`,
         Primary_Type = `Primary Type`,
         Loc_Desc = `Location Description`,
         Com_Area = `Community Area`,
         FBI_Code = `FBI Code`,
         X = `X Coordinate`,
         Y = `Y Coordinate`) %>%
  mutate(Date = mdy_hms(Date))

violent_crimes <- c("HOMICIDE", "BATTERY", 
                    "CRIMINAL SEXUAL ASSAULT", "CRIM SEXUAL ASSAULT",
                    "ASSAULT", "ROBBERY", "HUMAN TRAFFICKING")


###### First Model on Non-Domestic Crimes ######

non_dom_crimes <- crimes %>%
  filter(Primary_Type %in% violent_crimes) %>%
  filter(Domestic == FALSE) %>%
  mutate(YrMon = year(Date) + (month(Date) - 0.5)/12) %>%
  group_by(YrMon) %>%
  summarise(num_crimes = n()) %>%
  arrange(YrMon) %>%
  filter(YrMon >= 2010)

dom_crimes <- crimes %>%
  filter(Primary_Type %in% violent_crimes) %>%
  filter(Domestic == TRUE) %>%
  mutate(YrMon = year(Date) + (month(Date) - 0.5)/12) %>%
  group_by(YrMon) %>%
  summarise(num_crimes = n()) %>%
  arrange(YrMon) %>%
  filter(YrMon >= 2010)

ggplot() +
  geom_line(data = non_dom_crimes,
            mapping = aes(x = YrMon, y = num_crimes)) +
  geom_line(data = dom_crimes,
            mapping = aes(x = YrMon, y = num_crimes),
            color = "blue")

## Prepare the Weather Dataset for joining

weather_join <- weather %>%
  mutate(Year_Month = paste(year(datetime), month(datetime), sep = "-")) %>%
  group_by(Year_Month) %>%
  mutate(uvindex = mean(uvindex)) %>%
  summarise_all(max) %>%
  arrange(datetime) %>%
  mutate(YrMon = year(datetime) + (month(datetime) - 0.5) / 12) %>%
  select(YrMon, feelslike, uvindex, humidity, precip, windspeed, temp)

## Generate new Values for income
income <- vroom::vroom("data/median-household-income.csv", 
                       skip = 3,
                       col_names = c("Year", "Income"))
income_lm <- lm(Income ~ Year, data = income)
new_val <- predict(income_lm, newdata = data.frame(Year = c(2023, 2024)))
income <- rbind(income, tibble(Year = c(2023, 2024), Income = new_val))
income <- filter(income, Year >= 2010)


## Join the three datasets together
data <- left_join(non_dom_crimes, weather_join)

data_to_join <- data %>%
  mutate(Year = floor(YrMon))

X <- left_join(data_to_join, income) %>%
  select(-Year) %>%
  mutate(Income = Income / 1000) %>%
  model.matrix(num_crimes ~ -1 + Income + temp + uvindex + humidity + precip + windspeed, 
               data = .)

crimes_ts <- ts(data = non_dom_crimes$num_crimes,
                start = c(2010, 1), frequency = 12)

auto_results <- forecast::auto.arima(crimes_ts, ic = "aic",
                                     xreg = X)
crimes_arima <- forecast::Arima(crimes_ts,
                                order = c(1,0,1),
                                seasonal = c(2,0,0),
                                xreg = X)


crimes_arima %>%
  confint()

multcomp::glht(crimes_arima, 
               linfct = cbind(0,0,0,0,0,0,0,1,0,0,0)) %>%
  summary()

## Assumptions

# Independence

acf_non_dom <- forecast::ggAcf(resid(crimes_arima), lag.max = 20) +
  labs(
    title = "ACF Plot of Residuals : Non-Domestic Crimes"
  )
ggsave("acf_non_dom.png", acf_non_dom, path = "plots")


# Normality

dec_resid <- resid(crimes_arima)

normality_non_dom <- ggplot() +
  geom_histogram(aes(x = dec_resid), 
                 fill = "skyblue",
                 color = "white",
                 bins = 10) +
  labs(
    title = "Histogram of Decorrelated Residuals",
    x = "Decorrelated Residuals"
  )
ggsave("normality_non_dom.png", normality_non_dom, path = "plots")


# Equal Variance

equal_var_non_dom <- ggplot() +
  geom_point(aes(x = fitted(crimes_arima), y = dec_resid)) +
  labs(
    title = "Decorrelated Residuals vs Fitted Values",
    y = "Decorrelated Residuals",
    x = "Fitted Values"
  )
ggsave("equal_var_non_dom.png", equal_var_non_dom, path = "plots")


# Linearity
lm_mod <- left_join(data_to_join, income) %>%
  select(-Year) %>%
  mutate(Income = Income / 1000) %>%
  lm(num_crimes ~ Income + feelslike + uvindex + humidity + precip + windspeed, 
               data = .) 
car::avPlots(lm_mod)



# Visualization of the model fit
mse <- (non_dom_crimes$num_crimes - crimes_arima$fitted)^2 %>%
  mean() %>%
  sqrt()
viz_Non_Dom <- ggplot(data = non_dom_crimes) +
  geom_line(mapping = aes(x = YrMon, y = num_crimes),
            color = "#4c453f") +
  geom_line(mapping = aes(x = YrMon, y = fitted(crimes_arima)),
            color = "#c2272d") +
  theme_bw() +
  labs(
    title = "Model Fit Non-Domestic Crimes",
    subtitle = paste("Root Mean Square Error", round(mse, 3)),
    x = "Year",
    y = "Number of Crimes"
  )
ggsave("viz_non_dom.png", viz_Non_Dom, path = "plots")

# Cross Validation of the Model

# Get a subset of the data

X_train <- X[1:156,]
X_test <- X[157:nrow(X),]
y_train <- non_dom_crimes$num_crimes[1:156]
y_test <- non_dom_crimes$num_crimes[157:nrow(X)]

train_ts <- ts(y_train, start = c(2010, 1), frequency = 12)

train_arima <- forecast::Arima(train_ts,
                               order = c(1,0,1),
                               seasonal = c(2,0,0),
                               xreg = X_train)

preds <- forecast::forecast(train_arima,
                   h = 14,
                   xreg = X_test,
                   level = c(0.90, 0.95))
non_preds_df <- data.frame(preds)

rpmse_cv <- (non_preds_df$Point.Forecast - y_test)^2 %>%
  mean() %>%
  sqrt()

cv_plot <- ggplot() +
  geom_ribbon(data = non_preds_df,
              mapping = aes(x = non_dom_crimes$YrMon[157:nrow(X)],
                            ymin = Lo.95, ymax = Hi.95),
              fill = "skyblue",
              alpha = 0.3) +
  geom_ribbon(data = non_preds_df,
              mapping = aes(x = non_dom_crimes$YrMon[157:nrow(X)],
                            ymin = Lo.90, ymax = Hi.90),
              fill = "skyblue",
              alpha = 0.5) +
  geom_line(data = non_dom_crimes,
            mapping = aes(x = YrMon, y = num_crimes)) +
  geom_line(data = non_preds_df,
            mapping = aes(x = non_dom_crimes$YrMon[157:nrow(X)],
                          y = Point.Forecast),
            color = "red",
            linewidth = .8) +
  theme_bw() +
  labs(
    title = "Cross Validated Forecast of Non-Domestic Violent Crimes",
    subtitle = paste("Root Predictive Mean Square Error: ", 
                     round(rpmse_cv, 3)),
    x = "Year",
    y = "Number of Crimes"
  )
ggsave("cv-plot.jpg", cv_plot, path = "plots")


############# Do the same thing or domestic crimes ##############

dom_crimes <- crimes %>%
  filter(Primary_Type %in% violent_crimes) %>%
  filter(Domestic == TRUE) %>%
  mutate(YrMon = year(Date) + (month(Date) - 0.5)/12) %>%
  group_by(YrMon) %>%
  summarise(num_crimes = n()) %>%
  arrange(YrMon) %>%
  filter(YrMon >= 2010)

ggplot() +
  geom_line(data = non_dom_crimes,
            mapping = aes(x = YrMon, y = num_crimes)) +
  geom_line(data = dom_crimes,
            mapping = aes(x = YrMon, y = num_crimes),
            color = "blue")

## Prepare the Weather Dataset for joining

weather_join <- weather %>%
  mutate(Year_Month = paste(year(datetime), month(datetime), sep = "-")) %>%
  group_by(Year_Month) %>%
  mutate(uvindex = mean(uvindex)) %>%
  summarise_all(max) %>%
  arrange(datetime) %>%
  mutate(YrMon = year(datetime) + (month(datetime) - 0.5) / 12) %>%
  select(YrMon, feelslike, uvindex, humidity, precip, windspeed, temp)

## Generate new Values for income
income <- vroom::vroom("data/median-household-income.csv", 
                       skip = 3,
                       col_names = c("Year", "Income"))
income_lm <- lm(Income ~ Year, data = income)
new_val <- predict(income_lm, newdata = data.frame(Year = c(2023, 2024)))
income <- rbind(income, tibble(Year = c(2023, 2024), Income = new_val))
income <- filter(income, Year >= 2010)


## Join the three datasets together
data <- left_join(dom_crimes, weather_join)

data_to_join <- data %>%
  mutate(Year = floor(YrMon))

X <- left_join(data_to_join, income) %>%
  select(-Year) %>%
  mutate(Income = Income / 1000) %>%
  model.matrix(num_crimes ~ -1 + Income + temp + uvindex + humidity + precip + windspeed, 
               data = .)

crimes_ts <- ts(data = dom_crimes$num_crimes,
                start = c(2010, 1), frequency = 12)

auto_results <- forecast::auto.arima(crimes_ts, ic = "aic",
                                     xreg = X)
crimes_arima_dom <- forecast::Arima(crimes_ts,
                                order = c(2,0,2),
                                seasonal = c(2,0,0),
                                xreg = X)


crimes_arima_dom %>%
  confint()

multcomp::glht(crimes_arima_dom, 
               linfct = cbind(0,0,0,0,0,0,0,0,0,1,0,0,0)) %>%
  confint()

## Assumptions

# Independence

acf_dom <- forecast::ggAcf(resid(crimes_arima_dom), lag.max = 20) +
  labs(
    title = "ACF Plot of Residuals : Domestic Crimes"
  )
ggsave("acf_dom.png", acf_dom, path = "plots")

# Normality

dec_resid <- resid(crimes_arima_dom)

normality_dom <- ggplot() +
  geom_histogram(aes(x = dec_resid), 
                 fill = "skyblue",
                 color = "white",
                 bins = 9)
ggsave("normality_dom.png", normality_dom, path = "plots")

# Equal Variance

equal_var_dom <- ggplot() +
  geom_point(aes(x = fitted(crimes_arima_dom), y = dec_resid)) +
  labs(
    title = "Decorrelated Residuals vs Fitted Values",
    x = "Fitted Values",
    y = "Decorrelated Residuals"
  )
ggsave("equal_var_dom.png", equal_var_dom, path = "plots")

# Linearity
lm_mod <- left_join(data_to_join, income) %>%
  select(-Year) %>%
  mutate(Income = Income / 1000) %>%
  lm(num_crimes ~ Income + feelslike + uvindex + humidity + precip + windspeed, 
     data = .) 
car::avPlots(lm_mod)



# Visualization of the model fit
mse <- (dom_crimes$num_crimes - crimes_arima_dom$fitted)^2 %>%
  mean() %>%
  sqrt()
viz_dom <- ggplot(data = dom_crimes) +
  geom_line(mapping = aes(x = YrMon, y = num_crimes),
            color = "#4c453f") +
  geom_line(mapping = aes(x = YrMon, y = fitted(crimes_arima_dom)),
            color = "#c2272d") +
  theme_bw() +
  labs(
    title = "Model Fit Domestic Crimes",
    subtitle = paste("Root Mean Square Error", round(mse, 3)),
    x = "Year",
    y = "Number of Crimes"
  )
ggsave("viz_dom.png", viz_dom, path = "plots")

# Cross Validation of the Model

# Get a subset of the data

X_train <- X[1:156,]
X_test <- X[157:nrow(X),]
y_train <- dom_crimes$num_crimes[1:156]
y_test <- dom_crimes$num_crimes[157:nrow(X)]

train_ts <- ts(y_train, start = c(2010, 1), frequency = 12)

train_arima <- forecast::Arima(train_ts,
                               order = c(1,0,1),
                               seasonal = c(2,0,0),
                               xreg = X_train)

preds <- forecast::forecast(train_arima,
                            h = 14,
                            xreg = X_test,
                            level = c(0.90, 0.95))
dom_preds_df <- data.frame(preds)

rpmse_cv <- (dom_preds_df$Point.Forecast - y_test)^2 %>%
  mean() %>%
  sqrt()
  
ggplot() +
  geom_ribbon(data = dom_preds_df,
              mapping = aes(x = dom_crimes$YrMon[157:nrow(X)],
                            ymin = Lo.95, ymax = Hi.95),
              fill = "skyblue",
              alpha = 0.3) +
  geom_ribbon(data = dom_preds_df,
              mapping = aes(x = dom_crimes$YrMon[157:nrow(X)],
                            ymin = Lo.90, ymax = Hi.90),
              fill = "skyblue",
              alpha = 0.5) +
  geom_line(data = dom_crimes,
            mapping = aes(x = YrMon, y = num_crimes)) +
  geom_line(data = preds_df,
            mapping = aes(x = dom_crimes$YrMon[157:nrow(X)],
                          y = Point.Forecast),
            color = "red",
            linewidth = .8) +
  theme_bw() +
  labs(
    title = "Cross Validated Forecast of Domestic Violent Crimes",
    subtitle = paste("Root Predictive Mean Square Error: ", 
                     round(rpmse_cv, 3)),
    x = "Year",
    y = "Number of Crimes"
  )
ggsave("cv-plot-dom.jpg", cv_plot, path = "plots")

# Add predictions to get total crimes per month

month_crimes <- crimes %>%
  filter(Primary_Type %in% violent_crimes) %>%
  mutate(YrMon = year(Date) + (month(Date) - 0.5)/12) %>%
  group_by(YrMon) %>%
  summarise(num_crimes = n()) %>%
  arrange(YrMon) %>%
  filter(YrMon >= 2010)

added_fitted <- crimes_arima$fitted + crimes_arima_dom$fitted

total_rmse <- (added_fitted - month_crimes$num_crimes)^2 %>%
  mean() %>%
  sqrt()

######### Combined Model Inference ###########

# Finding the Standard Errors of new betas

non_dom_coef <- crimes_arima$coef[-(1:5)]
dom_coef <- crimes_arima_dom$coef[-(1:7)]

non_dom_stderr <- sqrt(diag(crimes_arima$var.coef))[-(1:5)]
dom_stderr <- sqrt(diag(crimes_arima_dom$var.coef))[-(1:7)]

beta_t_coef <- non_dom_coef + dom_coef
beta_t_stderr <- sqrt(non_dom_stderr^2 + dom_stderr^2)

### Estimating degrees of freedom
# Confidence Interval
degrees_freedom <- nrow(dom_crimes) * 2 - (12 + 5 + 7)
upper <- beta_t_coef + qt(0.975, degrees_freedom) * beta_t_stderr
lower <- beta_t_coef + qt(0.025, degrees_freedom) * beta_t_stderr
rbind(beta_t_coef, lower, upper) %>% t()

# Hypothesis Tests
t_stats <- beta_t_coef / beta_t_stderr
2*pt(abs(t_stats), degrees_freedom, lower.tail = FALSE) %>%
  round(., 4)


cor(crimes_arima$residuals, crimes_arima_dom$residuals)
plot(crimes_arima$residuals, crimes_arima_dom$residuals)

# Running a combined model for comparison of confidence intervals and predictions

total_crimes <- crimes %>%
  filter(Primary_Type %in% violent_crimes) %>%
  mutate(YrMon = year(Date) + (month(Date) - 0.5)/12) %>%
  group_by(YrMon) %>%
  summarise(num_crimes = n()) %>%
  arrange(YrMon) %>%
  filter(YrMon >= 2010)

data <- left_join(total_crimes, weather_join)

data_to_join <- data %>%
  mutate(Year = floor(YrMon))

plotting <- left_join(data_to_join, income)

X <- left_join(data_to_join, income) %>%
  select(-Year) %>%
  mutate(Income = Income / 1000) %>%
  model.matrix(num_crimes ~ -1 + Income + temp + humidity + precip + windspeed, 
               data = .)

crimes_ts <- ts(data = total_crimes$num_crimes,
                start = c(2010, 1), frequency = 12)

total_crimes_arima <- forecast::Arima(crimes_ts,
                                    order = c(1,0,1),
                                    seasonal = c(2,0,0),
                                    xreg = X)
total_crimes_arima %>%
  confint()

multcomp::glht(total_crimes_arima, linfct = t(c(rep(0, 9), 1))) %>%
  confint()

#### Additional EDA ####

ggplot(weather_join, aes(x = YrMon)) +
  geom_line(aes(y = feelslike))


day_crimes <- crimes %>%
  arrange(Date) %>%
  mutate(Act_date = date(Date)) %>%
  filter(Primary_Type %in% violent_crimes) %>%
  group_by(Act_date) %>%
  summarise(num_crimes = n()) %>%
  filter(year(Act_date) >= 2010)

nrow(day_crimes)
left_join(day_crimes, weather, by = join_by(Act_date == datetime)) %>%
  ggplot() +
  geom_point(aes(x = moonphase, y = num_crimes)) +
  theme_bw() +
  labs(
    title = "Does the moon affect crime???",
    x = "Moon Phase",
    y = "Number of Crimes"
  )

day_crimes %>%
  mutate(day = day(Act_date)) %>%
  filter(year(Act_date) == 2023) %>%
  ggplot() +
  geom_point(aes(x = day, y = num_crimes))

## Creating an image plot with the moon phases Icons

library(ggimage)

weather <- weather %>%
  mutate(image = case_when(moonphase == 0 ~ "new_moon.png",
                           moonphase < 0.25 ~ "waxing_crecent.png",
                           moonphase == 0.25 ~ "first_quarter.png",
                           moonphase < 0.5 ~ "waxing_gibbous.png",
                           moonphase == 0.5 ~ "full_moon.png",
                           moonphase < 0.75 ~ "waning_gibbous.png",
                           moonphase == 0.75 ~ "third_quarter.png",
                           moonphase <= 1 ~ "waning_crecent.png")) %>%
  mutate(image = paste("moon_images/", image, sep = ""))

plot_data <- left_join(day_crimes, weather, by = join_by(Act_date == datetime))

black_backround <- ggplot(data = plot_data,aes(x = moonphase, y = num_crimes)) +
  geom_image(aes(image = image), size = 0.05) +
  theme(panel.background = element_rect(fill = "black")) +
  labs(
    title = "Does the Moon Affect Crime???",
    x = "Moon Phase",
    y = "Number of Crimes"
  )

white_backround <- ggplot(data = plot_data,aes(x = moonphase, y = num_crimes)) +
  geom_image(aes(image = image), size = 0.05) +
  theme_bw() +
  labs(
    title = "Does the Moon Affect Crime???",
    x = "Moon Phase",
    y = "Number of Crimes"
  )

ggsave("new_moon_plot_black.png", black_backround, path = "plots",
       width = 6, height = 4)
ggsave("moon_plot_white.png", white_backround, path = "plots")

daymoon_black <- plot_data %>%
  mutate(day = day(Act_date)) %>%
  ggplot(data = .,aes(x = day, y = num_crimes)) +
  geom_image(aes(image = image), size = 0.05) +
  theme(panel.background = element_rect(fill = "black")) +
  labs(
    title = "Does the Moon Affect Crime???",
    x = "Day of the Month",
    y = "Number of Crimes"
  )

daymoon_white <- plot_data %>%
  mutate(day = day(Act_date)) %>%
  ggplot(data = .,aes(x = day, y = num_crimes)) +
  geom_image(aes(image = image), size = 0.05) +
  theme_bw() +
  labs(
    title = "Does the Moon Affect Crime???",
    x = "Day of the Month",
    y = "Number of Crimes"
  )

ggsave("daymoon_white.png", daymoon_white, path = "plots")
ggsave("daymoon_black.png", daymoon_black, path = "plots")

plot_data %>%
  mutate(ddate = decimal_date(Act_date)) %>%
  lm(num_crimes ~ moonphase + ddate, data = .) -> moon_lm

moon_lm %>%
  summary()
car::avPlots(moon_lm)

## Regular Time series plot with red line to match presentation theme

time_plot <- ggplot(month_crimes, aes(x = YrMon, y = num_crimes)) +
  geom_line(color = "#c2272d") +
  theme_bw() +
  labs(
    title = "Number of Crimes per Month Over Time",
    x = "Year",
    y = "Number of Crimes"
  )
ggsave("time_series_plot.png", time_plot, path = "plots")


## Create total Model Fitted Values Plot

total_fitted <- crimes_arima$fitted + crimes_arima_dom$fitted

rmse <- (total_fitted - month_crimes$num_crimes)^2 %>%
  mean() %>%
  sqrt()
fitted_plot <- ggplot() +
  geom_line(data = month_crimes,
            mapping = aes(x = YrMon, y = num_crimes),
            color = "#4c453f") +
  geom_line(aes(x = month_crimes$YrMon, y = total_fitted),
            color = "#c2272d") +
  theme_bw() +
  labs(
    title = "Time Series Fitted Values",
    subtitle = paste("Root Mean Square Error:", round(rmse, 3)),
    x = "Year",
    y = "Number of Crimes per Month"
  )
ggsave("fitted.png", fitted_plot, path = "plots",
       width = 6.88, height = 3.53, units = "in")

ggplot(plotting, aes(x = temp, y = num_crimes, color = Income)) +
  geom_point() +
  theme_bw() +
  labs(
    title = "Feels Like Temperature by Number of Crimes",
    x = "Feels Like Temperature (°F)",
    y = "Number of Crimes Per Month",
    color = "Median Income"
  )
cor(plotting$num_crimes, plotting$feelslike)
cor(plotting$num_crimes, plotting$temp)


## Creating the total_preds CV Plot

total_preds_df <- dom_preds_df + non_preds_df
total_preds_df

rpmse_cv <- (total_preds_df$Point.Forecast - 
  total_crimes$num_crimes[157:nrow(total_crimes)])^2 %>%
  mean() %>%
  sqrt()

cv_error <- ggplot() +
  geom_ribbon(data = total_preds_df,
              mapping = aes(x = total_crimes$YrMon[157:nrow(X)],
                            ymin = Lo.95, ymax = Hi.95),
              fill = "#f1c232",
              alpha = 0.3) +
  geom_ribbon(data = total_preds_df,
              mapping = aes(x =total_crimes$YrMon[157:nrow(X)],
                            ymin = Lo.90, ymax = Hi.90),
              fill = "#f1c232",
              alpha = 0.5) +
  geom_line(data = total_crimes,
            mapping = aes(x = YrMon, y = num_crimes),
            color = "#4c453f") +
  geom_line(data = total_preds_df,
            mapping = aes(x = dom_crimes$YrMon[157:nrow(X)],
                          y = Point.Forecast),
            color = "#c2272d",
            linewidth = .8) +
  theme_bw() +
  labs(
    title = "Cross Validated Forecast Violent Crimes",
    subtitle = paste("Root Predictive Mean Square Error: ", 
                     round(rpmse_cv, 3)),
    x = "Year",
    y = "Number of Crimes"
  )
ggsave("cv_total.png", cv_error, path = "plots",
       width = 6.88, height = 3.53, units = "in")

## Generating a Heatmap of the data
library(tidyverse)
library(ggmap)
library(ggimage)
library(RColorBrewer)
library(ggpubr)
library(png)

lower_right <- c(41.859480, -87.676057)
upper_left <- c(41.909538, -87.755846)

crimes %>%
  select(Latitude, Longitude) 
  arrange(Longitude)

map_bounds <- c(left = -87.74449, bottom = 41.866399, right = -87.6857, top = 41.903)

new_crimes <- crimes %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  filter(Latitude >= 41.866399, Latitude <= 41.903,
         Longitude >= -87.74449, Longitude <= -87.6857) %>%
  filter(Primary_Type %in% violent_crimes)

map_image <- readPNG("new_map_image.png")
ggplot() +
  background_image(map_image) +
  geom_point(data = new_crimes,
             aes(x = Longitude, y = Latitude, color = `Primary Type`),
             size = 0.01) +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0), limits = c(-87.745664, -87.6857)) +  # Adjust x-axis expansion
  scale_y_continuous(expand = c(0, 0), limits = c(41.866399, 41.903)) +
  labs(
    title = "Violent Crimes in District 11"
  )


new_crimes <- crimes %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  filter(Latitude >= 41.859480, Latitude <= 41.909538,
         Longitude >= -87.755846, Longitude <= -87.676057) %>%
  filter(Primary_Type %in% violent_crimes)
map_image <- readPNG("expanded_map.png")
boundries <- vroom::vroom("data/district_boundries.csv")
bars <- vroom::vroom("data/bars.csv")
map_plot <- ggplot() +
  background_image(map_image) +
  geom_raster(data = new_crimes, stat = "density2d",
              aes(x = Longitude, y = Latitude, fill = after_stat(density),
                  alpha = after_stat(density))) +
  scale_fill_distiller(palette="Spectral",na.value=NA) +
  geom_polygon(data = boundries,
               mapping = aes(x = Lon, y = Lat),
               linewidth = 1, fill = "transparent", color = "black") +
  geom_image(mapping = aes(x = -87.70531, y = 41.87383, 
                           image = "police-station.png"),
             size = 0.05) +
  geom_image(data = bars,
             mapping = aes(x = Lon, y = Lat, image = "cheers.png"),
             size = 0.05) +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0), limits = c(-87.755846, -87.676057)) +  # Adjust x-axis expansion
  scale_y_continuous(expand = c(0, 0), limits = c(41.859480, 41.909538)) +
  labs(
    title = "Violent Crimes in District 11"
  )

map_plot <- ggplot() +
  background_image(map_image) +
  geom_raster(data = new_crimes, stat = "density2d",
              aes(x = Longitude, y = Latitude, fill = after_stat(density),
                  alpha = after_stat(density))) +
  scale_fill_distiller(palette="Spectral",na.value=NA) +
  geom_image(mapping = aes(x = -87.70531, y = 41.87383, 
                           image = "police-station.png"),
             size = 0.05) +
  geom_image(data = bars,
             mapping = aes(x = Lon, y = Lat, image = "cheers.png"),
             size = 0.05) +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0), limits = c(-87.745664, -87.6857)) +  # Adjust x-axis expansion
  scale_y_continuous(expand = c(0, 0), limits = c(41.866399, 41.903)) +
  labs(
    title = "Heat Map of Crimes in District 11"
  )
ggsave("bars_map_plot.png", map_plot, path = "plots", width = 7, height = 5)

map_plot <- ggplot() +
  xlim(-87.74449, -87.6857) +
  ylim(41.866399, 41.903) +
  geom_point(data = new_crimes, aes(x = Longitude, y = Latitude),
             alpha = 0.5) +
  geom_raster(data = new_crimes, stat = "density2d",
              aes(x = Longitude, y = Latitude, fill = after_stat(density)),
              alpha = 0.8) +
  stat_density2d(data = new_crimes, aes(x = Longitude, y = Latitude),
                 color = "black") +
  geom_image(mapping = aes(x = -87.7053313, y = 41.87398905488389, 
                           image = "police-station.png"),
             size = 0.1) +
  scale_fill_distiller(palette="Spectral",na.value=NA) +
  theme_bw() +
  labs(
    title = "Heat Map of Crimes in District 11"
  )
ggsave("map_plot.png", map_plot, path = "plots")

## Making a plot of the number of crimes during holidays vs not

tail(holiday)
boxplot_holiday <- day_crimes %>%
  filter(year(Act_date) < 2024) %>%
  left_join(., holiday, by = join_by(Act_date == Date)) %>%
  mutate(Holiday = if_else(!is.na(Holiday), "Yes", "No")) %>%
  ggplot() +
  geom_boxplot(aes(x = Holiday, y = num_crimes, fill = Holiday)) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(
    title = "Number of Violent Crimes on Holidays",
    y = "Number of Crimes"
  )
ggsave("holiday_boxplot.png", boxplot_holiday, path = "plots")


time_day_data <- crimes %>%
  mutate(is_violent = Primary_Type %in% violent_crimes) %>%
  select(is_violent, Date) %>%
  mutate(hour = hour(Date)) %>%
  mutate(time_day = case_when(hour < 4 | hour == 24 ~ "12am-4am",
                              hour < 9 ~ "4am-9am",
                              hour < 12 ~ "9am-12pm",
                              hour < 16 ~ "12pm-4pm",
                              hour < 20 ~ "4pm-8pm",
                              hour < 24 ~ "8pm-12am")) %>%
  mutate(time_day = factor(time_day, 
                           levels = c("4am-9am", "9am-12pm", "12pm-4pm",
                                      "4pm-8pm", "8pm-12am", "12am-4am")))

true_prop <- sum(time_day_data$is_violent) / nrow(time_day_data)
time_day_plot <- time_day_data %>%
  group_by(time_day) %>%
  summarise(Prop_Crimes =  (sum(is_violent) / n())) %>%
  mutate(percent_dev = 100*(Prop_Crimes - true_prop) / true_prop) %>%
  mutate(color = if_else(percent_dev < 0, "greater", "less")) %>%
  ggplot(mapping = aes(x = time_day, y = percent_dev, fill = color)) +
  geom_col() +
  scale_fill_manual(values = c("#4c453f", "#c2272d")) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(
    title = "Percent Deviation from True Proportion of Violent Crimes (0.312)",
    x = "Time of Day",
    y = "Percent Deviation"
  )
ggsave("time_day_bar.png", time_day_plot, path = "plots",
       width = 6, height = 4)
