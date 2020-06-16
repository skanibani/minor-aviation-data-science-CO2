library(tidyverse)
library(ggfortify)
library(lubridate)
library(forecast)
library(astsa)
library(tsoutliers)
library(timetk)

library(ggthemes)
library(gridExtra)

set_CO2_EU_2017_2019 <- read_csv("export/CO2_EU_2017_2019.csv")
set_CO2_EU_2020 <- read_csv("export/CO2_EU_2020.csv")

total_CO2_EU_2019 <- set_CO2_EU_2017_2019 %>% 
  filter(flight_day >= ymd("2019-01-01") & flight_day <= ymd("2019-12-31")) %>% 
  summarize(CO2 = sum(CO2))

ts_daily_ATM_2020 <- set_CO2_EU_2020 %>% 
  select(flight_day) %>% 
  filter(flight_day >= ymd("2018-01-01")) %>% 
  group_by(flight_day) %>% 
  summarize(daily_ATM = n()) %>% 
  select(daily_ATM) %>% 
  ts(start = c(2020, 1, 1), frequency = 365) %>% 
  tsclean(replace.missing = FALSE, lambda = "auto")

plot(ts_daily_ATM_2020)

ts_daily_ATM_2018_2019 <- set_CO2_EU_2017_2019 %>% 
  select(flight_day) %>% 
  filter(flight_day >= ymd("2018-01-01") & flight_day <= ymd("2019-12-31")) %>% 
  group_by(flight_day) %>% 
  summarize(daily_ATM = n()) %>% 
  select(daily_ATM) %>% 
  ts(start = c(2018, 1, 1), frequency = 365) %>% 
  tsclean(replace.missing = FALSE, lambda = "auto")

plot(ts_daily_ATM_2018_2019)


acf2(diff(diff(log(ts_daily_ATM_2018_2019))), max.lag = 365)

# auto.arima(diff(diff(log(ts_daily_ATM_2018_2019))))

sarima(log(ts_daily_ATM_2018_2019), 3, 1, 3, 1, 0, 0, 7)
forecast_daily_ATM_2020 <- sarima.for(ts_daily_ATM_2018_2019, 3, 1, 3, 1, 0, 0, 31, n.ahead = 365)[[1]]
forecast_daily_ATM_2020 <- forecast_daily_ATM_2020

ts_daily_ATM_2018_2020 <- ts(c(ts_daily_ATM_2018_2019, forecast_daily_ATM_2020), start = c(2018, 1, 1), frequency = 365)
ts_daily_real_ATM_2018_2020 <- ts(c(ts_daily_ATM_2018_2019, ts_daily_ATM_2020), start = c(2018, 1, 1), frequency = 365)

plot(ts_daily_real_ATM_2018_2020)

# Plot
plot_1_ts_forecast <- tk_tbl(ts_daily_ATM_2018_2020, rename_index = "date") %>% 
  mutate(date = date_decimal(date),
         date = round_date(date, "second"),
         date = as_date(date)) %>% 
  rename(forecast = value)

plot_1_ts_real <- tk_tbl(ts_daily_real_ATM_2018_2020, rename_index = "date") %>% 
  mutate(date = date_decimal(date),
         date = round_date(date, "second"),
         date = as_date(date)) %>% 
  rename(real = value)

plot_1_combined <-plot_1_ts_forecast %>% 
  left_join(plot_1_ts_real, by = "date") %>% 
  pivot_longer(c("real", "forecast"), names_to = "data_source", values_to = "ATM") %>% 
  mutate(data_source = as.factor(data_source))

plot_1 <- plot_1_combined %>% 
  filter(date >= ymd("2019-06-01")) %>% 
  ggplot(aes(date, ATM, col = data_source, group = data_source)) +
  geom_line() +
  xlim(ymd("2019-10-01"), ymd("2020-04-20")) +
  ggtitle("Daily ATMs from and to European airports") +
  theme_classic() +
  theme(legend.justification = c(0, 0),
        legend.position = c(0.05, 0.05),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))
  

# CO2

ts_daily_CO2_2020 <- set_CO2_EU_2020 %>% 
  select(flight_day, CO2) %>% 
  group_by(flight_day) %>% 
  summarize(daily_CO2 = sum(CO2)) %>% 
  select(daily_CO2) %>% 
  ts(start = c(2020, 1, 1), frequency = 365) %>% 
  tsclean(replace.missing = FALSE, lambda = "auto")

plot(ts_daily_CO2_2020)

ts_daily_CO2_2018_2019 <- set_CO2_EU_2017_2019 %>% 
  select(flight_day, CO2) %>% 
  filter(flight_day >= ymd("2018-01-01") | flight_day <= ymd("2019-12-31")) %>% 
  group_by(flight_day) %>% 
  summarize(daily_CO2 = sum(CO2)) %>% 
  select(daily_CO2) %>% 
  ts(start = c(2018, 1, 1), end = c(2020), frequency = 365) %>% 
  tsclean(replace.missing = FALSE, lambda = "auto")

plot(ts_daily_CO2_2018_2019)

# auto.arima(ts_daily_CO2_2018_2019)

sarima(ts_daily_CO2_2018_2019, 2, 1, 1, 0, 1, 0, 7)
forecast_daily_CO2_2020 <- sarima.for(ts_daily_CO2_2018_2019, 2, 1, 1, 0, 1, 0, 31, n.ahead = 365)[[1]]

ts_daily_CO2_2018_2020 <- ts(c(ts_daily_CO2_2018_2019, forecast_daily_CO2_2020), start = c(2018, 1, 1), frequency = 365)
ts_daily_real_CO2_2018_2020 <- ts(c(ts_daily_CO2_2018_2019, ts_daily_CO2_2020), start = c(2018, 1, 1), frequency = 365)




# Plot
plot_2_ts_forecast <- tk_tbl(ts_daily_CO2_2018_2020, rename_index = "date") %>% 
  mutate(date = date_decimal(date),
         date = round_date(date, "second"),
         date = as_date(date)) %>% 
  rename(forecast = value)

plot_2_ts_real <- tk_tbl(ts_daily_real_CO2_2018_2020, rename_index = "date") %>% 
  mutate(date = date_decimal(date),
         date = round_date(date, "second"),
         date = as_date(date)) %>% 
  rename(real = value)

plot_2_combined <-plot_2_ts_forecast %>% 
  left_join(plot_2_ts_real, by = "date") %>% 
  pivot_longer(c("real", "forecast"), names_to = "data_source", values_to = "CO2") %>% 
  mutate(data_source = as.factor(data_source)) %>% 
  mutate(CO2 = ifelse(is.na(CO2), NA, CO2 / 1000))

plot_2 <- plot_2_combined %>% 
  filter(date >= ymd("2019-06-01")) %>% 
  ggplot(aes(date, CO2, col = data_source, group = data_source)) +
  geom_line() +
  xlim(ymd("2019-10-01"), ymd("2020-04-20")) +
  theme_classic() +
  ggtitle("Daily CO2 of flights from and to European airports") +
  xlab("Date") +
  ylab("CO2 in tonne") +
  theme(legend.justification = c(0, 0),
        legend.position = c(0.05, 0.05),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))

grid.arrange(plot_1, plot_2)

csv_write
