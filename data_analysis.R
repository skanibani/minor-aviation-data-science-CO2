library(tidyverse)
library(lubridate)
library(extremevalues)

library(future)
library(furrr)

plan(multiprocess)


remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}



# TODO
# 1. Per vliegtuig rol een tijdreeks van CO2 tussen 2019 en 2020

set_CO2_EU_2017_2019 <- read_csv("export/CO2_EU_2017_2019.csv")
set_CO2_EU_2020 <- read_csv("export/CO2_EU_2020.csv")

set_CO2_EU_2017_2020 <- set_CO2_EU_2017_2019 %>% 
  bind_rows(set_CO2_EU_2020)

plot_31_5 <- set_CO2_EU_2017_2020 %>%
  mutate(aircraft_type = as.factor(aircraft_type),
         aircraft_role = as.factor(aircraft_role)) %>% 
  select(flight_day, aircraft_type, aircraft_role, CO2) %>% 
  drop_na() %>% 
  group_by(flight_day, aircraft_role) %>% 
  summarize(CO2_daily = sum(CO2)) %>% 
  ungroup() %>% 
  group_by(aircraft_role) %>% 
  mutate(CO2_daily = remove_outliers(CO2_daily)) %>% 
  ungroup() %>% 
  arrange(flight_day) %>% 
  filter(CO2_daily > 0) %>% 
  ggplot(aes(flight_day, CO2_daily)) +
  geom_smooth(se = FALSE) +
  facet_wrap( ~ aircraft_role, scales = "free") +
  ggtitle("Daily CO2 emissions from flights from and to Europe") +
  xlab("Date") +
  ylab("Daily CO2 (kg)") +
  theme_minimal()

plot_31_5


# 2. Per vliegtuig type laten zien hoeveel CO2 zij tussen 2019 en 2020 uitstoten.

plot_6 <- set_CO2_EU_2017_2020 %>% 
  filter(flight_day >= ymd("2019-01-01") & flight_day <= ymd("2019-12-31")) %>% 
  group_by(aircraft_type) %>% 
  summarize(CO2_yearly = sum(CO2)) %>% 
  arrange(desc(CO2_yearly)) %>% 
  mutate(CO2_yearly = CO2_yearly/1000000) %>% 
  mutate(CO2_yearly = round(CO2_yearly)) %>% 
  rename(`Total CO2 (x10^6 kg)` = CO2_yearly,
         `Aircraft Type` = Type)


plot_2 <- set_CO2_EU_2017_2019 %>% 
  select(flight_day, CO2) %>% 
  drop_na() %>% 
  group_by(flight_day) %>% 
  summarize(flights = n(), CO2_daily = sum(CO2))

plot_2 %>% 
  ggplot(aes(flight_day, flights)) +
  geom_point()
