library(tidyverse)
library(readxl)
library(lubridate)
library(geosphere)

library(future)
library(furrr)

library(tictoc)
library(fs)

plan(multiprocess)

# Functions---------------------------------------------------------------------

# Find the great circle distance between two points in nmi.
airport_distance_nm <- function(long_1, lat_1, long_2, lat_2) {
  distHaversine(c(long_1, lat_1), c(long_2, lat_2), r = 6378137) * 0.000539956803
}

# Create model for each different aircraft type.
func_fuelburn_model <- function(df) {
  lm(`fuelburn` ~ poly(`distance`, 3, raw = TRUE), data = df)
}

# Calculate the fuelburn for a given distance in nmi.
func_fuelburn <- function(lm, x) {
  coef(lm)[[1]] + coef(lm)[[2]] * x + coef(lm)[[3]] * x + coef(lm)[[4]]
}

func_fuelburn_wiki <- function(x, l) {
  return(x * l)
}

rename_factor <- function(x) {
  if (x == "AEW/SAR/Patrol") {
    return("Non Commercial")
  } else if (x == "Combi") {
    return("Combi")
  } else if (x == "Conv Pax/Freight") {
    return("Combi")
  } else if (x == "Corporate/Government") {
    return("Non Commercial")
  } else if (x == "Freighter") {
    return("Cargo")
  } else if (x == "Medical Evacuation") {
    return("Non Commercial")
  } else if (x == "Military Freighter") {
    return("Non Commercial")
  } else if (x == "Military Tanker") {
    return("Non Commercial")
  } else if (x == "Military/Passenger/Trainer/Utility") {
    return("Non Commercial")
  } else if (x == "Passenger") {
    return("Passenger")
  } else if (x == "R & D") {
    return("Non Commercial")
  } else if (x == "Utility") {
    return("Non Commercial")
  }
}

# Data Collection --------------------------------------------------------------
import_flightlist_2017_2019 <- future_map_dfr(dir_ls("data", regexp = "flightlist_.*"),
                                         read_csv, .progress = TRUE)

import_opensky_aircrafts <- read_csv("data/aircraft_database.csv")
import_airports <- read_csv("data/airports_big_list.csv")
import_CAPA_fleet <- read_csv("data/CAPA-fleet-CO2.csv", skip = 10)
import_EEA_fuel_burn <- read_xlsx("data/fuelburn.xlsx", sheet = 2)

import_wiki_fuelburn <- read_xlsx("data/fuelburn_wiki.xlsx")

# Data Memory ------------------------------------------------------------------

set_airports_region <- import_airports %>% 
  select(`ICAO`, `continent`)

set_flightlist_EU_2017_2019 <- import_flightlist_2017_2019 %>% 
  drop_na(estdepartureairport, estarrivalairport) %>%
  filter(estdepartureairport != estarrivalairport) %>% 
  left_join(set_airports_region, by = c("estdepartureairport" = "ICAO")) %>% 
  rename(departure_continent = continent) %>% 
  left_join(set_airports_region, by = c("estarrivalairport" = "ICAO")) %>% 
  rename(arrival_continent = continent) %>% 
  mutate(departure_continent = as.factor(departure_continent), arrival_continent
         = as.factor(arrival_continent)) %>% 
  filter(departure_continent == "EU" | arrival_continent == "EU") %>% 
  select(-departure_continent, -arrival_continent)

remove(import_flightlist_2017_2019)
remove(set_airports_region)

# Data Cleaning ----------------------------------------------------------------
set_aircrafts <- import_opensky_aircrafts %>% 
  select(icao24, registration)

set_airports <- import_airports %>% 
  select(ICAO, longitude_deg, latitude_deg)

set_CAPA_fleet <- import_CAPA_fleet %>% 
  select(`Tail/Registration Number`, `Role`, starts_with("Engine"), starts_with("Aircraft")) %>% 
  rename(Registration = `Tail/Registration Number`)

CAPA_reg_aircraft_ICAO <- set_CAPA_fleet %>% 
  select(Registration, `Aircraft Variant ICAO Code`, `Role`) %>% 
  drop_na(Registration, `Aircraft Variant ICAO Code`)

CAPA_type_manu <- set_CAPA_fleet %>% 
  select(`Aircraft Variant ICAO Code`, `Aircraft Manufacturer`) %>% 
  drop_na() %>% 
  distinct()

set_wiki_fuelburn <- import_wiki_fuelburn %>% 
  select(Type_Code, `Fuel burn kg/nm`, `Minimum range nm`, `maximum range nm`) %>% 
  anti_join(import_EEA_fuel_burn, by = c("Type_Code" = "Type")) %>% 
  pivot_longer(cols = c("Minimum range nm", "maximum range nm"), values_to = "distance") %>% 
  select(Type_Code, `Fuel burn kg/nm`) %>% 
  group_by(Type_Code) %>% 
  filter(`Fuel burn kg/nm` == max(`Fuel burn kg/nm`)) %>% 
  distinct() %>% 
  rename(fuelburn = `Fuel burn kg/nm`, Type = `Type_Code`)

set_EEA_fuelburn <- import_EEA_fuel_burn %>% 
  pivot_longer(-Type, names_to = "distance", values_to = "fuelburn") %>% 
  mutate(`distance` = as.numeric(`distance`)) %>% 
  drop_na() %>% 
  group_by(Type) %>% 
  nest()

plot_31_EEA_fuelburn <- import_EEA_fuel_burn %>% 
  pivot_longer(-Type, names_to = "distance", values_to = "fuelburn") %>% 
  mutate(`distance` = as.numeric(`distance`)) %>% 
  drop_na() %>% 
  left_join(CAPA_type_manu, by = c("Type" = "Aircraft Variant ICAO Code")) %>% 
  mutate(`Aircraft Manufacturer` = ifelse(Type == "B350", "Beech Aircraft Corporation",
                                   ifelse(Type == "B77W", "Boeing",
                                   ifelse(Type == "BE20", "Beech Aircraft Corporation",
                                   ifelse(Type == "C550", "Cessna Aerospace",
                                   ifelse(Type == "F2TH", "Dassault Aviation",
                                   ifelse(Type == "RJ85", "Avro", `Aircraft Manufacturer`))))))) %>% 
  rename(type = Type, manu = `Aircraft Manufacturer` ) %>% 
  select(type, manu, distance, fuelburn)


write_csv(plot_31_EEA_fuelburn, "export/plot_31_EEA_fuelburn.csv")

plot_31_wiki_fuelburn <- set_wiki_fuelburn %>% 
  filter(str_detect(Type, "^A3") | str_detect(Type, "^B7")) %>% 
  arrange(Type)

write_csv(plot_31_wiki_fuelburn, "export/plot_31_wiki_fuelburn.csv")
  
  # pivot_wider(names_form = c("Minimum range nm", "maximum range nm"), values_from = "Fuel burn kg/nm", values_fn = "list")
  

tic()
set_flightlist_EU_2017_2019 <- set_flightlist_EU_2017_2019 %>% 
  left_join(set_aircrafts, by = c("icao24" = "icao24")) %>% 
  mutate(day = as_datetime(day),
         firstseen = as_datetime(firstseen),
         lastseen = as_datetime(lastseen)) %>% 
  select(day, icao24, registration, callsign,
         estdepartureairport, estarrivalairport, firstseen, lastseen) %>% 
  rename(departure = estdepartureairport, arrival = estarrivalairport) %>% 
  arrange(firstseen) %>% 
  left_join(set_airports, by = c("departure" = "ICAO")) %>% 
  rename(departure_long = longitude_deg, departure_lat = latitude_deg) %>% 
  left_join(set_airports, by = c("arrival" = "ICAO")) %>% 
  rename(arrival_long = longitude_deg, arrival_lat = latitude_deg) %>%
  drop_na(departure, arrival) %>% 
  mutate(distance = future_pmap_dbl(tibble(long_1 = departure_long,
                                          lat_1 = departure_lat,
                                          long_2 = arrival_long,
                                          lat_2 = arrival_lat), airport_distance_nm, .progress = TRUE))
toc()

# Improve data quality with official sources and rearange dataframe for further
# calculations.
set_flightlist_EU_2017_2019 <- set_flightlist_EU_2017_2019 %>% 
  inner_join(CAPA_reg_aircraft_ICAO, by = c("registration" = "Registration")) %>% 
  rename(flight_day = day,
         flight_firstseen = firstseen,
         flight_lastseen = lastseen,
         aircraft_ICAO24 = icao24,
         aircraft_registration = registration,
         aircraft_type = `Aircraft Variant ICAO Code`,
         aircraft_role = Role,
         flight_departure = departure,
         flight_arrival = arrival,
         flight_distance = distance) %>% 
  select(flight_day, flight_firstseen, flight_lastseen, aircraft_ICAO24,
         aircraft_registration, aircraft_type, aircraft_role, flight_departure,
         flight_arrival, flight_distance) %>% 
  distinct()

# New calculation --------------------------------------------------------------

set_wiki <- set_wiki_fuelburn %>% 
  rename(aircraft_type = `Type`) %>% 
  select(aircraft_type) %>% 
  distinct()

set_EEA <- set_EEA_fuelburn %>% 
  rename(aircraft_type = `Type`) %>% 
  select(aircraft_type) %>% 
  distinct()

# 0. Split flights
set_poly_2017_2019 <- set_flightlist_EU_2017_2019 %>% 
  inner_join(set_EEA, by = "aircraft_type")

set_scalar_2017_2019 <- set_flightlist_EU_2017_2019 %>% 
  inner_join(set_wiki, by = "aircraft_type")

remove(set_flightlist_EU_2017_2019)

# 1. Polynomial model aircrafts
# Calculate fuelburn model for each aircraft ICAO type.
set_poly_aircraft_fuelburn <- set_EEA_fuelburn %>%
  mutate(model = future_map(`data`, func_fuelburn_model, .progress = TRUE)) %>% 
  rename(type = Type)

calc_poly_CO2_EU_2017_2019 <- set_poly_2017_2019 %>% 
  inner_join(set_poly_aircraft_fuelburn, by = c("aircraft_type" = "type")) %>% 
  mutate(aircraft_fuelburn = map2_dbl(model, flight_distance, func_fuelburn)) %>% 
  mutate(CO2 = aircraft_fuelburn * 3.16) %>% 
  mutate(aircraft_role = as.factor(aircraft_role)) %>% 
  select(-data, -model)

# 2. Scalar value aircrafts
calc_scalar_CO2_EU_2017_2019 <- set_scalar_2017_2019 %>% 
  inner_join(set_wiki_fuelburn, by = c("aircraft_type" = "Type")) %>% 
  mutate(aircraft_fuelburn = flight_distance * fuelburn) %>% 
  mutate(CO2 = aircraft_fuelburn * 3.16) %>% 
  mutate(aircraft_role = as.factor(aircraft_role)) %>% 
  select(-fuelburn)

# 3. Combine
calc_CO2_EU_2017_2019 <- calc_poly_CO2_EU_2017_2019 %>% 
  bind_rows(calc_scalar_CO2_EU_2017_2019) %>% 
  distinct()

remove(calc_poly_CO2_EU_2017_2019, calc_scalar_CO2_EU_2017_2019,
       set_poly_2017_2019, set_scalar_2017_2019)

# End new calculation ----------------------------------------------------------

calc_CO2_EU_2017_2019 <- calc_CO2_EU_2017_2019 %>% 
  mutate(aircraft_role = future_map_chr(aircraft_role, rename_factor, .progress = TRUE)) %>% 
  mutate(aircraft_role = as.factor(aircraft_role)) %>% 
  mutate(aircraft_type = as.factor(aircraft_type))

calc_CO2_EU_2017_2019 <- calc_CO2_EU_2017_2019 %>%
  drop_na() %>% 
  arrange(flight_day)
write_csv(calc_CO2_EU_2017_2019, "export/C02_EU_2017_2019.csv")



# Time series ------------------------------------------------------------------
set_date_CO2 <- calc_CO2_EU_2017_2019 %>% 
  select(flight_day, CO2) %>% 
  group_by(flight_day) %>% 
  summarize(daily_CO2 = sum(CO2))

ts_date_CO2 <- ts(set_date_CO2$daily_CO2, start = c(2017, 1), frequency = 365)

plot(diff(diff(ts_date_CO2)))
  
set_date_ATM


# remove(set_flightlist_EU_2017_2019)