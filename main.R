#packages
x <- c("rworldmap", "geosphere", "ggmap", "tidyverse", "purrr", "readxl", "rnaturalearth", 
       "sf", "lwgeom", "mapview", "rvest", "shiny", "shinydashboard", "shiny", "shinythemes",
       "lubridate", "future", "furrrr") 

# Installeer de packages die niet geinstalleerd zijn 
inst <- !(x %in% installed.packages())
install.packages(x[.inst], dependencies = TRUE)
# Activate needed packages
lapply(x, require, character.only = T)
remove(x)
plan(multiprocess)

#importing flight data for januari, februari,  march and april 2020
import_flightlist_jan <- read_csv("data/flightlist_20200101_20200131.csv", 
                                  col_types = cols(
                                    firstseen = col_datetime(format = "%Y-%m-%d %H:%M:%S+00:00"),
                                    lastseen = col_datetime(format = "%Y-%m-%d %H:%M:%S+00:00"),
                                    day = col_date(format = "%Y-%m-%d %H:%M:%S+00:00")
                                  ))


import_flightlist_feb <- read_csv("data/flightlist_20200201_20200229.csv", 
                                  col_types = cols(
                                    firstseen = col_datetime(format = "%Y-%m-%d %H:%M:%S+00:00"),
                                    lastseen = col_datetime(format = "%Y-%m-%d %H:%M:%S+00:00"),
                                    day = col_date(format = "%Y-%m-%d %H:%M:%S+00:00")
                                  ))

import_flightlist_mar <- read_csv("data/flightlist_20200301_20200331.csv", 
                                  col_types = cols(
                                    firstseen = col_datetime(format = "%Y-%m-%d %H:%M:%S+00:00"),
                                    lastseen = col_datetime(format = "%Y-%m-%d %H:%M:%S+00:00"),
                                    day = col_date(format = "%Y-%m-%d %H:%M:%S+00:00")
                                  ))
import_flightlist_apr <- read_csv("data/flightlist_20200401_20200430.csv", 
                                  col_types = cols(
                                    firstseen = col_datetime(format = "%Y-%m-%d %H:%M:%S+00:00"),
                                    lastseen = col_datetime(format = "%Y-%m-%d %H:%M:%S+00:00"),
                                    day = col_date(format = "%Y-%m-%d %H:%M:%S+00:00")
                                  ))

#importing a list with airports and their location
airport_list <- read_csv("data/airports_big_list.csv")
airport_list <- select(airport_list, "ICAO", "name", 
                       "latitude_deg", "longitude_deg", "elevation_ft", "continent")

#importing a list of every airplane in the world and their details
import_CAPA_fleet <- read_excel("data/CAPA_Fleet2.xlsx")

#importing a lsit with the fuelburn of every aircraft
import_EEA_fuel_burn <- read_xlsx("data/fuelburn.xlsx", sheet = 2)
import_fuel_burn <- read_xlsx("data/aircraft-fuel-burn.xlsx")

#select the data needed for the project
set_CAPA_fleet <- import_CAPA_fleet %>% 
  select(`Tail/Registration Number`, starts_with("Engine"), starts_with("Aircraft")) %>% 
  rename(Registration = `Tail/Registration Number`)

#bind all the dataframe together   
set_flightlist_jan_apr <- import_flightlist_jan %>%
  bind_rows(import_flightlist_feb) %>% 
  bind_rows(import_flightlist_mar) %>% 
  bind_rows(import_flightlist_apr) %>% 
  left_join(airport_list, by = c("origin" = "ICAO")) %>%
  left_join(airport_list, by = c("destination" = "ICAO"))

#rename the collums to normal names
set_flightlist_jan_apr <- set_flightlist_jan_apr %>% 
  rename(
    Departure_name = name.x,
    Departure_lat = latitude_deg.x,
    Departure_long = longitude_deg.x,
    Departure_elevation = elevation_ft.x,
    Departure_continent = continent.x,
    Arrival_name = name.y,
    Arrival_lat = latitude_deg.y,
    Arrival_long = longitude_deg.y,
    Arrival_elevation = elevation_ft.y,
    Arrival_continent = continent.y)

#filter on flight to EU or from EU and filter out flight with the same destination as depature airpore
set_flightlist_jan_apr <- set_flightlist_jan_apr %>% 
  filter(Origin_continent == "EU"| Destination_continent == "EU") %>% 
  filter(ifelse(origin == destination, T, F) == F)

#create a database with callsign and their most use aircraft type
Callsigns <- set_flightlist_jan_apr %>% 
  select(callsign, registration,  typecode, origin, destination) %>%
  drop_na() %>% 
  distinct(callsign, .keep_all = TRUE)

set_flightlist_jan_apr <- set_flightlist_jan_apr %>% 
  left_join(Callsigns, by = "callsign")

#remove the unnessesarry collums
set_flightlist_jan_apr <- set_flightlist_jan_apr %>% 
  mutate(typecode.x = ifelse(is.na(typecode.x), typecode.y, typecode.x)) %>% 
  mutate(registration.x = ifelse(is.na(registration.x), registration.y, registration.x)) %>% 
  select(-c(typecode.y, origin.y, destination.y, registration.y))

#base url for webscrapping
url <- "https://planefinder.net/data/flight/"

missing_data <- set_flightlist_jan_apr %>% 
  filter(ifelse(is.na(typecode.x), T, F) == T) %>% 
  distinct(callsign, .keep_all = TRUE)

Callsign_online <- read.csv("data/Callsign_online.csv")

missing_data1 <- missing_data %>% 
  anti_join(Callsign_online, by = "callsign")

Search_online <- missing_data %>% 
  select(callsign, typecode.x) %>% 
  mutate(link = paste0(url, callsign))



rows <- 1:nrow(missing_data)

# #webscrapping from planefinder.net
# map_df(rows, function(i) {
#   
#   cat(".")
#   
#   data <- read_html(Search_online$link[i])
#   
#   data.frame(a = html_text(html_nodes(data, ".value")[1]),
#              stringsAsFactors=FALSE)
#   
# }) -> Search_online$typecode.x

# Callsign_online <- Search_online %>% 
#   mutate(Unknown = ifelse(typecode.x$aircraft == "N/A", NA, typecode.x$aircraft)) %>% 
#   select(callsign, Unknown)
# 
# write.csv(Callsign_online, file = "data/Callsign_online.csv")



airport_distance_nm <- function(long_1, lat_1, long_2, lat_2) {
  distHaversine(c(long_1, lat_1), c(long_2, lat_2), r = 6378137) * 0.000539956803
}

tb_map <- tibble(long_1 = set_flightlist_jan_apr$Departure_Long,
                 lat_1 = set_flightlist_jan_apr$Departure_Lat,
                 long_2 = set_flightlist_jan_apr$Arrival_Long,
                 lat_2 = set_flightlist_jan_apr$Arrival_Lat)

# Purr
set_distance <- future_pmap_dbl(tb_map, airport_distance_nm, .progress = TRUE)

set_flightlist_jan_apr <- set_flightlist_jan_apr %>% 
  mutate(Distance = set_distance)

CAPA_reg_aircraft_ICAO <- set_CAPA_fleet %>% 
  select(Registration, `Aircraft Variant ICAO Code`, `Role`) %>% 
  drop_na(Registration, `Aircraft Variant ICAO Code`)

set_flightlist_jan_apr <- set_flightlist_jan_apr %>% 
  inner_join(CAPA_reg_aircraft_ICAO, by = "Registration")

set_EEA_fuelburn <- import_EEA_fuel_burn %>% 
  pivot_longer(-Type, names_to = "Distance", values_to = "Fuelburn") %>% 
  mutate(`Distance` = as.numeric(`Distance`))


func_fuelburn_model <- function(df) {
  lm(`Fuelburn` ~ poly(`Distance`, 3, raw = TRUE), data = df)
}

set_EEA_fuelburn <- set_EEA_fuelburn %>% 
  drop_na() %>% 
  group_by(Type) %>% 
  nest()

calc_fuelburn <- set_EEA_fuelburn %>% 
  mutate(Model = future_map(`data`, func_fuelburn_model))

func_fuelburn <- function(lm, x) {
  coef(lm)[[1]] + coef(lm)[[2]] * x + coef(lm)[[3]] * x + coef(lm)[[4]]
}

calc_CO2_jan_apr <- set_flightlist_jan_apr %>% 
  inner_join(calc_fuelburn, by = c("Aircraft Variant ICAO Code" = "Type")) %>% 
  mutate(Fuelburn = future_map2_dbl(Model, Distance, func_fuelburn)) %>% 
  mutate(CO2 = Fuelburn * 3.16) %>% 
  select(Date, Registration, `Aircraft Variant ICAO Code`, Role, Departure, Arrival, Distance, Fuelburn, CO2) %>% 
  mutate(Role = as.factor(Role)) %>% 
  rename(`Aircraft ICAO` = `Aircraft Variant ICAO Code`, `Distance[NM]` = Distance, `Fuelburn[KG]` = Fuelburn, `CO2[KG]` = CO2)

plot_1 <- calc_CO2_jan_apr %>% 
  drop_na() %>% 
  group_by(Date, Role) %>% 
  summarize(`Cumulative CO2[KG]` = sum(`CO2[KG]`)) %>% 
  ggplot(aes(`Date`, `Cumulative CO2[KG]`, group = Role, color = Role)) +
  geom_smooth()

plot_1
