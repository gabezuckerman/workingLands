library(data.table)
library(tidyverse)
library(lubridate)
library(Hmisc)
library(sf)
library(gtools)

#bring in gps data
allGPS <- fread("../../pgAdmin/gps.csv")

#adding herd, gps id, time

sensAni <- fread("../../pgAdmin/sensorsAnimals.csv") %>% rename(gps_sensors_animals_id = V1) %>% 
  select(gps_sensors_animals_id, gps_sensors_code, animals_code) %>% distinct(gps_sensors_code, .keep_all = T)

animals <- fread("../../pgAdmin/animals.csv") %>% select(animals_code, herd = commonHerd) %>% animals_code


allGPS <- allGPS %>% merge(sensAni, by = "gps_sensors_code", all.x = T) 

allGPS <- allGPS %>% filter(!is.na(animals_code), !is.na(longitude)) 


allGPS <- allGPS %>% select(gps_sensors_animals_id, gps_sensors_code, animals_code, acquisition_time, longitude, latitude, dop) %>%
  merge(animals, all.x = T) %>% distinct(gps_sensors_animals_id, acquisition_time, .keep_all = T)

allGPS$acquisition_time <- ymd_hms(allGPS$acquisition_time)

#removing vhf, herds not for analyses, bad timestamps
allGPS <- allGPS %>% filter(!is.na(acquisition_time), herd %nin% c("Medicine Lodge", "South Bighorn", "North Bighorn"), acquisition_time > ymd("1999-1-1"))


#renaming big creek to be part of northern
allGPS <- allGPS %>% mutate(herd = ifelse(herd == "Big Creek", "Northern", herd))


#removing points from within 10km buffer from WGFD
cody <- allGPS %>% filter(herd == "Cody") %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

#creating a 10 km buffer around cody wgfd office
wgfd <- data.table(x = -109.024067, y = 44.495414) %>% st_as_sf(coords = c("x", "y"), crs = 4326) %>%
  st_transform(32612) %>% st_buffer(10000) %>% st_transform(4326)

intersection <- st_intersects(cody, wgfd) %>% as.matrix() %>% as.data.table()

cody$atWGFD <- intersection$V1

cody <- cody %>% filter(!atWGFD)

codyCoords <- st_coordinates(cody) %>% as.data.table() %>% rename(longitude = X, latitude = Y)

cody <- cody %>% st_drop_geometry() %>% select(-atWGFD) %>% cbind(codyCoords)

#adding cleaned cody back to table
allGPS <- allGPS %>% filter(herd != "Cody") %>% smartbind(cody)

#fwrite(allGPS, "gps.csv")


#now bursting to yearlong trajectorys