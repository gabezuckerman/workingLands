library(data.table)
library(lubridate)
library(tidyverse)
library(sf)

#reading in all bursts
bursts <- fread("C:/Users/MiddletonLab/Desktop/Gabe/Box Sync/Elk/Working Lands/burstsCleanedSubsetBothWintersLabeled.csv")
bursts$acquisition_time <- ymd_hms(bursts$acquisition_time)

#converting all to utm so coords are in meters
bursts <- bursts %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% st_transform(32612)
coords <- st_coordinates(bursts)
bursts <- bursts %>% st_drop_geometry() %>% cbind(coords)

#taking one point per day per elkYear
bursts <- bursts %>%
  mutate(yday = paste0(year(acquisition_time), yday(acquisition_time)),
         date = as_date(acquisition_time)) %>%
  distinct(elkYear, yday, .keep_all = T) %>% 
  arrange(acquisition_time) 

#selecting only the relevant columns to make loading easier
bursts <- bursts %>% select(elkYear, date, strategy, X, Y, elevation)

fwrite(bursts, "burstsForShiny.csv")



ind <- bursts %>% filter(elkYear == "10_1")
ind %>% mutate(distance = dist())


























