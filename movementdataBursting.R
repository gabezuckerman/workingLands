library(tidyverse)
library(data.table)
library(lubridate)
library(Hmisc)
library(foreach)
library(doParallel)
library(tictoc)

setwd("C:/Users/MiddletonLab/Desktop/Gabe/Box Sync/Elk/Working Lands/")

all <- fread("gps.csv")
all$acquisition_time <- ymd_hms(all$acquisition_time)

#bursting with elk years starting in december

#winter range is dec 1- march
#summer range is july 1-sep 15

#15 days in both winter and summer ranges

getBursts <- function(id) {
  ind <- all %>% filter(gps_sensors_animals_id == id)
  startMonth <- month(min(ind$acquisition_time))
  if(startMonth < 12) {
    startYear <- year(min(ind$acquisition_time)) - 1
  } else {
    startYear <- year(min(ind$acquisition_time))
  }
  
  #creating a table that matches time intervals of jan 1 - dec 31 to elk-years
  decStart <- ymd(paste0(startYear, "-12-1"))
  numYears <- ceiling(as.numeric(difftime(date(max(ind$acquisition_time)), decStart, units = "weeks"))/52)
  dates <- seq.Date(from = decStart, by = "1 year", length.out = numYears + 1)
  
  intervals <- data.table(dates[1:numYears], lead(dates)[1:numYears]) %>% mutate(tInt = interval(V1, V2)) %>% select(tInt)
  
  ind$year <- map_dbl(ind$acquisition_time, ~which(.x %within% intervals$tInt)[1])
  
  
  
  #taking years (that start in december) that have more than 30 days of winter data
  indSum <- ind %>% filter(month(acquisition_time) %in% c(12, 1, 2, 3)) %>% 
    group_by(gps_sensors_animals_id,year) %>% 
    summarise(winterDays = length(unique(date(acquisition_time))))
  
  #finding number of days in summer range per each elk-year
  
  julyStart <-  ymd(paste0(startYear, "-07-01"))
  
  numSummers <- ceiling(as.numeric(difftime(date(max(ind$acquisition_time)), julyStart, units = "weeks"))/52)
  summerStarts <- seq.Date(from = julyStart, by = "1 year", length.out = numYears + 1)
  
  septemberEnds <- ymd(paste0(startYear, "-09-15"))
  summerEnds <- seq.Date(from = septemberEnds, by = "1 year", length.out = numYears + 1)
  
  #creating table with all summers dates
  summerIntervals <- data.table(summerStarts, summerEnds) %>%
    mutate(tInt = interval(summerStarts, summerEnds)) %>% select(tInt)
  
  #adding a summer number to each point
  ind$summer <- map_dbl(ind$acquisition_time, ~which(.x %within% summerIntervals$tInt)[1])
  
  
  #finding number of days in each summer and merging it with winter days summary
  indSum <- ind %>% group_by(gps_sensors_animals_id, year, summer) %>%
    summarise(summerDays = length(unique(date(acquisition_time)))) %>% 
    filter(!is.na(summer)) %>% merge(indSum)
  
  #finding years with at least 15 days in both winter and summer range
  indSumComplete <- indSum %>% filter(summerDays >= 15, winterDays >= 15)
  
  #checking to see if there are multiple years
  #and then returning only if the years are in order
  if (nrow(indSumComplete) > 0) {
    return(ind %>% filter(year %in% indSumComplete$year))
  }
}



cl <- makeCluster(6)
registerDoParallel(cl)

tic()
bursts <- foreach(id = unique(all$gps_sensors_animals_id),
                       .errorhandling = 'pass',
                       .packages = c('tidyverse', 'lubridate',
                                     'data.table', 'Hmisc')) %dopar%
  getBursts(id)
bursts2 <- rbindlist(bursts) 
toc()

stopCluster(cl)

fwrite(bursts2, "burstsNC.csv")
