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
  
  decStart <- ymd(paste0(startYear, "-12-1"))
  
  #creating a table that matches time intervals of dec 1, start - dec 31, start + 1 to elk-years
  numYears <- ceiling(as.numeric(difftime(date(max(ind$acquisition_time)), decStart, units = "weeks"))/52)
  
  decStarts <- seq.Date(from = ymd(paste0(startYear, "-12-1")), by = "1 year", length.out = numYears + 1)
  
  
  #decEnds
  decEnds <- seq.Date(from = (decStart + years(1) + months(1) - days(1)), by = "1 year", length.out = numYears + 1)
  
  intervals <- interval(decStarts, decEnds)
  
  #bc some dates have multiple years, adding duplicate gps points for both years it has
  
  assignYear <- function(i) {
    ind %>% filter(acquisition_time %within% intervals[i]) %>%
      mutate(year = i, startDateYear = year(decStarts[i])) %>% return()
  }
  
  newInd <- map_dfr(1:length(intervals), assignYear)
  
  
  #counting number of days in start winter and summer, year by year
  #only returning if number of days in summer and winter is more than 15
  check15days <- function(y) {
    #getting data from elk year
    yearData <- newInd %>% filter(year == y)
    #finding dec start year
    startDateYear <- unique(yearData$startDateYear)
    
    #finding winter and summer dates
    winterInt <- interval(ymd(paste0(startDateYear, "-12-1")), 
                          ymd(paste0(startDateYear + 1, "-3-31")))
    summerInt <- interval(ymd(paste0(startDateYear + 1, "-7-1")), 
                          ymd(paste0(startDateYear + 1, "-9-15")))
    
    #calculating number of days
    winterDays <- yearData %>% filter(acquisition_time %within% winterInt) %>% 
      distinct(as_date(acquisition_time)) %>% nrow()
    
    summerDays <- yearData %>% filter(acquisition_time %within% summerInt) %>% 
      distinct(as_date(acquisition_time)) %>% nrow()
    
    if(winterDays >= 15 & summerDays >= 15) {
      return(yearData)
    }
  }

  map_dfr(1:length(intervals), check15days) %>% return()
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

#1938 elk-years, 1269 elk
bursts2 %>% distinct(gps_sensors_animals_id)
