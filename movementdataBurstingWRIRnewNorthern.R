library(tidyverse)
library(data.table)
library(lubridate)
library(Hmisc)
library(sf)
library(mapview)
library(raster)
library(velox)


setwd("C:/Users/MiddletonLab/Desktop/Gabe/Box Sync/Elk/Working Lands")

#start ids in 2000s

northernSensAni <- fread("../movementData/newNorthern/gpsSensorsAnimals.csv")
northernGPS <- fread("../movementData/newNorthern/gps.csv")

northern <- northernSensAni %>% dplyr::select(animals_code, gps_sensors_code) %>%
  mutate(gps_sensors_animals_id = 2000 + animals_code) %>%  merge(
  northernGPS %>% dplyr::select(gps_sensors_code, acquisition_time, longitude, latitude, dop)
) %>% mutate(herd = "Northern") %>% dplyr::select(gps_sensors_animals_id, everything())


wrirSensAni <- fread("../movementData/WRIR/sensorsAnimals.csv")
wrirGPS <- fread("../movementData/WRIR/gps.csv")

wrir <- wrirSensAni %>% dplyr::select(animals_code, gps_sensors_code) %>%
  mutate(row = seq(1, nrow(.)), gps_sensors_animals_id = 3000 + row) %>%
  dplyr::select(-row) %>% merge(
    wrirGPS %>% dplyr::select(gps_sensors_code, acquisition_time, longitude, latitude, dop)
  ) %>% mutate(herd = "WRIR") %>% dplyr::select(gps_sensors_animals_id, everything())


all <- rbind(northern, wrir)



#bursting with elk years starting in december

#winter range 1 is dec 1- march
#summer range is july 1-sep 15
#winter range 2 is dec 1 - march


#15 days in both winter ranges as well as summer ranges

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
  
  
  winterStarts <- seq.Date(from = decStart, by = "1 year", length.out = numYears + 1)
  winterEnds <- seq.Date(from = ymd(paste0(startYear + 1, "-3-31")), by = "1 year", length.out = numYears + 1)
  
  # summerStarts <-  seq.Date(from = ymd(paste0(startYear + 1, "-7-1")), by = "1 year", length.out = numYears + 1)
  # summerEnds <- seq.Date(from = ymd(paste0(startYear + 1, "-9-15")), by = "1 year", length.out = numYears + 1)
  
  
  #bc some dates have multiple years, adding duplicate gps points for both years it has
  assignYear <- function(i) {
    winterInt <- interval(winterStarts[i], winterEnds[i + 1])
    ind %>% filter(acquisition_time %within% winterInt) %>%
      mutate(year = i, startDateYear = year(winterStarts[i])) %>% return()
  }
  
  newInd <- map_dfr(1:numYears, assignYear)
  
  #counting number of days in winter 1, summer and winter 2
  check15days <- function(y) {
    #getting data from elk year
    yearData <- newInd %>% filter(year == y)
    #finding dec start year
    startDateYear <- unique(yearData$startDateYear)
    
    #finding winter 1 and 2 and summer dates
    winterRange1 <- interval(ymd(paste0(startDateYear, "-12-1")), 
                             ymd(paste0(startDateYear + 1, "-3-31")))
    summerRange <- interval(ymd(paste0(startDateYear + 1, "-7-1")), 
                            ymd(paste0(startDateYear + 1, "-9-15")))
    winterRange2 <- interval(ymd(paste0(startDateYear + 1, "-12-1")), 
                             ymd(paste0(startDateYear + 2, "-3-31")))
    
    #calculating number of days
    winter1days <- yearData %>% filter(acquisition_time %within% winterRange1) %>% 
      distinct(as_date(acquisition_time)) %>% nrow()
    
    summerDays <- yearData %>% filter(acquisition_time %within% summerRange) %>% 
      distinct(as_date(acquisition_time)) %>% nrow()
    
    winter2days <- yearData %>% filter(acquisition_time %within% winterRange2) %>% 
      distinct(as_date(acquisition_time)) %>% nrow()
    
    if(winter1days >= 15 & winter2days >= 15 & summerDays >= 15) {
      return(yearData)
    }
  }
  
  map_dfr(1:numYears, check15days) %>% return()
}


cl <- makeCluster(12)
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


bursts2 <- bursts2 %>% mutate(elkYear = paste(gps_sensors_animals_id, year, sep = "_")) %>%
  dplyr::select(elkYear, everything(), -year) %>% filter(dop < 7)

#adding elevation to all points
dem <- raster("../Switching2/covariateData/SnowData/DEM/gyaDEM.tif")
vDEM <- velox(dem)

burstSF <- bursts2 %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform("+proj=longlat +ellps=GRS80 +no_defs")

elevation <- vDEM$extract_points(burstSF)

bursts2$elevation <- elevation

fwrite(bursts2, "newNorthernWRIRburstsCleaned.csv")
         
#taking only the most recent year for each elk
elkYearsToInclude <- bursts2 %>% distinct(gps_sensors_animals_id, elkYear) %>% 
  arrange(desc(elkYear)) %>% 
  distinct(gps_sensors_animals_id, .keep_all = T)

subset <- bursts2 %>% filter(elkYear %in% elkYearsToInclude$elkYear)


fwrite(subset, "newNorthernWRIRburstsCleanedSubset.csv")

