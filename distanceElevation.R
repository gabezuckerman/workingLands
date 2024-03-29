library(adehabitatHR)
library(rgeos)
library(data.table)
library(lubridate)
library(sf)
library(NbClust)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(ggplot2)
library(tidyverse)
library(gtools)
library(mapview)
library(ggalluvial)
library(doParallel)
library(tictoc)
library(BBMM)


setwd("C:/Users/MiddletonLab/Desktop/Gabe/Box Sync/Elk/Working Lands/")

#removing rows with missing values, switched gps_sensors_code and animals_code
bursts <- fread("burstsCleanedSubsetBothWinters.csv")
bursts$acquisition_time <- ymd_hms(bursts$acquisition_time)

# #finding counties in which there is data
# counties <- st_read("tl_2019_us_county", "tl_2019_us_county") %>% st_transform(4326)
# 
# burstSF <- bursts %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
# 
# intersection <- st_intersects(counties, burstSF) %>% as.data.table()
# 
# countiesWithData <- counties[unique(intersection$row.id),]
# 
# 
# counties2 <- countiesWithData %>% distinct(STATEFP, NAME) %>% st_drop_geometry() %>%  merge(
#   data.table(STATEFP = c(56, 16, 30), state = c("Wyoming", "Idaho", "Montana"))
# ) %>% dplyr::select(state, NAME) %>% arrange(state, NAME)
# 
# 
# 
# 
# mapview(countiesWithData)


#function to find distance and elevation differecence and overlap between Jan-Mar HR and Jul 1-Sep 15 HR
getRangeDistance <- function(ey){
  
  #selecting individual burst
  ind <- bursts %>% filter(elkYear == ey) %>% mutate(month = month(acquisition_time))

  #creating df for function return
  returnDF <- ind %>% distinct(elkYear, gps_sensors_animals_id, year, herd, startDateYear)
  
  
  #finding winter and summer dates
  winterInt <- interval(ymd(paste0(returnDF$startDateYear, "-12-1")), 
                        ymd(paste0(returnDF$startDateYear + 1, "-3-31")))
  summerInt <- interval(ymd(paste0(returnDF$startDateYear + 1, "-7-1")), 
                        ymd(paste0(returnDF$startDateYear + 1, "-9-15")))
  
  #getting data for each season, converting to meters
  winter <- ind %>% filter(acquisition_time %within% winterInt) %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% st_transform(32612)
  winterCoords <- st_coordinates(winter)
  winter <- winter %>% st_drop_geometry() %>% cbind(winterCoords) %>% arrange(acquisition_time)
  
  summer <- ind %>% filter(acquisition_time %within% summerInt) %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% st_transform(32612)
  summerCoords <- st_coordinates(summer)
  summer <- summer %>% st_drop_geometry() %>% cbind(summerCoords) %>% arrange(acquisition_time)
  
  # 
  # #ensures HR can be fit
  # if (nrow(winter) < 50 | nrow(summer) < 50) {
  #   return()
  # }
  # 
  # 
  
  #adding elevation difference
  summerEl <- mean(summer$elevation, na.rm = T)
  winterEl <- mean(winter$elevation, na.rm = T)
  
  returnDF$elevDiff <- abs(summerEl - winterEl)
  
  #calculating summer and winter bbmm
  
  #winter
  winterTimeLag <- as.numeric(difftime(winter$acquisition_time, lag(winter$acquisition_time), 
                                       units = "mins"))[2:nrow(winter)]
  
  winterBBMM <- brownian.bridge(x = winter$X, y = winter$Y, 
                                time.lag = winterTimeLag, location.error = 30, cell.size = 100)
  
  #taking "centroid" as point with highest probability
  winterOut <- data.frame(x=winterBBMM$x,y=winterBBMM$y,z=winterBBMM$probability)
  
  winterCent <- winterOut[which.max(winterOut$z), 1:2]
  
  #summer
  summerTimeLag <- as.numeric(difftime(summer$acquisition_time, lag(summer$acquisition_time), 
                                       units = "mins"))[2:nrow(summer)]
  
  summerBBMM <- brownian.bridge(x = summer$X, y = summer$Y, 
                                time.lag = summerTimeLag, location.error = 30, cell.size = 100)
  
  #taking "centroid" as point with highest probability
  summerOut <- data.frame(x=summerBBMM$x,y=summerBBMM$y,z=summerBBMM$probability)
  
  summerCent <- summerOut[which.max(summerOut$z), 1:2]
  
  #adding distance between the "centroids" in meters to return obj
  returnDF$distance <- dist(rbind(summerCent, winterCent))
  
  
  return(returnDF)
}




##parallelizing calculation
cl <- makeCluster(6)

registerDoParallel(cl)

tic()
rangeDistances <- foreach(ey = unique(bursts$elkYear),
                          .errorhandling = 'pass',
                          .packages = c('tidyverse', 'lubridate', 'sf',
                                        'data.table', 'adehabitatHR',
                                        'rgeos', 'BBMM')) %dopar%
  getRangeDistance(ey)
toc()
stopCluster(cl)

rangeDistances2 <- rbindlist(rangeDistances)

fwrite(rangeDistances2, "rangeDistElevBothWintersBBMM.csv")





















