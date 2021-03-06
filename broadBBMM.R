library(data.table)
library(lubridate)
library(tidyverse)
library(BBMM)
library(sf)
library(doParallel)
library(tictoc)
library(raster)

setwd("C:/Users/MiddletonLab/Desktop/Gabe/Box Sync/Elk/Working Lands")

bursts <- fread("burstsForBBMM.csv")

#getting timing from google sheet
migTiming <- fread("migTiming - migTiming.csv", na.strings = c("NA", "")) %>% 
  dplyr::select(elkYear, manualSpStart, manualSpEnd, manualFaStart, manualFaEnd)

bursts$acquisition_time <- ymd_hms(bursts$acquisition_time)

#returns all points within 99% isopleth in all seasonal ranges
getSeasonalRanges <- function(ey) {
  ind <- bursts %>% filter(elkYear == ey)
  
  indTiming <- migTiming %>% filter(elkYear == ey)
  
  
  if(sum(is.na(indTiming)) == 0) {
    #########################
    #no NAs = migration: winter is one month before migration and one month after migration
    #########################
    
    
    #converting spreadsheet data to actual dates
    
    #start year is always december start
    spStart <- ymd(paste0(ind$startDateYear[1] +1, "-", indTiming$manualSpStart))
    spEnd <- ymd(paste0(ind$startDateYear[1] +1, "-", indTiming$manualSpEnd))
    
    #need to account for fall migrations that start or end after december
    #could be 
    
    #start
    if(substr(indTiming$manualFaStart, 1, 2) %in% 
       c("1-", "2-", "3-", "01", "02", "03")) {
      faStart <- ymd(paste0(ind$startDateYear[1] +2, "-", indTiming$manualFaStart))
    } else {
      faStart <- ymd(paste0(ind$startDateYear[1] +1, "-", indTiming$manualFaStart))
    }
    #end
    if(substr(indTiming$manualFaEnd, 1, 2) %in% 
       c("1-", "2-", "3-", "01", "02", "03")) {
      faEnd <- ymd(paste0(ind$startDateYear[1] +2, "-", indTiming$manualFaEnd))
    } else {
      faEnd <- ymd(paste0(ind$startDateYear[1] +1, "-", indTiming$manualFaEnd))
    }
    
    #setting intervals, adding 1 day buffer on either end of migrations (Sawyer et al 2009)
    firstMig <- interval(spStart - days(1), spEnd + days(1))
    summer <- interval(spEnd, faStart)
    secondMig <- interval(faStart - days(1), faEnd + days(1))
    winter1 <- interval(spStart - days(30), spStart)
    winter2 <- interval(faEnd, faEnd + days(30))
    
    intTable <- data.table(period = c("winter1", "mig1", "summer", "mig2", "winter2"),
                           interval = c(winter1, firstMig, summer, secondMig, winter2))
  } else if (sum(is.na(indTiming)) == 2) {
    ########################
    #two NAs = either disperser or no fall migration: winter is month before first migration, summer is month after
    ########################
    
    #converting spreadsheet data to actual dates
    
    #start year is always december start
    spStart <- ymd(paste0(ind$startDateYear[1] +1, "-", indTiming$manualSpStart))
    spEnd <- ymd(paste0(ind$startDateYear[1] +1, "-", indTiming$manualSpEnd))
    
    #setting intervals, adding 1 day buffer on either end of migrations (Sawyer et al 2009)
    firstMig <- interval(spStart - days(1), spEnd + days(1))
    summer <- interval(spEnd, spEnd + days(30))
    winter1 <- interval(spStart - days(30), spStart)
    
    intTable <- data.table(period = c("winter1", "mig1", "summer"),
                           interval = c(winter1, firstMig, summer))
  } else if (sum(is.na(indTiming)) == 4) {
    #########################
    #four NAs = resident: one range for entire time
    #########################
    
    intTable <- data.table(period = c("winter1"), 
                           interval = c(interval(min(ind$acquisition_time), max(ind$acquisition_time))))
    
  }
  
  getBBMM <- function(p) {
    
    print(p)
    
    #doing all periods separately
    int <- filter(intTable, period == p)$interval
    
    data <- ind %>% filter(acquisition_time %within% int)
    
    #for bbmm
    timeLag <- as.numeric(difftime(data$acquisition_time, lag(data$acquisition_time), 
                                   units = "mins"))[2:nrow(data)]
    
    #removing the top 1% of timeLag datapoints
    data <- data[-which(ntile(timeLag, 100) == 100), ]
    
    
    #running bbmm
    bb <- brownian.bridge(x = data$X, y = data$Y, 
                          time.lag = timeLag[-which(ntile(timeLag, 100) == 100)], 
                          location.error = 30, cell.size = 250)
    
    #getting 99 contour for corridors
    contours <- bbmm.contour(bb, levels=99, locations=data, plot = F)
    bbDF <- data.table(X = bb[["x"]], Y = bb[["y"]], prob = bb[["probability"]]) 
    r <-  rasterFromXYZ(bbDF,crs=CRS("+proj=utm +zone=12 +datum=WGS84 +units=m +no_defs"),digits=2)
    out <- tryCatch(
      {
        rasterToContour(r,levels=contours$Z) %>% st_as_sf() %>% st_cast("POLYGON") %>%
        mutate(elkYear = ey, period = p, herd = ind$herd[1], checkHerd = ind$checkHerd[1]) %>%
        dplyr::select(-level)
      },
        error = function(cond){
          NULL
        })
    out
    
    
  }
  #getting bbmm for all periods
  return(Filter(is.data.frame, map(intTable$period, getBBMM)) %>% mapedit:::combine_list_of_sf())
}





##parallelizing calculation
cl <- makeCluster(6)

registerDoParallel(cl)

tic()
seasonalRangeList <- foreach(ey = migTiming$elkYear,
                        .errorhandling = 'pass',
                        .packages = c('tidyverse', 'lubridate', 'sf',
                                      'data.table', 'BBMM', 'raster')) %dopar%
  getSeasonalRanges(ey)
toc()
stopCluster(cl)

seasonalRanges <- Filter(is.data.frame, seasonalRangeList) %>% mapedit:::combine_list_of_sf()

#combines all 99% isopleths into a single polygon, combining both winters as one, both migs as one
combineToSeasons <- function(h) {
  winter <- seasonalRanges %>% filter(herd == h, period %in% c("winter1", "winter2")) %>% 
    group_by(herd) %>% summarise() %>% mutate(season = "winter")
  
  mig <- seasonalRanges %>% filter(herd == h, period %in% c("mig1", "mig2"))  %>% 
    group_by(herd) %>% summarise() %>% mutate(season = "migration")
  
  summer <- seasonalRanges %>% filter(herd == h, period %in% c("summer")) %>% 
    group_by(herd) %>% summarise() %>% mutate(season = "summer")
  
  list(winter, mig, summer) %>% mapedit:::combine_list_of_sf()
}

#deer creek, greycliff only has winter, meaning only residents
all <- map(unique(seasonalRanges$herd), combineToSeasons) %>% mapedit:::combine_list_of_sf()

st_write(all, "broad99BBMM", "all", driver = "ESRI Shapefile")

allCheck <- st_read("broad99BBM", "all")


