library(data.table)
library(lubridate)
library(tidyverse)
library(BBMM)
library(sf)
library(doParallel)
library(tictoc)
library(raster)

setwd("C:/Users/MiddletonLab/Desktop/Gabe/Box Sync/Elk/Working Lands")

# bursts <- fread("burstsCleanedSubsetBothWintersLabeled.csv")
# 
# #converting to meters for BBMM
# bursts <- bursts %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% st_transform(32612)
# 
# #making 250 meter grid accross entire GYE
# grid <- st_make_grid(st_as_sfc(st_bbox(bursts)), cellsize = 250, what = "centers") %>% st_coordinates()
# 
# #returning to dataframe form
# coords <- st_coordinates(bursts)
# bursts <- bursts %>% st_drop_geometry() %>% cbind(coords)
# 
# #saving grid and bursts in meters
# fwrite(bursts, "burstsForBBMM.csv")
# fwrite(grid, "gridForBBMM.csv")

#reading in already created dfs

bursts <- fread("burstsForBBMM.csv")
grid <- fread("gridForBBMM.csv")

#getting timing from google sheet
migTiming <- fread("migTiming - migTiming.csv", na.strings = c("NA", "")) %>% 
  dplyr::select(elkYear, manualSpStart, manualSpEnd, manualFaStart, manualFaEnd)

bursts$acquisition_time <- ymd_hms(bursts$acquisition_time)

#returns 1 point grid even though mig should span 10km
ey <- "106_1"
p <- "mig2"

#returns all points within 99% isopleth for migrations and 50% isopleth for seasonal ranges
getCorridor <- function(ey) {
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
    
    #doing all periods separately
    
    #getting grid points within 99 isopleth for mig 1 and mig 2
    
    #getting grid points within 50 isopleth for summer and combined winters
    
    if(p == "mig1" | p == "mig2") {
      int <- filter(intTable, period == p)$interval
      
      data <- ind %>% filter(acquisition_time %within% int)
      
      #for bbmm
      timeLag <- as.numeric(difftime(data$acquisition_time, lag(data$acquisition_time), 
                                     units = "mins"))[2:nrow(data)]
      
      #subset overall grid to make fitting quicker (with 25 km buffer for probabalistic fitting)
      #still has standard coords for merging inds from same herd
      dataGrid <- grid %>% filter(X > (min(data$X, na.rm = T)-25000),
                                  X < (max(data$X, na.rm = T)+25000),
                                  Y > (min(data$Y, na.rm = T)-25000), 
                                  Y < (max(data$Y, na.rm = T)+25000))
      
      #running bbmm
      bb <- brownian.bridge(x = data$X, y = data$Y, time.lag = timeLag, 
                            location.error = 30, area.grid = dataGrid)
      
      #getting 99 contour for corridors
      contours <- bbmm.contour(bb, levels=99, locations=data, plot = F)
      bbDF <- data.table(X = bb[["x"]], Y = bb[["y"]], prob = bb[["probability"]]) 
      r <-  rasterFromXYZ(bbDF,crs=CRS("+proj=utm +zone=12 +datum=WGS84 +units=m +no_defs"),digits=2)
      raster.contour <- rasterToContour(r,levels=contours$Z) %>% st_as_sf() %>% st_cast("POLYGON")
      
      #finding points on master grid that are within 99% isopleth
      intersectingPoints <- dataGrid %>% st_as_sf(coords = c("X", "Y"), crs = 32612) %>%
        st_intersection(raster.contour)
      
      intersectingPointCoords <- st_coordinates(intersectingPoints)
      
      #adding individual information
      intersectingPoints %>% st_drop_geometry() %>% cbind(intersectingPointCoords) %>%
        mutate(elkYear = ey, period = p, herd = ind$herd[1], checkHerd = ind$checkHerd[1]) %>%
        dplyr::select(-level) %>% return()
    } else if (p %in% c("summer", "winter1", "winter2")) {
      return()
    } 
    
  }
  #getting bbmm for all periods
  return(do.call(rbind, map(intTable$period, getBBMM)))
}


getCorridor("100_2")



##parallelizing calculation
cl <- makeCluster(6)

registerDoParallel(cl)

tic()
corridorList <- foreach(ey = migTiming$elkYear,
                          .errorhandling = 'pass',
                          .packages = c('tidyverse', 'lubridate', 'sf',
                                        'data.table', 'BBMM', 'raster')) %dopar%
  getCorridor(ey)
toc()
stopCluster(cl)

corridors <- rbindlist(Filter(is.data.frame, corridorList))

fwrite(corridors, "allCorridorData.csv")
##############################################################################

corridors <- fread("allCorridorData.csv")

#dropping border, deer creeks, Gallating, and Greycliff, that way all seasons have more than 10 elk
corridors <- corridors %>% filter(herd %nin% c("Border", "Deer Creeks", "Gallatin", "Greycliff"))


#then caculate, for each herd, the points where 10-20% use, and points where 20%+ use
herdPeriodCounts <- corridors %>% dplyr::group_by(herd, period) %>%
  dplyr::summarize(totalInds = n_distinct(elkYear))
herdPeriodUse <- corridors %>% group_by(herd, period, X, Y) %>% 
  dplyr::summarise(numInds = n_distinct(elkYear))

herdTotals <- herdPeriodUse %>% merge(herdPeriodCounts) %>% 
  mutate(percent = numInds/totalInds)



getCorridorUseShapes<- function(num) {
  print(num)
  #1 is mid use, 2 is high use
  test <- herdTotals %>% filter(herd == herdPeriodCounts$herd[num], period == herdPeriodCounts$period[num]) %>% 
    filter(percent >= .10) %>% mutate(level = ifelse(percent < .2, 1, 2))
  
  if(nrow(test) < 2) {
    return()
  }
  
  testRaster <- rasterFromXYZ(cbind(test$X, test$Y, test$level))
  crs(testRaster) <- CRS("+init=epsg:32612")
  
  if(sum(test$level == 1) != 0) {
    mid <- rasterToPolygons(testRaster, fun=function(x){x==1}) %>% st_as_sf(crs = 32612) %>% group_by(layer) %>%
      summarise()
    
    mid$herd <- herdPeriodCounts$herd[num]
    mid$period <-  herdPeriodCounts$period[num]
    mid$level <- "mid"
  } else {
    mid <- NA
  }
  
  
  if(sum(test$level == 2) != 0) {
    high <- rasterToPolygons(testRaster, fun=function(x){x==2}) %>% st_as_sf(crs = 32612) %>% group_by(layer) %>%
      summarise()
    
    high$herd <- herdPeriodCounts$herd[num]
    high$period <-  herdPeriodCounts$period[num]
    high$level <- "high"
  } else {
    high <- NA
  }
  
  if(is.na(high)) {
    return(mid %>% dplyr::select(-layer))
  } else if (is.na(mid)) {
    return(high %>% dplyr::select(-layer))
  } else if(is.na(mid) & is.na(high)) {
    return()
  } else{
    return(rbind(mid, high) %>% dplyr::select(-layer))
  }
}

allCorridorShapesList <- map(1:nrow(herdPeriodCounts), getCorridorUseShapes)

allCorridorShapes <- mapedit:::combine_list_of_sf(allCorridorShapesList[-c(15, 17, 21, 22, 41)])


st_write(allCorridorShapes, "corridorRanges/corridors", "all", driver = "ESRI Shapefile")
