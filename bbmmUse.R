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

migTiming <- fread("migTiming - migTiming.csv", na.strings = c("NA", "")) %>% 
  dplyr::select(elkYear, manualSpStart, manualSpEnd, manualFaStart, manualFaEnd)

bursts$acquisition_time <- ymd_hms(bursts$acquisition_time)

ey <- migTiming$elkYear[errored[19]]


#start of code
rangeCorridor <- function(ey) {
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
    
    int <- filter(intTable, period == p)$interval
    
    #for some reason data is empty
    data <- ind %>% filter(acquisition_time %within% int)
    
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
    
    #no need to save points with 0 probability
    bbDF <- data.table(X = bb[["x"]], Y = bb[["y"]], prob = bb[["probability"]]) %>%
      filter(prob >= 0.00000001)
    
    #adding info to table
    bbDF$elkYear <- ey
    
    bbDF$period <- p 
    
    bbDF$herd <- ind$herd[1]
    
    bbDF$checkHerd <- ind$checkHerd[1]
    
    return(bbDF %>% dplyr::select(elkYear, herd, checkHerd, period, X, Y, prob))
  }
  
  #getting bbmm for all periods
  map_dfr(intTable$period, getBBMM)
}



##parallelizing calculation
cl <- makeCluster(6)

registerDoParallel(cl)

tic()
rangeCorridorList <- foreach(ey = migTiming$elkYear,
                          .errorhandling = 'pass',
                          .packages = c('tidyverse', 'lubridate', 'sf',
                                        'data.table', 'BBMM')) %dopar%
  rangeCorridor(ey)
toc()
stopCluster(cl)

#these are the ones that were successfull
allGood <- rbindlist(Filter(is.data.frame, rangeCorridorList))


#the following errored bc I did not account for 01-18 as a fall mig start date
errored <- c(38, 362, 367, 370,377, 384, 389, 408, 434, 441, 442, 455, 488, 
497, 528, 534, 537, 609, 641, 704, 714, 742, 750, 752, 758, 760, 823, 
849, 864, 866, 876, 888, 893, 894, 896, 897, 900, 901, 925, 926, 963, 966)



#rerunning errored ones with new code
cl <- makeCluster(6)

registerDoParallel(cl)

tic()
rangeCorridorList2 <- foreach(ey = migTiming$elkYear[errored],
                             .errorhandling = 'pass',
                             .packages = c('tidyverse', 'lubridate', 'sf',
                                           'data.table', 'BBMM')) %dopar%
  rangeCorridor(ey)
toc()
stopCluster(cl)

allFixed <- rbindlist(rangeCorridorList2)

all <- rbind(allGood, allFixed)

save(all, file = "rangeCorridorProbsFixed.RData")

fwrite(all, "rangeCorridorProbs.csv")

#after running for all inds, group by herd, x, y, period (combining both winters) and summarizing function of sum
#then divide by total of herd, period to get herd level period distributions

#joining both winters together
all <- all %>% mutate(period = ifelse(period == "winter1" | period == "winter2",
                               "winter", period))


sumProb <- all %>% group_by(herd, X, Y, period) %>%
  summarise(summedProb = sum(prob))

#finding total prob for each herd-period
totalHerdProb <- all %>% group_by(herd, period) %>%
  summarize(totalProb = sum(prob), nInds = n_distinct(elkYear))


probTable <- merge(sumProb, totalHerdProb) %>% mutate(
  herdProp = summedProb/totalProb
)


fwrite(probTable, "herdCorridorRanges.csv")


#converting to shapefile for each herd-period

createContours <- function(i) {
  hp <- probTable %>% filter(herd == totalHerdProb$herd[i],
                             period == totalHerdProb$period[i]) %>%
    mutate(percentile = ntile(herdProp, 100))
  
  
  high <- rasterFromXYZ(hp %>% filter(percentile > 75) %>%
                                  dplyr::select(X, Y, percentile)) %>%
    rasterToPolygons(dissolve = T) %>% st_as_sf() %>%
    st_set_crs(32612) %>% mutate(use = "high")
  
  mid <- rasterFromXYZ(hp %>% filter(percentile <= 75,
                                             percentile > 25) %>%
                                 dplyr::select(X, Y, percentile)) %>%
    rasterToPolygons(dissolve = T) %>% st_as_sf() %>%
    st_set_crs(32612) %>% mutate(use = "mid")
  
  low <- rasterFromXYZ(hp %>% filter(percentile <= 25) %>%
                                 dplyr::select(X, Y, percentile)) %>%
    rasterToPolygons(dissolve = T) %>% st_as_sf() %>%
    st_set_crs(32612) %>% mutate(use = "low")
  
  contours <- do.call("rbind", list(low, mid, high))
  
  contours$period <- totalHerdProb$period[i]
  contours$herd <- totalHerdProb$herd[i]
  
  return(contours)
}


##parallelizing contour generation

cl <- makeCluster(6)

registerDoParallel(cl)

tic()
allContours <- foreach(i = 1:nrow(totalHerdProb),
                              .errorhandling = 'pass',
                              .packages = c('tidyverse', 'lubridate', 'sf',
                                            'data.table', 'raster')) %dopar%
  createContours(i)
toc()
stopCluster(cl)

allContours2 <- do.call("rbind", allContours)

st_write(allContours2, "contours", "herdContours", driver = "ESRI Shapefile")

#####converting to shapefile for each period
#independent of herd
sumProb <- all %>% group_by(X, Y, period) %>%
  summarise(summedProb = sum(prob))

#finding total prob for each herd-period
gyeProb <- all %>% group_by(period) %>%
  summarize(totalProb = sum(prob), nInds = n_distinct(elkYear))


gyeProbTable <- merge(sumProb, gyeProb) %>% mutate(
  prop = summedProb/totalProb)

createGyeContours <- function(i) {
  table <- gyeProbTable %>% filter(period == gyeProb$period[i]) %>%
    mutate(percentile = ntile(prop, 100))
  
  
  high <- rasterFromXYZ(table %>% filter(percentile > 75) %>%
                          dplyr::select(X, Y, percentile)) %>%
    rasterToPolygons(dissolve = T) %>% st_as_sf() %>%
    st_set_crs(32612) %>% mutate(use = "high")
  
  mid <- rasterFromXYZ(table %>% filter(percentile <= 75,
                                     percentile > 25) %>%
                         dplyr::select(X, Y, percentile)) %>%
    rasterToPolygons(dissolve = T) %>% st_as_sf() %>%
    st_set_crs(32612) %>% mutate(use = "mid")
  
  low <- rasterFromXYZ(table %>% filter(percentile <= 25) %>%
                         dplyr::select(X, Y, percentile)) %>%
    rasterToPolygons(dissolve = T) %>% st_as_sf() %>%
    st_set_crs(32612) %>% mutate(use = "low")
  
  contours <- do.call("rbind", list(low, mid, high))
  
  contours$period <- gyeProb$period[i]

  return(contours)
}

##parallelizing contour generation

cl <- makeCluster(4)

registerDoParallel(cl)

tic()
allGyeContours <- foreach(i = 1:4,
                       .errorhandling = 'pass',
                       .packages = c('tidyverse', 'lubridate', 'sf',
                                     'data.table', 'raster')) %dopar%
  createGyeContours(i)
toc()
stopCluster(cl)

allGyeContours2 <- do.call("rbind", allGyeContours)

st_write(allGyeContours2, "contours", "gyeContours", driver = "ESRI Shapefile")
