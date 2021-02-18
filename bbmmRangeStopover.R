library(data.table)
library(lubridate)
library(tidyverse)
library(BBMM)
library(sf)
library(doParallel)
library(tictoc)
library(raster)
library(Hmisc)

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



#returns all points within 99% isopleth for migrations and 50% isopleth for seasonal ranges
getUseProps <- function(ey) {
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
    
    #getting grid points within and percent use for each herd
    
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
    
    #getting use proportions for ranges and stopovers, only need to save non-zero probality
    data.table(X = bb[["x"]], Y = bb[["y"]], prob = bb[["probability"]]) %>%
      filter(prob > 0.0000001) %>%
      mutate(elkYear = ey, period = p, herd = ind$herd[1], checkHerd = ind$checkHerd[1]) %>%
      return()
    
  }
  #getting bbmm for all periods
  return(do.call(rbind, map(intTable$period, getBBMM)))
}


##parallelizing calculation
cl <- makeCluster(6)

registerDoParallel(cl)

tic()
rangeStopoverList <- foreach(ey = migTiming$elkYear,
                        .errorhandling = 'pass',
                        .packages = c('tidyverse', 'lubridate', 'sf',
                                      'data.table', 'BBMM', 'raster')) %dopar%
  getUseProps(ey)
toc()
stopCluster(cl)

rangeStopoverUse <- rbindlist(Filter(is.data.frame, rangeStopoverList))

fwrite(rangeStopoverUse, "allRangeStopverData.csv")

###########################################################
rangeStopoverUse <- fread("allRangeStopverData.csv")

#then scale to 1 for each herd-period, then take 50% isopleth using rasterFromXYZ for winter/summer
#and 10% isopleth from each mig period for stopovers

allHerdPeriods <- rangeStopoverUse %>% group_by(herd, period) %>% summarise(elk = n_distinct(elkYear))

#dropping border, deer creeks, Gallating, and Greycliff, that way all seasons have more than 10 elk
allHerdPeriods <- allHerdPeriods %>% filter(herd %nin% c("Border", "Deer Creeks", "Gallatin", "Greycliff"))


# #table for paper
# summaryCount <- allHerdPeriods %>% pivot_wider(names_from = period, values_from = elk) %>%
#   mutate_all(funs(replace_na(.,0)))
# fwrite(summaryCount, "seasonalCounts.csv")



getRangeStopoverShapes <- function(i){
  print(i)
  
  h <- allHerdPeriods$herd[i]
  p <- allHerdPeriods$period[i]
  
  data <- rangeStopoverUse %>% filter(herd == h,
                                      period == p) %>% 
    mutate(normProb = prob/sum(prob))
  
  
  #code from BBMM contour to calculate level at which to draw 10%/50% isopleth
  v <- na.omit(data$normProb)
  probability <- NULL
  
  #stopovers are 10% and ranges are 50%
  if(p %in% c("winter1", "winter2", "summer")) {
    contour.z <- function(z) {
      abs(50/100 - sum(v[v >= z])/sum(v))
    }
  } else {
    contour.z <- function(z) {
      abs(10/100 - sum(v[v >= z])/sum(v))
    }
  }
  probability <- c(probability, optimize(contour.z, 
                                         c(0, max(v)), tol = .Machine$double.eps)$minimum)
  
  #convert to a raster to get contours from
  r <- rasterFromXYZ(cbind(data$X, data$Y, data$normProb))
  crs(r) <- CRS("+init=epsg:32612")

  #no stopovers
  if(p %in% c("mig1", "mig2") & probability >= max(data$normProb) - 0.00000001) {
    return()
  }
  
  ## convert raster object to matrix so it can be fed into contourLines
  xmin <- extent(r)@xmin
  xmax <- extent(r)@xmax
  ymin <- extent(r)@ymin
  ymax <- extent(r)@ymax
  rx <- seq(xmin, xmax, length.out=ncol(r))
  ry <- seq(ymin, ymax, length.out=nrow(r))
  rz <- t(as.matrix(r))
  rz <- rz[,ncol(rz):1] # reshape
  
  ## get contour lines and convert to SpatialLinesDataFrame, then to sf
  cl0 <- contourLines(rx, ry, rz, levels = probability)
  cl <- ContourLines2SLDF(cl0, proj4string = CRS("+proj=utm +zone=12 +datum=WGS84 +units=m +no_defs")) %>%
    st_as_sf(crs = 32612) %>% st_cast("LINESTRING")
  
  #finding and removing incomplete polygons
  cl$len <- map_dbl(1:nrow(cl), ~nrow((st_coordinates(cl$geometry[.x]))))
  
  #removing isolated polygons of less than 20000m2
  cl <- cl %>% filter(len > 2)  %>% st_cast("POLYGON") %>% mutate(area = as.numeric(st_area(geometry))) %>%
    filter(area >= 20000) 
  
  if(nrow(cl) == 0) {
    return()
  } else{
    cl %>% group_by(level) %>% dplyr::summarise() %>% mutate(herd = h, period = ifelse(p %in% c("mig1", "mig2"), paste0(p, "stopover"), p)) %>%
      dplyr::select(-level) %>% return()
  }
}


allRangeStopovers <- map(1:nrow(allHerdPeriods), getRangeStopoverShapes)

allRangeStopoversSF <- mapedit:::combine_list_of_sf(allRangeStopovers[-c(2, 31, 63, 82, 99, 100, 104)])

st_write(allRangeStopoversSF, "corridorRanges/rangeStopovers", "all", driver = "ESRI Shapefile", append = F)


