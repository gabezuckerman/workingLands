library(data.table)
library(tidyverse)
library(Hmisc)
library(lubridate)
library(momentuHMM)
library(raster)
library(sf)
library(tripack)
library(doParallel)
library(tictoc)

setwd("C:/Users/MiddletonLab/Desktop/Gabe/Box Sync/Elk/Working Lands")


timing <- fread("migTiming - migTiming.csv", na.strings = "")


bursts <- fread("burstsForBBMM.csv")
bursts$acquisition_time <- ymd_hms(bursts$acquisition_time)

timing <- bursts %>% distinct(elkYear, herd) %>% merge(timing) %>%
  filter(herd %nin% c("Border", "Deer Creeks", "Gallatin", "Greycliff"))

timing$resident <- rowSums(is.na(timing[,3:6]))==4


#summary of strategies
stratSum<- timing %>% group_by(herd) %>% dplyr::summarize(elk = n_distinct(elkYear),
                                        residents = sum(as.numeric(resident)),
                                        nonResidents = elk-residents)
#summary of distribution of seasons
seasonalCounts <- fread("seasonalCounts.csv")

#spring and fall migration median start dates

getExactTiming <- function(ey) {
  indTiming <- timing %>% filter(elkYear == ey)
  
  
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
    
    intTable <- data.table(elkYear = c(ey, ey, ey, ey),
                           period = c("spStart", "spEnd", "faStart", "faEnd"),
                           date = c(spStart, spEnd, faStart, faEnd))
  } else if (sum(is.na(indTiming)) == 2) {
    ########################
    #two NAs = either disperser or no fall migration: winter is month before first migration, summer is month after
    ########################
    
    #converting spreadsheet data to actual dates
    
    #start year is always december start
    spStart <- ymd(paste0(ind$startDateYear[1] +1, "-", indTiming$manualSpStart))
    spEnd <- ymd(paste0(ind$startDateYear[1] +1, "-", indTiming$manualSpEnd))
    
    intTable <- data.table(elkYear = c(ey, ey), period = c("spStart", "spEnd"),
                           date = c(spStart, spEnd))
  } else if (sum(is.na(indTiming)) == 4) {
    #########################
    #four NAs = resident: one range for entire time
    #########################
    
    intTable <- NULL
    
  }
  return(intTable)
}

allTiming <- map_dfr(timing$elkYear, getExactTiming) %>%
  merge(timing %>% dplyr::select(elkYear, herd)) %>% mutate(yday = yday(date))

dates <- allTiming %>% group_by(herd, period) %>% dplyr::summarise(date = round(median(yday), 0))
dates$date <- map_chr(dates$date, ~substring(ymd("2001-1-1") + days(.x), 6))
dates <- dates %>% pivot_wider(names_from = period, values_from = date) %>%
  dplyr::select(herd, spStart, spEnd, faStart, faEnd)

#spring and fall migration corridor lengths
#determining by 2*radius of the smallest drawn around circle around the 99% isopleth of each mig

corridors <- fread("allCorridorData.csv") %>%
  filter(herd %nin% c("Border", "Deer Creeks", "Gallatin", "Greycliff"))

corChoices <- corridors %>% distinct(elkYear, period)

getCorridorLength <- function(ey, p) {
  print(paste0(ey, ": ", p))
  data <- corridors %>% filter(elkYear == ey, period == p) %>%
    dplyr::select(herd, X, Y) 
  #a single 250m point
  if(nrow(data) == 1) {
    len <- 250
  } else if (nrow(data) == 2) {
    len <- 500
  } else {
    #drawing circle
    circle <- circumcircle(data$X, data$Y)
    #diameter of circle
    len <-  2*circle[[3]]
  }
  return(data.table(elkYear = ey, herd = data$herd[1], period = p, len))
}

##parallelizing calculation
cl <- makeCluster(6)

registerDoParallel(cl)

tic()
corridorLengthList <- foreach(ey = corChoices$elkYear,
                              p = corChoices$period,
                             .errorhandling = 'pass',
                             .packages = c('tidyverse', 'tripack',
                                           'data.table')) %dopar%
  getCorridorLength(ey, p)
toc()
stopCluster(cl)

corridorLength <- rbindlist(Filter(is.data.frame, corridorLengthList))

#issues with 250 meter migrants, comes from issues with BBMM
corridorSum <- corridorLength %>% group_by(herd) %>% dplyr::summarise(min = min(len),
                                                       mean = mean(len),
                                                       max = max(len))



#are stopovers contained within high use corridors? not really
corridorShapes <- st_read("corridorRanges/corridors", "all")
st_crs(corridorShapes) <- 32612

stopoverShape <- st_read("corridorRanges/rangeStopovers", "all") %>% 
  filter(period %in% c("mig1stopover", "mig2stopover"))
st_crs(stopoverShape) <- 32612
stopoverShape$period <- map_chr(stopoverShape$period, ~strsplit(as.character(.x), "stopover")[[1]][1])

#all stopovers
choices <- stopoverShape %>% st_drop_geometry() %>% distinct(herd, period) %>%
  mutate(herd = as.character(herd), period = as.character(period))


getStopoverIntersection <- function(h, p){
 
  s <- stopoverShape %>% filter(herd == h, period == p)
  
  #checking high and mid level 
  cMid <- corridorShapes %>% filter(herd == h, period == p, level == "mid")
  cHigh <- corridorShapes %>% filter(herd == h, period == p, level == "high")
  
  #calculating high and mid level intersection
  intMid <- as.numeric(st_intersection(s, cMid) %>% st_area())/as.numeric(st_area(s)) 
  intHigh <- as.numeric(st_intersection(s, cHigh) %>% st_area())/as.numeric(st_area(s))
  
  data.table(h, p, intMid, intHigh)
}


stopoverIntersection <- map2_dfr(choices$herd, choices$period, getStopoverIntersection) 
stopoverIntersection[is.na(stopoverIntersection)] <- 0

#how much do spring and fall migrations medium and high uses overlap? not much, more for high
herds <- unique(as.character(corridorShapes$herd))


getCorridorIntersection <- function(h){
  
  #checking both intersection
  springBoth <- corridorShapes %>% filter(herd == h, period == "mig1") %>% st_union()
  fallBoth <- corridorShapes %>% filter(herd == h, period == "mig2") %>% st_union()
  
  
  #checking high 
  springHigh <- corridorShapes %>% filter(herd == h, period == "mig1", level == "high")
  fallHigh <- corridorShapes %>% filter(herd == h, period == "mig2", level == "high")
  
  #checking mid 
  springMid <- corridorShapes %>% filter(herd == h, period == "mig1", level == "mid")
  fallMid <- corridorShapes %>% filter(herd == h, period == "mig2", level == "mid")
  

  #calculating intersection areas as a proportion of the smaller area
  intBoth <- as.numeric(st_intersection(springBoth, fallBoth) %>% st_area()) / 
    min(as.numeric(st_area(springBoth)),as.numeric(st_area(fallBoth)))

  intMid <- as.numeric(st_intersection(springMid, fallMid) %>% st_area()) / 
    min(as.numeric(st_area(springMid)),as.numeric(st_area(fallMid))) 
  
  intHigh <- as.numeric(st_intersection(springHigh, fallHigh) %>% st_area()) / 
    min(as.numeric(st_area(springHigh)),as.numeric(st_area(fallHigh)))
  
  data.table(h, intBoth, intMid, intHigh)
}

springFallIntersection <- map_dfr(herds, getCorridorIntersection)


#how much do winter1 and winter2 ranges overlap? not much
winterShape <- st_read("corridorRanges/rangeStopovers", "all") %>% 
  filter(period %in% c("winter1", "winter2"))
st_crs(winterShape) <- 32612


#note winter1 includes residents
getWinterIntersection <- function(h){
  
  winter1 <- winterShape %>% filter(herd == h, period == "winter1")
  winter2 <- winterShape %>% filter(herd == h, period == "winter2")
  
  #calculating intersection areas as a proportion of the smaller area
  intWinter <- as.numeric(st_intersection(winter1, winter2) %>% st_area()) / 
    min(as.numeric(st_area(winter1)),as.numeric(st_area(winter2)))

  data.table(h, intWinter)
}

winterIntersection <- map_dfr(herds, getWinterIntersection)


#area of medium and high use
midHighArea <- corridorShapes %>% mutate(area = as.numeric(st_area(.))) %>% st_drop_geometry()


