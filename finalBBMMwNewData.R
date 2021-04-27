library(data.table)
library(tidyverse)
library(lubridate)
library(Hmisc)
library(sf)
library(BBMM)
library(raster)
library(tripack)

setwd("C:/Users/MiddletonLab/Desktop/Gabe/Box Sync/Elk/Working Lands/")

#combining all movement data

# oldData1 <- fread("burstsForBBMM.csv")
# 
# newNorthernWRIR <- fread("newNorthernWRIRburstsCleanedSubset.csv") %>%
#   st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% st_transform(32612)
# 
# coords <- newNorthernWRIR %>% st_coordinates()
# 
# newNorthernWRIR<- newNorthernWRIR %>% st_drop_geometry() %>% cbind(coords)
# 
# 
# liveWRIR <- fread("liveWRIRburstsCleanedSubset.csv") %>%
#   st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% st_transform(32612)
# 
# coords <- liveWRIR %>% st_coordinates()
# 
# liveWRIR<- liveWRIR %>% st_drop_geometry() %>% cbind(coords)
# 
# allData <- rbindlist(list(dplyr::select(oldData1, colnames(liveWRIR)), liveWRIR, newNorthernWRIR))
# 
# #splitting wind river herds according to Pat
# wrirHerds <- st_read("WRIRpaths", "WRIRpaths2") %>% st_drop_geometry() %>% dplyr::select(elkYear, herd = Herd)
# 
# herds <- allData %>% distinct(elkYear, herd) %>% filter(elkYear %nin% wrirHerds$elkYear)
# allDataHerds <- rbind(herds, wrirHerds)
# 
# allData <- allData %>% dplyr::select(-herd) %>% merge(allDataHerds)
# 
# fwrite(allData, "allMovementData.csv")
# 
# #combining timing data
# oldTiming <- fread("migTiming - migTiming.csv", na.strings = c("NA", "")) %>% 
#   dplyr::select(elkYear, manualSpStart, manualSpEnd, manualFaStart, manualFaEnd)
# 
# newTiming <- fread("migTiming2 - timingStart2.csv", na.strings = c("NA", "")) %>% 
#   dplyr::select(elkYear, manualSpStart, manualSpEnd, manualFaStart, manualFaEnd)
# 
# allTiming <- rbind(oldTiming, newTiming)
# 
# fwrite(allTiming, "allTiming.csv")

################################


all <- fread("allMovementData.csv") %>% distinct(elkYear, acquisition_time, .keep_all = T)
all$acquisition_time <- ymd_hms(all$acquisition_time)


timing <- fread("allTiming.csv")

timing$elk <- as.numeric(map(timing$elkYear, ~str_split(.x, "_")[[1]][1]))
timing$year <- as.numeric(map(timing$elkYear, ~str_split(.x, "_")[[1]][2]))


#contains all relevant dates, and elk year, as well as year 1 strategy, year 2 strategy
getTiming <- function(ey) {
  
  indTiming <- timing %>% filter(elkYear == ey)
  startYear <- filter(all, elkYear == ey)$startDateYear[1]
  herd <- filter(all, elkYear == ey)$herd[1]
  
  if (sum(is.na(indTiming[,4:7])) != 4) {
    #start year is always december start
    spStart <- ymd(paste0(startYear +1, "-", indTiming$manualSpStart))
    spEnd <- ymd(paste0(startYear +1, "-", indTiming$manualSpEnd))
    
    #need to account for fall migrations that start or end after december
    #checking to see if there is a fall mig
    if(!is.na(indTiming$manualFaStart)) {
      #start
      if(substr(indTiming$manualFaStart, 1, 2) %in% 
         c("1-", "2-", "3-", "01", "02", "03")) {
        faStart <- ymd(paste0(startYear +2, "-", indTiming$manualFaStart))
      } else {
        faStart <- ymd(paste0(startYear +1, "-", indTiming$manualFaStart))
      }
      #end
      if(substr(indTiming$manualFaEnd, 1, 2) %in% 
         c("1-", "2-", "3-", "01", "02", "03")) {
        faEnd <- ymd(paste0(startYear +2, "-", indTiming$manualFaEnd))
      } else {
        faEnd <- ymd(paste0(startYear +1, "-", indTiming$manualFaEnd))
      }
    } else {
      faStart <- NA
      faEnd <- NA
    }
  } else {
    faStart <- NA
    faEnd <- NA
    spStart <- NA
    spEnd <- NA
  }
  
  
  #joining it all together
  returnTable <- data.table(elkYear = ey, elk = str_split(ey, "_")[[1]][1],
                            herd, decStartYear = startYear,
                            spStart = as.character(spStart), 
                            spEnd = as.character(spEnd),
                            faStart = as.character(faStart),
                            faEnd = as.character(faEnd))
  
  
  returnTable
}

library(doSNOW)
cl <- makeCluster(11)
registerDoSNOW(cl)

pb <- txtProgressBar(max = nrow(timing), style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)


allTiming <- foreach(i = timing$elkYear,
                         .errorhandling = 'pass',
                         .options.snow = opts,
                         .combine = 'rbind',
                         .packages = c('tidyverse', 'lubridate','data.table')) %dopar%  getTiming(i)
stopCluster(cl)



##labelling actual gps data by period
##there will be some overlap, so timestamps will be duplicated
##adding one day on either side of migrations
##winter is 60 days before spring mig and 60 days after fall mig
##summer (for a single movement elk) is 60 days after spring mig

labelGPSdata <- function(ey) {
  ind <- allTiming %>% filter(elkYear == ey)
 
  #2 movements
  if(sum(is.na(ind[,5:8])) == 0) {
    winter1 <- interval(ymd(ind$spStart) - days(60), ymd(ind$spStart))
    springMig <- interval(ymd(ind$spStart) - days(1), ymd(ind$spEnd) + days(1))
    summer <- interval(ymd(ind$spEnd), ymd(ind$faStart))
    fallMig <- interval(ymd(ind$faStart) - days(1), ymd(ind$faEnd) + days(1))
    winter2 <- interval(ymd(ind$faEnd), ymd(ind$faEnd) + days(60))
    
    winter1Data <- all %>% filter(elkYear == ey, acquisition_time %within% winter1) %>%
      mutate(period = "winter1")
    springMigData <- all %>% filter(elkYear == ey, acquisition_time %within% springMig) %>%
      mutate(period = "springMig")
    summerData <- all %>% filter(elkYear == ey, acquisition_time %within% summer) %>%
      mutate(period = "summer")
    fallMigData <- all %>% filter(elkYear == ey, acquisition_time %within% fallMig) %>%
      mutate(period = "fallMig")
    winter2Data <- all %>% filter(elkYear == ey, acquisition_time %within% winter2) %>%
      mutate(period = "winter2")
    
    return(rbindlist(list(winter1Data, springMigData, summerData, fallMigData, winter2Data)))
  } else if(sum(is.na(ind[,5:8])) == 2) { #1 movement
    winter1 <- interval(ymd(ind$spStart) - days(60), ymd(ind$spStart))
    springMig <- interval(ymd(ind$spStart) - days(1), ymd(ind$spEnd) + days(1))
    summer <- interval(ymd(ind$spEnd), ymd(ind$spEnd) + days(60))
    
    winter1Data <- all %>% filter(elkYear == ey, acquisition_time %within% winter1) %>%
      mutate(period = "winter1")
    springMigData <- all %>% filter(elkYear == ey, acquisition_time %within% springMig) %>%
      mutate(period = "fallMig")
    summerData <- all %>% filter(elkYear == ey, acquisition_time %within% summer) %>%
      mutate(period = "summer")
    
    return(rbindlist(list(winter1Data, springMigData, summerData)))
  } else if(sum(is.na(ind[,5:8])) == 4) { #1 movement
    all %>% filter(elkYear == ey) %>% mutate(period = "winter1") %>% return()
  }
}


cl <- makeCluster(11)
registerDoSNOW(cl)

pb <- txtProgressBar(max = nrow(timing), style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)


allGPSlabeled <- foreach(i = allTiming$elkYear,
                     .errorhandling = 'pass',
                     .options.snow = opts,
                     .combine = 'rbind',
                     .packages = c('tidyverse', 'lubridate','data.table')) %dopar%  labelGPSdata(i)
stopCluster(cl)


#fwrite(allGPSlabeled, "allGPSlabeledSeasons.csv")

allGPSlabeled <- fread("allGPSlabeledSeasons.csv")


allGPSlabeled <- allGPSlabeled %>% mutate(herd = ifelse(herd == "Owl Creek", "Owl Creek Mountain", herd))

#to get time lag percetiles
range01 <- function(x){(x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))}

allPeriods <- allGPSlabeled %>% distinct(elkYear, period)



getBBMM99isopleth <- function(row){
  #subsetting to period
  data <- allGPSlabeled %>% filter(elkYear == allPeriods$elkYear[row],
                                   period == allPeriods$period[row]) %>%
    dplyr::select(elkYear, startDateYear, acquisition_time, X, Y) %>% arrange(acquisition_time)
  
  #fitting bbmm
  timeLag <- as.numeric(difftime(data$acquisition_time, lag(data$acquisition_time), 
                                 units = "mins"))
  #removing either the top or bottom 1% of timelags
  timeLagPercentiles <- range01(timeLag)
  
  time <- data.table(timeLag, timeLagPercentiles)
  
  data <- cbind(data, time) %>% arrange(acquisition_time)
  
  #removing the top 1% of fixes, unless all the data are in the top 1%, then remove bottom 1%
  numInTop1 <- filter(data, timeLagPercentiles >= 0.99) %>% nrow()
  bottom99 <- filter(data, timeLagPercentiles < 0.99) %>% nrow()
  if (length(unique(data$timeLag[2:nrow(data)])) != 1) {
    #checking to see if there is more data in the top 1% then the rest
    if (numInTop1 > bottom99) {
      data <- data[-which(data$timeLagPercentiles < .01),]
    } else {
      data <- data[-which(data$timeLagPercentiles > .99),]
    }
  }
  
  #ought to be 25 meters to get all to fit correctly, but takes too long
  
  bb <- brownian.bridge(x = data$X, y = data$Y, time.lag = data$timeLag[2:nrow(data)], 
                        location.error = 30, cell.size = 100)
  
  
  #getting 99 contour for corridors
  contours <- bbmm.contour(bb, levels=99, locations=data, plot = F)
  bbDF <- data.table(X = bb[["x"]], Y = bb[["y"]], prob = bb[["probability"]]) 
  r <-  rasterFromXYZ(bbDF,crs=CRS("+proj=utm +zone=12 +datum=WGS84 +units=m +no_defs"),digits=2)
  out <- tryCatch(
    {
      rasterToContour(r,levels=contours$Z) %>% st_as_sf() %>% st_cast("POLYGON") %>%
        mutate(elkYear = allPeriods$elkYear[row], period = allPeriods$period[row], herd = ind$herd[1]) %>%
        dplyr::select(-level)
    },
    error = function(cond){
      NULL
    })
  out
}


cl <- makeCluster(11)
registerDoSNOW(cl)

pb <- txtProgressBar(max = 11, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)


rangesList <- foreach(i = 1:11,
                         .errorhandling = 'pass',
                         .options.snow = opts,
                         .packages = c('tidyverse', 'lubridate','data.table',
                                       'BBMM', 'sf', 'raster')) %dopar%  getBBMM99isopleth(i)
stopCluster(cl)

ranges <- Filter(is.data.frame, rangesList) %>% mapedit:::combine_list_of_sf()

st_write(ranges, "finalBBMMrangesWithNewData", "all", driver = "ESRI Shapefile", append = F)
