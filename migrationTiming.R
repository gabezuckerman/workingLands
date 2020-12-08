library(migrateR)
library(plyr)
library(data.table)
library(lubridate)
library(doParallel)
library(tictoc)
library(lubridate)

setwd("C:/Users/MiddletonLab/Desktop/Gabe/Box Sync/Elk/Working Lands")

bursts <- fread("burstsCleanedSubsetLabeled.csv")
bursts$acquisition_time <- ymd_hms(bursts$acquisition_time)

ey <- unique(bursts$elkYear)[4]

#for one ind at a time
#nothing for residents, elevation for elevM, nsd for sdm, ldm
getTiming <- function(ey) {
  
  ind <- bursts %>% filter(elkYear == ey) %>% arrange(acquisition_time)
  
  strategy <- ind$strategy[1]
  
  if (strategy == "R"){
    return(data.table(elkYear = ey, strategy, spStart = NA, spEnd = NA, faStart = NA, faEnd = NA))
  }
  
  #converting to ltraj, with elevation
  track <- as.ltraj(xy = cbind(ind$longitude, ind$latitude), 
                    date = ind$acquisition_time, id = ey, infolocs = data.frame(elev = ind$elevation)) 
  

  #finding best start date for model
  rlocs <- findrloc(track)
  
  if(strategy == "ElevM") {
    #fitting NSD for elevational migrants
    nsd <- mvmtClass(track, fam = "elev")
  } else {
    #fitting NSD
    nsd <- mvmtClass(track, rloc = rlocs$rloc)
  }
  
  

  #plot(nsd)
  
  #getting timing table
  #trying both migrant and mixmig
  timeTable <- tryCatch({mvmt2dt(nsd)},
                        error=function(cond) {
                          NA
                        })
  
  #and mixmig
  if(is.na(timeTable) | is.null(timeTable[[1]])) {
    timeTable <- tryCatch({mvmt2dt(nsd, mod = "mixmig")},
                          error=function(cond) {
                            NA
                          })
  }
  
  #if neither work, then returning empty timing
  if(is.na(timeTable)) {
    return(data.table(elkYear = ey, strategy, spStart = NA, spEnd = NA, faStart = NA, faEnd = NA))
  }
  
  #extracting start and end dates
  spStart <- ldply(timeTable, data.frame)$date[1]
  spEnd <- ldply(timeTable, data.frame)$date[2]
  faStart <- ldply(timeTable, data.frame)$date[3]
  faEnd <- ldply(timeTable, data.frame)$date[4]
  
  #returning
  return(data.table(elkYear = ey, strategy, spStart, spEnd, faStart, faEnd))
}


##parallelizing calculation
cl <- makeCluster(6)

registerDoParallel(cl)

tic()
timing <- foreach(ey = unique(bursts$elkYear),
                          .errorhandling = 'pass',
                          .packages = c('tidyverse', 'lubridate', 'plyr',
                                        'data.table', 'migrateR')) %dopar%
                    getTiming(ey)
toc()
stopCluster(cl)

timing2 <- bind_rows(timing)
fwrite(timing2, "migTimingDraft.csv")




        