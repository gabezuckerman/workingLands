library(migrateR)
library(plyr)
library(data.table)
library(lubridate)
library(doParallel)
library(tictoc)

setwd("C:/Users/MiddletonLab/Desktop/Gabe/Box Sync/Elk/Working Lands")

ldm <- fread("ldmGPS.csv")
ldm$acquisition_time <- ymd_hms(ldm$acquisition_time)


#for one ind at a time
getTiming <- function(ey) {
  
  ind <- ldm %>% filter(elkYear == ey) %>% arrange(acquisition_time)
  
  #converting to ltraj
  track <- as.ltraj(xy = cbind(ind$longitude, ind$latitude), 
                    date = ind$acquisition_time, id = ey) 
  
  #finding best start date for model
  rlocs <- findrloc(track)
  
  #fitting NSD
  nsd <- mvmtClass(track, rloc = rlocs$rloc)

  plot(nsd)
  
  #getting timing table
  #if no mig model, then using mixed migrant timing
  timeTable <- tryCatch({
    mvmt2dt(nsd, mod = "migrant")
  }, error = function(e){
    mvmt2dt(nsd, mod = "mixmig")
  })
  
  #extracting start and end dates
  spStart <- ldply(timeTable, data.frame)$date[1]
  spEnd <- ldply(timeTable, data.frame)$date[2]
  faStart <- ldply(timeTable, data.frame)$date[3]
  faEnd <- ldply(timeTable, data.frame)$date[4]
  
  #returning
  return(data.table(elkYear = ey, spStart, spEnd, faStart, faEnd))
}

##parallelizing calculation
cl <- makeCluster(6)

registerDoParallel(cl)

tic()
timing <- foreach(ey = unique(ldm$elkYear),
                          .errorhandling = 'pass',
                          .packages = c('tidyverse', 'lubridate', 'plyr',
                                        'data.table', 'migrateR')) %dopar%
                    getTiming(ey)
toc()
stopCluster(cl)

timing2 <- rbindlist(timing, use.names = TRUE, fill = TRUE)
fwrite(timing2, "migTimingDraft.csv")




#


        