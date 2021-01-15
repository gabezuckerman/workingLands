library(data.table)
library(tidyverse)
library(dbscan)
library(lubridate)

source("stdbscan.R")


bursts <- fread("../burstsCleanedSubsetBothWintersLabeled.csv")
bursts$acquisition_time <- ymd_hms(bursts$acquisition_time)

#converting all to utm so coords are in meters
bursts <- bursts %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% st_transform(32612)
coords <- st_coordinates(bursts)
bursts <- bursts %>% st_drop_geometry() %>% cbind(coords)

bursts <- bursts %>% select(elkYear, X, Y, date = acquisition_time, elevation, strategy)

###################################
#trying st dbscan

########################################################################
# INPUTS :                                                             #
# traj = traj gps (x, y and time)                                      #
# eps = distance minimum for longitude and latitude                    #
# eps2 =  distance minimum for date (in minimum unit of timestamp)     #
# minpts = number of points to consider a cluster                      #
########################################################################

ey <- unique(bursts$elkYear)


ind <- bursts %>% filter(elkYear == ey[3])


#using kud to determine min distance




first <- ind[1, 2:3]

ind$distance <- map_dbl(1:nrow(ind), ~dist(rbind(first, ind[.x, 2:3])))

#1 mile, 7 days
numClusters <- 1
scale <- 0
while(numClusters < 4){
  scale <- scale + 1
  res <- stdbscan(ind, x = ind$X, y = ind$Y, time = ind$date,
                  eps = max(ind$distance)/scale, eps2 = (60*60*24*7), minpts = log(nrow(ind)))
  
  ind$cluster <- res$cluster
  numClusters <- length(unique(ind$cluster))
}

ggplotly({
  ggplot(ind) + geom_point(aes(x = date, y = distance, color = as.factor(cluster))) + 
  theme(legend.position = "none")
})


plot_ly(ind, x = ~X, y = ~Y, z = ~date, color = ~as.factor(cluster), size = 1)

ggplotly({
  ggplot(ind) + geom_point(aes(x = X, y = Y, color = as.factor(cluster))) + 
    theme(legend.position = "none")
})



summary <- ind %>% group_by(cluster) %>% 
  summarise(numDays = n(), startDate = min(date), endDate = max(date)) %>%
  arrange(desc(numDays))

ranges <- summary[1:3,] %>% arrange(startDate)
ranges$range <- c("Winter 1", "Summer", "Winter 2")

rest <- summary %>% anti_join(ranges) %>% arrange(cluster)

rest$range <- c("Migrating", rep("Stopover", (nrow(rest) - 1)))

summary2 <- rbind(rest, ranges)

ind <- ind %>% merge(summary2 %>% select(cluster, range))

plot_ly(ind, x = ~date, y = ~distance, color = ~range) %>%
    add_markers()

