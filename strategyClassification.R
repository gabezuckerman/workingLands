library(tidyverse)
library(data.table)
library(NbClust)
library(ggplot2)
library(plotly)
library(sf)
library(mapview)
library(Hmisc)


setwd("C:/Users/MiddletonLab/Desktop/Gabe/Box Sync/Elk/Working Lands")

rangeDistances <- fread("rangeDistElevBBMM.csv") 
# %>%
#   filter(elkYear %nin% c("702_4", "58_2", "23_1", "457_2", "883_2"))



rangeDistances <- rangeDistances %>% 
  mutate(scaledDistance = scale(distance),
       scaledElevDiff = scale(elevDiff))

forCluster <- rangeDistances %>% dplyr::select(scaledDistance, scaledElevDiff) 

##Determining optimal number of clusters


#NbClust(data = forCluster, method = "kmeans", index = "all")

#clustering with 3 groups
k3_twoD <- kmeans(forCluster, centers = 3, nstart = 25)

rangeDistances$cluster3 <- k3_twoD$cluster

ggplot(rangeDistances) + geom_point(aes(x = elevDiff, y = distance, color = as.factor(cluster3)))


labels <- rangeDistances %>% group_by(cluster3) %>% summarise(dist = mean(distance)) %>% 
  arrange(dist) %>% dplyr::select(-dist)
labels$strategyDE <- c("R/SDM", "ElevM", "LDM")
rangeDistances <- rangeDistances %>% merge(labels) 

#15 km reclassification
rangeDistances <- rangeDistances %>% mutate(strategyDE = ifelse((strategyDE == "R/SDM" & distance <= 15000), "R", strategyDE),
                            strategyDE = ifelse((strategyDE == "R/SDM" & distance > 15000), "SDM", strategyDE)) %>%
  dplyr::select(-cluster3)



#plotting to show clusters
clusterPlot <- ggplot(rangeDistances, aes(text = paste0("ind-year: ", elkYear, "\n",
                                                "herd: ", herd))) +
  geom_jitter(aes(y = distance, x = elevDiff, color = strategyDE)) +
  ylab("Distance between ranges (m)") + xlab("Elevation difference between ranges (m)") +
  scale_color_brewer(palette = "Accent")


ggplotly(clusterPlot)

table(rangeDistances$strategyDE)


#adding strategy to gps data
bursts <- fread("burstsCleanedSubset.csv")

bursts <- bursts %>% merge(
  rangeDistances %>% dplyr::select(elkYear, strategy = strategyDE)
)

fwrite(bursts, "burstsCleanedSubsetLabeled.csv")





#215 LDM; 14 herds (out of 25)
ldm <- rangeDistances %>% filter(strategyDE == "LDM")

table(ldm$herd)

#getting gps data for LDM and making map
ldmData <- fread("burstsCleaned2.csv") %>% filter(elkYear %in% ldm$elkYear) 

fwrite(ldmData, "ldmGPS.csv")

#
choices <- ldmData %>% distinct(elkYear, herd)

herds <- unique(choices$herd)



getIndsForMap <- function(h) {
  ht <- filter(choices, herd == h)
  if(nrow(ht) <= 10) {
    return(ht)
  }
  else {
    return(ht %>% sample_n(10))
  }
}


indsForMap <- map_dfr(herds, getIndsForMap)



getIndData <- function(ey) {
  return(ldmData %>% filter(elkYear == ey))
}

forMap <- map_dfr(indsForMap$elkYear, getIndData) %>% 
  arrange(herd, acquisition_time) %>% distinct(elkYear, acquisition_time, .keep_all = T)

paths <- forMap %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  dplyr::select(elkYear, herd, geometry) %>% 
  group_by(elkYear, herd) %>% summarise(do_union=FALSE) %>% 
  st_cast("LINESTRING")



mapview(paths, zcol = "herd", color = sf.colors(n=13, categorical = T), 
        alpha = .75, layer.name = "Herd", lwd = 1, legend = FALSE)
