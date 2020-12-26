library(data.table)
library(lubridate)
library(plotly)
library(leaflet)
library(htmltools)
library(crosstalk)
library(tidyverse)
library(sf)

#CHANGE
setwd("C:/Users/MiddletonLab/Desktop/Gabe/Box Sync/Elk/Working Lands")

bursts <- fread("burstsCleanedSubsetBothWintersLabeled.csv")
bursts$acquisition_time <- ymd_hms(bursts$acquisition_time)

#converting all to utm so coords are in meters
bursts <- bursts %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% st_transform(32612)
coords <- st_coordinates(bursts)
bursts <- bursts %>% st_drop_geometry() %>% cbind(coords)

#add distance to elevM curves

#for one ind at a time, taking one point per day
makeNSD <- function(ey) {
  ind <- bursts %>% filter(elkYear == ey)  %>%
    mutate(yday = paste0(year(acquisition_time), yday(acquisition_time))) %>% distinct(yday, .keep_all = T) %>% 
    arrange(acquisition_time)
  
  strategy <- ind$strategy[1]
  
  #adding squared displacement and elev from first point to all
  first <- ind[1, 13:14]
  
  firstEl <- ind$elevation[1]
  
  
  #function to get dist
  ind$distance <- map_dbl(1:nrow(ind), ~dist(rbind(first, ind[.x, 13:14])))
  #function to get elevation
  ind$elevation <- map_dbl(1:nrow(ind), ~(ind$elevation[.x]-firstEl))
  ind$day <- 1:nrow(ind)
  
  ind$date <- as_date(ind$acquisition_time)
  
  forPlots <- ind %>% st_as_sf(coords = c("X", "Y"), crs = 32612) %>%
    st_transform(4326) %>% highlight_key()
  
  nsdPlot <- plot_ly(forPlots, x = ~date, y = ~distance) %>%
      add_markers() %>% highlight("plotly_hover", opacityDim = .1) %>% 
      layout(title=paste(ey, "classified as", strategy))
  
  nsdElevPlot <- plot_ly(forPlots, x = ~date, y = ~elevation) %>%
      add_markers() %>% highlight("plotly_hover", opacityDim = .1) %>% 
      layout(title=paste(ey, "classified as", strategy))
  
  #need to also plot points on a map
  map <- leaflet(forPlots) %>% addProviderTiles(providers$Esri.WorldTopoMap) %>%
    addCircles(label = ~htmlEscape(date), opacity = 1) %>%
    addScaleBar()
  
  
  
  if(strategy %in% c("LDM")) {
      bscols(device = "xs",
      widths = c(6, 6), 
      nsdPlot, map)
    
  } else {
      bscols(
    widths = c(4, 4, 4), device = "xs",
    nsdPlot, nsdElevPlot, map)
  }
  
}



#RUN THE FOLLOWING, UPDATING INDEX FOR EACH INDIVIDUAL
makeNSD(unique(bursts$elkYear)[10])




