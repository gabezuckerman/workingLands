library(tidyverse)
library(data.table)
library(lubridate)
library(Hmisc)
library(sf)
library(mapview)
library(raster)
library(velox)


setwd("C:/Users/MiddletonLab/Desktop/Gabe/Box Sync/Elk/Working Lands")

#checking to see if there are any elk years with issues in the paths
#removing issue elk-years

bursts <- fread("burstsNC.csv") %>%
  mutate(elkYear = paste0(gps_sensors_animals_id, "_", year),
         acquisition_time = ymd_hms(acquisition_time)) %>% select(-summer) %>%
  distinct(gps_sensors_animals_id, acquisition_time, .keep_all = T)


herds <- unique(bursts$herd)

#Northern
northern <- bursts %>% filter(herd == herds[1])  %>% arrange(acquisition_time)

#looking at paths
# paths <- northern %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% group_by(elkYear) %>% 
#   summarise(do_union=FALSE) %>% st_cast("LINESTRING")
bad <- c()
# mapview(paths, zcol = "elkYear", color = sf.colors(nrow(paths), categorical = T), 
#         alpha = .75, label = paths$elkYear, lwd = 1, legend = FALSE)
bad <- c("834_2", "827_2", "1375_1", "1386_1", "1383_1", "1366_1", "1376_1",
         "829_1", "1375_2", "1361_2", "1364_1", "1317_1", "1370_1", "1367_1", "1361_1",
         "1354_1")
# mapview(paths %>% filter(elkYear %nin% bad), zcol = "elkYear", color = sf.colors(nrow(paths), categorical = T), 
#         alpha = .75, label = paths$elkYear, lwd = 1, legend = FALSE)

#removing bad inds
northern <- northern %>% filter(elkYear %nin% bad)

#removing from table, then adding edited table
bursts <- bursts %>% filter(herd != "Northern") %>% rbind(northern)

#########################################################################

#Gallatin

#good
#########################################################################

#North Madison
nMadison <- bursts %>% filter(herd == herds[3])  %>% arrange(acquisition_time)

#looking at paths
# paths <- nMadison %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% group_by(elkYear) %>% 
#   summarise(do_union=FALSE) %>% st_cast("LINESTRING")
bad <- c()
# mapview(paths, zcol = "elkYear", color = sf.colors(nrow(paths), categorical = T), 
#         alpha = .75, label = paths$elkYear, lwd = 1, legend = FALSE)
bad <- c("1241_2")
# mapview(paths %>% filter(elkYear %in% bad), zcol = "elkYear", color = sf.colors(nrow(paths), categorical = T), 
#         alpha = .75, label = paths$elkYear, lwd = 1, legend = FALSE)

#removing bad inds
nMadison <- nMadison %>% filter(elkYear %nin% bad)

#removing from table, then adding edited table
bursts <- bursts %>% filter(herd != "North Madison") %>% rbind(nMadison)

#########################################################################


#Jackson

#866_2 crazy: maybe cody

jackson <- bursts %>% filter(herd == herds[4])  %>% arrange(acquisition_time)

#looking at paths
# paths <- jackson %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% group_by(elkYear) %>% 
#   summarise(do_union=FALSE) %>% st_cast("LINESTRING")
bad <- c()
# mapview(paths, zcol = "elkYear", color = sf.colors(nrow(paths), categorical = T), 
#         alpha = .75, label = paths$elkYear, lwd = 1, legend = FALSE)
bad <- c("462_1")
# mapview(paths %>% filter(elkYear %nin% bad), zcol = "elkYear", color = sf.colors(nrow(paths), categorical = T), 
#         alpha = .75, label = paths$elkYear, lwd = 1, legend = FALSE)

#removing bad inds
jackson <- jackson %>% filter(elkYear %nin% bad)

#removing from table, then adding edited table
bursts <- bursts %>% filter(herd != "Jackson") %>% rbind(jackson)

#########################################################################

#Gooseberry


goose <- bursts %>% filter(herd == herds[5])  %>% arrange(acquisition_time) 

#looking at paths
# paths <- goose %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% group_by(elkYear) %>% 
#   summarise(do_union=FALSE) %>% st_cast("LINESTRING")
bad <- c()
# mapview(paths, zcol = "elkYear", color = sf.colors(nrow(paths), categorical = T), 
#         alpha = .75, label = paths$elkYear, lwd = 1, legend = FALSE)
bad <- c("1738_1", "1675_1", "1708_2", "1708_1", "1717_1", "1717_2", 
         "1641_2", "1641_1", "1632_2", "1675_2", "1632_1", "1689_1",
         "1735_1")
# mapview(paths %>% filter(elkYear %nin% bad), zcol = "elkYear", color = sf.colors(nrow(paths), categorical = T), 
#         alpha = .75, label = paths$elkYear, lwd = 1, legend = FALSE)

#removing bad inds
goose <- goose %>% filter(elkYear %nin% bad)

#removing from table, then adding edited table
bursts <- bursts %>% filter(herd != "Gooseberry") %>% rbind(goose)

#########################################################################

#Madison

madison <- bursts %>% filter(herd == herds[6])  %>% arrange(acquisition_time) 

#looking at paths
# paths <- madison %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% group_by(elkYear) %>% 
#   summarise(do_union=FALSE) %>% st_cast("LINESTRING")
bad <- c()
# mapview(paths, zcol = "elkYear", color = sf.colors(nrow(paths), categorical = T), 
#         alpha = .75, label = paths$elkYear, lwd = 1, legend = FALSE)
bad <- c("1298_1")
# mapview(paths %>% filter(elkYear %in% bad), zcol = "elkYear", color = sf.colors(nrow(paths), categorical = T), 
#         alpha = .75, label = paths$elkYear, lwd = 1, legend = FALSE)

#removing bad inds
madison <- madison %>% filter(elkYear %nin% bad)

#removing from table, then adding edited table
bursts <- bursts %>% filter(herd != "Madison") %>% rbind(madison)

#########################################################################

#Targhee

targhee <- bursts %>% filter(herd == herds[7])  %>% arrange(acquisition_time) 

#looking at paths
# paths <- targhee %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% group_by(elkYear) %>% 
#   summarise(do_union=FALSE) %>% st_cast("LINESTRING")
bad <- c()
# mapview(paths, zcol = "elkYear", color = sf.colors(nrow(paths), categorical = T), 
#         alpha = .75, label = paths$elkYear, lwd = 1, legend = FALSE)
bad <- c("1134_2")
# mapview(paths %>% filter(elkYear %in% bad), zcol = "elkYear", color = sf.colors(nrow(paths), categorical = T), 
#         alpha = .75, label = paths$elkYear, lwd = 1, legend = FALSE)

#removing bad inds
targhee <- targhee %>% filter(elkYear %nin% bad)

#removing from table, then adding edited table
bursts <- bursts %>% filter(herd != "Targhee") %>% rbind(targhee)

#########################################################################

#Clark's Fork

cf <- bursts %>% filter(herd == herds[8])  %>% arrange(acquisition_time) 

#looking at paths
# paths <- cf %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% group_by(elkYear) %>% 
#   summarise(do_union=FALSE) %>% st_cast("LINESTRING")
bad <- c()
# mapview(paths, zcol = "elkYear", color = sf.colors(nrow(paths), categorical = T), 
#         alpha = .75, label = paths$elkYear, lwd = 1, legend = FALSE)
bad <- c("801_1", "790_2", "755_3", "749_1", "728_3", "746_1", "746_2",
         "748_2", "760_2", "740_1", "767_1", "736_1", "742_2", "754_2",
         "760_1", "732_2", "765_1", "762_3", "760_3", "745_2", "744_2")
# mapview(paths %>% filter(elkYear %in% bad), zcol = "elkYear", color = sf.colors(nrow(paths), categorical = T), 
#         alpha = .75, label = paths$elkYear, lwd = 1, legend = FALSE)

#removing bad inds
cf <- cf %>% filter(elkYear %nin% bad)

#removing from table, then adding edited table
bursts <- bursts %>% filter(herd != "Clarks Fork") %>% rbind(cf)

#########################################################################

#Blacktail
#good

#########################################################################

#Border

#good

#########################################################################
#Silver Run

#good

#########################################################################

#Greycliff

#########################################################################

#Deer Creeks

#good
#########################################################################

#Mill Creek

mc <- bursts %>% filter(herd == herds[14])  %>% arrange(acquisition_time) 

#looking at paths
# paths <- mc %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% group_by(elkYear) %>%
#   summarise(do_union=FALSE) %>% st_cast("LINESTRING")
# bad <- c()
# mapview(paths, zcol = "elkYear", color = sf.colors(nrow(paths), categorical = T),
#         alpha = .75, label = paths$elkYear, lwd = 1, legend = FALSE)
bad <- c("1497_4")
# mapview(paths %>% filter(elkYear %nin% bad), zcol = "elkYear", color = sf.colors(nrow(paths), categorical = T),
#         alpha = .75, label = paths$elkYear, lwd = 1, legend = FALSE)

#removing bad inds
mc <- mc %>% filter(elkYear %nin% bad)

#removing from table, then adding edited table
bursts <- bursts %>% filter(herd != "Mill Creek") %>% rbind(mc)
#########################################################################

#Hoback

#good
#########################################################################

#Pinedale

pinedale <- bursts %>% filter(herd == herds[16])  %>% arrange(acquisition_time) 

# #looking at paths
# paths <- pinedale %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% group_by(elkYear) %>%
#   summarise(do_union=FALSE) %>% st_cast("LINESTRING")
# bad <- c()
# mapview(paths, zcol = "elkYear", color = sf.colors(nrow(paths), categorical = T),
#         alpha = .75, label = paths$elkYear, lwd = 1, legend = FALSE)
bad <- c("634_2")
# mapview(paths %>% filter(elkYear %nin% bad), zcol = "elkYear", color = sf.colors(nrow(paths), categorical = T),
#         alpha = .75, label = paths$elkYear, lwd = 1, legend = FALSE)

#removing bad inds
pinedale <- pinedale %>% filter(elkYear %nin% bad)

#removing from table, then adding edited table
bursts <- bursts %>% filter(herd != "Pinedale") %>% rbind(pinedale)
#########################################################################

#Piney

piney <- bursts %>% filter(herd == herds[17])  %>% arrange(acquisition_time) 

# #looking at paths
# paths <- piney %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% group_by(elkYear) %>%
#   summarise(do_union=FALSE) %>% st_cast("LINESTRING")
# bad <- c()
# mapview(paths, zcol = "elkYear", color = sf.colors(nrow(paths), categorical = T),
#         alpha = .75, label = paths$elkYear, lwd = 1, legend = FALSE)
bad <- c("267_1", "268_2", "225_1", "83_2", "538_1", "120_2", "584_1", "579_2")
# mapview(paths %>% filter(elkYear %nin% bad), zcol = "elkYear", color = sf.colors(nrow(paths), categorical = T),
#         alpha = .75, label = paths$elkYear, lwd = 1, legend = FALSE)

#removing bad inds
piney <- piney %>% filter(elkYear %nin% bad)

#removing from table, then adding edited table
bursts <- bursts %>% filter(herd != "Piney") %>% rbind(piney)
#########################################################################

#Afton

afton <- bursts %>% filter(herd == herds[18])  %>% arrange(acquisition_time) 

#looking at paths
# paths <- afton %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% group_by(elkYear) %>%
#   summarise(do_union=FALSE) %>% st_cast("LINESTRING")
# bad <- c()
# mapview(paths, zcol = "elkYear", color = sf.colors(nrow(paths), categorical = T),
#         alpha = .75, label = paths$elkYear, lwd = 1, legend = FALSE)
bad <- c("303_1", "253_2")
# mapview(paths %>% filter(elkYear %nin% bad), zcol = "elkYear", color = sf.colors(nrow(paths), categorical = T),
#         alpha = .75, label = paths$elkYear, lwd = 1, legend = FALSE)

#removing bad inds
afton <- afton %>% filter(elkYear %nin% bad)

#removing from table, then adding edited table
bursts <- bursts %>% filter(herd != "Afton") %>% rbind(afton)
#########################################################################

#Green River

gr <- bursts %>% filter(herd == herds[19])  %>% arrange(acquisition_time) 

#looking at paths
# paths <- gr %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% group_by(elkYear) %>%
#   summarise(do_union=FALSE) %>% st_cast("LINESTRING")
# bad <- c()
# mapview(paths, zcol = "elkYear", color = sf.colors(nrow(paths), categorical = T),
#         alpha = .75, label = paths$elkYear, lwd = 1, legend = FALSE)
bad <- c("282_1")
# mapview(paths %>% filter(elkYear %nin% bad), zcol = "elkYear", color = sf.colors(nrow(paths), categorical = T),
#         alpha = .75, label = paths$elkYear, lwd = 1, legend = FALSE)

#removing bad inds
gr <- gr %>% filter(elkYear %nin% bad)

#removing from table, then adding edited table
bursts <- bursts %>% filter(herd != "Green River") %>% rbind(gr)
#########################################################################

#Fall Creek

fc <- bursts %>% filter(herd == herds[20])  %>% arrange(acquisition_time) 

#looking at paths
# paths <- fc %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% group_by(elkYear) %>%
#   summarise(do_union=FALSE) %>% st_cast("LINESTRING")
# bad <- c()
# mapview(paths, zcol = "elkYear", color = sf.colors(nrow(paths), categorical = T),
#         alpha = .75, label = paths$elkYear, lwd = 1, legend = FALSE)
bad <- c("375_2", "672_2", "375_2", "673_2")
# mapview(paths %>% filter(elkYear %nin% bad), zcol = "elkYear", color = sf.colors(nrow(paths), categorical = T),
#         alpha = .75, label = paths$elkYear, lwd = 1, legend = FALSE)

#removing bad inds
fc <- fc %>% filter(elkYear %nin% bad)

#removing from table, then adding edited table
bursts <- bursts %>% filter(herd != "Fall Creek") %>% rbind(fc)
#########################################################################

#South Wind River

swr <- bursts %>% filter(herd == herds[21])  %>% arrange(acquisition_time) 

#looking at paths
# paths <- swr %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% group_by(elkYear) %>%
#   summarise(do_union=FALSE) %>% st_cast("LINESTRING")
# bad <- c()
# mapview(paths, zcol = "elkYear", color = sf.colors(nrow(paths), categorical = T),
#         alpha = .75, label = paths$elkYear, lwd = 1, legend = FALSE)
bad <- c("364_1")
# mapview(paths %>% filter(elkYear %nin% bad), zcol = "elkYear", color = sf.colors(nrow(paths), categorical = T),
#         alpha = .75, label = paths$elkYear, lwd = 1, legend = FALSE)

#removing bad inds
swr <- swr %>% filter(elkYear %nin% bad)

#removing from table, then adding edited table
bursts <- bursts %>% filter(herd != "South Wind River") %>% rbind(swr)
#########################################################################

#Greeley

greeley <- bursts %>% filter(herd == herds[22])  %>% arrange(acquisition_time) 

#looking at paths
# paths <- greeley %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% group_by(elkYear) %>%
#   summarise(do_union=FALSE) %>% st_cast("LINESTRING")
# bad <- c()
# mapview(paths, zcol = "elkYear", color = sf.colors(nrow(paths), categorical = T),
#         alpha = .75, label = paths$elkYear, lwd = 1, legend = FALSE)
bad <- c("1522_1")
# mapview(paths %>% filter(elkYear %nin% bad), zcol = "elkYear", color = sf.colors(nrow(paths), categorical = T),
#         alpha = .75, label = paths$elkYear, lwd = 1, legend = FALSE)

#removing bad inds
greeley <- greeley %>% filter(elkYear %nin% bad)

#removing from table, then adding edited table
bursts <- bursts %>% filter(herd != "Greeley") %>% rbind(greeley)
#########################################################################

#Sand Creek

sc <- bursts %>% filter(herd == herds[23])  %>% arrange(acquisition_time) 

#looking at paths
# paths <- sc %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% group_by(elkYear) %>%
#   summarise(do_union=FALSE) %>% st_cast("LINESTRING")
# bad <- c()
# mapview(paths, zcol = "elkYear", color = sf.colors(nrow(paths), categorical = T),
#         alpha = .75, label = paths$elkYear, lwd = 1, legend = FALSE)
bad <- c("935_2")
# mapview(paths %>% filter(elkYear %nin% bad), zcol = "elkYear", color = sf.colors(nrow(paths), categorical = T),
#         alpha = .75, label = paths$elkYear, lwd = 1, legend = FALSE)

#removing bad inds
sc <- sc %>% filter(elkYear %nin% bad)

#removing from table, then adding edited table
bursts <- bursts %>% filter(herd != "Sand Creek") %>% rbind(sc)
#########################################################################

#Wiggins Fork

#good
#########################################################################

#Cody

cody <- bursts %>% filter(herd == herds[25])  %>% arrange(acquisition_time) %>% filter(dop < 5)

#looking at paths
# paths <- cody %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% group_by(elkYear) %>%
#   summarise(do_union=FALSE) %>% st_cast("LINESTRING")
# bad <- c()
# mapview(paths, zcol = "elkYear", color = sf.colors(nrow(paths), categorical = T),
#         alpha = .75, label = paths$elkYear, lwd = 1, legend = FALSE)
bad <- c("1179_2", "57_2", "28_2", "1168_2", "1798_1", "1808_1", "30_2", "1638_2", "1628_2",
         "1637_2", "1638_1", "1628_1", "1790_1", "1791_1", "1809_1", "1794_1", "1177_1",
         "1806_1", "1786_1", "1177_2", "1803_1", "1804_1", "1799_1", "1795_1", "1787_1",
         "1801_1", "1789_1", "1797_1", "1788_1", "1802_1", "1810_1", "1172_1", "1561_1",
         "1172_2", "1796_1", "31_2", "18_2", "1805_1", "1169_2", "2_1", "1169_5", "1793_1",
         "1792_1", "18_1", "29_1", "1569_1")
# mapview(paths %>% filter(elkYear %nin% bad), zcol = "elkYear", color = sf.colors(nrow(paths), categorical = T),
#         alpha = .75, label = paths$elkYear, lwd = 1, legend = FALSE)

#removing bad inds
cody <- cody %>% filter(elkYear %nin% bad)

#removing from table, then adding edited table
bursts <- bursts %>% filter(herd != "Cody") %>% rbind(cody)
#########################################################################


#adding elevation to all points
dem <- raster("../Switching2/covariateData/SnowData/DEM/gyaDEM.tif")
vDEM <- velox(dem)

burstSF <- bursts %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs ")

elevation <- vDEM$extract_points(burstSF)

bursts$elevation <- elevation

hist(bursts$elevation)

fwrite(bursts, "burstsCleaned.csv")

#1800 elk years, 1213 elk, 25 herds

sum <- bursts %>% group_by(herd) %>% summarise(elkYears = n_distinct(elkYear))
