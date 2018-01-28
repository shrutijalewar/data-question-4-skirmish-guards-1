install.packages(c("plyr", "ggplot2","rgeos", "maptools", "gpclib", "sp", "raster", "rgdal", "sf"))
library(plyr)
library(ggplot2)
library(rgeos)
library(maptools)
library(sp)
library(raster)
library(rgdal)
library(sf)

setwd("C:/Users/Carmijh0/Desktop/GIT/Q4/data-question-4-skirmish-guards-1")
tn <- readShapeSpatial("data/TN_counties.shp")
plot(tn)

View(tn)

tn <- fortify(tn, region = 'NAME')
ggplot() + geom_map(data = merged_county, aes(map_id = , fill = Graduation), 
                    map = tn) + expand_limits(x = tn$long, y = tn$lat)

?geom_map()
