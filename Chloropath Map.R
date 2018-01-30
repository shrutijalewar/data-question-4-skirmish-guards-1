install.packages(c("plyr", "ggplot2","rgeos", "maptools", "gpclib", "sp", "raster", "rgdal", "sf"))
library(plyr)
library(ggplot2)
library(rgeos)
library(maptools)
library(sp)
library(raster)
library(rgdal)
library(sf)
library(broom)

setwd("C:/Users/Carmijh0/Desktop/GIT/Q4/data-question-4-skirmish-guards-1")
tn <- readShapeSpatial("data/TN_counties.shp")
plot(tn)

View(tn)
tn <- fortify(tn)

ggplot() + geom_polygon(data = merged_county, aes(x = long, y = lat, fill = Graduation), 
                    map = tn) + expand_limits(x = tn$long, y = tn$lat)


