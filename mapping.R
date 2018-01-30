install.packages(c("ggplot2", "devtools", "stringr"))
install.packages(c("maps", "mapdata"))
install.packages("ggmap")
install.packages("dplyr")
library(dplyr)
library(plyr)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(rgeos)
library(maptools)
library(sp)
library(raster)
library(rgdal)
library(sf)
library(broom)
library(stringr)


setwd("C:/Users/Carmijh0/Desktop/GIT/Q4/data-question-4-skirmish-guards-1")
tn <- readShapeSpatial("data/TN_counties.shp")
plot(tn)

View(tn)

tn_df <- tidy(tn)
View(tn_df)

tn <- fortify(tn)
View(merged_county)

ggplot() + geom_polygon(data = tn, aes(x = long, y = lat, group = group), 
                    map = tn) + expand_limits(x = tn$long, y = tn$lat)

------------------------------------------------------------------------------------------------


usa <- map_data("usa")
ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)

states <- map_data("state")
head(states)

tn_df <- subset(states, region == "tennessee")
head(tn_df)

tn_base <- ggplot(data = tn_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")
tn_base + theme_nothing()

ggplot() + geom_polygon(data = tn_counties, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)

counties <- map_data("county")
tn_counties <- subset(counties, region == "tennessee")
View(tn_counties)

tn_base + theme_nothing() + 
  geom_polygon(data = tn_counties, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)  # get the state border back on top

#I need to be able to merge these two to create the ggmap

core <- merged_county
colnames(core)[2] <- "subregion" #renaming to match
core[[2]] <- tolower(core[[2]]) #changing to all lowercase
View(core)

#removing 'county' from the column

str_detect(core$subregion, "[:space:].")
grep(" county$", core$subregion, value = TRUE)
core$subregion <- gsub(" county$", "", core$subregion)
View(core)

#merging the two dataframes together with nnner_join

tn_core <- inner_join(tn_counties, core, by = "subregion")
View(tn_core)

#Attempting to plot

ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

tn_1 <- tn_base + 
  geom_polygon(data = tn_core, aes(fill = Graduation), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  ditch_the_axes

tn_1 <- tn_1 + 
  scale_fill_gradientn(colours = rev(rainbow(7)),
                       breaks = c(50, 65, 75, 85, 95, 100))
                    

tn_1
