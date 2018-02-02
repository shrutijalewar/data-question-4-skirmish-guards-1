setwd("C:/Users/Carmijh0/Desktop/GIT/Q4/data-question-4-skirmish-guards-1")

library("dplyr")
library("plyr")
library("ggplot2")
library("ggmap")
library("maps")
library("mapdata")
library("rgeos")
library("maptools")
library("sp")
library("raster")
library("rgdal")
library("sf")
library("broom")
library("stringr")

-------------------------------------------------------------------------------------------------------

#USING GGPLOT2 mapping to explore data

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

tn_base + theme_nothing() + 
  geom_polygon(data = tn_counties, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)  # get the state border back on top

#I need to be able to merge these two to create the ggmap

View(merged_county)
core_raw <- merged_county
colnames(core_raw)[1] <- "subregion" #renaming to match
core_raw[[1]] <- tolower(core_raw[[1]]) #changing to all lowercase

#removing 'county' from the column and tidying for merge

core_raw$subregion <- gsub(" county$", "", core_raw$subregion)
tn_counties$subregion <- replace(tn_counties$subregion, tn_counties$subregion=="de kalb", "dekalb")

View(tn_counties)
View(core_raw)

#merging the two dataframes together with nnner_join

tn_core <- inner_join(tn_counties, core_raw, by = "subregion")

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

tn_sci <- tn_base + 
  geom_polygon(data = tn_core, aes(fill = avg_Sci), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  ditch_the_axes

tn_sci

tn_eng <- tn_base + 
  geom_polygon(data = tn_core, aes(fill = avg_Eng), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  ditch_the_axes

tn_eng

tn_math <- tn_base + 
  geom_polygon(data = tn_core, aes(fill = avg_Math), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  ditch_the_axes

tn_math

tn_per_pupil <- tn_base + 
  geom_polygon(data = tn_core, aes(fill = Per_Pupil_Expenditures), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  ditch_the_axes

tn_per_pupil

tn_per_BHN <- tn_base + 
  geom_polygon(data = tn_core, aes(fill = Pct_BHN), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  ditch_the_axes

tn_per_BHN

tn_act <- tn_base + 
  geom_polygon(data = tn_core, aes(fill = ACT_Composite), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  ditch_the_axes

tn_act

tn_graduation <- tn_base + 
  geom_polygon(data = tn_core, aes(fill = Graduation), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  ditch_the_axes

tn_graduation

tn_dropout <- tn_base + 
  geom_polygon(data = tn_core, aes(fill = Dropout), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  ditch_the_axes

tn_dropout

View(tn_core)


#merging IRS data with map data 


irs <- merged
irs <- rename(irs, c("county"="subregion"))
irs<- irs %>% mutate(subregion = tolower(subregion))
irs$subregion <- gsub(" county$", "", irs$subregion)
tn_irs <- inner_join(tn_counties, irs, by = "subregion")

tn_agi <- tn_base + 
  geom_polygon(data = tn_irs, aes(fill = agi_amt_avg), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  ditch_the_axes + 
  scale_fill_gradientn(colours = rev(rainbow(7)),
                       breaks = c(50, 100, 150, 200))

tn_agi


