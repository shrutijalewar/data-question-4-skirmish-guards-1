library("tidyverse")
library("readxl")

#Importing the zip code and core dataframes.  

core <- read_csv("data/achievement_profile_data_with_CORE.csv")
zips <- zip_code_database
View(core)
View(zips)

#Fist I need to isolate only the TN zipcodes within zips. 

TN_zips <- zips %>%
  filter(state == 'TN')

#Removing unnecessary columns from zips. 

TN_zips <- TN_zips[-c(2, 3, 5, 6, 9:12)]
View(TN_zips)

#Removing system

core <- core[-c(1)]
View(core)

#Adding aggregate columns for each discipline

core <- core %>%
  rowwise() %>%
  mutate(avg_Math = mean(c(Math, AlgI, AlgII), na.rm = TRUE),
         avg_Eng = mean(c(ELA, EngI, EngII, EngIII), na.rm = TRUE),
         avg_Sci = mean(c(Science, BioI, Chemistry), na.rm = TRUE))

#------------------------------------------------------------------------------------