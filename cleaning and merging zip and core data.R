library("tidyverse")
library("dplyr")
library("readxl")

#Importing the zip code and core dataframes.  

core <- read_csv("data/achievement_profile_data_with_CORE.csv")
zips <- read_excel("data/zip_code_database.xlsx")

#Fist I need to isolate only the TN zipcodes within zips. 

TN_zips <- zips %>%
  filter(state == 'TN')

#Removing unnecessary columns from zips. 

TN_zips <- TN_zips[-c(2, 3, 5, 6, 9:12)]

#Removing the system column, and adding aggregate columns for each discipline.

core <- core[-c(1)]
colnames(core)[1] <- 'county'
core$county <- gsub("Jackson-Madison County", "Madison County", core$county)
core$county <- gsub("Gibson County Special School District", "Gibson County", core$county)
core$county <- gsub("South Carroll Special School District", "Carroll County", core$county)

View(core)
View(TN_zips)

core <- core %>%
  rowwise() %>% 
  mutate(avg_Math = mean(c(Math, AlgI, AlgII), na.rm = TRUE),
         avg_Eng = mean(c(ELA, EngI, EngII, EngIII), na.rm = TRUE),
         avg_Sci = mean(c(Science, BioI, Chemistry), na.rm = TRUE))

View(core)
#Merging via county and rearranging the colnames to display the zip's and county's in th first two columns.  

merged_county <- core %>% 
  inner_join(TN_zips, by = 'county') 

View(merged_county)

#I want to find the top and bottom 5 counties when it comes to graduation rates.  

gradrate_by_county <- merged_county %>% 
  group_by(county) %>% 
  summarise(Graduation = mean(Graduation, na.rm = TRUE)) %>% 
  arrange(desc(Graduation)) %>% 
  ungroup()

View(gradrate_by_county)

top_5_gradrate <- gradrate_by_county %>% 
  slice(1:5)
bottom_5_gradrate <- gradrate_by_county %>% 
  tail(5)
  
#Figuring out the top and bottom 5 county dropout rates.   

droprate_by_county <- merged_county %>% 
  group_by(county) %>% 
  summarise(Dropout = mean(Dropout, na.rm = TRUE)) %>% 
  arrange(desc(Dropout)) %>% 
  ungroup()

Top_5_droprate <- droprate_by_county %>% 
  slice(1:5)
Bottom_5_droprate <- droprate_by_county %>% 
  tail(5)

#Exploring how the avg Math, Eng, and Sci rates compare within counties.  

avgs_by_county <- merged_county %>% 
  group_by(county) %>% 
  summarise(avg_Math = mean(avg_Math),
            avg_Eng = mean(avg_Eng),
            avg_Sci = mean(avg_Sci)) %>% 
  arrange(desc(avg_Math), desc(avg_Eng), desc(avg_Sci)) %>% 
  ungroup()

View(avgs_by_county)

#How many Core regions are there? ---> 8

merged_county %>% 
  group_by(CORE_region) %>% 
  summarize(count = n())

#Exploring avg's of core subjects amongst regions.

avgmath_by_region <- merged_county %>% 
  group_by(CORE_region) %>% 
  summarise(avg_Math = mean(avg_Math))
  arrange(desc(avg_Math))
  
ggplot(avgmath_by_region, aes(x = CORE_region, y = avg_Math)) +
  geom_col()

avgEng_by_region <- merged_county %>% 
  group_by(CORE_region) %>% 
  summarise(avg_Eng = mean(avg_Eng)) %>% 
  arrange(desc(avg_Eng))

ggplot(avgEng_by_region, aes(x = CORE_region, y = avg_Eng)) +
  geom_col()

avgSci_by_region <- merged_county %>% 
  group_by(CORE_region) %>% 
  summarise(avg_Sci = mean(avg_Sci)) %>% 
  arrange(desc(avg_Sci))

ggplot(avgSci_by_region, aes(x = CORE_region, y = avg_Sci)) +
  geom_col()



#------------------------------------------------------------------------------------
