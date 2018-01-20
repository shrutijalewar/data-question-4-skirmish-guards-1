setwd('/Users/ssharma/code/nss-ds/data-question-4-skirmish-guards-1')
library("tidyverse")
library("dplyr")
library("magrittr")
library("ggplot2")
# install.packages("readxl")
library("readxl")

# reading the cleaner all year IRS data
irs_2011 <- read_csv("data/irs_2011.csv")

irs_2012 <- read_csv("data/irs_2012.csv")

irs_2013 <- read_csv("data/irs_2013.csv")

irs_2014 <- read_csv("data/irs_2014.csv")

irs_2015 <- read_csv("data/irs_2015.csv")

edu_2014 <- read_csv("data/education_data.csv")
names(edu_2014)[2] <- 'zip_code'
View(edu_2014)
# Putthing it all together in one df
total_irs <- rbind(irs_2011,irs_2012,irs_2013,irs_2014,irs_2015)
View(total_irs)
glimpse(total_irs)
glimpse(edu_2014)

# filtering to only total values per zip code to combine with education data.
total_irs <- filter(total_irs, agi_range == 'Total')
View(total_irs)
edu_2014$zip_code <- edu_2014$zip
# Merge the edu and irs together
total <- merge(total_irs,edu_2014,by="zip_code")
glimpse(total)
View(total)
