setwd('/Users/ssharma/code/nss-ds/data-question-4-skirmish-guards-1')
library("tidyverse")
library("dplyr")
library("magrittr")
library("ggplot2")
# install.packages("readxl")
library("readxl")
library("GGally")

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

# Doing initial data exploration
plot(total$agi_amt_avg, total$Dropout)

plot(total$salary_avg, total$Graduation)
plot(as.factor(total$zip_code), total$Pct_Chronically_Absent)
plot(total$zip_code, total$salary_avg)
hist(total$Pct_BHN)
hist(total$Dropout)
hist(total$Graduation)
hist(total$return_count)
hist(total$agi_amt_avg)
plot(total$property_tax_amt, total$Dropout)
plot(as.factor(total$CORE_region), total$Pct_Chronically_Absent)
plot(as.factor(total$CORE_region), total$Pct_BHN)
plot(as.factor(total$CORE_region), total$agi_amt_avg)
plot(total$avg_Eng, total$avg_Math)
plot(total$avg_Sci,total$avg_Math)
plot(total$avg_Eng, total$avg_Sci)

ggcorr(total)

pairs(~avg_Math+Graduation+Pct_BHN+avg_Eng, data=total,
      main="Simple Scatterplot Matrix")

total_1 <- select(total, county, zip_code,CORE_region, year, agi_amt_avg,salary_avg,mortgage_amt_avg,prop_tax_avg,taxable_income_avg,
                  taxable_income_amt, earned_income_credit_avg,excess_eaned_income_credit_avg,avg_Eng, avg_Sci, avg_Math, Pct_Expelled, Pct_BHN,
                  Pct_Suspended, ACT_Composite,earned_income_credit_avg, Pct_ED,Pct_SWD
                  )
ggcorr(total_1)
