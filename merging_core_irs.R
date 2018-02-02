library("tidyverse")
library("dplyr")
library("magrittr")
library("ggplot2")
#install.packages("readxl")
library("readxl")
#install.packages("GGally")
library("GGally")
#install.packages("broom")
library("broom")

# reading in each of the IRS data sets and the core dataframe

tn_2011 <- read.csv("data/irs_2011.csv")
tn_2012 <- read.csv("data/irs_2012.csv")
tn_2013 <- read.csv("data/irs_2013.csv")
tn_2014 <- read.csv("data/irs_2014.csv")
tn_2015 <- read.csv("data/irs_2015.csv")
core <- read.csv("data/education_data.csv")

View(core)

#rowbinding all the irs data together

tn_irs <- rbind(tn_2011, tn_2012, tn_2013, tn_2014, tn_2015)

#filtering for totals on the agi_range columns to create a concise total tn irs dataframe

tn_irs_total <- tn_irs %>% 
  filter(agi_range == 'Total')

View(tn_irs_total)

#merging core data with irs data - first need to make both zip_code columns are of the same type and column name

colnames(merged_county)[colnames(merged_county) == 'zip'] <- 'zip_code'
merged_county$zip_code <- as.numeric(merged_county$zip_code)
tn_irs_total$zip_code <- as.numeric(tn_irs_total$zip_code)

merged_all <- tn_irs_total %>% 
  inner_join(merged_county, by = 'zip_code')

View(merged_all)

write.csv(merged_all, "merged_all.csv")

#Starting at a birds-eye view of total correlation
#subsetting the df to take a closer look at specific correlation

ggcorr(merged)

total_2 <- select(year, agi_amt_avg,salary_avg,mortgage_amt_avg,prop_tax_avg,taxable_income_avg,
                  taxable_income_amt, earned_income_credit_avg,excess_eaned_income_credit_avg,avg_Eng, avg_Sci, avg_Math, Pct_Expelled, Pct_BHN,
                  Pct_Suspended, ACT_Composite,earned_income_credit_avg, Pct_ED,Pct_SWD
)

chart.Correlation(total_2, histogram=TRUE, pch=19)

#What do I want to explore?

avgs_by_county <- merged %>% 
  group_by(county, year, CORE_region) %>% 
  summarise(avg_Math = mean(avg_Math),
            avg_Eng = mean(avg_Eng),
            avg_Sci = mean(avg_Sci)) %>% 
  arrange(desc(avg_Math), desc(avg_Eng), desc(avg_Sci)) %>% 
  ungroup() %>% 
  View()

#Basic Scatterplot Matrices using the pairs function to visualize broad relationships

pairs(~as.numeric(agi_amt)+Graduation+Pct_BHN+Dropout, data=merged,
      main="Simple Scatterplot Matrix")

pairs(~as.numeric(avg_Math)+Graduation+Pct_BHN+as.numeric(avg_Eng), data=merged,
      main="Simple Scatterplot Matrix")

pairs(~as.numeric(agi_amt)+Graduation+Pct_BHN+as.numeric(ACT_Composite), data=merged,
      main="Simple Scatterplot Matrix")

#Exploring the correlation between salary_avg and charitable_contribution_count

mod <- lm(salary_avg ~ charitable_contribution_count, data = merged)

coef(mod)
summary(mod)
plot(mod)
aug <- augment(mod)
glimpse(aug)

#Calculating R^2 (the coefficient of determination)

aug %>%
  summarize(var_y = var(charitable_contribution_count), var_e = var(.resid)) %>%
  mutate(R_squared = 1 -(var_e / var_y))


