setwd('/Users/ssharma/code/nss-ds/data-question-4-skirmish-guards-1')
library("tidyverse")
library("dplyr")
library("magrittr")
library("ggplot2")
# install.packages("readxl")
library("readxl")
library("GGally")
library("PerformanceAnalytics")

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


# Performance analytics
total_2 <- select(total, year, agi_amt_avg,salary_avg,mortgage_amt_avg,prop_tax_avg,taxable_income_avg,
                  taxable_income_amt, earned_income_credit_avg,excess_eaned_income_credit_avg,avg_Eng, avg_Sci, avg_Math, Pct_Expelled, Pct_BHN,
                  Pct_Suspended, ACT_Composite,earned_income_credit_avg, Pct_ED,Pct_SWD
)
chart.Correlation(total_2, histogram=TRUE, pch=19)

colnames(edu_2014)
# avg_Eng, avg_Sci, avg_Math, Pct_Expelled, Pct_BHN,Pct_Suspended, ACT_Composite,earned_income_credit_avg, Pct_ED,Pct_SWD
edu_1 <- select( edu_2014, county, AlgII ,BioI,Chemistry,ELA,EngI,EngII,EngIII,Math,Science,ACT_Composite,Graduation,Pct_Native_American,Pct_SWD,
                 Pct_Black,Pct_Hispanic,Pct_EL,Pct_ED,Pct_BHN,Pct_Chronically_Absent,
                 Pct_Suspended,Pct_Expelled,Per_Pupil_Expenditures,Enrollment
)
View(edu_1)
ggcorr(edu_1)
pairs(~edu_1$Enrollment+ edu_1$Per_Pupil_Expenditures, data=edu_1, main="Simple Scatterplot Matrix")

# read the edu data only
edu_raw <- read.csv('data/achievement_profile_data_with_CORE.csv')
View(edu_raw)

edu_2 <- select( edu_raw, system_name,BioI,Chemistry,ELA,EngI,EngII,EngIII,AlgI,AlgII,Math,Science,ACT_Composite,Graduation,Pct_Native_American,Pct_SWD,
                 Pct_Black,Pct_Hispanic,Pct_EL,Pct_ED,Pct_BHN,Pct_Chronically_Absent,
                 Pct_Suspended,Pct_Expelled,Per_Pupil_Expenditures,Enrollment
)
ggcorr(edu_2)

ggcorr(total_irs)

total_irs_1 <- select( total_irs, agi_amt_avg,salary_avg,mortgage_amt_avg,
                      prop_tax_avg,
                      taxable_income_avg,
                      earned_income_credit_avg,
                      excess_eaned_income_credit_avg,year)
ggcorr(total_irs_1)
# avg mortgage amt is going down over the years unlike other avg params
total_irs_2 <- select(total_irs,  'tot_tax_credit_count',
                      'tot_tax_credit_amt',
                      'earned_income_credit_count',
                      'earned_income_credit_amt',
                      'excess_eaned_income_credit_count',
                      'excess_eaned_income_credit_amt',
                      'tax_liability_count' ,
                      'tax_liability_amt' ,
                      'balance_due_count' ,
                      'balance_due_amt' ,
                      'refund_count',
                      'refund_amt',
                      'paid_prep_count',year)

ggcorr(total_irs_2)

# create avg values from IRS attributes
irs_total_avg <- total_irs %>%
    mutate(salary_avg = as.numeric(salary_wages_amt)/as.numeric(salary_wages_count),
           taxable_interest_avg = as.numeric(taxable_interest_amt)/as.numeric(taxable_interest_count),
           ordinary_dividends_avg = as.numeric(ordinary_dividends_amt)/as.numeric(ordinary_dividends_count),
           business_income_avg = as.numeric(business_income_amt)/as.numeric(business_income_count),
           net_capital_gain_avg = as.numeric(net_capital_gain_amt)/as.numeric(net_capital_gain_count),
           taxable_ira_dist_avg = as.numeric(taxable_IRA_dist_amt)/as.numeric(taxable_IRA_dist_count),
           pension_avg = as.numeric(pension_income_amt)/as.numeric(pension_income_count),
           unemployment_avg = as.numeric(unemployment_amt)/as.numeric(unemployment_count),
           social_security_avg = as.numeric(social_security_amt)/as.numeric(social_security_count),
           item_deduction_avg = as.numeric(item_deduc_amt)/as.numeric(item_deduc_count),
           charity_avg = as.numeric(charitable_contribution_amt)/as.numeric(charitable_contribution_count),
           state_local_income_tax_avg = as.numeric(state_local_income_tax_amt)/as.numeric(state_local_income_tax_count),
           state_local_sales_tax_avg = as.numeric(state_local_sales_tax_amt)/as.numeric(state_local_sales_tax_count),
           total_tax_credit_avg = as.numeric(tot_tax_credit_amt)/as.numeric(tot_tax_credit_count),
           tax_liability_avg = as.numeric(tax_liability_amt)/as.numeric(tax_liability_count),
           balance_due_avg = as.numeric(balance_due_amt)/as.numeric(balance_due_count),
           refund_avg = as.numeric(refund_amt)/as.numeric(refund_count)
    )
View(irs_total_avg)

irs_avg <- select(irs_total_avg, zip_code,year, agi_amt_avg,salary_avg, taxable_interest_avg, mortgage_amt_avg,
                  prop_tax_avg,taxable_income_avg,earned_income_credit_avg,excess_eaned_income_credit_avg,ordinary_dividends_avg,
                  business_income_avg, net_capital_gain_avg,taxable_ira_dist_avg, pension_avg, unemployment_avg, social_security_avg,
                  item_deduction_avg, charity_avg, state_local_income_tax_avg, state_local_sales_tax_avg, total_tax_credit_avg,
                  balance_due_avg, refund_avg )  #%>% View()
    # filter(zip_code == '37219')
write_csv( irs_avg,'irs_avg.csv')
ggcorr(irs_avg)

plot(irs_avg$year, irs_avg$mortgage_amt_avg)

# subset irs_avg top 5 zip_codes

irs_salary_avg <- select(irs_total_avg, zip_code, year, salary_avg, taxable_interest_avg, mortgage_amt_avg,
                  prop_tax_avg,taxable_income_avg,earned_income_credit_avg,
                  business_income_avg, net_capital_gain_avg, pension_avg, unemployment_avg, social_security_avg,
                  item_deduction_avg, state_local_income_tax_avg, total_tax_credit_avg,
                  balance_due_avg, refund_avg ) %>%
    filter(zip_code %in% c(38139, 37350, 37205,37215, 38120)) #%>% View()

irs_mortgage_avg <- select(irs_total_avg, zip_code, year, salary_avg, taxable_interest_avg, mortgage_amt_avg,
                         prop_tax_avg,taxable_income_avg,earned_income_credit_avg,
                         business_income_avg, net_capital_gain_avg, pension_avg, unemployment_avg, social_security_avg,
                         item_deduction_avg, state_local_income_tax_avg, total_tax_credit_avg,
                         balance_due_avg, refund_avg ) %>%
    filter(zip_code %in% c(37402,38028, 37350,37025, 37215)) #%>% View()

irs_business_income <- select(irs_total_avg, zip_code, year, salary_avg, taxable_interest_avg, mortgage_amt_avg,
                         prop_tax_avg,taxable_income_avg,earned_income_credit_avg,
                         business_income_avg, net_capital_gain_avg, pension_avg, unemployment_avg, social_security_avg,
                         item_deduction_avg, state_local_income_tax_avg, total_tax_credit_avg,
                         balance_due_avg, refund_avg ) %>%
    filter(zip_code %in% c(37203,37215, 38139,38120, 37205)) #%>% View()

irs_taxable_income <- select(irs_total_avg, zip_code, year, salary_avg, taxable_interest_avg, mortgage_amt_avg,
                         prop_tax_avg,taxable_income_avg,earned_income_credit_avg,
                         business_income_avg, net_capital_gain_avg, pension_avg, unemployment_avg, social_security_avg,
                         item_deduction_avg, state_local_income_tax_avg, total_tax_credit_avg,
                         balance_due_avg, refund_avg ) %>%
    filter(zip_code %in% c(37402, 37350, 37205,37215, 38120)) #%>% View()

irs_unemployment <- select(irs_total_avg, zip_code, year, salary_avg, taxable_interest_avg, mortgage_amt_avg,
                                                 prop_tax_avg,taxable_income_avg,earned_income_credit_avg,
                                                 business_income_avg, net_capital_gain_avg, pension_avg, unemployment_avg, social_security_avg,
                                                 item_deduction_avg, state_local_income_tax_avg, total_tax_credit_avg,
                                                 balance_due_avg, refund_avg ) %>%
    filter(zip_code %in% c(38103, 37201, 37403,37014, 37212)) #%>% View()
ggcorr(irs_unemployment)

irs_earned_income_credit <- select(irs_total_avg, zip_code, year, salary_avg, taxable_interest_avg, mortgage_amt_avg,
                           prop_tax_avg,taxable_income_avg,earned_income_credit_avg,
                           business_income_avg, net_capital_gain_avg, pension_avg, unemployment_avg, social_security_avg,
                           item_deduction_avg, state_local_income_tax_avg, total_tax_credit_avg,
                           balance_due_avg, refund_avg ) %>%
    filter(zip_code %in% c(38126, 38127, 38108, 38106, 38114)) #%>% View()

irs_charity <- select(irs_total_avg, zip_code, year, salary_avg, taxable_interest_avg, mortgage_amt_avg,
                                          prop_tax_avg,taxable_income_avg,earned_income_credit_avg,
                                          business_income_avg, net_capital_gain_avg, pension_avg, unemployment_avg, social_security_avg,
                                          item_deduction_avg, state_local_income_tax_avg, total_tax_credit_avg,
                                          balance_due_avg, refund_avg ) %>%
    filter(zip_code %in% c(37402, 37201, 38103,37350, 37203)) #%>% View()
ggcorr(irs_charity)
plot( as.factor(irs_charity$year),irs_charity$salary_avg)
plot( irs_charity$year,irs_charity$item_deduction_avg)
plot(irs_business_income$year,irs_business_income$taxable_income_avg)
ggcorr(irs_total_avg)

hist(irs_salary_avg$salary_avg)
hist(irs_total_avg$salary_avg)
hist(irs_earned_income_credit$salary_avg)

write_csv( irs_total_avg, 'irs_total_avg.csv')

theme_set(theme_bw())

# Data Prep
 # mean(irs_total_avg$salary_avg,na.rm=TRUE) %>% View()
total %>%  select(county,salary_avg) %>%  filter(year == '2014') %>% group_by(county) %>% View()

total$sal_avg_z <- round((total$salary_avg - mean(total$salary_avg,na.rm=TRUE))/sd(total$salary_avg,na.rm=TRUE), 2)  # compute normalized mpg
total$sal_type <- ifelse(total$sal_avg_z < 0, "below", "above")  # above / below avg flag
total <- total[order(total$sal_avg_z), ]  # sort
total$county <- factor(total$county, levels = total$county)  # convert to factor to retain sorted order in plot.

View(total)
# Diverging Barcharts

ggplot(total_2014, aes(x=county, y=sal_avg_z, label=sal_avg_z)) +
    geom_bar(stat='identity', aes(fill=sal_type), width=.5)  +
    scale_fill_manual(name="Average Salary",
                      labels = c("Above Average", "Below Average"),
                      values = c("above"="#00ba38", "below"="#f8766d")) +
    labs(subtitle="Normalised Average Salary for Counties",
         title= "Diverging Bars") +
    coord_flip()
