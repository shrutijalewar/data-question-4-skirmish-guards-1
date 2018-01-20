library("tidyverse")
library("dplyr")
library("magrittr")
library("ggplot2")
#install.packages("readxl")
library("readxl")

# reading the 2015 IRS data
tn_2015 <- read_xls("data/IRS_2015_original.xls", range = cell_rows(3:4724))
tn_2015 <- tn_2015[4:4724,]

View(tn_2015)

# getting the data in the predecided order
tn_2015_df <- select(tn_2015,`[Money amounts are in thousands of dollars]`,
                     X__1,
                     X__2,
                     X__7,
                     X__8,
                     X__16,
                     X__19,
                     X__20,
                     X__21,
                     X__22,
                     X__23,
                     X__24,
                     X__29,
                     X__30,
                     X__37,
                     X__31,
                     X__32,
                     X__33,
                     X__34,
                     X__35,
                     X__36,
                     X__38,
                     X__39,
                     X__40,
                     X__41,
                     X__60,
                     X__61,
                     X__73,
                     X__74,
                     X__71,
                     X__72,
                     X__67,
                     X__68,
                     X__63,
                     X__64,
                     X__65,
                     X__66,
                     X__75,
                     X__76,
                     X__83,
                     X__84,
                     X__107,
                     X__108,
                     X__109,
                     X__110,
                     X__119,
                     X__120,
                     X__125,
                     X__126,
                     X__127,
                     X__128,
                     X__6)
View(tn_2015_df)         

# standardizing col names in a vector
cols <-c ('zip_code',
          'agi_range',
          'return_count',
          'exemption_count',
          'dependent_count' ,
          'agi_amt' ,
          'salary_wages_count',
          'salary_wages_amt',
          'taxable_interest_count',
          'taxable_interest_amt',
          'ordinary_dividends_count',
          'ordinary_dividends_amt',
          'business_income_count',
          'business_income_amt' ,
          'farm_income_count' ,
          'net_capital_gain_count',
          'net_capital_gain_amt',
          'taxable_IRA_dist_count',
          'taxable_IRA_dist_amt',
          'pension_income_count',
          'pension_income_amt',
          'unemployment_count',
          'unemployment_amt',
          'social_security_count' ,
          'social_security_amt' ,
          'item_deduc_count',
          'item_deduc_amt',
          'charitable_contribution_count' ,
          'charitable_contribution_amt' ,
          'mortgage_int_count',
          'mortgage_int_amt',
          'property_tax_count',
          'property_tax_amt',
          'state_local_income_tax_count',
          'state_local_income_tax_amt',
          'state_local_sales_tax_count' ,
          'state_local_sales_tax_amt' ,
          'taxable_income_count',
          'taxable_income_amt',
          'tot_tax_credit_count',
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
          'paid_prep_count'
)
# reanming column names in df
colnames(tn_2015_df) <- cols
View(tn_2015_df)

# Adding year as a column

tn_2015_df$year <- rep(2015,nrow(tn_2015_df))
View(tn_2015_df)

#omitting na in a row
tn_2015_df <- tn_2015_df[rowSums(is.na(tn_2015_df))!=ncol(tn_2015_df), ]

# Adding total to the agi_range for further filtering
tn_2015_df$agi_range[is.na(tn_2015_df$agi_range)] <- "Total"

tn_2015_df <- tn_2015_df %>%
  mutate(agi_amt_avg = as.numeric(agi_amt)/as.numeric(return_count),
         salary_avg = as.numeric(salary_wages_amt)/as.numeric(salary_wages_count),
         mortgage_amt_avg = as.numeric(mortgage_int_amt)/as.numeric(mortgage_int_count),
         prop_tax_avg = as.numeric(property_tax_amt)/as.numeric(property_tax_count),
         taxable_income_avg = as.numeric(taxable_income_amt)/as.numeric(taxable_income_count),
         earned_income_credit_avg = as.numeric(earned_income_credit_amt)/as.numeric(earned_income_credit_count),
         excess_eaned_income_credit_avg = as.numeric(excess_eaned_income_credit_amt)/as.numeric(excess_eaned_income_credit_count)
  )

View(tn_2015_df)

write.csv(tn_2015_df, "irs_2015.csv")
