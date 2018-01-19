setwd('/Users/ssharma/code/nss-ds/data-question-4-skirmish-guards-1')
library("tidyverse")
library("dplyr")
library("magrittr")
library("ggplot2")
# install.packages("readxl")
library("readxl")

# reading the 2011 IRS data
tn_2011 <- read_xls("data/2011tn.xls", range = cell_rows(3:4724))
tn_2011 <- tn_2011[4:4724,]
View(tn_2011)
# getting the data in the predecided order
tn_2011_df <- select(tn_2011,`[Money amounts are in thousands of dollars]`,
                     X__1,
                     X__2,
                     X__5,
                     X__6,
                     X__7,
                     X__8,
                     X__9,
                     X__10,
                     X__11,
                     X__12,
                     X__13,
                     X__16,
                     X__17,
                     X__18,
                     X__19,
                     X__20,
                     X__21,
                     X__22,
                     X__23,
                     X__24,
                     X__25,
                     X__26,
                     X__27,
                     X__28,
                     X__31,
                     X__32,
                     X__43,
                     X__44,
                     X__41,
                     X__42,
                     X__37,
                     X__38,
                     X__33,
                     X__34,
                     X__35,
                     X__36,
                     X__45,
                     X__46,
                     X__47,
                     X__48,
                     X__57,
                     X__58,
                     X__59,
                     X__60,
                     X__65,
                     X__66,
                     X__67,
                     X__68,
                     X__69,
                     X__70,
                     X__4
                     )
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
colnames(tn_2011_df) <- cols
#omitting na in a row
tn_2011_df <- tn_2011_df[rowSums(is.na(tn_2011_df))!=ncol(tn_2011_df), ]
# Adding year as a column
tn_2011_df$year <- rep(2011,nrow(tn_2011_df))
View(tn_2011_df)
# Adding total to the agi_range for further filtering
tn_2011_df$agi_range[is.na(tn_2011_df$agi_range)] <- "Total"
# Feature engineering: creating avg values for the following metrics
irs_2011 <- tn_2011_df %>%
    mutate(agi_amt_avg = as.numeric(agi_amt)/as.numeric(return_count),
           salary_avg = as.numeric(salary_wages_amt)/as.numeric(salary_wages_count),
           mortgage_amt_avg = as.numeric(mortgage_int_amt)/as.numeric(mortgage_int_count),
           prop_tax_avg = as.numeric(property_tax_amt)/as.numeric(property_tax_count),
           taxable_income_avg = as.numeric(taxable_income_amt)/as.numeric(taxable_income_count),
           earned_income_credit_avg = as.numeric(earned_income_credit_amt)/as.numeric(earned_income_credit_count),
           excess_eaned_income_credit_avg = as.numeric(excess_eaned_income_credit_amt)/as.numeric(excess_eaned_income_credit_count)
           )
    View(irs_2011)
    # write output as csv
    write.csv(irs_2011,'data/irs_2011.xls')
# reading in 2012
tn_2012 <- read_xls("data/2012tn.xls", range = cell_rows(3:4725))
tn_2012 <- tn_2012[4:4725,]
View(tn_2012)
# getting the data in the predecided order
tn_2012_df <- select(tn_2012,`[Money amounts are in thousands of dollars]`,
                     X__1,
                     X__2,
                     X__7,
                     X__8,
                     X__9,
                     X__10,
                     X__11,
                     X__12,
                     X__13,
                     X__14,
                     X__15,
                     X__18,
                     X__19,
                     X__20,
                     X__21,
                     X__22,
                     X__23,
                     X__24,
                     X__25,
                     X__26,
                     X__27,
                     X__28,
                     X__29,
                     X__30,
                     X__33,
                     X__34,
                     X__46,
                     X__47,
                     X__44,
                     X__45,
                     X__40,
                     X__41,
                     X__36,
                     X__37,
                     X__38,
                     X__39,
                     X__48,
                     X__49,
                     X__52,
                     X__53,
                     X__60,
                     X__61,
                     X__62,
                     X__63,
                     X__68,
                     X__69,
                     X__70,
                     X__71,
                     X__72,
                     X__73,
                     X__6)
# reanming column names in df
colnames(tn_2012_df) <- cols
#omitting na in a row
tn_2012_df <- tn_2012_df[rowSums(is.na(tn_2012_df))!=ncol(tn_2012_df), ]
# Adding year as a column
tn_2012_df$year <- rep(2012,nrow(tn_2012_df))
View(tn_2012_df)
# Adding total to the agi_range for further filtering
tn_2012_df$agi_range[is.na(tn_2012_df$agi_range)] <- "Total"
# Feature engineering: creating avg values for the following metrics
irs_2012 <- tn_2012_df %>%
    mutate(agi_amt_avg = as.numeric(agi_amt)/as.numeric(return_count),
           salary_avg = as.numeric(salary_wages_amt)/as.numeric(salary_wages_count),
           mortgage_amt_avg = as.numeric(mortgage_int_amt)/as.numeric(mortgage_int_count),
           prop_tax_avg = as.numeric(property_tax_amt)/as.numeric(property_tax_count),
           taxable_income_avg = as.numeric(taxable_income_amt)/as.numeric(taxable_income_count),
           earned_income_credit_avg = as.numeric(earned_income_credit_amt)/as.numeric(earned_income_credit_count),
           excess_eaned_income_credit_avg = as.numeric(excess_eaned_income_credit_amt)/as.numeric(excess_eaned_income_credit_count)
    )
    View(irs_2012)
    # write as csv for sharing
    write.csv(irs_2012,'data/irs_2012.xls')
