#begin

setwd("/Users/vinaykhedkar/GIT/data-question-4-skirmish-guards-1")
library("tidyverse")
library("dplyr")
#library("magrittr")
library("ggplot2")
# install.packages(“readxl”)
library("readxl")
library(psych) #install.packages("psych")

# read 2011 TN IRS data file #

         tn_2011 <- read_xls("data/2011tn.xls", range = cell_rows(3:4724))
         tn_2011 <- tn_2011[4:4724,]
         #View(tn_2011)
         
         tn_2011_df <- select(tn_2011,`[Money amounts are in thousands of dollars]`,#'zip_code'
         X__1, #'agi_range',
         X__3, #'return_count',
         X__5, #'exemption_count',
         X__6, #'dependent_count' ,
         X__7, #'agi_amt' ,
         X__8, #'salary_wages_count',
         X__9, #'salary_wages_amt',
         X__10, #'taxable_interest_count',
         X__11, #'taxable_interest_amt',
         X__12, #'ordinary_dividends_count',
         X__13, #'ordinary_dividends_amt',
         X__16, #'business_income_count',
         X__17, #'business_income_amt' ,
         X__18, #'farm_income_count' ,
         X__19, #'net_capital_gain_count',
         X__20, #'net_capital_gain_amt',
         X__21, #'taxable_IRA_dist_count',
         X__22, #'taxable_IRA_dist_amt',
         X__23, #'pension_income_count',
         X__24, #'pension_income_amt',
         X__25, #'unemployment_count',
         X__26, #'unemployment_amt',
         X__27, #'social_security_count' ,
         X__28, #'social_security_amt' ,
         X__31, #'item_deduc_count',
         X__32, #'item_deduc_amt',
         X__43, #'charitable_contribution_count' ,
         X__44, #'charitable_contribution_amt' ,
         X__41, #'mortgage_int_count',
         X__42, #'mortgage_int_amt',
         X__37, #'property_tax_count',
         X__38, #'property_tax_amt',
         X__33, #'state_local_income_tax_count',
         X__34, #'state_local_income_tax_amt',
         X__35, #'state_local_sales_tax_count' ,
         X__36, #'state_local_sales_tax_amt' ,
         X__45, #'taxable_income_count',
         X__46, #'taxable_income_amt',
         X__47, #'tot_tax_credit_count',
         X__48, #'tot_tax_credit_amt',
         X__57, #'earned_income_credit_count',
         X__58, #'earned_income_credit_amt',
         X__59, #'excess_eaned_income_credit_count',
         X__60, #'excess_eaned_income_credit_amt',
         X__65, #'tax_liability_count' ,
         X__66, #'tax_liability_amt' ,
         X__67, #'balance_due_count' ,
         X__68, #'balance_due_amt' ,
         X__69, #'refund_count',
         X__70, #'refund_amt',
         X__4)  #'paid_prep_count'
         
         
         #View(tn_2011_df)
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
         colnames(tn_2011_df) <- cols
         #View(tn_2011_df)
         
# cleanup NA's by filtering them out
         tn_2011_df <- tn_2011_df %>%
           drop_na()
         #View(tn_2011_df)

         #2014 IRS TN file
         tn_2014 <- read_xls("data/2014tn.xls", range = cell_rows(3:4740))   #, range = cell_rows(3:4724)
         tn_2014 <- tn_2014[4:4737,]
         #View(tn_2014)
         
         
         tn_2014_df <- select(tn_2014,'[Money amounts are in thousands of dollars]',#'zip_code'
                              X__1, #'agi_range',
                              X__2, #'return_count',
                              X__7, #'exemption_count',
                              X__8, #'dependent_count' ,
                              X__12, #'agi_amt' ,
                              X__15, #'salary_wages_count',
                              X__16, #'salary_wages_amt',
                              X__17, #'taxable_interest_count',
                              X__18, #'taxable_interest_amt',
                              X__19, #'ordinary_dividends_count',
                              X__20, #'ordinary_dividends_amt',
                              X__25, #'business_income_count',
                              X__26, #'business_income_amt' ,
                              X__33, #'farm_income_count' ,
                              X__27, #'net_capital_gain_count',
                              X__28, #'net_capital_gain_amt',
                              X__29, #'taxable_IRA_dist_count',
                              X__30, #'taxable_IRA_dist_amt',
                              X__31, #'pension_income_count',
                              X__32, #'pension_income_amt',
                              X__34, #'unemployment_count',
                              X__35, #'unemployment_amt',
                              X__36, #'social_security_count' ,
                              X__37, #'social_security_amt' ,
                              X__56, #'item_deduc_count',
                              X__57, #'item_deduc_amt',
                              X__69, #'charitable_contribution_count' ,
                              X__70, #'charitable_contribution_amt' ,
                              X__67, #'mortgage_int_count',
                              X__68, #'mortgage_int_amt',
                              X__63, #'property_tax_count',
                              X__64, #'property_tax_amt',
                              X__59, #'state_local_income_tax_count',
                              X__60, #'state_local_income_tax_amt',
                              X__61, #'state_local_sales_tax_count' ,
                              X__62, #'state_local_sales_tax_amt' ,
                              X__71, #'taxable_income_count',
                              X__72, #'taxable_income_amt',
                              X__79, #'tot_tax_credit_count',
                              X__80, #'tot_tax_credit_amt',
                              X__103, #'earned_income_credit_count',
                              X__104, #'earned_income_credit_amt',
                              X__105, #'excess_eaned_income_credit_count',
                              X__106, #'excess_eaned_income_credit_amt',
                              X__115, #'tax_liability_count' ,
                              X__116, #'tax_liability_amt' ,
                              X__121, #'balance_due_count' ,
                              X__122, #'balance_due_amt' ,
                              X__123, #'refund_count',
                              X__124, #'refund_amt',
                              X__6)  #'paid_prep_count'
         
         
         #View(tn_2014_df)
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
         colnames(tn_2014_df) <- cols
         #View(tn_2014_df)
         
         #omitting na in a row
         tn_2014_df <- tn_2014_df[rowSums(is.na(tn_2014_df))!=ncol(tn_2014_df), ]
         # Adding year as a column
         tn_2014_df$year <- rep(2014,nrow(tn_2014_df))
         #View(tn_2014_df)
         # Adding total to the agi_range for further filtering
         tn_2014_df$agi_range[is.na(tn_2014_df$agi_range)] <- "Total"
         # Feature engineering: creating avg values for the following metrics
         irs_2014 <- tn_2014_df %>%
           mutate(agi_amt_avg = as.numeric(agi_amt)/as.numeric(return_count),
                  salary_avg = as.numeric(salary_wages_amt)/as.numeric(salary_wages_count),
                  mortgage_amt_avg = as.numeric(mortgage_int_amt)/as.numeric(mortgage_int_count),
                  prop_tax_avg = as.numeric(property_tax_amt)/as.numeric(property_tax_count),
                  taxable_income_avg = as.numeric(taxable_income_amt)/as.numeric(taxable_income_count),
                  earned_income_credit_avg = as.numeric(earned_income_credit_amt)/as.numeric(earned_income_credit_count),
                  excess_eaned_income_credit_avg = as.numeric(excess_eaned_income_credit_amt)/as.numeric(excess_eaned_income_credit_count)
           ) 
           #View(irs_2014)
         write.csv(irs_2014,'data/irs_2014.csv')
         
         #Importing the zip code and core dataframes.  
         
         core <- read_csv('data/achievement_profile_data_with_CORE.csv')
         zips <- read_excel("data/zip_code_database.xlsx")
         #View(core)
         #View(zips)
         
         #Fist we need to isolate only the TN zipcodes within zips. 
         
         TN_zips <- zips %>%
           filter(state == 'TN')
         
         #Removing unnecessary columns from zips. 
         
         TN_zips <- TN_zips[-c(2, 3, 5, 6, 9:12)]
         #View(TN_zips)
         
         #Removing the system column, and adding aggregate columns for each discipline.
         
         core <- core[-c(1)]
         colnames(core)[1] <- 'county'
         core <- core %>%
           rowwise() %>%
           mutate(avg_Math = mean(c(Math, AlgI, AlgII), na.rm = TRUE),
                  avg_Eng = mean(c(ELA, EngI, EngII, EngIII), na.rm = TRUE),
                  avg_Sci = mean(c(Science, BioI, Chemistry), na.rm = TRUE))
         
         #Merging via county and rearranging the colnames to display the zip's and county's in th first two columns.  
         
         merged_county <- core %>% 
           inner_join(TN_zips, by = 'county') 
         
         merged_county <- select(merged_county, zip, county, avg_Math, avg_Eng, avg_Sci, everything())
         colnames(merged_county)[1] <- 'zip_code'
         #View(merged_county)
         #(irs_2014)
         irs_2014$zip_code = as.numeric(irs_2014$zip_code)
         
         str(merged_county)
         str(irs_2014)
         
         # join irs dataset with zipcode and core achievement dataset
         merged_irs <- inner_join(irs_2014, merged_county, by=c('zip_code' = 'zip_code'))
         #merged_irs <- irs_2014 %>% 
         #  inner_join(merged_county, by = 'zip_code') 
         View(merged_irs)
         
         summary(merged_irs)
         
         describe(merged_irs)
         
         #mfv(merged_irs)
         
         cor(as.numeric(merged_irs$agi_amt), merged_irs$zip_code)
         
         
           # Basic Scatterplot Matrix
           pairs(~as.numeric(agi_amt)+Graduation+Pct_BHN+Dropout, data=merged_irs, 
                 main="Simple Scatterplot Matrix")
           
           #res <- cor.test(merged_irs$agi_amt, merged_irs$Pct_BHN, 
           #                method = "pearson")
           #res
           
           library(PerformanceAnalytics)
           
           
           
           total_2 <- select(merged_irs, year, agi_amt_avg,salary_avg,mortgage_amt_avg,
                             prop_tax_avg,taxable_income_avg,
                             earned_income_credit_avg,
                             excess_eaned_income_credit_avg, avg_Eng, avg_Sci, 
                             avg_Math, Pct_Expelled, Pct_BHN,
                             Pct_Suspended, ACT_Composite,earned_income_credit_avg, 
                             Pct_ED,Pct_SWD, Enrollment, Graduation, Dropout, county
           )
           
          View(total_2)
           
           #total_2[taxable_income_amt] = as.numeric(taxable_income_amt)
           str(total_2)
           
           
           #  multivariable regression model  for agi #
           model_multi <- lm(formula = agi_amt_avg ~ Enrollment + Graduation + Dropout + Pct_BHN, data = total_2)
           plot(model_multi)
           summary(model_multi)
           
           model_multi <- lm(formula = agi_amt_avg ~  Graduation + Dropout + Pct_BHN, data = total_2)
           plot(model_multi)
           summary(model_multi)
           
           # use model to predict the results
           test_model <- data.frame(Graduation = 80,  Dropout = 10, Pct_BHN = 30)
           
           #test_counts <- model_multi$agi_amt_avg
           
           predict(model_multi, test_model)
           
           lm(formula = ACT_Composite ~  avg_Eng + avg_Math + avg_Sci + Dropout + Pct_Suspended,  data = total_2)
           model_multi <- lm(formula = ACT_Composite ~  avg_Eng + avg_Math + avg_Sci, data = total_2)
           plot(model_multi)
           summary(model_multi)
           
           Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
           (Intercept) 12.114030   0.083757 144.633  < 2e-16 ***
             avg_Eng      0.095159   0.003889  24.470  < 2e-16 ***
             avg_Math     0.010742   0.001466   7.325 2.88e-13 ***
             avg_Sci      0.016612   0.002811   5.910 3.72e-09 ***
             
             # Equation of the model, ignoring the residual error component
             #ACT_Composite = 12.114030 + 0.095159 * avg_Eng + 0.010742 * avg_Math + 0.016612 * avg_Sci
             
           
           
           # use model to predict the results
           test_model <- data.frame(avg_Eng = 80, avg_Math = 80,  avg_Sci = 80)
           test_model2 <- data.frame(avg_Eng = 90, avg_Math = 90,  avg_Sci = 90)
           test_model3 <- data.frame(avg_Eng = 60, avg_Math = 50,  avg_Sci = 60)
           #test_counts <- model_multi$agi_amt_avg
           
           predict(model_multi, test_model)
           predict(model_multi, test_model2)
           predict(model_multi, test_model3)
           
           # use model to predict the results - Test the effect of Math Vs Science
           test_model <- data.frame(avg_Eng = 80, avg_Math = 80,  avg_Sci = 80)
           test_model2 <- data.frame(avg_Eng = 80, avg_Math = 90,  avg_Sci = 80)
           test_model3 <- data.frame(avg_Eng = 90, avg_Math = 80,  avg_Sci = 80)
           test_model4 <- data.frame(avg_Eng = 80, avg_Math = 80,  avg_Sci = 90)
           #test_counts <- model_multi$agi_amt_avg
           
           predict(model_multi, test_model)
           predict(model_multi, test_model2)
           predict(model_multi, test_model3)
           predict(model_multi, test_model4)
           
           #plot(p - test_counts)
           
           # Is English better predictor of ACT score??
           model_multi <- lm(formula = ACT_Composite ~  avg_Eng + avg_Sci , data = total_2)
           plot(model_multi)
           summary(model_multi)
           
           # use model to predict the results
           test_model <- data.frame(avg_Eng = 80, avg_Sci = 80)
           test_model2 <- data.frame(avg_Eng = 90, avg_Sci = 80)
           test_model3 <- data.frame(avg_Eng = 80, avg_Sci = 90)
           #test_counts <- model_multi$agi_amt_avg
           
           predict(model_multi, test_model)
           predict(model_multi, test_model2)
           predict(model_multi, test_model3)
           
           
           
           chart.Correlation(total_2, histogram=TRUE, pch=19)
           
         ggcorr(merged_irs)  
           
         
         # Doing initial data exploration
         plot(merged_irs$agi_amt_avg, merged_irs$Dropout)
         
         plot(total$salary_avg, total$Graduation)
         plot(as.factor(merged_irs$zip_code), merged_irs$Pct_Chronically_Absent)
         plot(merged_irs$zip_code, merged_irs$salary_avg)
         hist(merged_irs$Pct_BHN)
         hist(merged_irs$Dropout)
         hist(merged_irs$Graduation)
         hist(merged_irs$return_count)
         hist(merged_irs$agi_amt_avg)
         plot(merged_irs$property_tax_amt, merged_irs$Dropout)
         plot(as.factor(merged_irs$CORE_region), merged_irs$Pct_Chronically_Absent)
         plot(as.factor(merged_irs$CORE_region), merged_irs$Pct_BHN)
         plot(as.factor(merged_irs$CORE_region), merged_irs$agi_amt_avg)
           # Correlations/covariances among numeric variables in 
         # data frame mtcars. Use listwise deletion of missing data. 
         #cor(merged_irs$agi_amt, use="complete.obs", method="kendall") 
         #cov(merged_irs$agi_amt, use="complete.obs")
         
         #Create the model and validate the prediction quality. Let’s start by using a linear regression algorithm (lm) to create a model, and use the test data set to validate how good is the prediction from the model.
         model=lm(agi_amt_avg ~ Graduation, merged_irs,)
         summary(model)
         
         train_data = merged_irs[merged_irs$Graduation >80,]
         test_data = merged_irs[merged_irs$Graduation == 80,]
         #...and also save this specific column as a vector 
         #we will use it for a bulk check of the quality of the prediction
         test_counts <- test_data$agi_amt_avg
         
         p = predict(model, test_data)
         plot(p - test_counts)
         
         #https://rcompanion.org/rcompanion/e_05.html
         
         
         #we want to find the top and bottom 5 counties when it comes to graduation rates.  
         
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
         