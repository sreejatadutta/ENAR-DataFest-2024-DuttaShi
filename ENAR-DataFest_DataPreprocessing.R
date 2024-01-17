##############################################################################################
## Data management by Sreejata Dutta on 12/18/2023
## Edited by Fred Shi on 12/26/2023 and 1/6/2024
## Update: remove diet data; add income and alcohol
##############################################################################################
## Set working directory
rm(list=ls())
# setwd("C:\\Users\\sreej\\OneDrive\\Documents\\ENAR\\ENAR-DataFest")
# setwd("C://Users/shixi/Desktop/ENAR/Data")

## Install package from github
# library(devtools)
# devtools::install_github("jhs-hwg/cardioStatsUSA")

## Required libraries
library(cardioStatsUSA)
library(dplyr)
library(table1)
library(haven)
library(tidyverse)
library(nhanesA)

## Pull the nhanes data from cardioStatsUSA package
dat <- cardioStatsUSA::nhanes_data
colnames(dat)

##############################################################################################
## Data across years::2011-2012 or xxxx_G
##############################################################################################
## Downloaded data
#nhanesTables('DEMO', 2011)
#nhanesTableVars('DEMO_G')
demo1 <- nhanes('DEMO_G')
alcohol1 <- nhanes('ALQ_G')
bmi1 <- nhanes('BMX_G')
diet11 <- nhanes('DR1TOT_G')
diet12 <- nhanes('DR2TOT_G')

## Merge the data
## list all the data frames
dat$SEQN <- dat$svy_id
dat_list1 <- list(dat[dat$svy_year=="2011-2012"], 
                  demo1, alcohol1, bmi1, diet11, diet12)
dat_2011_2012 <- dat_list1 %>% reduce(full_join, by='SEQN')
dat_2011_2012$avg_sodium = (dat_2011_2012$"DR1TSODI"+dat_2011_2012$"DR2TSODI")/2
dat_2011_2012$sodium = ifelse(is.na(dat_2011_2012$avg_sodium)==FALSE,
                              dat_2011_2012$avg_sodium, dat_2011_2012$DR1TSODI)
dat_2011_2012$DMDMARTL1 <- as.numeric(dat_2011_2012$DMDMARTL)
table(dat_2011_2012$DMDMARTL);table(dat_2011_2012$DMDMARTL1)
dat_2011_2012$DMDEDUC21 <- as.numeric(dat_2011_2012$DMDEDUC2)
table(dat_2011_2012$DMDEDUC2);table(dat_2011_2012$DMDEDUC21)
dat_2011_2012$DMDEDUC31 <- as.numeric(dat_2011_2012$DMDEDUC3)
table(dat_2011_2012$DMDEDUC3);table(dat_2011_2012$DMDEDUC31)
dat_2011_2012$ALQ1511 <- as.numeric(dat_2011_2012$ALQ151)
table(dat_2011_2012$ALQ151);table(dat_2011_2012$ALQ1511)

## Keeping important variables only
dat_2011_2012_clean1 <- dat_2011_2012 %>%
  mutate(bmi = BMXBMI,
         marital = case_when(DMDMARTL1 %in% c(1,6) ~ 1, #married/partnered
                             DMDMARTL1 %in% c(2,3,4) ~ 2, #widowed/divorced/separated
                             DMDMARTL1 == 5 ~ 3, #never married
                             TRUE ~ NA),
         edu = case_when(DMDEDUC21 %in% c(1,2,3) | (DMDEDUC31 <=14 & DMDEDUC31 >=0) 
                         | DMDEDUC31 %in% c(55,66) ~ 1, #HS or less
                         DMDEDUC21 == 4 | DMDEDUC31 ==15 ~ 2, #some college
                         DMDEDUC21 == 5 ~ 3, #college grad and above
                         TRUE ~ NA),
         incomeratio = INDFMPIR, #ratio of family income to poverty
         alcohol = case_when(ALQ1511 == 1 ~ 1, #Ever have 4/5 or more drinks every day
                             ALQ1511 == 2 ~ 0,
                             TRUE ~ NA),
         marital_factor = factor(marital, labels=c("Married/partnered", "Widowed/divorced/separated", "Never married")),
         edu_factor = factor(edu, labels=c("High school or less", "Some college", "College or above")),
         alcohol_factor = factor(alcohol, labels=c("No", "Yes"))
         ) %>% 
  select("svy_id", ## survey id
         "svy_weight_mec", #mobile exam. center weights
         "svy_psu",#primary sampling unit
         "svy_strata", #strata
         "svy_year", # year
         "svy_subpop_htn", ## subpopulation for hypertension,
         "demo_age_years", "demo_age_cat", #age
         "demo_race", "demo_race_black", #race 
         "demo_gender", #sex
         "bp_sys_mean", "bp_dia_mean", #bp variables
         "htn_aware", #htn aware
         "bp_med_n_class", "bp_med_use", #bp meds cat
         "cc_smoke","bmi","cc_diabetes","cc_ckd",
         "cc_cvd_any", #cc_cdv_any is a self reported variable for elf-reported history of coronary heart disease, myocardial infarction, stroke or heart failure
         "edu", #updated education level
         "marital", #marital status
         "incomeratio", #ratio of family income to poverty
         "alcohol", #alcohol
         "edu_factor",
         "marital_factor",
         "alcohol_factor",
         "sodium"
  )

label(dat_2011_2012_clean1$edu_factor) <- "Education level"
label(dat_2011_2012_clean1$incomeratio) <- "Ratio of family income to poverty"
label(dat_2011_2012_clean1$marital_factor) <- "Marital status"
label(dat_2011_2012_clean1$alcohol_factor) <- "Ever have 4/5 or more drinks every day"

delete_missing_2011_2012 <- na.omit(dat_2011_2012_clean1) 
table1(~demo_age_years+demo_age_cat+demo_race+demo_gender+bp_sys_mean+bp_dia_mean+
        htn_aware+bp_med_n_class+bp_med_use+cc_smoke+bmi+cc_diabetes+cc_ckd+
        cc_cvd_any+incomeratio+edu_factor+marital_factor+alcohol_factor+sodium, 
      data=delete_missing_2011_2012)

##############################################################################################
## Data across years::2013-2014 or xxxx_H
##############################################################################################
## Downloaded data
demo1 <- nhanes('DEMO_H')
alcohol1 <- nhanes('ALQ_H')
bmi1 <- nhanes('BMX_H')
diet11 <- nhanes('DR1TOT_H')
diet12 <- nhanes('DR2TOT_H')

## Merge the data
## list all the data frames
dat$SEQN <- dat$svy_id
dat_list1 <- list(dat[dat$svy_year=="2013-2014"], 
                  demo1, alcohol1, bmi1, diet11, diet12)
dat_2013_2014 <- dat_list1 %>% reduce(full_join, by='SEQN')
dat_2013_2014$avg_sodium = (dat_2013_2014$"DR1TSODI"+dat_2013_2014$"DR2TSODI")/2
dat_2013_2014$sodium = ifelse(is.na(dat_2013_2014$avg_sodium)==FALSE,
                              dat_2013_2014$avg_sodium, dat_2013_2014$DR1TSODI)
dat_2013_2014$DMDMARTL1 <- as.numeric(dat_2013_2014$DMDMARTL)
table(dat_2013_2014$DMDMARTL);table(dat_2013_2014$DMDMARTL1)
dat_2013_2014$DMDEDUC21 <- as.numeric(dat_2013_2014$DMDEDUC2)
table(dat_2013_2014$DMDEDUC2);table(dat_2013_2014$DMDEDUC21)
dat_2013_2014$DMDEDUC31 <- as.numeric(dat_2013_2014$DMDEDUC3)
table(dat_2013_2014$DMDEDUC3);table(dat_2013_2014$DMDEDUC31)
dat_2013_2014$ALQ1511 <- as.numeric(dat_2013_2014$ALQ151)
table(dat_2013_2014$ALQ151);table(dat_2013_2014$ALQ1511)

## Keeping important variables only
dat_2013_2014_clean1 <- dat_2013_2014 %>%
  mutate(bmi = BMXBMI,
         marital = case_when(DMDMARTL1 %in% c(1,6) ~ 1, #married/partnered
                             DMDMARTL1 %in% c(2,3,4) ~ 2, #widowed/divorced/separated
                             DMDMARTL1 == 5 ~ 3, #never married
                             TRUE ~ NA),
         edu = case_when(DMDEDUC21 %in% c(1,2,3) | (DMDEDUC31 <=14 & DMDEDUC31 >=0) 
                         | DMDEDUC31 %in% c(55,66) ~ 1, #HS or less
                         DMDEDUC21 == 4 | DMDEDUC3 ==15 ~ 2, #some college
                         DMDEDUC21 == 5 ~ 3, #college grad and above
                         TRUE ~ NA),
         incomeratio = INDFMPIR, #ratio of family income to poverty
         alcohol = case_when(ALQ1511 == 1 ~ 1, #Ever have 4/5 or more drinks every day
                             ALQ1511 == 2 ~ 0,
                             TRUE ~ NA),
         marital_factor = factor(marital, labels=c("Married/partnered", "Widowed/divorced/separated", "Never married")),
         edu_factor = factor(edu, labels=c("High school or less", "Some college", "College or above")),
         alcohol_factor = factor(alcohol, labels=c("No", "Yes"))
         ) %>% 
  select("svy_id", ## survey id
         "svy_weight_mec", #mobile exam. center weights
         "svy_psu",#primary sampling unit
         "svy_strata", #strata
         "svy_year", # year
         "svy_subpop_htn", ## subpopulation for hypertension,
         "demo_age_years", "demo_age_cat", #age
         "demo_race", "demo_race_black", #race 
         "demo_gender", #sex
         "bp_sys_mean", "bp_dia_mean", #bp variables
         "htn_aware", #htn aware
         "bp_med_n_class", "bp_med_use", #bp meds cat
         "cc_smoke","bmi","cc_diabetes","cc_ckd",
         "cc_cvd_any", #cc_cdv_any is a self reported variable for elf-reported history of coronary heart disease, myocardial infarction, stroke or heart failure
         "edu", #education level
         "marital", #marital status
         "incomeratio", #ratio of family income to poverty
         "alcohol", #alcohol
         "edu_factor",
         "marital_factor",
         "alcohol_factor",
         "sodium"
         )

label(dat_2013_2014_clean1$edu_factor) <- "Education level"
label(dat_2013_2014_clean1$incomeratio) <- "Ratio of family income to poverty"
label(dat_2013_2014_clean1$marital_factor) <- "Marital status"
label(dat_2013_2014_clean1$alcohol_factor) <- "Ever have 4/5 or more drinks every day"

delete_missing_2013_2014 <- na.omit(dat_2013_2014_clean1) 
table1(~demo_age_years+demo_age_cat+demo_race+demo_gender+bp_sys_mean+bp_dia_mean+
         htn_aware+bp_med_n_class+bp_med_use+cc_smoke+bmi+cc_diabetes+cc_ckd+
         cc_cvd_any+incomeratio+edu_factor+marital_factor+alcohol_factor+sodium, 
       data=delete_missing_2013_2014)

##############################################################################################
## Data across years::2015-2016 or xxxx_I
##############################################################################################
## Downloaded data
demo1 <- nhanes('DEMO_I')
alcohol1 <- nhanes('ALQ_I')
bmi1 <- nhanes('BMX_I')
diet11 <- nhanes('DR1TOT_I')
diet12 <- nhanes('DR2TOT_I')

## Merge the data
## list all the data frames
dat$SEQN <- dat$svy_id
dat_list1 <- list(dat[dat$svy_year=="2015-2016"], 
                  demo1, alcohol1, bmi1, diet11, diet12)
dat_2015_2016 <- dat_list1 %>% reduce(full_join, by='SEQN')
dat_2015_2016$avg_sodium = (dat_2015_2016$"DR1TSODI"+dat_2015_2016$"DR2TSODI")/2
dat_2015_2016$sodium = ifelse(is.na(dat_2015_2016$avg_sodium)==FALSE,
                              dat_2015_2016$avg_sodium, dat_2015_2016$DR1TSODI)
dat_2015_2016$DMDMARTL1 <- as.numeric(dat_2015_2016$DMDMARTL)
table(dat_2015_2016$DMDMARTL);table(dat_2015_2016$DMDMARTL1)
dat_2015_2016$DMDEDUC21 <- as.numeric(dat_2015_2016$DMDEDUC2)
table(dat_2015_2016$DMDEDUC2);table(dat_2015_2016$DMDEDUC21)
dat_2015_2016$DMDEDUC31 <- as.numeric(dat_2015_2016$DMDEDUC3)
table(dat_2015_2016$DMDEDUC3);table(dat_2015_2016$DMDEDUC31)
dat_2015_2016$ALQ1511 <- as.numeric(dat_2015_2016$ALQ151)
table(dat_2015_2016$ALQ151);table(dat_2015_2016$ALQ1511)


## Keeping important variables only
dat_2015_2016_clean1 <- dat_2015_2016 %>%
  mutate(bmi = BMXBMI,
         marital = case_when(DMDMARTL1 %in% c(1,6) ~ 1, #married/partnered
                             DMDMARTL1 %in% c(2,3,4) ~ 2, #widowed/divorced/separated
                             DMDMARTL1 == 5 ~ 3, #never married
                             TRUE ~ NA),
         edu = case_when(DMDEDUC21 %in% c(1,2,3) | (DMDEDUC31 <=14 & DMDEDUC31 >=0) 
                         | DMDEDUC31 %in% c(55,66) ~ 1, #HS or less
                         DMDEDUC21 == 4 | DMDEDUC3 ==15 ~ 2, #some college
                         DMDEDUC21 == 5 ~ 3, #college grad and above
                         TRUE ~ NA),
         incomeratio = INDFMPIR, #ratio of family income to poverty
         alcohol = case_when(ALQ1511 == 1 ~ 1, #Ever have 4/5 or more drinks every day
                             ALQ1511 == 2 ~ 0,
                             TRUE ~ NA),
         marital_factor = factor(marital, labels=c("Married/partnered", "Widowed/divorced/separated", "Never married")),
         edu_factor = factor(edu, labels=c("High school or less", "Some college", "College or above")),
         alcohol_factor = factor(alcohol, labels=c("No", "Yes"))
         ) %>% 
  select("svy_id", ## survey id
         "svy_weight_mec", #mobile exam. center weights
         "svy_psu",#primary sampling unit
         "svy_strata", #strata
         "svy_year", # year
         "svy_subpop_htn", ## subpopulation for hypertension,
         "demo_age_years", "demo_age_cat", #age
         "demo_race", "demo_race_black", #race 
         "demo_gender", #sex
         "bp_sys_mean", "bp_dia_mean", #bp variables
         "htn_aware", #htn aware
         "bp_med_n_class", "bp_med_use", #bp meds cat
         "cc_smoke","bmi","cc_diabetes","cc_ckd",
         "cc_cvd_any", #cc_cdv_any is a self reported variable for elf-reported history of coronary heart disease, myocardial infarction, stroke or heart failure
         "edu", #education level
         "marital", #marital status
         "incomeratio", #ratio of family income to poverty
         "alcohol", #alcohol
         "edu_factor",
         "marital_factor",
         "alcohol_factor",
         "sodium"
         )

label(dat_2015_2016_clean1$edu_factor) <- "Education level"
label(dat_2015_2016_clean1$incomeratio) <- "Ratio of family income to poverty"
label(dat_2015_2016_clean1$marital_factor) <- "Marital status"
label(dat_2015_2016_clean1$alcohol_factor) <- "Ever have 4/5 or more drinks every day"

delete_missing_2015_2016 <- na.omit(dat_2015_2016_clean1) 
table1(~demo_age_years+demo_age_cat+demo_race+demo_gender+bp_sys_mean+bp_dia_mean+
         htn_aware+bp_med_n_class+bp_med_use+cc_smoke+bmi+cc_diabetes+cc_ckd+
         cc_cvd_any+incomeratio+edu_factor+marital_factor+alcohol_factor+sodium, 
       data=delete_missing_2015_2016)

##############################################################################################
## Data across years::2017-2020 or P_xxxx
##############################################################################################
## Downloaded data
demo1 <- nhanes('P_DEMO')
alcohol1 <- nhanes('P_ALQ')
bmi1 <- nhanes('P_BMX')
diet11 <- nhanes('P_DR1TOT')
diet12 <- nhanes('P_DR2TOT')

## Merge the data
## list all the data frames
dat$SEQN <- dat$svy_id
dat_list1 <- list(dat[dat$svy_year=="2017-2020"], 
                  demo1, alcohol1, bmi1, diet11, diet12)
dat_2017_2020 <- dat_list1 %>% reduce(full_join, by='SEQN')
dat_2017_2020$avg_sodium = (dat_2017_2020$"DR1TSODI"+dat_2017_2020$"DR2TSODI")/2
dat_2017_2020$sodium = ifelse(is.na(dat_2017_2020$avg_sodium)==FALSE,
                              dat_2017_2020$avg_sodium, dat_2017_2020$DR1TSODI)
dat_2017_2020$DMDMARTZ1 <- as.numeric(dat_2017_2020$DMDMARTZ)
table(dat_2017_2020$DMDMARTZ);table(dat_2017_2020$DMDMARTZ1)
dat_2017_2020$DMDEDUC21 <- as.numeric(dat_2017_2020$DMDEDUC2)
table(dat_2017_2020$DMDEDUC21);table(dat_2017_2020$DMDEDUC2)
dat_2017_2020$DMDEDUC31 <- as.numeric(dat_2017_2020$DMDEDUC3)
table(dat_2017_2020$DMDEDUC3);table(dat_2017_2020$DMDEDUC31)
dat_2017_2020$ALQ1511 <- as.numeric(dat_2017_2020$ALQ151)
table(dat_2017_2020$ALQ151);table(dat_2017_2020$ALQ1511)

## Keeping important variables only
dat_2017_2020_clean1 <- dat_2017_2020 %>%
  mutate(bmi = BMXBMI,
         marital = case_when(DMDMARTZ1 == 1 ~ 1, #married/partnered
                             DMDMARTZ1 == 2 ~ 2, #widowed/divorced/separated
                             DMDMARTZ1 == 3 ~ 3, #never married
                             TRUE ~ NA),
         edu = case_when(DMDEDUC21 %in% c(1,2,3) ~ 1, #HS or less
                         DMDEDUC21 == 4 ~ 2, #some college
                         DMDEDUC21 == 5 ~ 3, #college grad and above
                         TRUE ~ NA),
         incomeratio = INDFMPIR, #ratio of family income to poverty
         alcohol = case_when(ALQ1511 == 1 ~ 1, #Ever have 4/5 or more drinks every day
                             ALQ1511 == 2 ~ 0,
                             TRUE ~ NA),
         marital_factor = factor(marital, labels=c("Married/partnered", "Widowed/divorced/separated", "Never married")),
         edu_factor = factor(edu, labels=c("High school or less", "Some college", "College or above")),
         alcohol_factor = factor(alcohol, labels=c("No", "Yes"))
         ) %>% 
  select("svy_id", ## survey id
         "svy_weight_mec", #mobile exam. center weights
         "svy_psu",#primary sampling unit
         "svy_strata", #strata
         "svy_year", # year
         "svy_subpop_htn", ## subpopulation for hypertension,
         "demo_age_years", "demo_age_cat", #age
         "demo_race", "demo_race_black", #race 
         "demo_gender", #sex
         "bp_sys_mean", "bp_dia_mean", #bp variables
         "htn_aware", #htn aware
         "bp_med_n_class", "bp_med_use", #bp meds cat
         "cc_smoke","bmi","cc_diabetes","cc_ckd",
         "cc_cvd_any", #cc_cdv_any is a self reported variable for elf-reported history of coronary heart disease, myocardial infarction, stroke or heart failure
         "edu", #education level
         "marital", #marital status
         "incomeratio", #ratio of family income to poverty
         "alcohol", #alcohol
         "edu_factor",
         "marital_factor",
         "alcohol_factor",
         "sodium"
         )

label(dat_2017_2020_clean1$edu_factor) <- "Education level"
label(dat_2017_2020_clean1$incomeratio) <- "Ratio of family income to poverty"
label(dat_2017_2020_clean1$marital_factor) <- "Marital status"
label(dat_2017_2020_clean1$alcohol_factor) <- "Ever have 4/5 or more drinks every day"

delete_missing_2017_2020 <- na.omit(dat_2017_2020_clean1) 
table1(~demo_age_years+demo_age_cat+demo_race+demo_gender+bp_sys_mean+bp_dia_mean+
         htn_aware+bp_med_n_class+bp_med_use+cc_smoke+bmi+cc_diabetes+cc_ckd+
         cc_cvd_any+incomeratio+edu_factor+marital_factor+alcohol_factor+sodium, 
       data=delete_missing_2017_2020)

##############################################################################################
## Export the clean data sets
##############################################################################################
write.csv(delete_missing_2011_2012, "dat_2011_2012_new.csv", row.names=F)
write.csv(delete_missing_2013_2014, "dat_2013_2014_new.csv", row.names=F)
write.csv(delete_missing_2015_2016, "dat_2015_2016_new.csv", row.names=F)
write.csv(delete_missing_2017_2020, "dat_2017_2020_new.csv", row.names=F)
