
## load necessary packages
library(tidyverse)
library(readxl)
library(lubridate)

## ## will read data from source excel file .xlsx into 4 different data frames

pt <- read_excel("data/antixa_data_extract_20211203_nopt.xlsx",sheet = "ptlist")
hep <- read_excel("data/antixa_data_extract_20211203_nopt.xlsx",sheet = "heparin")
lab <- read_excel("data/antixa_data_extract_20211203_nopt.xlsx",sheet = "labresults")
bl <- read_excel("data/antixa_data_extract_20211203_nopt.xlsx",sheet = "bleedingscores")

print ("01_all relevant data are loaded into 4 df.")
## the source data is an excel spread sheet with 4 tabs. 
## Tab 1 is a master tab of patient in a unique MRNs
# Tab 2 is a patient heparin prescription
# tab 3 is a labresults 
# tab 4 is a bleeding scores.
## all read as per above code into 4 df. 

## ensure data is in appropriate classes.
# first we clean pt df 

ftptcols <- c("Cohort","Diagnosis","DiagnosisCategory","Mode","SurvivedECMO","SurvivedICU")
pt[ftptcols] <- lapply(pt[ftptcols],factor)

# clean the levels of these factor columns
# keen to keep original column  make a new column which is a summary of diagnosis column.
pt$dxsum <- pt$Diagnosis

levels(pt$dxsum) <-
        c(
                "flu_a",
                "flu_a+fungal",
                "flu_a+bac",
                "flu_a+bac+pe", 
                "covid19_pneu",
                "covid19_pneu",
                "covid19_pneu+hlh",
                "covid19_pneu",
                "covid19_pneu",
                "covid19_pneu",
                "covid19_pneu+lscs",
                "covid19_pneu+lscs",
                "flu_a",
                "flu_a",
                "flu_a+bac",
                "flu_a+h1n1+copd",
                "flu_a+bac",
                "flu_a+bac",
                "flu_b",
                "flu_b+bac",
                "flu_b+sep",#flu B + sepsis 
                "flu_b+bac",
                "flu_b+bac",
                "flu_b+bac",
                "h1n1",
                "h1n1+bac",
                "h1n1+bac",
                "h1n1+bac",
                "h1n1+bac",
                "h1n1+bac",
                "h1n1+bac",
                "flu_a",#influenza A
                "h1n1",#influenza a h1n1
                "flu_b",#influenza B
                "flu_b+bac", #influenza B and mrsa pvl pneumonia#
                "flu_b+bac",#influenza b and mssa
                "flu_b+bac",#influenza b and pvl mssa
                "flu_b+bac",#influenza b and pvl staph
                "flu_b+bac",#infleunza b with pneumoc and mof
                "flu_b+bac",#infleunza b sepsis and mod
                "flu_a+bac+pe",#multifactorial srf , flu , prev klebsiella, {E}
                "paraflu",#parainfluenza
                "paraflu+measles",#parainfluneza and measles
                "pcp+cmv+hiv",#pcp,cmv,hiv
                "pneum",#pneumonitis
                "rsv",#rsv
                "flu_a+sep",#flu a and septic shock
                "sepsis",
                "flu_a+sep",
                "flu_b+sep"
                
        )

levels(pt$Mode) <- c("vv","vv")
levels(pt$SurvivedECMO) <- c("no","no","yes","yes")
levels(pt$SurvivedICU) <- c("no","no","yes","yes")
#sense check 1 : - summary(unique(pt$Number))
# this is 0 which made sense as no pts gonna receive a 2nd ecmo. 
print ("02_df pt is cleaned and in appropriate data classes.")