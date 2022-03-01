###THIS FILE SHOULD BE OPENED FIRST TO LOAD NECESSARY DATA SOURCES,IMPORT THEM INTO R AND CLEAN THEM####

# 0.  INITIALISING ------
## load necessary packages
pkgs <- c("tidyverse","readxl","lubridate","compare")
lapply(pkgs,require,character.only = TRUE)

# 1. READING DATA -----
## ## will read data from source excel file .xlsx into 4 different data frames

pt <- read_excel("data/antixa_data_extract_20211203_nopt.xlsx",sheet = "ptlist")
hep <- read_excel("data/antixa_data_extract_20211203_nopt.xlsx",sheet = "heparin")
lab <- read_excel("data/antixa_data_extract_20211203_nopt.xlsx",sheet = "labresults")
bl <- read_excel("data/antixa_data_extract_20211203_nopt.xlsx",sheet = "bleedingscores")

message ("01_all relevant data are read into 4 df.")
  
## the source data is an excel spread sheet with 4 tabs. 
## Tab 1 is a master tab of patient in a unique MRNs, age, date information about admission and ecmo. 
# Tab 2 is a patient heparin prescription
# tab 3 is a labresults of patients.
# tab 4 is a bleeding scores.
## all read as per above code into 4 df. 

## Afterwards, data cleaning , relevant transformation and engineering will be done.

# 2.0. CLEANING DF 1 of 4 -----

## 2.1. Cleaning column classes,and levels.-----

# first we clean pt df 

#first, we put the columns into appropriate classes. 
#assign factors.
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
#recode column names to allow easier access.
colnames(pt) <- c(
        "cohort",
        "admissiondate",
        "mrn",
        "age",
        "diagnosis",
        "diagnosiscat",
        "mode",
        "datecannulated",
        "dateDEcannulated",
        "survivedecmo",
        "survivedicu",
        "dischargedate",
        "dxsum"
)

#sense check 1 : - summary(unique(pt$Number))
# this is 0 which made sense as no pts gonna receive a 2nd ecmo.CLARIFIED with Dr R.

## 2.2. Creating a separate column for ?PE patients and ECMO run times.--------

# need a data column to indicate PE patients will have higher target.
pt$pe <- ifelse(pt$dxsum == "flu_a+bac+pe","yes","no")
#showed only 2 patients are on PE.
#create new column of ecmoruntimes.
pt$ecmorunt <- pt$dateDEcannulated  - pt$datecannulated

##
## 2.3. ---DF1 Sense Check -----
if (sum(duplicated(pt$mrn)) == 0 ) {
        message("Sensecheck 1 : All patients only received one run of ECMO")
} else {
        message("NEED ACTION -Some patients receive more than one run of ECMO. RECHECK.")
}


#change all mrn's  to upper 

pt$mrn <- toupper(pt$mrn)

message ("02_df pt is cleaned and in appropriate data classes.")

#datetime sensechecks : 
#logic - discharge date => admission date, decann date > cann date, date cann >= admn date

#pt$dateDEcannulated > pt$datecannulated - all true
#pt$admissiondate < pt$dischargedate  - not true for 6960892X - to review.
#pt$admissiondate <= pt$datecannulated - not true 5 entries. thus will explore. 

#pt %>% 
#        select(mrn, admissiondate,datecannulated) %>% 
#        mutate(timetocan = datecannulated - admissiondate) %>% 
#        group_by(timetocan) %>% 
#        arrange((timetocan))

#This above code showed 6842347C,6578155C,6578347K,1087895N,6939825N have issues.
#these will be discussed to resolve those issues.


# 3.0. CLEANING DF 2 of 4 -----

## 3.1. Cleaning cases for mrn col, colnames, col classes----
#Now we will address lab df.

#change colnames to easily typed names.
colnames(lab) <- c("mrn","dtm","axa","apttr")

#mrn in lab df should link and be identical to mrn in pt df. 
#sensecheck : setdiff(unique(pt$mrn),unique(lab$mrn))
#this is not identical and is different in "6606299s" in small letter "s".
pt$mrn[pt$mrn == "6606299s"] <- "6606299S"
#above to fix the typo.
#repeat sense check. 
#sensecheck : compare(unique(pt$mrn),unique(lab$mrn),ignoreOrder = TRUE)
#we are now satisfied that lab mrn and pt mrn are the same.
if (compare(unique(pt$mrn),unique(lab$mrn),ignoreOrder = T)$result == TRUE ) { 
        message("03_SENSECHECKED : mrn's in dataframe lab and pt matched.")
}else {
                message("ACTION REQUIRED :mrn in dataframes lab and pt are not matched.")
        }
#now we gonna change df lab to all correct col classes.
lab$axa <- as.double(lab$axa)
lab$apttr <- as.double(lab$apttr)

#change all mrn to upper
lab$mrn <- toupper(lab$mrn)

message("04_lab df is cleaned and in appropriate classes.")
# CLEANING DF 3 of 4 ------

#rename the colnames appropriately.
colnames(hep) <- c(
        "mrn",#for MRN
        "chart_t",#for chartTIME
        "s_label",#for shortLabel
        "t_form",#for terseForm
        "unit",#for unitOfMeasure
        "kg"#forkg
        )

#note setdiff order is important.
#setdiff x , y means things that are in x that is not in y. 
#so ideally should be applied both ways.

#sensecheck with summary(hep$mrn %in% pt$mrn) showed that all the patients in this df are also in main df.
#sensecheck 2 with setdiff showed the following is missing.
#setdiff(unique(pt$mrn),unique(hep$mrn))

#this code showed the missing hep prescriptions.

if (compare(unique(pt$mrn),unique(hep$mrn),ignoreOrder = T)$result == TRUE ) { 
      message("05_mrn's in dataframe hep and pt matched.") 
}else {
      message("05_REQUIRED ACTION : mrn in dataframes hep and pt are NOT matched.")
  }
#

#change mrn to upper
hep$mrn <- toupper(hep$mrn)

missing_hep <- setdiff(unique(pt$mrn),unique(hep$mrn))
#these patients have missing heparin prescriptions.
hep$t_form <- as.numeric(hep$t_form)
hep$s_label <- as.factor(hep$s_label)
hep$unit <- as.factor(hep$unit)

message("06_hep is cleaned and in appropriate classes.")

# CLEANING DF 4 of 4 -----

#change col names into appropriate forms
colnames(bl) <- c(
        "mrn",#lifeTimeNumber
        "chart_t",#chartTime
        "s_label",#shortLabel
        "sd_label",#attr_shortLabel
        "t_form",#terseForm
        "v_form"#verboseform
)

#significant number of missing data in this- there is only 63 patients.

#setdiff(unique(pt$mrn),unique(bl$mrn))

bl$s_label <- as.factor(bl$s_label)
bl$sd_label <- as.factor(bl$sd_label)
bl$t_form <- as.factor(bl$t_form)
bl$v_form <- as.factor(bl$v_form)

bl$mrn <- toupper(bl$mrn)

missing_bl <- setdiff(unique(pt$mrn),unique(bl$mrn))
length(missing_bl) 
#this showed that 138 patient data is missing, 

#Like before we will enrich it with some further information by appending

bl <- full_join(
  bl,
  pt %>% select(mrn,cohort,datecannulated,dateDEcannulated,ecmorunt),
  by = "mrn"
)

message(
  "Df's 
  pt : clean and engineered and include patient main details
  lab : raw lab values, cleaned and in appropriate classes.  
  hep : cleaned and appeneded ecmorunt
  missing_bl : missing complications explicit.
  missing_hep : missing heparin prescription explicit.
  "
)

