# INITIALISING ------
## load necessary packages
library(tidyverse)
library(readxl)
library(lubridate)
library(compare)

# READING DATA -----
## ## will read data from source excel file .xlsx into 4 different data frames

pt <- read_excel("data/antixa_data_extract_20211203_nopt.xlsx",sheet = "ptlist")
hep <- read_excel("data/antixa_data_extract_20211203_nopt.xlsx",sheet = "heparin")
lab <- read_excel("data/antixa_data_extract_20211203_nopt.xlsx",sheet = "labresults")
bl <- read_excel("data/antixa_data_extract_20211203_nopt.xlsx",sheet = "bleedingscores")

message ("01_all relevant data are loaded into 4 df.")
  
## the source data is an excel spread sheet with 4 tabs. 
## Tab 1 is a master tab of patient in a unique MRNs
# Tab 2 is a patient heparin prescription
# tab 3 is a labresults 
# tab 4 is a bleeding scores.
## all read as per above code into 4 df. 

## ensure data is in appropriate classes.

# CLEANING DF 1 of 4 -----
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
# this is 0 which made sense as no pts gonna receive a 2nd ecmo.
#change col names to easily typed names

##---DF1 Sense Check -----
if (sum(duplicated(pt$mrn)) == 0 ) {
        message("All patients only received one run of ECMO")
} else {
        message("Some patients receive more than one run of ECMO. RECHECK.")
}
       
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

# CLEANING DF 2 of 4 -----
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
        message("mrn's in dataframe lab and pt matched.")
}else {
                message("mrn in dataframes lab and pt are not matched.")
        }
#now we gonna change df lab to all correct col classes.
lab$axa <- as.double(lab$axa)
lab$apttr <- as.double(lab$apttr)

## note quite a lot of NAs, is there a pattern ? 
#the date range of each pt should correspond to date range of ecmo run times.

#need to check date and time range for the lab values match the ecmo run times.
#need to count number of NAs for each mrn.

#will work in dates only as individual second / hour difference on the day of ecmo run 
#wont matter.

labdur <- lab %>% 
        group_by(mrn) %>% 
        mutate(dtmd = as.Date(dtm)) %>% 
        summarise(
                done = min(dtmd), 
                dn = max (dtmd), 
                labd = max(dtmd)-min(dtmd)
                )

ptdur <- pt %>%
        select(mrn,datecannulated,dateDEcannulated) %>%
        mutate(dcn = as.Date(datecannulated),dDEcn=as.Date(dateDEcannulated))%>% 
        group_by(mrn) %>% 
        select(mrn,dcn,dDEcn)%>% 
        mutate(ptdur = dDEcn - dcn)

checkdf <- full_join(ptdur,labdur, by = "mrn")

tdiff <- checkdf$ptdur - checkdf$labd
#this showed that patient duration is shorter than lab duration.
# could it be that lab counts the date admission to date discharge from icu ?
# but pt dur shouldnt be longer than lab duration (which suggests missing data in lab)
sum(checkdf$ptdur - checkdf$labd > 0)
checkdf[(checkdf$ptdur - checkdf$labd >0 ),]
#looking at this data frame looks like there is only one or two discrepancy for most cases.
#except 6381359H (missing data 3 days on blood),6855512Y(missing data 2/3 days pre),

#lets check the admission date - discharge date and compare with lab data. 

ptdur2 <- pt %>%
        select(mrn,admissiondate,dischargedate) %>%
        mutate(admn = as.Date(admissiondate),dcd=as.Date(dischargedate))%>% 
        group_by(mrn) %>% 
        select(mrn,admn,dcd)%>% 
        mutate(ptadmndur = dcd - admn)

checkdf2 <- full_join(ptdur2,labdur,by = "mrn")
checkdf2$ptadmndur - checkdf2$labd
# looks abit better
checkdf2[(checkdf2$ptadmndur - checkdf2$labd < 0),]

#should really use ecmo run time as gold standard. 

#now we will look at NA's within those ranges. 
#lets 
ptdur$datedecanplus1 <- ptdur$dDEcn + 1
ptdur$datedecanplus1 <- as.POSIXct(ptdur$datedecanplus1, tz = "GMT")
#to c onvert to UTC in mid seconds as 
#lets be generous and give a 1 days after decannulation and 1 day before decannulation. 
ptdur$dprecan <-ptdur$dcn - 1
ptdur$dprecan <- as.POSIXct(ptdur$dprecan, tz = "GMT")

labecmodur <- full_join(
        lab,
        ptdur %>% select(mrn,dcn,dprecan,datedecanplus1),
        by = "mrn"
)

clab <- labecmodur %>% 
        group_by (mrn) %>% 
        filter (dtm >= dcn & dtm <datedecanplus1)

#subset date into ecmo runs only.
#then we will find out how much NA's there are.

#lets make sense of this NA value by appending cohort information.

clab <- full_join(
        clab,
        pt %>% 
                mutate(ecmorunt = dateDEcannulated - datecannulated) %>%
                select(mrn,cohort,ecmorunt),
        by="mrn")

clabna<- clab %>% 
        select(mrn,axa,apttr,cohort,dtm,ecmorunt)%>%
        group_by(mrn) %>%
        mutate(
                no_lab = n(),
                na_axa = sum(is.na(axa)),
                na_axa_perc = ((sum(is.na(axa)))/n()) * 100,
                na_apttr = sum(is.na(apttr)),
                na_apttr_perc = ((sum(is.na(apttr)))/n()) * 100,
        ) 

clabna <- clabna %>% 
        select(
                mrn,
                cohort,
                ecmorunt,
                no_lab,
                na_axa,
                na_axa_perc,
                na_apttr,
                na_apttr_perc
        )

clabna <- unique(clabna)
#let's explore maximum , median, mean, and minimum NA's per axa apttr in each pt.
#let's adjust for ecmorunt


# we need to adjust missing data per ecmo run times.

#lab with ecmo run times 


##actually what we need is a data frame with column names 
##in_ mrn
##in_ cohort
##in_ ecmorunt
##in_ bloodtestrunt
##in_no of na in axa
##in_ no of na in apttr
##in_ propn 


##d use make_difftime , unit = "das"
#lets make a dataframe that has day of blood tests as ecmo run days.

clab$dcn <- as.POSIXct(clab$dcn,tz = "GMT")
clab$min_on_e <- round(clab$dtm - clab$dcn)
clab$day_on_e <- round(difftime(clab$dtm,clab$dcn , units = "days"))

message("Df 2 is cleaned and ready as `clab`")

### CLEANING DF 3 of 4.

