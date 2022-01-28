
## load necessary packages
library(tidyverse)
library(readxl)
library(lubridate)
library(compare)

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
#change col names to easily typed names


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
 
if (sum(duplicated(pt$mrn)) == 0 ) {
        print("All patients only received one run of ECMO")
} else {
        print("Some patients receive more than one run of ECMO. RECHECK.")
}
       
print ("02_df pt is cleaned and in appropriate data classes.")

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
        print("mrn's in dataframe lab and pt matched.")
}else {
                print("mrn in dataframes lab and pt are not matched.")
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
                dayone = min(dtmd), 
                dayn = max (dtmd), 
                labdur = max(dtmd)-min(dtmd)
                )

ptdur <- pt %>%
        select(mrn,datecannulated,dateDEcannulated) %>%
        mutate(dcn = as.Date(datecannulated),dDEcn=as.Date(dateDEcannulated))%>% 
        group_by(mrn) %>% 
        select(mrn,dcn,dDEcn)%>% 
        mutate(ptdur = dDEcn - dcn)

checkdf <- full_join(ptdur,labdur, by = "mrn")

checkdf$ptdur - checkdf$labdur
#this showed that patient duration is shorter than lab duration.
# could it be that lab counts the date admission to date discharge from icu ?
# but pt dur shouldnt be longer than lab duration (which suggests missing data in lab)
sum(checkdf$ptdur - checkdf$labdur > 0)
checkdf[(checkdf$ptdur - checkdf$labdur >0 ),]
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
checkdf2$ptadmndur - checkdf2$labdur
# looks abit better
checkdf2[(checkdf2$ptadmndur - checkdf2$labdur < 0),]

#should really use ecmo run time as gold standard. 

#now we will look at NA's within those ranges. 
ptdur$datedecanplus2 <- ptdur$dDEcn + 2
ptdur$datedecanplus2 <- as.POSIXct(ptdur$datedecanplus2, tz = "GMT")
#to c onvert to UTC in mid seconds as 

ptd <- ptdur %>% select(mrn,datedecanplus2)
labecmod <- full_join(lab,ptd, by = "mrn")

clab <- labecmod %>% 
        group_by (mrn) %>% 
        filter (dtm >= min(dtm) & dtm <datedecanplus2)

#subset date into ecmo runs only.
#then we will find out how much NA's there are.

#lets make sense of this NA value by appending cohort information.

jv <- pt %>% select(mrn,cohort)
clab <- full_join(clab,jv,by="mrn")
rm(jv)

clab<- clab %>% 
        select(mrn,axa,apttr,cohort,dtm)%>%
        group_by(mrn) %>%
        mutate(
                dtm = min(dtm),
                na_axa = sum(is.na(axa)),
                na_axa_perc = ((sum(is.na(axa)))/n()) * 100,
                na_apttr = sum(is.na(apttr)),
                na_apttr_perc = ((sum(is.na(apttr)))/n()) * 100,
        )

#let's explore maximum , median, mean, and minimum NA's per axa apttr in each pt.
#let's adjust for ecmorunt
t <- pt %>% mutate(ecmorunt = dateDEcannulated - datecannulated)
dlab <- full_join(clab,t,by = "mrn")

so actually what we need is a data frame with column names 
- mrn
- cohort
- ecmorunt
- bloodtestrunt
-no of na in axa
- no of na in apttr
- propn 

