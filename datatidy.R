# 0.  INITIALISING ------
## load necessary packages
pkgs <- c("tidyverse","readxl","lubridate","compare","tidylog")
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

## 3.2. Creating a dataframe to explore duration of lab values ----

###We need to explore NA and their impact on data. Whether there is a pattern?
### The data range of each patient should corespond to ecmo run times.
### In the absence, we will use ecmo run time as gold standard.
### We will use day instead of individual second/ hour difference for clinical impact. 

labdur <- lab %>% 
        group_by(mrn) %>% 
        mutate(dtmd = as.Date(dtm)) %>% 
        summarise(
                done = min(dtmd), 
                dn = max (dtmd), 
                labd = max(dtmd)-min(dtmd)
                )
#labdur is a df  where you can find out when blood results begin to available and last blood results recorded.
#this is enriched with further info on run times, cohort,etc to create a checkdf 
checkdf <- full_join(
            labdur,
            pt %>% select(mrn,ecmorunt,datecannulated,dateDEcannulated,cohort),
            by = "mrn"
)
tdiff <- checkdf$ecmorunt - checkdf$labd
#this showed that patient duration is shorter than lab duration.

# could it be that lab counts the date admission to date discharge from icu ?
# but pt dur shouldnt be longer than lab duration (which suggests missing data in lab)
sum(checkdf$ecmorunt - checkdf$labd > 0)
checkdf[(checkdf$ecmorunt - checkdf$labd >0 ),]
#looking at this data frame looks like there is only one or two discrepancy for most cases as you would expect toelrance of 1 or 2 days either way.

#except 6381359H (missing data 3 days on blood but towards the end so shouldnt be a proble,
#),6855512Y(missing data 2/3 days pre),
rm(tdiff)

## 3.3 Creation of columns which have tolerance of 1 +/- of ecmo run times to subset lab values. ----

#now we will look at NA's within those ranges. 
#lets make new columns where we give a tolerance of +1 or -1 day of ecmo duration.
checkdf$dateDEplus1 <-checkdf$dateDEcannulated + 1
checkdf$dateDEplus1 <- as.POSIXct(checkdf$dateDEplus1, tz = "GMT")

checkdf$dateprecan <- checkdf$datecannulated - 1
checkdf$dateprecan <- as.POSIXct(checkdf$dateprecan, tz = "GMT")

labecmodur <- full_join(
        lab,
        checkdf %>% select(mrn,dateprecan,dateDEplus1,ecmorunt,cohort,datecannulated,dateDEcannulated),
        by = "mrn"
)

clab <- labecmodur %>% 
        group_by (mrn) %>% 
        filter (dtm >= dateprecan & dtm <dateDEplus1) %>% 
        ungroup()
#clab is a dataframe where blood values are subsettd by ecmo duration +1 day pre ecmo and 1 day post ecmo.
#then we will find out how much NA's there are.
#this reduces to 9280 entries from 12309 entries

#lets make sense of this NA value by appending cohort information.
## 3.4. Exploring missing data NA values within this range. ----

clabna<- clab %>% 
        select(mrn,axa,apttr,cohort,dtm,ecmorunt)%>%
        group_by(mrn) %>%
        summarize(
                no_lab = n(),
                na_axa = sum(is.na(axa)),
                na_axa_perc = ((sum(is.na(axa)))/n()) * 100,
                na_apttr = sum(is.na(apttr)),
                na_apttr_perc = ((sum(is.na(apttr)))/n()) * 100,
                erunt = min(ecmorunt)
        ) 
#this is a dataframe where we explore NA values for both apttr and axa in clean lab values.
#this data frames make summary of number of lab values for each patient, number of missing AXA and number of missing APTTR
clabna <- full_join(
        clabna,
        pt %>% select(mrn,cohort,datecannulated),
        by = "mrn"
        )

c2 <- clabna %>% 
          group_by(mrn) %>% 
          summarize(
            missingaxa = (no_lab - na_axa)/as.numeric(erunt),
            missingapttr = (no_lab - na_apttr)/as.numeric(erunt)
                 )

clabna <- full_join(
  clabna,
  c2 %>% select(mrn,missingaxa,missingapttr),
  by = "mrn"
)
rm(c2)


message("04_Clean lab df to explore NA calues : clabna is ready.")
#let's explore maximum , median, mean, and minimum NA's per axa apttr in each pt.

# we need to adjust missing data per ecmo run times.

# histogram of ecmoruntimes showed a median run time of 13 days.
#histogram of number of lab results showed a median of 34 days.
# thus dividing median by median results = 34/13 = 2.6 blood draws per day.
#so if there is a missing data but it is still less than 2.6 blood draws per day than its ok.

#note 1st Nov 2019 is where we switch.

#manual inspection  of the data verified missing data for APTTR is acceptable for flu cohort.
# manual inspection oft he data for AXA

#NOTE although axa is demarcated from 1st november - 5th november cannulation is still using apttr
### 3.4.a clabna2 df is now genrated which showed patients in axa monitoring group but really uses apttr.-----

clabna2 <- clabna[,c("mrn","no_lab","na_axa","erunt","cohort","datecannulated","missingaxa","na_apttr")]
clabna2 <- clabna2 %>% filter(missingaxa < 1 & cohort != "Flu")
#needs decision on which arm to consider these.
#d <- c("6849178A",
#       "6851184E",
#       "5277619S","6852899A","6855137S",
#       "6857152G","6860416Y","6935380E","6950251Z","6956232H","6968816C","5277619S")
#d %in% clabna2$mrn 
#all true here 
# some consultants prefer apttr

## 3.5. Creation of column where axa / apttr are in/out of range ----
#now lets make a column where we label in and out of range for apttr and axa, bearing in mind PE patients.
pt %>% filter(pe == "yes") %>% select(mrn)
#this code showed that patients with PE have MRN 6513466R, and 6584394E.
clabpe <- clab %>% filter(mrn == "6513466R"| mrn == "6584394E")
clabpe <- clabpe %>%
          group_by(mrn) %>%
          mutate ( axarange = case_when(
              axa <0.6 ~ "below",
              axa >= 0.6 & axa <= 1.0 ~ "in",
              axa > 1.0 ~ "above"),
              apttrrange = case_when(
                apttr <2.0 ~ "below",
                apttr >= 2.0 & apttr <= 2.5 ~ "in",
                apttr > 2.5 ~ "above"
              )
            )%>%
          ungroup()
          

clabnpe <- clab %>% filter(!mrn %in% c("6513466R","6584394E"))
clabnpe <- clabnpe %>%
  group_by(mrn) %>%
  mutate ( axarange = case_when(
    axa <0.6 ~ "below",
    axa >= 0.6 & axa <= 1.0 ~ "in",
    axa > 1.0 ~ "above"),
    apttrrange = case_when(
      apttr <2.0 ~ "below",
      apttr >= 2.0 & apttr <= 2.5 ~ "in",
      apttr > 2.5 ~ "above"
    )
  )%>%
  ungroup()

#make 2 dataframes clabnpe and clabpe to treat and make sure correct range for PE and non PE 
#pts

clab <- rbind(clabnpe,clabpe)

rm(clabpe,clabnpe,checkdf,labdur,labecmodur)

message("05_Df 2 is cleaned and ready as `clab` for cleaned lab data and 'clabna' for exploring NA")

## 3.6. Creating a day on ecmo column  ----

#let's create a day on ecmo column to append to each row.

clab <- clab %>% 
  group_by(mrn) %>% 
  mutate(ecmo_day = round(difftime(dtm,datecannulated,units = "days")),ecmo_min = dtm - datecannulated ) %>%
  ungroup()

clab$ecmo_day <- as.factor(clab$ecmo_day)


## 3.7. Creating a column assigning monitoring groups ----
#lets think about assigning group.

# Several ways of assigning groups 




### 3.7.a. Using a simple defintion ----

#axa monitoring was established on 1 Nov 2019. Thus any date before should be apttr and those after would be axa. 

clab$g1 <- ifelse(clab$datecannulated < as.Date("2019-11-01"),"apt","axa")
clab$g1 <- as.factor(clab$g1)

#clab %>% count(g1,mrn) %>% summary() 
# this showed that there is 72 people apt and 129 in axa groups.

### 3.7.b. Using a different defintion -----

#to do this we need to review the clabna df.

# "6776201S" - date of cannulation 2019-11-12 is in apt monitoring group as large missing values in axa but its only 50% so could 
#axa doesnt require that much monitoring.

# 6771817W from 11-05 however has 90% missing and therefore should be for apt.
#thus cut off should be 2019 11 06.

# "5277619S","6852899A","6860416Y","6851184E" wave 1 patient so thought in axa group they hve no axa measured. 
# "6950251Z","6956232H","6935380E","6968816C" wave 2 patients who have more apt than axa monitoring.

clab$g2 <- ifelse(clab$datecannulated < as.Date ("2019-11-06"),"apt","axa")
reassign1 <- c("5277619S","6852899A","6860416Y","6851184E","6950251Z","6956232H","6935380E","6968816C")

#cplab[cplab$mrn %in% reassign_group_70p,]$group <- "apt"

clab[clab$mrn %in% reassign1,]$g2 <- "apt"
clab$g2 <- as.factor(clab$g2)

# now there is 81 in apt group and 120 in axa group. 


## 3.8.a. Now let's count the first time axa/apt get in range using g1 definitions. ----
dfg1 <- clab %>%
        select(mrn,dtm,ecmorunt,cohort,datecannulated,axarange,apttrrange,ecmo_day,g1)%>% 
        filter((g1 == "axa" & axarange == "in")|(g1 == "apt" & apttrrange == "in")) %>% 
        group_by(mrn) %>%
        slice_min(dtm)%>% 
        ungroup()
        
dfg1m <- setdiff(pt$mrn,dfg1$mrn)  
dfg1m <- clab %>% select(mrn,g1,ecmorunt,cohort,datecannulated) %>% filter(mrn %in% dfg1m)
dfg1m <- unique(dfg1m)  
#this 35 patients never got in range. Do they have a worse outcome? Mainly below 
#dfg1[duplicated(dfg1$mrn),]
 # this show there are 2 duplicated entries. "6941358K" "6956232H"

# 6941358K has two time stamp entries 2020-12-07 02:09:00 where there is one axa and one na. 
# 2021-01-19 23:32:0 timestamp for 6956232H - are both in axa group
#now let's remove those two entries.


dfg1 <- dfg1[!((dfg1$mrn == "6941358K")& (is.na(dfg1$apttrrange))),]
dfg1 <-unique(dfg1)

#That is resolved ! :) 

#let's calculate the time to first in range.
dfg1$tfirst <- dfg1$dtm - dfg1$datecannulated
dfg1$tfirst <- as.numeric(dfg1$tfirst)
#now let's assign 'event as yes' becuase they all achieved 'event.

#tfirst for dfg1m should be ecmorunt and event shoudl be no as they NEVER achieve it.
#let's create a sepearte df. 
#dfg1p for processed. 

dfg1p  <- dfg1 %>% select(mrn,ecmorunt,cohort,datecannulated,g1,tfirst)
dfg1p$event <- "yes"

dfg1m$event <- "no"
dfg1m$tfirst <- as.numeric(dfg1m$ecmorunt)


dfg1p <- rbind(dfg1p,dfg1m)
dfg1p$event <- as.factor(dfg1p$event)
dfg1p <- unique(dfg1p)

### 3.8.b. let's use a g2 definiton for same treatmnet -----

dfg2 <- clab %>%
  select(mrn,dtm,ecmorunt,cohort,datecannulated,axarange,apttrrange,ecmo_day,g2)%>% 
  filter((g2 == "axa" & axarange == "in")|(g2 == "apt" & apttrrange == "in")) %>% 
  group_by(mrn) %>%
  slice_min(dtm)%>% 
  ungroup()  


dfg2m <- setdiff(pt$mrn,dfg2$mrn)  
dfg2m <- clab %>% select(mrn,g2,ecmorunt,cohort,datecannulated) %>% filter(mrn %in% dfg2m)
dfg2m <- unique(dfg2m)
#here there is 34 diferences. 

sum(duplicated(dfg2$mrn))
# this showed that 6941358K is the duplicated.
dfg2 <- dfg2[!((dfg2$mrn == "6941358K")& (is.na(dfg2$apttrrange))),]

dfg2$tfirst <- dfg2$dtm - dfg2$datecannulated
dfg2$tfirst <- as.numeric(dfg2$tfirst)

dfg2p <- dfg2 %>% select(mrn,ecmorunt,cohort,datecannulated,g2,tfirst)
dfg2p$event <- "yes"

dfg2m$event <- "no"
dfg2m$tfirst <- as.numeric(dfg2m$ecmorunt)

dfg2p <- rbind(dfg2p,dfg2m)
dfg2p$event <- as.factor(dfg2p$event)

rm(dfg1m,dfg2m)
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
#

missing_hep <- setdiff(unique(pt$mrn),unique(hep$mrn))
#these patients have missing heparin prescriptions.
hep$t_form <- as.numeric(hep$t_form)
hep$s_label <- as.factor(hep$s_label)
hep$unit <- as.factor(hep$unit)

#Likewise we will subset heparin prescriptions with ecmoruntimes.
#unlike blood tests prescriptions should be more or less the same as cannulation and decan.
hep <- full_join(
  hep,
  pt %>% select(mrn,cohort,datecannulated,dateDEcannulated,ecmorunt),
  by = "mrn"
)

#now subset into clean dataframe.
chep <- hep %>% 
        group_by(mrn) %>%
        filter(chart_t >= datecannulated & chart_t <= dateDEcannulated) %>%
        ungroup()

#let's explore if there are implicit missing data by comparing with ecmoruntimes.

nahep <- chep %>%
          group_by(mrn) %>% 
          summarise(
            chart_tone = min(chart_t),
            chart_tlast = max(chart_t),
            chardur = ((chart_tlast - chart_tone)/86400)
          ) %>% 
        ungroup() 

nahep <- left_join(
  nahep,
  pt %>% select(mrn,cohort,datecannulated,dateDEcannulated,ecmorunt),
  by = "mrn"
)

#okay cool so we can use rle and length to get the number of drug prescription changes.

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

missing_bl <- setdiff(unique(pt$mrn),unique(bl$mrn))
length(missing_bl) 
#this showed that 138 patient data is missing, 

#Like before we will enrich it with some further information by appending

bl <- full_join(
  bl,
  pt %>% select(mrn,cohort,datecannulated,dateDEcannulated,ecmorunt),
  by = "mrn"
)

#now we will explore the explicit missing data.


nabl <- bl %>%
  group_by(mrn) %>% 
  summarise(
    chart_tone = min(chart_t),
    chart_tlast = max(chart_t),
    chardur = ((chart_tlast - chart_tone)/86400)
  ) %>% 
  ungroup() 

nabl <- left_join(
  nabl,
  pt %>% select(mrn,cohort,datecannulated,dateDEcannulated,ecmorunt),
  by = "mrn"
)

#this showed that only 63 
#however visual inspection of this 63 showed
# 6929427Q 8.25 to 24 char duration vs ecmoruntime
# 6956374C chart duration >> ecmoruntime
# 6971769E missing 4 

message ("06_all df cleaned")

message(
  "Df's 
  pt : clean and engineered and include patient main detail.s
  lab : raw lab values. 
  clab :clean and engineered lab values. 
  clabna :clean lab values in NA to explore explicit missing data. 
  hep : cleaned and engineered
  missing_bl : missing complications explicit.
  missing_hep : missing heparin prescription explicit.
  "
)

