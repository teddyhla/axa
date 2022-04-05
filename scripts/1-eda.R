### HERE THIS SCRIPT FOLLOWS 0-importclean.R and will inherit dataframes there. 
## It will explore missing values, missing data and group assignment  issues.


## 0. Source the original script.-----

source("scripts/0-importclean.R")

message ("0 - necessary packages loaded.")

message ("1 - source script run and loaded.")


###Step 1 are designed to clean and engineer lab values df. 
###1.3.a. is exploring missingness
### 1.4 to 1.5. is data engineering to append some other additional information.
### 1.6. to 1.7 is assigning group and calculating time to first therapeutic range 'tfirst' in hours .

## 1.1. Creating a dataframe 'checkdf' to explore duration of lab values ----

###We need to explore NA and their impact on data. Whether there is a pattern?
### The data range of each patient should correspond to ecmo run times.
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
rm(tdiff,labdur)

## 1.2.  Creation of clean lab values 'clab' s which have tolerance of 1 +/- of ecmo run times ----

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
#clab is a dataframe where blood values are subsetted by ecmo duration +1 day pre ecmo and 1 day post ecmo.
#then we will find out how much NA's there are.
#this reduces to 9280 entries from 12309 entries

#lets make sense of this NA value by appending cohort information.
## 1.3.a. Creating 'clabna' dataframe to explore missingness ----

#visdat::vis_dat(clab) #showed that the missingness of AXA and APTTR are not random.

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
rm(c2,labecmodur)


message("04_ lab cleaned to create 'clab' and 'clabna'.")
#let's explore maximum , median, mean, and minimum NA's per axa apttr in each pt.

# we need to adjust missing data per ecmo run times.

# histogram of ecmoruntimes showed a median run time of 13 days.
#histogram of number of lab results showed a median of 34 days.
# thus dividing median by median results = 34/13 = 2.6 blood draws per day.
#so if there is a missing data but it is still less than 2.6 blood draws per day than its ok.
# this may not be correct as axa monitoring may require less blood draws.

#note 1st Nov 2019 is where we switch.

#manual inspection  of the data verified missing data for APTTR is acceptable for flu cohort.
# manual inspection of the data for AXA

#NOTE although axa is demarcated from 1st november - 5th november cannulation is still using apttr
### 1.3.b. Creating 'clabna2' which showed patients in axa monitoring group but really uses apttr.-----

clabna2 <- clabna[,c("mrn","no_lab","na_axa","erunt","cohort","datecannulated","missingaxa","na_apttr")]
clabna2 <- clabna2 %>% filter(missingaxa < 1 & cohort != "Flu")
#needs decision on which arm to consider these.
#d <- c("6849178A",
#       "6851184E",
#       "5277619S","6852899A","6855137S",
#       "6857152G","6860416Y","6935380E","6950251Z","6956232H","6968816C","5277619S")
#d %in% clabna2$mrn 

#all true here 
# some consultants prefer apttr?


## 1.4. Appending 'clab' with columns - axarange and apttrrange for whether axa apttr by range status ----
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



## 1.5. Appending 'clab' with day on ecmo 'ecmo_day' column  ----

#let's create a day on ecmo column to append to each row.

clab <- clab %>% 
        group_by(mrn) %>% 
        mutate(ecmo_day = round(difftime(dtm,datecannulated,units = "days")),ecmo_min = dtm - datecannulated ) %>%
        ungroup()

clab$ecmo_day <- as.factor(clab$ecmo_day)


## 1.6. Appending 'clab' with column 'g' to designate monitoring group -----

#lets think about assigning group.

# Several ways of assigning groups 



### 1.6.a. Using a simple definition ----

#axa monitoring was established on 1 Nov 2019. Thus any date before should be apttr and those after would be axa. 

clab$g1 <- ifelse(clab$datecannulated < as.Date("2019-11-01"),"apt","axa")
clab$g1 <- as.factor(clab$g1)

#clab %>% count(g1,mrn) %>% summary() 
# this showed that there is 72 people apt and 129 in axa groups.

### 1.6.b. Using a sophisticated definition -----

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


## 1.7.a. Creating a new df 'dfg1p' that measure time to first in range for monitoring group 'g1' ----

# Now let's count the first time axa/apt get in range using g1 definitions.
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

#dfg1m is 35, dfg1 is 166 = sum is 201.

#That is resolved ! :) 

#let's calculate the time to first in range.
dfg1$tfirst <- dfg1$dtm - dfg1$datecannulated
dfg1$tfirst <- as.numeric(dfg1$tfirst)
#now let's assign 'event as yes' because they all achieved 'event.

#tfirst for dfg1m should be ecmorunt and event should be no as they NEVER achieve it.
#let's create a sepearte df. 
#dfg1p for processed. 

dfg1p  <- dfg1 %>% select(mrn,ecmorunt,cohort,datecannulated,g1,tfirst)
dfg1p$event <- "yes"

dfg1m$event <- "no"
dfg1m$tfirst <- as.numeric(dfg1m$ecmorunt)


dfg1p <- rbind(dfg1p,dfg1m)
dfg1p$event <- as.factor(dfg1p$event)


### 1.7.b. Creating a new df 'dfg2p' that measures ttfr for monitoring group 'g2' -----

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

#calculating tfirst
dfg2$tfirst <- dfg2$dtm - dfg2$datecannulated
dfg2$tfirst <- as.numeric(dfg2$tfirst)

dfg2p <- dfg2 %>% select(mrn,ecmorunt,cohort,datecannulated,g2,tfirst)
dfg2p$event <- "yes"

dfg2m$event <- "no"
dfg2m$tfirst <- as.numeric(dfg2m$ecmorunt)

dfg2p <- rbind(dfg2p,dfg2m)
dfg2p$event <- as.factor(dfg2p$event)



## Now Step 2 - we are going to address heparin prescriptions in hep df.
#Note that all the heparin prescription absences take into account stopped prescriptions.
# there is a meaning behind missing data.

# 2.0. Creating a new clean 'chep' df -----


# 2.1. First assigning a groups g1 and g2 as per previous ----
#Likewise we will subset heparin prescriptions with ecmoruntimes.
#unlike blood tests prescriptions should be more or less the same as cannulation and decan.
hep <- full_join(
        hep,
        pt %>% select(mrn,cohort,datecannulated,dateDEcannulated,ecmorunt),
        by = "mrn"
)
#we should also append g1 and g2 to hep

hep$g1 <- ifelse(hep$datecannulated < as.Date("2019-11-01"),"apt","axa")
hep$g1 <- as.factor(hep$g1)
#simpler group g1 assignment

## g2 assignment

hep$g2 <- ifelse(hep$datecannulated < as.Date ("2019-11-06"),"apt","axa")
# using reassign1 vector from earlier. 

hep[hep$mrn %in% reassign1,]$g2 <- "apt"
hep$g2 <- as.factor(hep$g2)

#now subset into clean dataframe.
chep <- hep %>% 
        group_by(mrn) %>%
        filter(chart_t >= datecannulated & chart_t <= dateDEcannulated) %>%
        ungroup()


### 2.2. Adding patients who never received heparin ---- 
#lets add patients who never receive hepearin.
nh <- setdiff(unique(pt$mrn),unique(chep$mrn))
heparinever <- sprintf("never",1:16)

nh_df <- data.frame(nh,heparinever)
names(nh_df) <- c("mrn","heparinever")
#nh %in% chep is all false so it checked out.

chep$heparinever <- "yes"
chep <- full_join(
        chep,
        nh_df,
        by = "mrn"
        
)

chep$heparinever <- paste(chep$heparinever.x,chep$heparinever.y)
#paste the two columns
#then delete the .x and .y's

chep <- chep %>% select(-c(heparinever.x,heparinever.y))

chep$heparinever <- as.factor(chep$heparinever)
levels(chep$heparinever) <- c("never","yes")

#this way all patients who never received heparin are appended.

# neeed to explore duplicated data.
sum(duplicated(chep)) 
#showed that there is 21 duplicated in chep.

#there is a vast diff between chep and hep. why?

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

#### 3.0. exploring complications from 'bl' df. ----

#now we will explore the explicit missing data.

### 3.1. we should try and add some group info to bl. ----

bl$g1 <- ifelse(bl$datecannulated < as.Date("2019-11-01"),"apt","axa")
bl$g1 <- as.factor(bl$g1)
##treatment for g1


bl$g2 <- ifelse(bl$datecannulated < as.Date ("2019-11-06"),"apt","axa")
# using reassign1 vector from earlier. 

bl[bl$mrn %in% reassign1,]$g2 <- "apt"
bl$g2 <- as.factor(bl$g2)


### 3.2. exploring NA values.----

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
