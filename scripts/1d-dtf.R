

# 1.0. RATIONALE ---------------------------------------------------------------

#This file is for feature engineering.
#tick marked if feature engineered. 
# [/] - df as -"tdf" is a circuit change and complications 
# [/] - df2 is a tidy long form of complications.
# [/] - df3 -- df3 is a data frame of count of complications
# [/] - df4 -- this is data frame of intracranial bleed
# [/] - df5 - this is data frame of hemorrhagic complications
# [/] - dfbl --  blood investigation values
# [/] - tbl -- blood investigations values clean 
# [/] - dfcoag -- axa and apttr values
        # - dttr - pdf and xdf as data frames with TTR rose applied
# [/] - dfcore - main dataframe demographics 
# [ ] - dfhep -- heparin prescriptions
# [/] - dfhydrocortinf as "thinf"
# [/] - dfprd as "tf"  blood products transfused
# [/] - dg1 is a blood product transfused dataframe ready for model
# [/] - tf is a blood products per day on ecmo
# [/] - dfrx -"trx" -- other medications
# [/] - dftxa - as "ttxainf" - tranexemic acid infusions 
# [ ] - dataframe for survival analysis AND relevant feature engineernig for dttg

# 2.0. SOURCE ------------------------------------------------------------------
## 2.1. DATA ---------------------------------------------------------------
source("scripts/3-outcomes.R")
source("scripts/1c-clean.R")
#data sourcing 3-outcomes including circuit change information
#1c-clean which includes clean df on features.

## 2.2. DEPENDENCIES & LIBRARIES & FUNCTIONS -----------------------------------------------
library(tidyverse)
library(ggplot2)

source("scripts/ttr_rose.R")
source("scripts/utils.R")


# 3.0. DATA MANIPULATION -------------------------------------------------------

## 3.1. DFCORE ------------------------------------------------------------

#wrangle dfcore to append some group and ecmod 

#since we are using "absolute calendar days"

#duration on ecmo in hours
#column ecmoh = hours on ecmo
dfcore$ecmoh <- round(difftime(dfcore$ecmo_finish,dfcore$ecmo_start),1)

#duration on ecmo in days
#column ecmod = days on ecmo
dfcore$ecmod <- round(difftime(dfcore$ecmo_finish,dfcore$ecmo_start,units = "days"))

#assigning group 
dfcore$group <- ifelse(dfcore$date_can < as.Date("2019-12-01"),"gapt","gaxa")
dfcore$group <- as.factor(dfcore$group)

dfcore$bmi <- dfcore$wkg / (dfcore$hcm/100)^2
## 3.2. DFPRD --------------------------------------------------------------------
tf <- left_join(
        dfprd,
        dfcore %>% select(mrn,ecmo_start,ecmo_finish,group,ecmod),
        by = "mrn"
)

tf <- tf %>% 
        group_by(mrn) %>%
        filter(chart_t >= ecmo_start & chart_t <=ecmo_finish) %>%
        ungroup()

#make a new dataframe from blood products, "tf" for transfused 
#try and label blood products transfused as 'n-th' day on ecmo column "nd"
tf$chart_t2 <- as.Date(tf$chart_t, format = "%Y-%m-%d")
tf$es2 <- as.Date(tf$ecmo_start, format = "%Y-%m-%d")

tf$nd <- round(difftime(tf$chart_t2,tf$es2,units = "days"))
tf$nd <- as.numeric(tf$nd)
#makes it numeric to make it easier then lets add 1 to prevent issue of day 0 on ecmo
tf$nd <- tf$nd + 1 

tf <- tf %>% select(-c(chart_t2,es2))

## 3.3. DFBL --------------------------------------------------------------
#now we need to wrangle "dfbl" into similar using custom func 
tbl <- wr(dfbl,dfcore)

#blood values 
blvars <- c(
        "hb",
        "plt",
        "neut",
        "fib",
        "ldh",
        "ferritin",
        "ck",
        "crp",
        "pct",
        "bili",
        "alb",
        "creat",
        "gfr",
        "ca",
        "corr_ca",
        "bicarb",
        "lactate",
        "ph"
)
#list of interested blood vars

filter_day = 3
#but needs to see how many missing values are 
d1e <- tbl %>%
        group_by(mrn)%>% #group wise operaeted
        filter(nd < filter_day) %>%   #select first days of blood
        select_if(function(x) is.character(x)| is.numeric(x)) %>%
        select(-nd)

#this selects numbers and mrn
d1e <- pivot_longer(
        d1e,
        cols = !mrn,
        names_to = "test",
        values_to = "val",
        values_drop_na = TRUE
        )
#this will drop tests that dont have any result.

summary(d1e)
#this shows there is no missing values here.
#for each mrn see if they have hb missing.
#

d1t <- d1e %>% group_by(mrn) %>% group_split()
#group split

#custom function blz
d1t <- map(d1t,blz)

d1t <- plyr::ldply(d1t,data.frame)
d1t <- as.tibble(d1t)
        
d1bl <- tbl %>%
        group_by(mrn)%>% #group wise operaeted
        filter(nd < filter_day) %>%   #select first days of blood
        select_if(function(x) is.character(x)| is.factor(x)| is.numeric(x)) %>%# select dbl + mrn + group
        summarise(across(
                where(is.double),#execute numbers using folloing list offunction, and naming convention
                list(median = median,
                     min = min,
                     max = max),
                na.rm = TRUE,
                .names = "{.col}_{.fn}"
        ))



#let's create a 1 st 24 hour blood results dataframe.

## 3.4. DFCOAG ------------------------------------------------------------


tco <- wr(dfcoag,dfcore)
#there is a lot of extreme outlier values
#variabilities are individually examined.
#6580709X 2019-01-04 23:48:00  NA  39.4 gapt
# 6584394E 2019-02-06 12:21:00  NA  60.0
# 6584394E 2019-02-06 20:27:00  NA  59.7
# 6584394E 2019-02-07 10:06:00  NA  66.6
# 6419229C 2019-05-17 12:52:00  NA  41.9

tco <- tco %>% 
        filter(!(mrn == "6580709X" & apttr == 39.4)) %>%
        filter(!(mrn =="6584384E" & apttr == 59.7)) %>%
        filter(!(mrn =="6584384E" & apttr == 60.0)) %>%
        filter(!(mrn =="6584384E" & apttr == 66.6)) %>%
        filter(!(mrn =="6419229C" & apttr == 41.9))

## 3.5. DFHEP ------------------------------------------------------------

pt_without_heparin <- as.data.frame(pt_without_heparin)
names(pt_without_heparin)<-"mrn"

pt_without_heparin <- left_join(
        pt_without_heparin,
        dfcore %>% select(mrn,group),
        by = "mrn"
)
#7 in gaxa and #10 in gapt
#means with 180 in gaxa and 74 in gapt 
#should have 171 in gaxa and 64 in gapt 

thep <- wr(dfhep,dfcore)
#here is 237 (which checks out as 237 + 17 = 254)

#dfhep seems to have multiple prescriptions
summary(thep$s_label)
prop.table(table(thep$s_label))
#this showed that combination of ecmo Xa IV and heparin together covers 99.7%
#we will ignore other levels. 
thep %>% filter(s_label == "hep_ecmoiv" & u != "units/kg/hr")
#this reutnred 0 so checked out
thep %>% filter(s_label == "hep_sysinf" & u != "units/hr")
#this also returned 0 - so it checked out

#looks like all systemic inf has unit as units/hour
#and looks like ecmo xa all has units/kg/hr 

#So, now let's look at hep_ecmo iv with weights 0.
thep %>% filter(s_label == "hep_ecmoiv" & wkg <= 0 )
#this turned empty. so all hep ecmo prescriptions have 'wkg;

#Now let's see if all the wkgs change within the ecmo runs for each patient. 

thep %>% 
        filter(s_label == "hep_ecmoiv") %>%
        group_by(mrn,wkg) %>%
        summarise(freq = n()
        )

wdif <- thep %>% 
        filter(s_label == "hep_ecmoiv") %>%
        group_by(mrn) %>% 
        summarise(
                meanwkg = mean(wkg),
                minwkg = min(wkg),
                medianwkg = median(wkg),
                maxwkg = max(wkg)
        ) %>% 
        mutate(diff = maxwkg - minwkg) %>% 
        ungroup() %>% 
        arrange(desc(diff))

#this showed that some have insane weight changes. e.g., 75, 48, etc
#let's check this with main "dfcore
wdif <- left_join(
        wdif,
        dfcore %>% select(mrn,wkg),
        by = "mrn"
)

#lets see what are the differences with original recoreded weight
wdif$neud <- wdif$wkg - wdif$medianwkg

xtabs(~group+s_label,data = thep)
#this code showed most of hep ecmo iv is in gaxa.

hx <- thep %>% 
        filter(s_label == "hep_sysinf" | s_label == "hep_ecmoiv") %>%
        filter(group == "gaxa")%>%
        group_by(mrn)%>%
        arrange(chart_t)
#here is 173 - and expected is 173. so checked out


hpt <- thep %>% 
        filter(s_label == "hep_sysinf" | s_label == "hep_ecmoiv") %>%
        filter(group == "gapt")%>%
        group_by(mrn)%>%
        arrange(chart_t)
#here is 59 and expected is 64 so 5 lost.
tm1 <- setdiff(dfcore$mrn[dfcore$group=="gapt"],unique(hpt$mrn))
tm2 <- setdiff(tm1,pt_without_heparin$mrn[pt_without_heparin$group == "gapt"])

#this code identified the 5 missing patients.
#this line of code showed patients that are in group apt but not has had heparin.

tm2 %in% unique(hpt$mrn)
#thus checked out that these are the five missing patients
#they are all patients with icb

#so this is where the losses occur. 
thep %>% filter(group == "gapt") %>% 
        group_by(mrn) %>% 
        summarise(
                n = n(),
                p_hep = sum(s_label == "hep")/n,
                p_hep_ecmoiv = sum(s_label == "hep_ecmoiv")/n,
                p_hep_ivbol = sum(s_label == "hep_ivbol")/n,
                p_hep_rrt = sum(s_label == "hep_rrt")/n,
                p_hep_sysinf = sum(s_label == "hep_sysinf")/n
        ) %>%
        arrange(p_hep,p_hep_rrt,p_hep_ivbol)

xtabs(~group+s_label,data= hx)
#this needs sepearte treatment but because all hep_ecmoiv has wkg. 
#we should assign 1kg to all NA values.
hx[["wkg"]][is.na(hx[["wkg"]])]<-1
#this assigns all NA's to 1


xtabs(~group+s_label,data= hpt)
#apttr group is just sysinf so dont need anything special. just need to multiple with tinv



hx <- hx %>% 
        mutate(
                dose = case_when(
                        s_label == "hep_sysinf" ~ t_form,
                        s_label == "hep_ecmoiv" ~ t_form * wkg
                )
        )

hpt <- hpt %>% 
        mutate(
                dose = case_when(
                        s_label == "hep_sysinf" ~ t_form,
                        s_label == "hep_ecmoiv" ~ t_form * wkg
                )
        )

#now split groups

hx <- hx %>% 
        group_by(mrn) %>%
        arrange(chart_t)%>%
        group_split()



hpt <- hpt %>% 
        group_by(mrn) %>%
        arrange(chart_t)%>%
        group_split()

#setdiff(setdiff(dfcore$mrn,dcumhep$mrn),pt_without_heparin)
#there are discrepancy of six patients
#"5764499G" "5970380Q" "5972607Y" "5978426J" "6389948G"

#now apply custom function to calculate cumulative doses
ahx <- map(hx,hti)
ahpt <- map(hpt,hti)


ahx <- plyr::ldply(ahx,data.frame)
ahx <- as.tibble(ahx)

ahpt <- plyr::ldply(ahpt,data.frame)
ahpt <- as.tibble(ahpt)

dcumhep  <- rbind(ahx,ahpt)


#let's calculate per kg per day 

dcumhep <- left_join(
        dcumhep,
        dfcore %>% select(mrn,ecmod,wkg),
        by = "mrn"
)

dcumhep$ecmod <- as.numeric(dcumhep$ecmod)
dcumhep$hep_day<- dcumhep$cudose/dcumhep$ecmod
dcumhep$hep_wkgday <- (dcumhep$cudose/dcumhep$wkg)/dcumhep$ecmod


#next question is how to calculate run_length
#need to make sure same patient dont have ecmoiv and heparin inf together.

### 3.5.2.  Run Length Feature Engineering ---------------------------------------------

bhx <- map(hx,hr)
bhpt <- map(hpt,hr)

bhx <- plyr::ldply(bhx,data.frame)
bhx <- as.tibble(bhx)

bhpt <- plyr::ldply(bhpt,data.frame)
bhpt <- as.tibble(bhpt)

dheprl <- rbind(bhx,bhpt)


#as per above lets work out adjusted for run length

dheprl <- left_join(
        dheprl,
        dfcore %>% select(mrn,ecmod),
        by = "mrn"
)

dheprl$ecmod <- as.numeric(dheprl$ecmod)
dheprl$rl_day <- dheprl$runl/dheprl$ecmod

##make a run length per each day

chx <- map(hx,ht)
chpt <- map(hpt,ht)

chx <- plyr::ldply(chx,data.frame)
chx <- as.tibble(chx)

chpt <- plyr::ldply(chpt,data.frame)
chpt <- as.tibble(chpt)

dgrhep <- rbind(chx,chpt)

dgrhep <- left_join(
        dgrhep,
        dfcore %>% select (mrn,group),
        by = "mrn"
)

#thep <- thep %>%
#        group_by(mrn) %>%
#        arrange(chart_t)%>%
#        mutate( tdiff = difftime(chart_t,lag(chart_t),units = "hours")) %>%
#        mutate(tdiffnum = as.numeric(tdiff)) %>%
#        ungroup()
#this generates time differences between each prescription.
#perhaps we need mutate if here ! because you only want that for hep ecmo ones 

#thep <- thep %>% 
#        group_by(mrn) %>% 
#        mutate(
#                dose = 
#                        case_when(
#                               s_label == "hep_ecmoiv" ~ t_form * wkg,
#                               s_label == "hep_sysinf" ~ t_form
#                        )
#        )
##now we have a thep with dose.   

#thep <- thep %>%
#        group_by(mrn) %>%
#        mutate(tdose = tdiffnum * dose)%>% 
#        ungroup()
#
#dhep <- thep %>% 
#        mutate(ecmod = as.numeric(ecmod))%>% 
#        group_by(mrn)%>%
#        summarise(
#                cumdose = sum(tdose,na.rm = T),
#                
#        )
#   
#dhep <- left_join(
#        dhep,
#        dfcore %>% select(mrn,ecmod,group,wkg),
#        by = "mrn"
#)     
#
#dhep$ecmod <- as.numeric(dhep$ecmod)
#dhep$dosepd <- dhep$cumdose/dhep$ecmod
#dhep$wdosepd <- dhep$dosepd/dhep$wkg
#
### 3.6. blood products dataframe for model -----------------------------------------------

#GOAL : to have a format x vs. y for model purposes

#Lets start with prd > tf 

dg1 <- tf %>% 
        select(mrn,s_label,group,nd) %>%
        group_by(mrn,nd) %>% 
        summarise(
                totrbc = sum(s_label == "prbcs"),
                totffp = sum(s_label == "ffp"),
                totplt = sum(s_label == "plt"),
                totcryo = sum(s_label == "cryo"),
                totall = totrbc + totffp + totplt + totcryo
        ) %>% 
        ungroup()

#so now we have a dataframe dg1 that has no of days on ecmo and blood product transfused.

dg1$mid <- paste(dg1$mrn,dg1$nd, sep = "")

#now this checks out! :) hooray.

#we make a unique column for each mrn and each day
dg <- dfcore %>%
        select(mrn,ecmod) %>%
        mutate(ecmod = as.numeric(ecmod)) %>%
        group_by(mrn) %>%
        uncount(weights = ecmod, .id = "ecmod") %>% 
        ungroup()
#we are now using uncount to get empty rows basically. 
#this also checks out. 

dg$mid <- paste(dg$mrn,dg$ecmod, sep = "")
#same as before we now create a unique mid column and can join ! 

dg <- left_join(
        dg,
        dg1,
        by = "mid"
)
#NOTE ORDER OF THIS LEFT JOIN IS IMPORTANT as it is to generate "NA"

#now need to make a zeros and then join blood products. using replace na etcetc

dg <- dg %>% 
        mutate(across(where(is.numeric),~replace_na(.x,0))) %>% 
        ungroup()

dg <- dg %>% select(-c(mrn.y,mid))
names(dg)[names(dg)=="mrn.x"] <- "mrn"
#rechange name column
rm(dg1)
#remove this unnecessary df. 

#so "nd" and "ecmod" should check out on dg. 
#thus, nd - ecmod should be 0 or less than 0 .
table(dg$nd - dg$ecmod)
#it checked out ! 

#Lets append some group info
dg <- left_join(
        dg,
        dfcore %>% select(mrn,group),
        by = "mrn"
)

dprd <- dg %>% 
        group_by(mrn) %>% 
        summarise(bldtot = sum(totall))


## 3.7.  DF results -----------------------------------------------------

tdf <- left_join(
        df,
        dfcore %>% select(mrn,group,ecmod),by = "mrn"
)
# this line of code selects patient with events 

dkf <- tdf %>% 
        select(mrn,group,admn_ct_results,intct_dtm,comp1_dtm,int_ct_results,other_comp_1,comp_1_quali) %>%
        mutate(tp1 = intct_dtm - comp1_dtm) %>% 
        mutate(tp1 = as.numeric(tp1))

#1st pt int ct 2018-01-20 happened after comp 1 which is on 2018-01-19,thus 
#tp1 is positive, and if tp1 is positive we should take comp1_dtm results
#this line of code selects patient events as whether they have interval scanning or complication before scanning 

dkf <- dkf %>% 
        mutate(et = case_when(
                tp1 > 0 ~ comp1_dtm,
                tp1 < 0 ~ intct_dtm,
                is.na(tp1) & is.na(comp1_dtm) & !is.na(intct_dtm) ~ intct_dtm,
                is.na(tp1) & is.na(comp1_dtm) & is.na(intct_dtm) ~ intct_dtm,
                is.na(tp1) & !is.na(comp1_dtm)& is.na(intct_dtm) ~ comp1_dtm
        ))

dkf <- dkf %>% 
        mutate(ev = case_when(
                #et == comp1_dtm ~ paste(other_comp_1,comp_1_quali,sep = "+"),
                et == intct_dtm ~ int_ct_results
        )) 

dkf$tp2 <- dkf$comp1_dtm == dkf$et
#this line of code is still tidying those 
dkf <- dkf %>% 
        mutate(ev2 = case_when(
                tp2 == TRUE ~ paste(other_comp_1,comp_1_quali,sep = "+")
                
        ))


dkf <- dkf %>%
        select(
                mrn,
                group,
                #tp1,
                admn_ct_results,
                et,
                ev,
                ev2
        )
#now this line of code combines the outcomes into column z 
dkf <- dkf %>% unite("z",ev:ev2,na.rm=TRUE,remove = FALSE)

dkf <- left_join(
        dkf,
        tdf %>% select(mrn,ecmo_dtm,ecmod,ecmo_outcome),
        by = "mrn"
)

dkf <- left_join(
        dkf,
        dfcore %>% select(mrn,ecmoh,ecmo_finish),
        by = "mrn"
)


dkf$z <- as.factor(dkf$z)
levels(dkf$z) <- c(
        "no_comp",#1
        "both",#2
        "haem",#3
        "haem",#4
        "haem",
        "no_comp",#6
        "haem",
        "throm",#8
        "throm",
        "throm",#10
        "throm",
        "throm",#12
        "throm"
)

dkf <- dkf %>%
        mutate( t = 
                        case_when(
                                z == "no_comp" ~ as.numeric(ecmod) * 24,
                                z != "no_comp" ~ as.numeric(et - ecmo_dtm)
                        )
                
        )

dkf <- dkf %>% 
        mutate(
                end = case_when(
                        !is.na(et) ~ et,
                        is.na(et) ~ ecmo_finish
                )
        )
 
###
####### DF for the survival analysis
#let's get ttrg and sigm for the 1st time to bleeding df.

#tx1 <- tco %>% 
#        select(mrn,chart_t,ecmo_start,ecmo_finish,axa,apttr,group)
##        mutate(teb = difftime(chart_t,ecmo_start,units = "hours"))%>%
##        mutate(teb = as.numeric(teb))


#tx1 <- left_join(
#        tx1,
#        dkf %>% select(mrn,end),
#        by = "mrn"
#)

#tx1 <- tx1 %>%
#        group_by(group) %>%
#        filter(chart_t < end) %>%
#        ungroup()

#ta <-  tx1 %>% 
#        filter(group=="gaxa")%>%
#        group_by(mrn)%>%
#        drop_na(axa)%>%
#        arrange(chart_t)%>%
#        group_split()

#tb <-  tx1 %>% 
#        filter(group=="gapt")%>%
#        group_by(mrn)%>%
#        drop_na(apttr)%>%
#        arrange(chart_t)%>%
#        group_split()

##as per above
#ta <-map(ta,wo)
#ta <- map(ta,lwg)
#ta <- map(ta,mwt)
#tar <- map(ta,rap)
#tar <- map(tar,edd)
#tarf <- plyr::ldply(tar,data.frame)
#tarf <-as.tibble(tarf)
##
##tb <-map(tb,wo)
##tb <- map(tb,lwg)
##tb <- map(tb,mwt)
##tbr <- map(tb,rap)
##tbr <- map(tbr,edd)
##
##tbrf <- plyr::ldply(tbr,data.frame)
##tbrf <-as.tibble(tbrf)
##
##darf <- rbind(tarf,tbrf)
##
###tao <- map(ta,fnm)
##tbo <- map(tb,fnm)
#
##taor <- plyr::ldply(tao,data.frame)
##taor <- as.tibble(taor)
#
#tbor <- plyr::ldply(tbo,data.frame)
#tbor <- as.tibble(tbor)
#
#dorf <- rbind(taor,tbor)
#
#dkf <- left_join(
##        dkf,
#        darf %>% select(mrn,ttrg),
#        by = "mrn"
#)
#
#dkf <- left_join(
#        dkf,
#        dorf %>% select(mrn,sigm),
#        by = "mrn"
#)
#

## 3.8. DFhydrocortinf -----------------------------------------------------

thinf <- wr(dfhydrocortinf,dfcore)


## 3.9. DFtxainf -----------------------------------------------------------

ttxainf <- wr(dftxa,dfcore)


## 3.10. dfrx  -------------------------------------------------------------

summary(dfrx)

#this shows that there are 5846 entries
#out of this this drugs are too infrequently used to be included in model.

#definition of infrequently used = 90% not used.
#if entry is less than 5261 (i.e., 5846 - 10%  then we count as infreq)
drx_lowuse <- names(dfrx)
drx_lowuse <- drx_lowuse[-c(9:16)]
drx_lowuse <- drx_lowuse[-c(1,2,6)]

trx <- dfrx %>%
        select(-all_of(drx_lowuse))

trx <- wr(trx,dfcore)

# 4.0. Complex data transformations -Quality Markers ----------------------------------------------------

#Quality markers are defined as :-
# time to first threapeutic range
# time in therapeutic range but chosen for - time "above"
# both individually independently and as a feature
# percent time in TTR as well as proportion in range
# INR variability using Fihn method 
# how about scaling and centering as per 


## 4.1.  data prep dfcoag  --------------------------------------------------------
#to feed into ttrcalc function, we need for each patient,
#col1 = time interval
#col2 = v1 , value 1 at beginning of time interval
#col3 = v2 , value 2 at the end of time interval
#then we need to set custom function parameters of lower and upper

#we have df "tco" which has 

#first we need to subset for ecmo durations
dim(tco)
#showed there is 12311 rows for 11 columns.
#lets subset ---

tco <- tco %>%
        group_by(mrn) %>%
        filter(chart_t >= ecmo_start & chart_t <= ecmo_finish)%>%
        ungroup()
dim(tco)
#its the same so sounds legit.
#tco %>%group_by(mrn) %>% filter(chart_t >= ecmo_finish)
#tco %>%group_by(mrn) %>% filter(chart_t <= ecmo_start)
#both turned out 0 so it checked out.

### lets try reshaping with sample df. SAMPLE -------
samp <- tco %>% filter(mrn == "1103375H")

#we need to add ecmo_start and finish time as rows

samp <- samp %>% 
        add_row(
                mrn = sample(.$mrn,size =1),
                chart_t = sample(.$ecmo_start,size=1))%>%
        add_row(
                mrn = sample(.$mrn,size =1),
                chart_t = sample(.$ecmo_finish,size = 1)
        )%>%
        arrange(chart_t)

#then we need to "lag"
samp <- samp %>% 
        select(mrn,chart_t,axa,group) %>%
        mutate(t2=lag(chart_t),a2 = lag(axa)) %>% 
        select(mrn,chart_t,t2,axa,a2,group)

samp <- samp %>% mutate(ivet = as.numeric(t2 - chart_t))
samp$ivet <- samp$ivet * -0.0002777778
samp$ttrose <- ttrcalc(lower = 0.2999,upper = 0.7001,x =samp$axa,y =samp$a2)

#TTRrose works

### Lets calculate Fihn's style variability -----

#formula for Fihn's style variability 
#Variance growth rate Fihn(method A)

#lhs = sigma 2 = variance
#$$\sigma^2 = \frac{1}{n}* \sum \limits_{i=1}^{n}\frac {(INR_i - target_i)^2}{\tau_i}$$

#make a new col
samp$a = (samp$axa - 0.5)^2
samp$b = samp$a / samp$ivet
sum(samp$b,na.rm=TRUE)
#this looks legit on review but needs to work on removing 0's

#steps to do
# 1. time need to be sorted. so chart_t needs to be desc.
# 2. remove divide by 0 errors.
# therefore we should remove "ivethr = 0" and "value of blood test = 0"
#####




# 4.3. TTR Rosendaal ------------------------------------------------------


# 4.3.1. Data prep for TTR rose -------------------------------------------

#the issue now is that data frame can be grouped but cannot do add row function
#to a grouped data frame, so lets make a list
pe_axa <- c("8022005E","8050928N","6101323L","8065809T")

#first for axa group, dont forget 'arrange by time to work
tlix <- tco %>% 
        select(mrn,chart_t,ecmo_start,ecmo_finish,axa,group)%>%
        filter(!mrn %in% pe_axa) %>% 
        filter(group=="gaxa")%>%
        group_by(mrn)%>%
        drop_na(axa)%>%
        arrange(chart_t)%>%
        group_split()

#8022005E,
#8050928N
#6101323L
#8065809T
#at admissions
txpe <- tco %>% 
        select(mrn,chart_t,ecmo_start,ecmo_finish,axa,group)%>%
        filter(mrn %in% pe_axa) %>% 
        filter(group=="gaxa")%>%
        group_by(mrn)%>%
        drop_na(axa)%>%
        arrange(chart_t)%>%
        group_split()


#seperate treatment for PE patients 
        
pe_apt <- c("6513466R","6584394E")

tlip <- tco %>% 
        select(mrn,chart_t,ecmo_start,ecmo_finish,apttr,group)%>%
        filter(!mrn %in% pe_apt)%>%
        filter(group=="gapt")%>%
        group_by(mrn)%>%
        drop_na(apttr)%>%
        arrange(chart_t)%>%
        group_split()

trpe <- tco %>% 
        select(mrn,chart_t,ecmo_start,ecmo_finish,apttr,group)%>%
        filter(mrn %in% pe_apt)%>%
        filter(group=="gapt")%>%
        group_by(mrn)%>%
        drop_na(apttr)%>%
        arrange(chart_t)%>%
        group_split()

# 6513466R 
# 6584394E are two patients with PE in gapt
#at admissions

#tlix and tlip data frames for each patient in each group
#lets' apply series of custom functions

tlix <- map(tlix,wo)
txpe <- map(txpe,wo)
#this should now look like there is an added row for start and finish.

tlix <- map(tlix,lwg)
txpe <- map(txpe,lwg)
#this should now have a t2,v2 cols

tlix <- map(tlix,mwt)
txpe <- map(txpe,mwt)
#this should now have interval calculated

# Applying TTR rose custom function ---------------------------------------

#now apply custom function ttrrose
tlixr <- map(tlix,rap)
#this seems to work.
#now apply sepeate custom function for PE patients
txper <- map(txpe,rpe)

tlixr <- map(tlixr,edd)
#this works in trying to get a summary df
txper <- map(txper,edd)

#now lets' convert back to df
xdf <- plyr::ldply(tlixr,data.frame)
xdf <- as.tibble(xdf)
#now lets apply for tlip

xpef <- plyr::ldply(txper,data.frame)
xpef <- as.tibble(xpef)

#as per above
tlip <-map(tlip,wo)
trpe <- map(trpe,wo)

tlip <- map(tlip,lwg)
trpe <- map(trpe,lwg)

tlip <- map(tlip,mwt)
trpe <- map(trpe,mwt)

tlipr <- map(tlip,rap)
tper <- map(trpe,rpe)

tlipr <- map(tlipr,edd)
tper <- map(tper,edd)

#what we really care about is individual patient, 

#cross-check - all the ivethr should add up to the same.

#so now we will convert these list items to dataframe. 
pdf <- plyr::ldply(tlipr,data.frame)
pdf<-as.tibble(pdf)

tprf <- plyr::ldply(tper,data.frame)
tprf <- as.tibble(tprf)

#4.3.2. FINAL engineered TTRrose ------------------------------------------------

dttr  <- rbind(xdf,xpef,pdf,tprf)

#lets' do the same for a different time cut off.

##### different time cut off ####


# 4.4. Fihn's method A variability ------------------------------------------
# applying Fihn's --------------------------------------------------

tf1 <- tco %>% 
        select(mrn,chart_t,ecmo_start,ecmo_finish,axa,group)%>%
        filter(!mrn %in% pe_axa) %>%
        filter(group=="gaxa")%>%
        group_by(mrn)%>%
        drop_na(axa)%>%
        arrange(chart_t)%>%
        group_split()

fxpe <- tco %>% 
        select(mrn,chart_t,ecmo_start,ecmo_finish,axa,group)%>%
        filter(mrn %in% pe_axa) %>% 
        filter(group=="gaxa")%>%
        group_by(mrn)%>%
        drop_na(axa)%>%
        arrange(chart_t)%>%
        group_split()

tf2 <- tco %>% 
        select(mrn,chart_t,ecmo_start,ecmo_finish,apttr,group)%>%
        filter(!mrn %in% pe_apt)%>%
        filter(group=="gapt")%>%
        group_by(mrn)%>%
        drop_na(apttr)%>%
        arrange(chart_t)%>%
        group_split()

frpe <- tco %>% 
        select(mrn,chart_t,ecmo_start,ecmo_finish,apttr,group)%>%
        filter(mrn %in% pe_apt)%>%
        filter(group=="gapt")%>%
        group_by(mrn)%>%
        drop_na(apttr)%>%
        arrange(chart_t)%>%
        group_split()

#apply custom function wo to add begin and end times.
tf1 <- map(tf1,wo)
fxpe <- map(fxpe,wo)
tf2 <- map(tf2,wo)
frpe <- map(frpe,wo)

#apply custom function lwg
tf1 <- map(tf1,lwg)
fxpe <- map(fxpe,lwg)
tf2 <- map(tf2,lwg)
frpe<-map(frpe,lwg)

#apply custom func mwt
tf1 <- map(tf1,mwtv)
fxpe <- map(fxpe,mwtv)
tf2 <- map(tf2,mwtv)
frpe <- map(frpe,mwtv)

#apply custom function fnm
tf1 <- map(tf1,fnm)
fxpe <- map(fxpe,fpe)
tf2 <- map(tf2,fnm)
frpe <- map(frpe,fpe)

#tf1[[14]] is the problem because axa values are all NA's'
#let's explore NA's in each group, for each patient. 
#but NA here is meaningful 

dxna <- tco %>% 
        select(mrn,chart_t,ecmo_start,ecmo_finish,axa,group)%>%
        filter(group=="gaxa")%>%
        group_by(mrn)%>%
        arrange(chart_t)%>%
        summarise(
                n = length(axa),
                n_na = sum(is.na(axa)),
                p = round(n_na/n,digits =2)
        ) %>% 
        arrange(desc(p))

dxap <- tco %>% 
        select(mrn,chart_t,ecmo_start,ecmo_finish,apttr,group)%>%
        filter(group=="gapt")%>%
        group_by(mrn)%>%
        arrange(chart_t)%>%
        summarise(
                n = length(apttr),
                n_na = sum(is.na(apttr)),
                p = round(n_na/n,digits =2)
        ) %>% 
        arrange(desc(p))

#conver these list back to df. 
tf1 <- plyr::ldply(tf1,data.frame)
tf1 <- as.tibble(tf1)

tf2 <- plyr::ldply(tf2,data.frame)
tf2 <- as.tibble(tf2)

fxpe <- plyr::ldply(fxpe,data.frame)
fxpe <- as.tibble(fxpe)

frpe <- plyr::ldply(frpe,data.frame)
frpe <- as.tibble(frpe)

dsig  <- rbind(tf1,fxpe,tf2,frpe)


#####dfci 

dfci <- left_join(
        dfci,
        dfcore %>% select(mrn,group,ecmod,surv_ecmo),
        by = "mrn"
)
dfci$ecmod <- as.numeric(dfci$ecmod)

dfci$cday <- dfci$totc / dfci$ecmod

###




# EXPORTAGE ---------------------------------------------------------------

#export this to Rdata file

l = setdiff(ls(),lsf.str())
frm <- c(
        "ahpt",
        "ahx",
        "bhpt",
        "bhx",
        "colfactors",
        "d1e",
        "edd",
        "eg",
        "hpt",
        "hx",
        "nums4",
        "nums5",
        "pdf",
        "ptid",
        "samp",
        "tf1",
        "tf2",
        "tlip",
        "tlix",
        "tlipr",
        "tlixr",
        "xdf"
        
)

l <- l[!l %in% frm]

#these are just names- not actual dataframe.
# so we need to use get() to obtain.

#to <- lapply(l,get)

save(list = l,file = "data/clean/out.RData")

message("SUCCCESS 1d-dtf")
