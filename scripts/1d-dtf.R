

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


# 2.0. SOURCE ------------------------------------------------------------------
## 2.1. DATA ---------------------------------------------------------------
source("scripts/3-outcomes.R")
source("scripts/1c-clean.R")
#data sourcing 3-outcomes including circuit change information
#1c-clean which includes clean df on features.

## 2.2. DEPENDENCIES & LIBRARIES -----------------------------------------------
library(tidyverse)
library(ggplot2)


## 2.3. CUSTOM FUNCTIONS  ---------------------------------------------------
source("scripts/ttr_rose.R")
#custom rosenthaal calculation function
#this function requires data in the format
#time interval, value 1, value 2 to work.

#custom function 1. this is basically to append ecmo finish and start time in corect
#formats to each data frames
wr <- function (x,dfcore) {
        #func to append ecmo start finish columns to each dataframes
        x <- left_join(
                x,
                dfcore %>% select(mrn,ecmo_start,ecmo_finish,group,ecmod),
                by = "mrn"
        )
        x<- x %>% 
                group_by(mrn) %>% 
                filter(chart_t >= ecmo_start & chart_t <= ecmo_finish)%>%
                ungroup()
        
        x$chart_t2 <- as.Date(x$chart_t,format = "%Y-%m-%d")
        x$es2 <- as.Date(x$ecmo_start, format = "%Y-%m-%d")
        
        x$nd <- round(difftime(x$chart_t2,x$es2,units = "days"))
        x$nd <- as.numeric(x$nd)
        #makes it numeric to make it easier then lets add 1 to prevent issue of day 0 on ecmo
        x$nd <- x$nd + 1 
        
        return(x)
}

#2nd custom function
#this is basically function add begin and end times in blood tests
wo <- function(x){
        #function to add begin and end times in blood tests
        x <- x %>%
                add_row(
                        mrn = sample(.$mrn,size =1),
                        chart_t = sample(.$ecmo_start,size=1),
                        group = sample(.$group,size=1)
                ) %>% 
                add_row(
                        mrn = sample(.$mrn,size = 1),
                        chart_t = sample(.$ecmo_finish,size=1),
                        group = sample(.$group,size=1)
                )%>%
                arrange(chart_t)
        return(x)
}

#3rd custom function
#function to essentially lag the blood results and times so can make a interval


lwg <- function (x){
        #function to add lagged values both times and blood,
        #need to select axa or apt
        
        if ("axa" %in% names(x)){
                x <- x %>%
                        select(mrn,chart_t,axa,group) %>%
                        mutate(
                                t2=lag(chart_t),
                                v2 = lag(axa)
                        ) %>%
                        select(mrn,chart_t,t2,axa,v2,group)
                return(x)
        } else {
                x <- x %>%
                        select(mrn,chart_t,apttr,group) %>%
                        mutate(
                                t2=lag(chart_t),
                                v2 = lag(apttr)
                        ) %>%
                        select(mrn,chart_t,t2,apttr,v2,group)
                return(x)
        }
        
}

#4th custom function
#function to calculate time intervals in hours 
mwt <- function (x){
        #fuction calculates time intervals in hours
        x<- x %>% 
                mutate(ivethr = as.numeric(difftime(t2,chart_t,units = "hours")))
        x$ivethr <- x$ivethr * -1
        #this line converts secs to hours
        return(x)
}

#5th custom function
#lets apply the ttr function 

rap <- function (a){
        #function to apply ttrose function
        #need to select axa or apt
        
        if ("axa" %in% names(a)){
                l = 0.2999
                u = 0.7001
                a$ttrose <- ttrcalc(lower = l,upper = u, x=a$axa,y=a$v2)
                return(a)
        } else {
                l = 1.49999
                u = 2.00001
                a$ttrose <- ttrcalc(lower = l, upper = u, x=a$apttr,y=a$v2)
                return(a)
        }
        
}

#6th custom function
#here the goal is to generate a summary value for each patient.
edd <- function (a){
        mrn <- sample(a$mrn,size=1)
        group <- sample(a$group,size =1)
        totalhr <- sum(a$ivethr,na.rm=TRUE)
        tlow <- sum(a[which(a$ttrose == "low"),"ivethr"])
        thi <- sum(a[which(a$ttrose == "high"),"ivethr"])
        
        a$ttrn<- as.numeric(a$ttrose)
        a$it <- a$ttrn * a$ivethr
        sumttr <- sum(a$it,na.rm=TRUE)
        ttrg <- sum(a$it,na.rm = TRUE)/sum(a$ivethr,na.rm = TRUE)
        
        df <- data.frame(mrn,group,totalhr,tlow,thi,sumttr,ttrg)
        return(df)
}

#7th custom function
#fihn's method

fnm <- function(a){
        #this is a function to calculate variability using Fihn's method
        #variability is a summary feature, so we can ignore individual NA's
        
        if ("axa" %in% names(a)){
                z = 0.4 #mid point of axa target
                a <- a %>% 
                        select(mrn,axa,group,ivethr) %>%
                        drop_na(axa) %>%
                        filter(ivethr>0) %>%
                        mutate(
                                temp1 = (axa - 0.4)^2,
                                temp2 = temp1 / ivethr
                        )
                mrn <- sample(a$mrn,size=1)
                group <- sample(a$group,size =1)
                sigm <- (sum(a$temp2))/(dim(a)[1])
                df <- data.frame(mrn,group,sigm)
                return(df)
                
        } else {
                z = 1.75 #midpoint of apttr target
                a <- a %>%
                        select(mrn,apttr,group,ivethr) %>%
                        drop_na(apttr)%>%
                        filter(ivethr > 0) %>%
                        mutate(
                                temp1 = (apttr - 1.75)^2 ,
                                temp2 = temp1 / ivethr
                        )
                
                mrn <- sample(a$mrn,size=1)
                group <- sample(a$group,size =1)
                sigm <- (sum(a$temp2))/(dim(a)[1])
                df <- data.frame(mrn,group,sigm)
                return(df)
                
        }
        
        
        #a <- a[!is.na(a$y),]
        #we will filter time interval 0 or NA in values
      
        #a$temp1 <- (a$y - z)^2 
        #a$temp2 <- a$temp1 / ivethr
        
        #mrn <- sample(a$mrn,size=1)
        #group <- sample(a$group,size =1)
        #sigm <- (sum(a$temp2))/(dim(a)[1])
        #df <- data.frame(mrn,group,sigm)
        #return(df)
     
        #
}


hti <- function (x){
        #function to add lagged values for heparin
        
        
        x <- x %>%
                 select(mrn,chart_t,s_label,t_form,u,wkg,dose,group) %>%
                 mutate(t2=lag(chart_t)) %>%
                 mutate(tinv = as.numeric(difftime(t2,chart_t,units = "hours")))%>%
                 mutate(tinv = -1 * tinv) %>%
                 mutate(tidose = tinv * dose)
        
        mrn <- sample(x$mrn,size=1)
        group <- sample(x$group,size =1)
        cudose <- sum(x$tidose,na.rm = TRUE)
        cumtime <- sum(x$tinv,na.rm = TRUE)
        df <- data.frame(mrn,group,cumtime,cudose)
        return(df)
}

hr <- function(x){
        #function to count run length and tell how many changes 
        eg <- rle(x$t_form)
        runl <- length(eg$lengths)
        mrn <- sample(x$mrn,size =1)
        group <- sample(x$group,size =1)
        
        df<- data.frame(mrn,group,runl)
        return(df)
}

blz <- function(x) {
        #function to detect and find missing values for blood vars
        if (sum(x$test %in% blvars) < 18){
                status <- "missing"
                no_m <- (24 - sum(x$test %in% blvars))
                col_m <- setdiff(blvars,x$test)
        } else {
                status <- "no_missing"
                no_m <- 0
                col_m <- "none"
        }
        
        mrn <- sample(x$mrn, size = 1)
        df <- data.frame(mrn,status,no_m,col_m)
        return(df)
}

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

#but needs to see how many missing values are 
d1e <- tbl %>%
        group_by(mrn)%>% #group wise operaeted
        filter(nd < 2) %>%   #select first days of blood
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
        filter(nd < 2) %>%   #select first days of blood
        select_if(function(x) is.character(x)| is.factor(x)| is.numeric(x)) %>%# select dbl + mrn + group
        summarise(across(
                where(is.double),#execute numbers using folloing list offunction, and naming convention
                list(mean = mean,
                     min = min,
                     max = max),
                na.rm = TRUE,
                .names = "{.col}_{.fn}"
        ))



#let's create a 1 st 24 hour blood results dataframe.

## 3.4. DFCOAG ------------------------------------------------------------
tco <- wr(dfcoag,dfcore)

## 3.5. DFHEP ------------------------------------------------------------
thep <- wr(dfhep,dfcore)

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

hpt <- thep %>% 
        filter(s_label == "hep_sysinf" | s_label == "hep_ecmoiv") %>%
        filter(group == "gapt")%>%
        group_by(mrn)%>%
        arrange(chart_t)

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


#now apply custom function to calculate cumulative doses
ahx <- map(hx,hti)
ahpt <- map(hpt,hti)


ahx <- plyr::ldply(ahx,data.frame)
ahx <- as.tibble(ahx)

ahpt <- plyr::ldply(ahpt,data.frame)
ahpt <- as.tibble(ahpt)

dcumhep  <- rbind(ahx,ahpt)


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


## 3.7.  DF results -----------------------------------------------------

tdf <- left_join(
        df,
        dfcore %>% select(mrn,group,ecmod),by = "mrn"
)

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

#first for axa group, dont forget 'arrange by time to work
tlix <- tco %>% 
        select(mrn,chart_t,ecmo_start,ecmo_finish,axa,group)%>%
        filter(group=="gaxa")%>%
        group_by(mrn)%>%
        drop_na(axa)%>%
        arrange(chart_t)%>%
        group_split()

tlip <- tco %>% 
        select(mrn,chart_t,ecmo_start,ecmo_finish,apttr,group)%>%
        filter(group=="gapt")%>%
        group_by(mrn)%>%
        drop_na(apttr)%>%
        arrange(chart_t)%>%
        group_split()

#tlix and tlip data frames for each patient in each group
#lets' apply series of custom functions

tlix <- map(tlix,wo)
#this should now look like there is an added row for start and finish.

tlix <- map(tlix,lwg)
#this should now have a t2,v2 cols

tlix <- map(tlix,mwt)
#this should now have interval calculated

# Applying TTR rose custom function ---------------------------------------

#now apply custom function ttrrose
tlixr <- map(tlix,rap)
#this seems to work.

tlixr <- map(tlixr,edd)
#this works in trying to get a summary df

#now lets' convert back to df
xdf <- plyr::ldply(tlixr,data.frame)
xdf <- as.tibble(xdf)
#now lets apply for tlip

#as per above
tlip <-map(tlip,wo)
tlip <- map(tlip,lwg)
tlip <- map(tlip,mwt)
tlipr <- map(tlip,rap)
tlipr <- map(tlipr,edd)

#what we really care about is individual patient, 

#cross-check - all the ivethr should add up to the same.

#so now we will convert these list items to dataframe. 
pdf <- plyr::ldply(tlipr,data.frame)
pdf<-as.tibble(pdf)

#4.3.2. FINAL engineered TTRrose ------------------------------------------------

dttr  <- rbind(xdf,pdf)

# 4.4. Fihn's method A variability ------------------------------------------
# applying Fihn's --------------------------------------------------

tf1 <- tco %>% 
        select(mrn,chart_t,ecmo_start,ecmo_finish,axa,group)%>%
        filter(group=="gaxa")%>%
        group_by(mrn)%>%
        drop_na(axa)%>%
        arrange(chart_t)%>%
        group_split()

tf2 <- tco %>% 
        select(mrn,chart_t,ecmo_start,ecmo_finish,apttr,group)%>%
        filter(group=="gapt")%>%
        group_by(mrn)%>%
        drop_na(apttr)%>%
        arrange(chart_t)%>%
        group_split()

#apply custom function wo to add begin and end times.
tf1 <- map(tf1,wo)
tf2 <- map(tf2,wo)

#apply custom function lwg
tf1 <- map(tf1,lwg)
tf2 <- map(tf2,lwg)

#apply custom func mwt
tf1 <- map(tf1,mwt)
tf2 <- map(tf2,mwt)

#apply custom function fnm
tf1 <- map(tf1,fnm)
tf2 <- map(tf2,fnm)

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

dsig  <- rbind(tf1,tf2)

#export this to write.csv

l = setdiff(ls(),lsf.str())
frm <- c(
        "ahpt",
        "ahx",
        "bhpt",
        "bhx",
        "colfactors",
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

l = l[!l %in% frm]

#these are just names- not actual dataframe.
# so we need to use get() to obtain.

#to <- lapply(l,get)
message("SUCCCESS")
save(list = l,file = "data/clean/out.RData")
