

# 1.0. RATIONALE ---------------------------------------------------------------

#Exploring differences in blood product requirements between axa and apttr groups
#Assumptions 1: blood products given in "units". Thus, units treated as "integers"

#Assumptions 2: decision for blood products usually made slowly(i.e., time-insensitive) OR
#rapidly "time-sensitive". 
#"Time-sensitive" are likely to be major haemorrhage OR mass transfusions 
#"Time-insensitive" means that decision for transfusion and physical transfusion
#is enacted much slower. 
#Therefore "a calendar day" is decided as unit of time.
#This is due to unnecessary complication of rules for exact 24 hour from blood product
#and the blood test values before the 24 hour mark or say at 6 hour mark. 
#we will do further testing to declare fitness of this.

# 2.0. SOURCE ------------------------------------------------------------------
## 2.1. DATA ---------------------------------------------------------------
source("scripts/3-outcomes.R")
source("scripts/1c-clean.R")
#data sourcing 3-outcomes including circuit change information
#1c-clean which includes clean df on features.

## 2.2. DEPENDENCIES ----------------------------------------------------
library(tidyverse)
library(ggplot2)


## 2.3. CUSTOM FUNCTION wr ---------------------------------------------------
source("scripts/ttr_rose.R")
#custom rosenthaal calculation function
#this function requires data in the format
#time interval, value 1, value 2 to work.

wr <- function (x,dfcore) {
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


## 3.3. DFBL --------------------------------------------------------------
#now we need to wrangle "dfbl" into similar using custom func 
tbl <- wr(dfbl,dfcore)
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

### 3.5.2.  Feature Engineering ---------------------------------------------

thep <- thep %>%
        group_by(mrn) %>%
        arrange(chart_t)%>%
        mutate( tdiff = difftime(chart_t,lag(chart_t),units = "hours")) %>%
        mutate(tdiffnum = as.numeric(tdiff)) %>%
        ungroup()
#this generates time differences between each prescription.
#perhaps we need mutate if here ! because you only want that for hep ecmo ones 

thep <- thep %>% 
        group_by(mrn) %>% 
        mutate(
                dose = 
                        case_when(
                               s_label == "hep_ecmoiv" ~ t_form * wkg,
                               s_label == "hep_sysinf" ~ t_form
                        )
        )
#now we have a thep with dose.   

thep <- thep %>%
        group_by(mrn) %>%
        mutate(tdose = tdiffnum * dose)%>% 
        ungroup()

dhep <- thep %>% 
        mutate(ecmod = as.numeric(ecmod))%>% 
        group_by(mrn)%>%
        summarise(
                cumdose = sum(tdose,na.rm = T),
                
        )
   
dhep <- left_join(
        dhep,
        dfcore %>% select(mrn,ecmod,group,wkg),
        by = "mrn"
)     

dhep$ecmod <- as.numeric(dhep$ecmod)
dhep$dosepd <- dhep$cumdose/dhep$ecmod
dhep$wdosepd <- dhep$dosepd/dhep$wkg
## 3.6. DF for model ------------------------------------------------------

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

# 4.0. DEMOs -------------------------------------------------------------
## 4.1. Products transfused table -----------------------------------------

#let's demonstrate that there are differences in blood products tx using simple 
# table - labelled as "pt1", ctotcryo = cumulative total cryo and so on 

#Assumption : each row is for 1 unit of blood products ONLY.

sum(tf$t_form < 1 ,na.rm=T)
sum(tf$t_form > 1, na.rm = T)
#assumption passed 


pt1 <- tf %>%
        select(mrn,group,s_label,nd) %>%
        group_by(group) %>% 
        summarise(
                ctotcryo = sum(s_label == "cryo"),
                ctotffp = sum(s_label == "ffp"),
                ctotplt = sum(s_label == "plt"),
                ctotrbc = sum(s_label == "prbcs")
        )
        
#lets perform statistical test to confirm pt1 
#chisq test due to proportions in group.
chisq.test(xtabs(~group + s_label,data = tf))
#lets test specifically for rbc by preparing a new datframe "t1" for testing1
t1 <- tf %>% select(group,s_label) %>% filter(s_label == "prbcs")
#lets check this t1 and it checked out with findings from pt1
summary(t1) 

### Statistical Test ------------------------------------------------
#note chi sq doesnt like small things 
test1 <- chisq.test(table(t1$s_label == "prbcs",t1$group))

## 4.2. ADJUSTED products table -------------------------------------------

#We need to adjust as numbers and duration on ecmo VASTLY differ in the two group.

tf$ecmod <- as.numeric(tf$ecmod)

pt2 <- tf %>% 
        select(mrn,s_label,group,ecmod)%>% 
        group_by(mrn) %>% 
        #add all the blood products transfused for each type per patient
        summarise (
                cryopp = sum(s_label == "cryo"),
                ffppp = sum(s_label == "ffp"),
                pltpp = sum(s_label == "plt"),
                rbcpp = sum(s_label == "prbcs")
        ) %>% 
        ungroup()

pt2 <- left_join(
        pt2,
        dfcore %>% select(mrn,ecmod,group),
        by = "mrn"
)

pt2$ecmod <- as.numeric(pt2$ecmod)

        #devide this by the duratin of each patient ecmo run but to make small
        #no visible, we multiple by 1000
pt2 <- pt2 %>% 
        group_by(mrn) %>% 
        transmute(
                cryopp = (cryopp / ecmod)*1000,
                ffppp = (ffppp / ecmod)*1000,
                pltpp = (pltpp / ecmod)*1000,
                rbcpp = (rbcpp / ecmod)*1000
        ) %>% 
        ungroup () 

pt2 <- left_join (
        pt2,
        dfcore %>% select(mrn,group),
        by = "mrn"
)
        

pt2 %>% group_by(group) %>% group_map(~summary(.x))

#lets check this by groupwise 
sum(pt2$rbcpp[pt2$group == "gapt"])/63
#essentially summing all the rbcpp for group apt and dividing by n.
#it checks out


### Statistical Test  -----------------------------------------------
test2 <- wilcox.test(rbcpp ~ group, data = pt2)


## 4.3. UNIT "TIME" ---------------------------------------------------------

#Is it justifiable to use "calendar day" as a unit of time?
pt3 <- dg %>% 
        group_by(mrn) %>% 
        summarise(
                maxd = max(ecmod),
                dtx = sum(totall != 0)
        ) %>% 
        mutate(prpn = dtx/maxd)

quantile(pt3$prpn)
#this showed that over 75% doesnt receive daily blood products. 
#thus is okay to use calendar day as a unit of time.




## 4.4.  COMPLICATIONS  -------------------------------------------------

#lets look at if complications are the same between groups. 
br <- tdf %>% 
        select(mrn,group,tot,tot_circhange,ecmod)%>% 
        mutate( ecmod = as.numeric(ecmod)) %>% 
        mutate(tot = as.numeric(tot)) %>%
        mutate(tot_circhange = as.numeric(tot_circhange)) %>%
        mutate(across(where(is.numeric),~replace_na(.x,0))) %>% 
        group_by(mrn)%>%
        mutate(
                bter = tot / ecmod,
                ccr = tot_circhange / ecmod
        ) %>% 
        ungroup()
        
xtabs(~tot+group , data = tdf)
#this code showed that there are 
br %>% group_by(group) %>% summarise(mean(bter),median(bter),mean(ccr),median(ccr))
# 5.0. MODEL 1 --------------------------------------------------------------


#however Var seems to be greater than Mean in where 0 are counted and not counted
#likely OVERDISPERSION 
m1 <- glm(totall ~ group, data =dg, family = poisson(link = "log"))
m2 <- glm(totall ~ group, data =dg, family = quasipoisson(link = "log"))
#univariate analysis looks promising.

#lets try a logistic regression to see if it stands.
dm <- dg
dm$y <- ifelse(dm$totall == 0 , 0,1)
dm <- left_join(
        dm,
        dfcore %>% select(mrn,age,surv_ecmo,wkg,apache),by="mrn"
)

m3 <- glm (y ~ group + age + surv_ecmo + wkg + apache + group*surv_ecmo, data = dm, family = binomial)


#lets also see if this remains true for RBCs 

m4 <- glm(totrbc ~ group, data = dg, family = poisson(link="log"))
#looking good so far 

#KEY ASSUMPTIONS OF MODELS
#independence - a bit dififcult

#checking overdispersion
#residual deviance is great than degrees of freedom then overdispersion exists

# Multivariate regression----

#lets' look at dfcore variables such as ethnic, weight, gender, age, apache
# lets look at dfbl variables such as hb, min, max, mean, plt min max mean, lactate
#crp min max mean
#also as admission variable and also as a delta ! 
dt <- left_join(dg, dfcore %>% select(mrn,age,ethnic,apache,wkg,sex), by = "mrn")
m5 <- glm(totall ~ group + age + ethnic + apache + wkg + sex, data =dt, family = poisson(link = "log"))
#note ethnic is problematic as too many factor levels.
m5mod <- glm(totall ~ group + age + apache + wkg + sex + ecmod + ecmod*group, data =dt, family = poisson(link = "log"))


#let's look at "rate" 
dgm <- dg %>% 
        select(mrn,ecmod,totall,group)%>% 
        group_by(mrn) %>% 
        summarise(maxecmod = max(ecmod),sumtx = sum(totall))

dgmt <- left_join(
        dgm,
        dfcore %>% select(mrn,age,ethnic,apache,sex,group,wkg),
        by = "mrn"
)

dgmt$rate <- log(dgmt$maxecmod)

#trial of univariate "rate" pois regression 
m6 <- glm(sumtx ~ group + age + offset(rate),family = poisson(link="log"),data = dgmt)
m7 <- glm(sumtx ~ group + age + apache + wkg + sex + offset(rate),family = poisson(link="log"),data = dgmt)

# 5.1. RANDOM FOREST ----


# 6.0. Quality Markers ----------------------------------------------------

#Quality markers are defined as :-
# time to first threapeutic range
# time in therapeutic range but chosen for - time "above"
# both individually independently and as a feature
# percent time in TTR as well as proportion in range
# INR variability using Fihn method 
# how about scaling and centering as per 


# 6.1.  data prep  --------------------------------------------------------
#to feed into ttrcalc function, we need for each patient,
#col1 = time interval
#col2 = v1 , value 1 at begining of time interval
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

#lets try reshaping with sample df. 
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

#Lets calculate Fihn's style variability 
#make a new col
samp$a = (samp$axa - 0.5)^2
samp$b = samp$a / samp$ivet
sum(samp$b,na.rm=TRUE)
#this looks legit on review but needs to work on rmoving 0's

#steps to do
# 1. time need to be sorted. so chart_t needs to be desc. 
#####



# 6.2. 1.  TTR traditional OR proportion ----------------------------------

#in this approach, we count number of blood tests as denominator and numerator
#numerator = no of blood tests in range. 
#using targets of 0.3 to 0.7 for axa
#using targets of 1.5 to 2.0 for apttr 

#to ensure fair comparison, we should work out the person time in each group.
dfcore %>% 
        group_by(group)%>% 
        summarise(n= n(),persondays = sum(ecmod))
#this showed the person days
message("above showed person days")

tco %>% 
        filter(group == "gapt") %>% 
        select(apttr)%>%
        summarise(
                total_apttr_tests=sum(!is.na(apttr)),
                total_above_range= sum(apttr>2.0,na.rm=TRUE),
                total_in_range = sum(apttr>=1.5 & apttr <= 2.0,na.rm=TRUE)
                )
#this showeed for APPTTR group , raw or traditional TTR values at aggregate level 
tco %>% 
        filter(group == "gaxa") %>% 
        select(axa)%>%
        summarise(
                total_axa_tests=sum(!is.na(axa)),
                total_above_range= sum(axa>0.7,na.rm=TRUE),
                total_in_range = sum(axa>=0.3 & axa <= 0.7,na.rm=TRUE)
        )
#this showeed for AXA group , raw or traditional TTR values at aggregate level 


# 6.2.2. Statistical Tests for traditional TTR ----------------------------

tco %>% 
        select(mrn,group,axa)%>% 
        filter(group=="gaxa")%>%
        drop_na()%>%
        group_by(mrn)%>%
        summarise(
                no_tests_per_pts = n(),
                no_tests_above_range = sum(axa>0.7,na.rm=TRUE),
                no_tests_in_range = sum(axa>=0.3 & axa <= 0.7,na.rm=TRUE)
        ) -> gaxatrad

gaxatrad$g <- 'gaxa'
gaxatrad$prop <- gaxatrad$no_tests_in_range/gaxatrad$no_tests_per_pts

tco %>% 
        select(mrn,apttr,group)%>% 
        filter(group=="gapt")%>%
        drop_na()%>%
        group_by(mrn)%>%
        summarise(
                no_tests_per_pts = n(),
                no_tests_above_range = sum(apttr>2.0,na.rm=TRUE),
                no_tests_in_range = sum(apttr>=1.5 & apttr <= 2.0,na.rm=TRUE)
        ) -> gapttrad

gapttrad$g <- "gapt"
gapttrad$prop <- gapttrad$no_tests_in_range/gapttrad$no_tests_per_pts

pl_ttr_trad <- rbind(
        gaxatrad %>% select(g,prop),
        gapttrad %>% select(g,prop)
)

pl_ttr_trad$g <- as.factor(pl_ttr_trad$g)

#undertake Statistical test
wilcox.test(prop~ g , data = pl_ttr_trad)
t.test(prop~g,data = pl_ttr_trad)

#ggplot code
ggplot(data = pl_ttr_trad, aes(x=g,y=prop,group = g)) + geom_boxplot()

#ISSUES
#- need to review the above some patients have very high and very low TTR 

#- need to review why there is a drop off of a few patients on gaxa
#- need to have a globally set ggplot2 theme
        
        

#model the day on ecmo - weak interaction 

