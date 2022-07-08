

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
## 2.1.  DATA ---------------------------------------------------------------
source("scripts/3-outcomes.R")
source("scripts/1c-clean.R")
#data sourcing 3-outcomes including circuit change information
#1c-clean which includes clean df on features.

## 2.2.  DEPENDENCIES ----------------------------------------------------
library(tidyverse)
library(ggplot2)


## 2.3.  CUSTOM FUNCTION wr ---------------------------------------------------

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

## 3.1.  DFCORE ------------------------------------------------------------

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


## 3.3.  DFBL --------------------------------------------------------------
#now we need to wrangle "dfbl" into similar using custom func 
tbl <- wr(dfbl,dfcore)
## 3.4.  DFCOAG ------------------------------------------------------------
tco <- wr(dfcoag,dfcore)
## 3.5. DFHEP ------------------------------------------------------------
thep <- wr(dfhep,dfcore)

## 3.6.  DF for model ------------------------------------------------------

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
#we make a unique column for each mrn and each day
dg <- dfcore %>%
        select(mrn,ecmod) %>%
        mutate(ecmod = as.numeric(ecmod)) %>%
        group_by(mrn) %>%
        uncount(weights = ecmod, .id = "ecmod")
#we are now using uncount to get empty rows basically. 

dg$mid <- paste(dg$mrn,dg$ecmod, sep = "")
#same as before we now create a unique mid column and can join ! 

dg <- left_join(
        dg1,
        dg,
        by = "mid"
)

now need to make a zeros and then join blood products. using replace na etcetc

# 4.0.  DEMOs -------------------------------------------------------------
## 4.1.  Products transfused table -----------------------------------------

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

###  Statistical Test ------------------------------------------------
#note chi sq doesnt like small things 
test1 <- chisq.test(table(t1$s_label == "prbcs",t1$group))

## 4.2.  ADJUSTED products table -------------------------------------------

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


###  Statistical Test  -----------------------------------------------
test2 <- wilcox.test(rbcpp ~ group, data = pt2)


#####


#ISSUES


        
        