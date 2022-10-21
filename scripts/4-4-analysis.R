#########################
#SURVIVAL OR EVENT TIME MODELLING
########################


# SOURCE DATA -------------------------------------------------------------
load(file="data/clean/out.RData")
library(tidyverse)
library(survival)
library(survminer)
library(ggsurvfit)
#library(Hmisc)
#library(rms)
#clean data sourced from 1d-dtf


# ANALYSIS PLAN -----------------------------------------------------------


#
#1. Time TO therapeutic range
# 2. Time IN therapeutic range 
#3. less variability 
#4. Frequency of changes
#5. total per person per day dose of heparin ?
#6. Patient outcomes
#7. time to first complication
#8. poisson regressions for blood products
#


# ASSUMPTIONS -------------------------------------------------------------


# 1.0. Using 1st 24 hr lab values --------------------------------------------
dfcore %>% select(ecmod) %>% count(ecmod)
dfcore %>% select(ecmoh) %>% count(ecmoh)
#This code show that there is only 2 patient who are on ecmo for 1 day.
#looking at hours - it is at 18.8 hours and 29.8 hours


# 2.0. WRANGLING ---------------------------------------------------------------

#Features we will use.

# 2.1.  Blood results features -----------------------------------------------------
#Haemoglobin, Platelet, Neutrophils, Fibrinogen, LDH, Ferritin, CK, CRP, PCT,Bilirubin
#Albumin, Creatinine, corrected calcium, bicarbonate, lactate, ph
#16 variables

#d1bl


# 2.2. Anticoagulation Features ------------------------------------------
#globa_ttr,time_to_first_ttr,
#variability fihn, variability standardised

#dttr
#dsig

# 2.3. Heparin related features -------------------------------------------
#cumulative heparin doses
#no of prescription changes

#dcumhep 
#dheprl

# 2.4. Circuit Changes / Complications ?----------------------------------
#dfci 
# 2.5. Prescription related features -------------------------------------


# 2.6. Demographic features -----------------------------------------------

#dfcore

# 2.7. Blood products features  --------------------------------------------

#transfusion in 1st 24 hour, cumulative 
#dg


# 2.8. time as factor vs continuous ---------------------------------------



# 2.9. Final df -----------------------------------------------------------

#add demographic variables
dm <- dfcore %>% 
        select(
                mrn,
                age,
                surv_ecmo,
                surv_icu,
                acute_phys_score,
                age_score,
                chron_health_score,
                apache,
                ethnic,
                sex,
                wkg,
                group,
                ecmod,
                bmi,
                ecmo_start,
                cohort
        )

#254 * 13
dm$ecmos <- as.Date(dm$ecmo_start)

dm <- dm %>% 
        mutate(ecmosb = case_when(
                ecmo_start < as.Date("2015-04-01") ~ "15",
                (as.Date("2015-04-01") < ecmo_start) & (ecmo_start < as.Date("2016-04-01")) ~ "15-16",
                (as.Date("2016-04-01") < ecmo_start) & (ecmo_start < as.Date("2017-04-01")) ~ "16-17",
                (as.Date("2017-04-01") < ecmo_start) & (ecmo_start < as.Date("2018-04-01")) ~ "17-18",
                (as.Date("2018-04-01") < ecmo_start) & (ecmo_start < as.Date("2019-04-01")) ~ "18-19",
                (as.Date("2019-04-01") < ecmo_start) & (ecmo_start < as.Date("2020-04-01")) ~ "19-20",
                (as.Date("2020-04-01") < ecmo_start) & (ecmo_start < as.Date("2021-04-01")) ~ "20-21",
                as.Date("2021-04-01") < ecmo_start ~ "21-22"
        ))

#ecmo is a winter game so treated as winter intervals
dm$ecmosb <- as.factor(dm$ecmosb)

dm$ecmod <- as.numeric(dm$ecmod)

#add blood test variables
dm <- left_join(
        dm,
        d1bl %>% select(-c(nd_mean,nd_min,nd_max)),
        by = "mrn"
)

# 254 *67 

#add ttrg related variables
dm <- left_join(
        dm,
        dttr %>% select(mrn,tlow,thi,ttrg),
        by = "mrn"
)

#add variability related variables
dm <- left_join(
        dm,
        dsig %>% select(mrn,sigm),
        by = "mrn"
)

#add hep day per dose relatd variables
dm <- left_join(
        dm,
        dcumhep %>% select(mrn,hep_wkgday),
        by = "mrn"
)

#add no of prescription changes
dm <- left_join(
        dm,
        dheprl %>% select(mrn,rl_day),
        by = "mrn"
)

#add circuitchange 
dm <- left_join(
        dm,
        dfci %>% select(mrn,totc,cday),
        by = "mrn"
)

##adding haemorhhagic comps

df3$hboth <- df3$toth + df3$totboth

dm <- left_join(
        dm,
        df3 %>% select(mrn,toth,hboth),
        by = "mrn"
)

# add blood products transfused

dm <- left_join(
        dm,
        dprd,
        by = "mrn"
)


#remove messy repeated ones
dm2 <- dm

dm <- dm %>% select(
        - c(
                ethnic,
                surv_icu,
                acute_phys_score,
                age_score,
                chron_health_score,
                
        )
)

#remove missing datas
dm <- dm %>% select(
        - c(
                plt_min,
                plt_max,
                fib_min,
                fib_max,
                ldh_min,
                ldh_max,
                ferritin_min,
                ferritin_max,
                ck_min,
                ck_max,
                crp_min,
                crp_max,
                pct_min,
                pct_max,
                bili_min,
                bili_max,
                gfr_min,
                gfr_max,
                ca_min,
                ca_max,
                corr_ca_min,
                corr_ca_max,
                ph_min,
                ph_max
                
        )
)

###

###NEED TO CORRECT Na's

## 
dm[is.na(dm)] <- 0


########



# model for duration or length of ecmo ------------------------------------

#median ecmo duration is 14 days.


#new df dms 

#discharged alive = 1 
#dead = 0 
#still on ecmo = 0 
#this is what we want.

#lets code from our data.
#survival ecmo= yes & ecmod <=14  ~ discharged alive (i.e.,1)
#surviva ecmo = yes % ecmod > 14 ~ 0 (because still on ecmo)
#surival no & ecmod <= 14 ~ 0 (because dead whilst on ecmo)
#survival no  & ecmod 
dms <- dm
ftime <- 14 
#variable ftime for "follow up time"

dms <- dms %>% 
        mutate(
                los = case_when(
                        ecmod <ftime | ecmod == ftime ~ ecmod,
                        ecmod > ftime ~ ftime
                ),
                so = case_when(
                        surv_ecmo == "yes" & ecmod <= ftime ~ 1,
                        surv_ecmo == "no" & ecmod <= ftime ~ 0,
                        surv_ecmo == "yes" & ecmod >= ftime ~ 1,
                        surv_ecmo == "no" & ecmod >= ftime ~ 1
                )
        ) %>% 
        mutate(los = as.numeric(los))

s <- Surv(dms$los,dms$so)


# model A for 14 days

ma <- coxph(Surv(los,so)~ age + sex + apache, data = dms, model = TRUE)

summary(ma)

mb <- coxph(Surv(los,so)~ age + sex + apache + group + bmi , data = dms, model = TRUE)

mc <- coxph(Surv(los,so)~ age + sex + group + age:group , data = dms, model = TRUE)


md <- coxph(Surv(los,so)~ age + sex + group + cohort + ecmosb, data = dms, model = TRUE)
me <- coxph(Surv(los,so)~ age + sex + group + cohort + ecmosb + cohort:ecmosb, data = dms, model = TRUE)
#anova(md,me)
#this is 1. and loglik is the same. no effect of interaction basically 
mf <- coxph(Surv(los,so)~ age + sex + group + cohort + age:cohort, data = dms, model = TRUE)
#anova(md,mf)
#this does not show any interaction effects.
mg  <- coxph(Surv(los,so)~ age + sex + group + cohort, data = dms, model = TRUE)
mh  <- coxph(Surv(los,so)~ age + sex + apache+ group + cohort, data = dms, model = TRUE)
#adding apache doesnt really help

anova(ma,mb,mc)

#final contender is mg


# model for 30 days -------------------------------------------------------

ftime <- 30
dm3 <- dm

dm3 <- dm3 %>% 
        mutate(
                los = case_when(
                        ecmod <ftime | ecmod == ftime ~ ecmod,
                        ecmod > ftime ~ ftime
                ),
                so = case_when(
                        surv_ecmo == "yes" & ecmod <= ftime ~ 1,
                        surv_ecmo == "no" & ecmod <= ftime ~ 0,
                        surv_ecmo == "yes" & ecmod >= ftime ~ 1,
                        surv_ecmo == "no" & ecmod >= ftime ~ 1
                )
        ) %>% 
        mutate(los = as.numeric(los))

m3a <- coxph(Surv(los,so)~ age + sex + apache, data = dm3, model = TRUE)

summary(m3a)

m3b <- coxph(Surv(los,so)~ age + sex + apache + group + bmi , data = dm3, model = TRUE)

m3c <- coxph(Surv(los,so)~ age + sex + group + age:group , data = dm3, model = TRUE)

m3g  <- coxph(Surv(los,so)~ age + sex + group + cohort, data = dm3, model = TRUE)
m3h  <- coxph(Surv(los,so)~ age + sex + apache+ group + cohort, data = dm3, model = TRUE)
#adding apache doesnt really help
anova(m3a,m3b,m3c,m3g,m3h)

m3i  <- coxph(Surv(los,so)~ age + sex + group + cohort + age:cohort + group:cohort, data = dm3, model = TRUE)
anova(m3g,m3i)
#no effect of interaction

#https://thomaselove.github.io/432-notes/cox-regression-models-for-survival-data-example-2.html#model-a-coxph-model-for-survival-time-using-age-at-diagnosis
#in case we want to do it with rms
#we got something here m3g 
 
