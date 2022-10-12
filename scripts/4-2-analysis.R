#########################

########################


# SOURCE DATA -------------------------------------------------------------
load(file="data/clean/out.RData")

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

# 2.8. Final df -----------------------------------------------------------

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
                ecmod
        )

#254 * 13
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



#remove messy repeated ones

dm <- dm %>% select(
        - c(
                mrn,
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



###NEED TO CORRECT Na's

## 
dm[is.na(dm)] <- 0


# DO A MULTI VAR ANALYSIS NOW 
lg1 <- glm(surv_ecmo ~ ., family = binomial(link = "logit"),data = dm)

lg2 <- MASS::stepAIC(lg1)

## with the blood variables, age,gender,ldh_mean,ferritin_mean,pct_mean,alb_mean,alb_max,creat_min,
## lactate_mean, ttrg,cday

lg3 <- glm(surv_ecmo ~ age + sex + ttrg + sigm + ttrg*sigm + group + ttrg*group + sigm*group
           ,
           family = binomial(link = "logit"),
           data = dm)

lg4 <- MASS::stepAIC(lg3)

lg5 <- glm(surv_ecmo ~ age + group + age *group, family = binomial(link="logit"),data = dm)