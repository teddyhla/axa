#########################

########################


# SOURCE DATA -------------------------------------------------------------
load(file="data/clean/out.RData")
library(tidyverse)
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


###### TTR traditional




########
dheprl$y <- log(dheprl$runl)

dh <- left_join(
        dheprl %>% select(mrn,y,rl_day),
        dm %>% select(mrn,age,sex,apache,group,wkg,cohort,ttrg,ph_mean,ldh_mean),
        by = "mrn"
)

dh <- dh %>% select(-mrn)

lm0 <- lm(y ~ 1, data = dh)

lm1 <- lm(y ~ . , data = dh)
#RSE 0.86, pvalue 0.34

lm2 <- lm(y ~ group + cohort,data = dh)
summary(lm2)

anova(lm0,lm1, test = "Chisq")

l0 <- lm(rl_day ~ 1, data = dh)
l1<-lm(rl_day ~ ., data = dh)
l2 <- lm(rl_day ~group + cohort + wkg, data= dh)
l3 <- lm(rl_day ~ group + wkg + ttrg, data = dh)
l4 <- lm(rl_day ~ group + ttrg + group:ttrg, data = dh)
#no interaction
l5 <- lm(rl_day ~ group + ttrg, data = dh)
l6 <- lm(rl_day ~ group + cohort + ttrg,data = dh)
summary(l0)
summary(l1)
summary(l2)

anova(l0,l5,l6,test="Chisq")
sjPlot::plot_model(l5)

g2 <- sjPlot::plot_model(l5,show.values = TRUE, value.offset = .3,title = "Prescription Changes adjusted for other variables",vline.color = "red")


#l5 is the winner 


ggsave("products/presentations/final_presentations/src/g2.png",plot = g2, device = "png",dpi = 320)
sjPlot::tab_model(l5)
##

#bldtot

mb1 <- glm(bldtot ~ group, data =dm, family = poisson(link = "log"))
mb2 <- glm(bldtot ~ group + age + apache + wkg + ttrg + sigm + cohort, family = poisson(link="log"),data=dm)
mb3 <- glm(bldtot ~ group + sex + age + apache + wkg + ttrg + sigm + cohort, family = poisson(link="log"),data=dm)
mb4 <- glm(bldtot ~ group + sex + apache + wkg + ttrg + sigm + cohort, family = poisson(link="log"),data=dm)

mb5 <- glm(bldtot ~ group + sex + apache + wkg + ttrg + ecmosb + cohort, family = poisson(link="log"),data=dm)
mb6 <- glm(bldtot ~ group + sex + apache + wkg + ttrg + rl_day, family = poisson(link = "log"),data = dm)
anova(mb1,mb3,mb4,test="Chisq")
#according to this mb3 is the best

#prescription change distors so much

mb8 <- glm(bldtot ~ group + sex + age + apache + wkg + ttrg + sigm, data = dm, family = poisson(link="log"))

mb9 <- glm(bldtot ~ group + sex + age + apache + wkg + ttrg + sigm + ecmod, data = dm, family = poisson(link="log"))
#aic 2574.4


mbx <- glm(bldtot ~ group + sex  + apache + ttrg + ecmod, data = dm, family = poisson(link="log"))
#aic 2574


mbxi <- glm(bldtot ~ group + sex + age + apache + ttrg + sigm + ecmod, data = dm, family = poisson(link="log"))
#aic 2574.4


mbxii <- glm(bldtot ~ group + sex + age + apache + wkg + ttrg + sigm + ecmod, data = dm, family = quasipoisson(link="log"))
#aic 1574.4
anova(mb3,mb6, test = "Chisq")

mbi <- glm(bldtot ~ group + sex + age + apache + wkg + ttrg + cohort + ecmod, data = dm, family = quasipoisson(link="log"))
#aic 1574.4

mp9 <- sjPlot::plot_model(mb9,show.values=TRUE,value.offset = .3)
mb12 <- sjPlot::plot_model(mbxii,show.values=TRUE,value.offset = .3)

g3 <- sjPlot::plot_model(mb3,show.values = TRUE, value.offset = .3,title = "Blood Products Transfusion adjusted for other variables",vline.color = "yellow")
ggsave("products/presentations/final_presentations/src/g3.png",plot = g3, device = "png",dpi = 320)
sjPlot::tab_model(mb3)

g6 <- sjPlot::plot_model(mb6,show.values = TRUE, value.offset = .3,title = "Blood Products Transfusion adjusted for other variables",vline.color = "yellow")
ggsave("products/presentations/final_presentations/src/g3.png",plot = g6, device = "png",dpi = 320)
sjPlot::tab_model(mb6)
##

#finally mb8 wins

g8 <- sjPlot::plot_model(mb8,show.values = TRUE, value.offset = .3,title = "Blood Products Transfusion adjusted for other variables",vline.color = "yellow")
ggsave("products/presentations/final_presentations/src/g8.png",plot = g8, device = "png",dpi = 320)
sjPlot::tab_model(mb8)

#how abut

#CIRCUIT CHANGE

c0 <- glm(totc ~ 1, family = poisson(link="log"),data=dm)
#aic 780, resi dev 467
c1 <- glm(totc ~ group + sex + apache + wkg + ttrg + rl_day, family = poisson(link = "log"),data = dm)

c2 <- glm(totc ~ group + sex + ttrg + sigm + bldtot + rl_day, family = poisson(link = "log"),data = dm)

c3 <- glm(totc ~ group + age+ sex + ttrg + sigm + bldtot + rl_day, family = poisson(link = "log"),data = dm)

anova(c0,c1,c2,c3,test="Chisq")

#c2 is the winner 
c2 <- sjPlot::plot_model(c2,show.values = TRUE, value.offset = .3,title = "Circuit change adjusted for other variables",vline.color = "yellow")
#ggsave("products/presentations/final_presentations/src/c2.png",plot = c2, device = "png",dpi = 320)
#sjPlot::tab_model(c2)


##
o0 <- glm(surv_ecmo ~ 1 ,data = dm,family=binomial(link="logit"))
o1 <- glm(surv_ecmo ~ age + apache + wkg + bldtot + totc + rl_day + ttrg + sigm ,data = dm, family = binomial(link="logit"))
o2 <- glm(surv_ecmo ~ age + bldtot + totc + ttrg + sigm ,data = dm, family = binomial(link="logit"))
o3 <- glm(surv_ecmo ~ age + bldtot + totc + ttrg + sigm + cohort ,data = dm, family = binomial(link="logit"))
o4 <- glm(surv_ecmo ~ age + bldtot + totc + ttrg + group +group:cohort + cohort ,data = dm, family = binomial(link="logit"))

o5 <- glm(surv_ecmo ~ age + bldtot + totc + ttrg + group + hboth  ,data = dm, family = binomial(link="logit"))
o6 <- glm(surv_ecmo ~ age + bldtot + totc + ttrg + group + toth  ,data = dm, family = binomial(link="logit"))
o7<- glm(surv_ecmo ~ age + bldtot + totc + ttrg + group + toth +hboth  ,data = dm, family = binomial(link="logit"))

o31 <- glm(surv_ecmo ~ age + bldtot + totc + ttrg + group,data = dm, family = binomial(link="logit"))
#aic 235
oz <- glm(surv_ecmo ~ age + bldtot + totc + group,data = dm, family = binomial(link="logit"))
#aic 244
oz1 <- glm(surv_ecmo ~ age + bldtot + totc + group + ttrg + group:ttrg,data = dm, family = binomial(link="logit"))
#aic 237

ozm <- glm(surv_ecmo ~ age + sex + bldtot + totc + group + ttrg + group:ttrg,data = dm, family = binomial(link="logit"))
#aic 

ozo <- glm(surv_ecmo ~ age + bldtot + totc + group + toth,data = dm, family = binomial(link="logit"))
#doesnt really improve

anova(o31,oz,oz1,ozm,test="Chisq")

goz <- sjPlot::plot_model(oz, show.values = TRUE,value.offset = .3, title = "Adjusted Odds Ratio for ECMO surival")
ggsave("products/presentations/final_presentations/src/gz.png",plot = goz, device = "png",dpi = 320)
sjPlot::tab_model(oz)

##let's do complications



h0 <- glm(toth ~ 1, family = poisson(link = "log"),data = dm)

h1 <- glm(toth ~ group + sex + apache + wkg + ttrg + sigm + cohort+ rl_day, family = poisson(link = "log"),data = dm)
h2 <- glm(toth ~ ttrg, data= dm, family = poisson(link = "log"))
anova(h0,h1,h2,test="Chisq")


hoz <- sjPlot::plot_model(h1, show.values = TRUE,value.offset = .3, title = "Adjusted Odds Ratio for bleeding complications")
ggsave("products/presentations/final_presentations/src/hoz.png",plot = hoz, device = "png",dpi = 320)




###
##