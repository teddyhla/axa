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

###

###NEED TO CORRECT Na's

## 
dm[is.na(dm)] <- 0



# COX PH ------------------------------------------------------------------


#####

#lets try coxph
dm2$surv_ecmo <- as.numeric(dm2$surv_ecmo)

so <- Surv(time = dm2$ecmod, event = dm2$surv_ecmo)
sf1 <- survfit(so ~ group, data = dm2)
sf2 <- coxph(so ~ age + group + ttrg + sigm, data = dm2 )


# SPLIT DATA  -------------------------------------------------------------


#train test split using CARET

set.seed(1234)
trainIndex <- caret::createDataPartition(dm$surv_ecmo, p = .8 , list = FALSE, times = 1)

dmTrain <- dm[trainIndex,]
dmTest  <- dm[-trainIndex,]


# FIT A NULL MODEL --------------------------------------------------------

lg0 <- glm(surv_ecmo ~ 1, family = binomial(link = "logit"),data = dmTrain)


# FIT AN ALL VAR MODEL ----------------------------------------------------


# DO A MULTI VAR ANALYSIS NOW 
lg1 <- glm(surv_ecmo ~ ., family = binomial(link = "logit"),data = dmTrain)

fit1 <- predict(lg1,newdata = dmTest,type = "response")
fit1 <- ifelse(fit1>0.5,1,0)
fit1r <- ROCR::prediction(fit1,dmTest$surv_ecmo)
fit1rf <- ROCR::performance(fit1r, measure = "tpr",x.measure = "fpr")
plot(fit1rf)

lg2 <- MASS::stepAIC(lg1)
#AIC 206.

## with the blood variables, age,gender,ldh_mean,ferritin_mean,pct_mean,alb_mean,alb_max,creat_min,
## lactate_mean, ttrg,cday

lg3 <- glm(surv_ecmo ~ age + sex + ttrg + sigm + ttrg*sigm + group + ttrg*group + sigm*group +ferritin_mean + ph_mean + hep_wkgday+ rl_day
           ,
           family = binomial(link = "logit"),
           data = dmTrain)

lg4 <- MASS::stepAIC(lg3)

lg5 <- glm(surv_ecmo ~ age + group + age *group, family = binomial(link="logit"),data = dmTrain)

#lg6 based on stepAIC output from lg2
lg6 <- glm(
        surv_ecmo ~ 
                age + sex + neut_min + 
                ldh_mean + ferritin_mean + pct_mean + alb_mean + alb_max + creat_min +
                lactate_mean + ttrg + sigm + cday + toth + hboth + bldtot,
        family = binomial(link = "logit"), data = dmTrain
)

lg7 <- glm(
        surv_ecmo ~ 
                age + sex + neut_min + 
                ldh_mean + ferritin_mean + pct_mean + alb_max + creat_min +
                lactate_mean + ttrg + sigm + cday + toth + hboth + bldtot,
        family = binomial(link = "logit"), data = dmTrain
)

#f
anova(lg0,lg2,lg6,lg7, test = "Chisq")

lg8 <- MASS::dropterm(lg1,test = "F")

#this yielded "age,sex,ldh mean,ferritin mean, ck mean, pct mean, bili mean, alb_max, 
# lactate_mean, ttrg, cday, toth,hboth,bldtot

lg9 <- glm(
        surv_ecmo ~ 
                age + sex + 
                ldh_mean + ferritin_mean + pct_mean + creat_min +
                lactate_mean + ttrg + sigm + cday + toth + hboth + bldtot,
        family = binomial(link = "logit"), data = dmTrain
)

anova(lg2,lg6,lg7,lg9,test = "Chisq")

func <- function(x){
        pscl::pR2(x)["McFadden"]
}
mlist <- list(lg1,lg2,lg3,lg4,lg5,lg6,lg7,lg9)

glist <- map(mlist,func)


##randomforest stuff
rm1 <- randomForest::randomForest(surv_ecmo ~ ., data = dmTrain, importance = TRUE)

#default setting rm1 OOB error = 23.04%
rm2 <- randomForest::randomForest(surv_ecmo ~ ., data = dmTrain,ntree = 500,mtry = 5, importance = TRUE)
#with mtry going from 2 to 5 22.06

#lets try a couple of mtries

a = c()
i = 5

for (i in 2:8){
        rm3 <- randomForest::randomForest(surv_ecmo ~ ., data = dmTrain, ntree = 500,mtry = i,importance = TRUE)
        predValid <- predict(rm3,dmTest,type = "class")
        a[i-1] <- mean(predValid == dmTest$surv_ecmo)
}


rm4 <- randomForest::randomForest(surv_ecmo ~ ., data = dmTrain,ntree = 500,mtry = 6, importance = TRUE)
#with mtry going from 2 to 5 22.06


#
#fit1 <- predict(rm4,newdata = dmTest,type = "response")
#fit1 <- ifelse(fit1>0.5,1,0)
#fit1r <- ROCR::prediction(fit1,dmTest$surv_ecmo)
#fit1rf <- ROCR::performance(fit1r, measure = "tpr",x.measure = "fpr")
#
#auc <- ROCR::performance(fit1r, measure = "auc")
#auc <- auc@y.values[[1]]
#auc

plot(fit1rf)


##rand

rfpt <- predict(rm4 , type = "response")[,2]
rfptr <- ROCR::prediction(rfpt,dmTest$surv_ecmo)
rauc <- ROCR::performance(rfptr,measure = "auc")@y.values[[1]]



####

##MVR for TTR

library(ggplot2)

gr1 <- ggplot(data =dm , aes(x= ttrg, colour = group)) +
        geom_density()

gr2 <- ggplot(data = dm ,aes(y = sigm, color = group)) +
        geom_boxplot(outlier.shape = NA)+
        coord_cartesian(ylim = c(0,0.3))

gr3 <- ggplot(data = dm ,aes(x = group, color = group,y=sigm)) +
        geom_boxplot(outlier.shape = NA)+
        coord_cartesian(ylim = c(0,0.3))

gr4 <- ggplot(data = dm, aes(x=sigm, color = group)) +
        geom_density()+
        coord_cartesian(xlim = c(0,0.3))

#ggsave("products/presentations/final_presentations/src/gr1",plot = gr1, device = "png",dpi = 320)


#ggsave("products/presentations/final_presentations/src/gr3.png",plot = gr3, device = "png",dpi = 600)

#ggsave("products/presentations/final_presentations/src/gr4.png",plot = gr4, device = "png",dpi = 600)

##dd

dm$ttrgf <- ((dm$ttrg * 253) + 0.5)/254   


bm1 <- betareg::betareg(ttrgf ~ age + sex + group + apache + ecmod + sigm + ldh_mean + ph_mean , data = dm)
#pseudo R2 0.11 

bm2 <- betareg::betareg(ttrgf ~ sex + group + sex*group + ecmod + sigm , data = dm)
#pseudo R2 0.10

bm3 <- betareg::betareg(ttrgf ~  group +sex + ecmod +ecmosb, data = dm)
#pseudo R2 0.146
bm4<- betareg::betareg(ttrgf ~  group + sex + ecmod , data = dm)
#pseudo R2 0.10
bm5 <- betareg::betareg(ttrgf ~  group + sex + ecmod +cohort, data = dm)
#no association
#pseudo R2

bm7 <- betareg::betareg(ttrgf ~  group + sex + ecmod + cohort, data = dm)

####
sjPlot::plot_model(bm3,show.values = TRUE, value.offset = .3,title = "Adjusted Odds Ratio fortransformed  Time in Therapeutic Range")

##lets fit for variance.

sm1 <- lm (sigm ~. , data = dm)
sm2 <- MASS::stepAIC(sm1)


## 
library(survival)
cm1 <- coxph(formula = Surv(ecmod,surv_ecmo), data = dm)

s1 <- survfit(Surv(ecmod,surv_ecmo) ~ 1 , data = dm)

survdiff(Surv(ecmod,surv_ecmo)~sex + group, data = dm)
sm2 <- survfit(Surv(ecmod,surv_ecmo) ~ group + sex , data = dm)

##
hm1 <- lm(hep_wkgday ~ .,  data= dm)
hm2 <- MASS::stepAIC(hm1)

rlm1 <- lm(rl_day ~ .,  data= dm)
rlm2 <- MASS::stepAIC(rlm1)
rlm3 <- lm(rl_day ~ sex + wkg + group + ecmod + hb_max + lactate_mean + ttrg + sigm + bldtot + hboth + hep_wkgday 
            ,data = dm)
rlm4 <- lm(rl_day ~ sex + wkg + group + ecmod + hb_max + ttrg + hep_wkgday 
           ,data = dm)
rlm5 <- lm(rl_day ~ sex + wkg + group + ecmod + hb_max + lactate_mean + ttrg + sigm + bldtot + hboth 
           ,data = dm)
##



###

car::vif(lg1)

vif_values <- car::vif(lg1)
barplot(vif_values, main = "VIF values", horiz = TRUE, col = "steelblue")
abline(v=5,lwd = 3, lty= 2)

#vif quite high
##you want VIF < 4 is perfect
#does standard error of model improve

dcrs <- dm %>% select(where(is.numeric))
cor1 <- cor(dcrs)







# Harrell -----------------------------------------------------------------
# [ ]Poisson circuit change. Circuit change as an outcome because we are not setting it a priori
# [ ]Poison of blood transfusion. same as above. 
# [ ]As well as log reg of both. same as above
# [/]And adjust for year of admission.  time as both bins and time as continuou
# [/]Talk about multi state model. 
# [ ]Bleeding complications Poisson. 
# [ ]Finalise with multi variate Alive. dead



# mutlivarmodel -----------------------------------------------------------

#based on lg2, age, apache, wkg, group, ecmos,ecmosb,neut_mean,ldh_mean,ferritin_mean,alb_mean,
#creat_mean, bicarb meaen, lactate_mean, rl_day,totc, cday, toth,hboth,bldtot

## ttrg, sigm
## totc = total circuit change, cday = circuit change adjusted for run length
## toth = total hemorrhage complications
## hboth == both haem and thrombotic
##blodtot = total blood products
## AIC 155.36 without using rms package

f <- "surv_ecmo ~ age + apache + wkg + group + ecmosb + neut_mean + ldh_mean + ferritin_mean + alb_mean
+ creat_mean + bicarb_mean + lactate_mean + rl_day + totc + cday + toth + hboth + bldtot +ttrg +sigm"

#m1 <- lrm(
#        formula = surv_ecmo ~ age + apache + wkg + group + ecmosb + neut_mean + ldh_mean + ferritin_mean + alb_mean
#        + creat_mean + bicarb_mean + lactate_mean + rl_day + totc + cday + toth + hboth + bldtot +ttrg +sigm ,
#        data = dm,
#        x = TRUE,
#        y = TRUE
#)
#

m1 <- glm(surv_ecmo ~ age + apache + wkg + group + ecmosb + neut_mean + ldh_mean + ferritin_mean + alb_mean 
          + creat_mean + bicarb_mean + lactate_mean + rl_day + totc + cday + toth + hboth + bldtot +ttrg +sigm,
                family = binomial(link = "logit"), data = dm
        
)
#pseudo R2 0.45, c 0.86 , aic 245
#high VIF = remove hboth, 
# cday, toth, hboth, bldtot, ttrg, 

#m2 <- lrm(
#        formula = surv_ecmo ~ age + apache + wkg + group + ecmosb + ldh_mean + cday + toth + bldtot +ttrg +sigm ,
#        data = dm,
#        x = TRUE,
#        y = TRUE
#)

m2 <- glm(surv_ecmo ~ age + apache + wkg + group + ecmosb + ldh_mean + cday + toth + hboth + bldtot +ttrg +sigm,
          family = binomial(link = "logit"), data = dm
          
)
#pseudo R = 0.39, brier c 0.83 , aic 239 
#m3 <- lrm(
#        formula = surv_ecmo ~ age + group + ldh_mean + cday + toth + bldtot +ttrg ,
#        data = dm,
#        x = TRUE,
#        y = TRUE
#)
#
m3 <- glm(surv_ecmo ~ age + group + ldh_mean + cday + toth + bldtot +ttrg +sigm,
          family = binomial(link = "logit"), data = dm
          
)
#aic 233 
m4 <- glm(surv_ecmo ~ age + group + cday + bldtot +ttrg +sigm,
          family = binomial(link = "logit"), data = dm
          
)
#aic 232

m5 <- glm(surv_ecmo ~ age + group + cday + bldtot +ttrg,
          family = binomial(link = "logit"), data = dm
          
)
#aic 230

m0<- glm(surv_ecmo ~ 1,
          family = binomial(link = "logit"), data = dm
          
)
#aic 286

#anova(m0,m1,m2,m3,m4,m5,test = "Chisq")

#lmtest::lrtest(m2,m5)
#showed that m1 is still the best 
#pseudoR2 is 0 for m0
#pseoduR2 is 0.29 for m2



#m1 <- glm(surv_ecmo ~ age + apache + wkg + group + ecmosb + neut_mean + ldh_mean + ferritin_mean + alb_mean 
#          + creat_mean + bicarb_mean + lactate_mean + rl_day + totc + cday + toth + hboth + bldtot +ttrg +sigm,
#          family = binomial(link = "logit"), data = dm, na.action = na.pass
#          
#)
#mm <- MuMIn::dredge(m1)
#head(mm)
#


# --- ---------------------------------------------------------------------

library(caret)
model <- m0<- glm(surv_ecmo ~ .,
                  family = binomial(link = "logit"), data = dmTrain
                  
)

mz <- glm(surv_ecmo ~ sigm + ttrg + age,
                  family = binomial(link = "logit"), data = dmTrain
                  
)

prob <- model %>% predict(dmTest, type = "response")
pre_c <- ifelse(prob>0.5,"yes","no")
mean(pre_c == dmTest$surv_ecmo)

dmTrain %>% 
        mutate(prob = ifelse(surv_ecmo == "yes",1,0)) %>%
        ggplot(aes(hboth,prob))+
        geom_point(alpha = 0.2)+
        geom_smooth(method = "glm",method.args = list(family="binomial"))+
        labs(
                title = "mvlogreg",
                x = "ttrg",
                y = "probability of alive"
        )

#perhaps 



###

hist(dm$sigm,breaks = 100,xlim = c(0,0.8),main = "variability")

dm$lsigm <- log(dm$sigm)


hist(dm$lsigm,breaks = 100,main = "log variability")

mv0 <- lm(sigm ~ age + group + ecmod + ecmosb + cohort + bicarb_mean + apache + wkg, data = dm)
sjPlot::plot_model(mv0)

mv1 <- sm2 <- MASS::stepAIC(mv0)

dmt <- dm %>% filter(sigm >0)
mv2 <- lm(sigm ~ age + group + sex + wkg , data = dmt)

#find optimal lambda for box cox 
bc <- MASS::boxcox(mv2)
lambda <- bc$x[which.max(bc$y)]

dmt$tsigm <- (dmt$sigm^lambda-1)/lambda
#


rmt <- randomForest::randomForest(sigm ~ age + group +  sex + wkg , data = dmTrain,ntree = 500, importance = TRUE,)
#with mtry going from 2 to 5 22.06
randomForest::importance(rmt)


mz <- glm(hboth ~ group + sigm + ttrg + age + sex + cohort + ecmod + ecmosb , data =dm, family = poisson(link = "log"))
mz2 <- MASS::stepAIC(mz)

mr <- glm(hboth ~ group + sigm + ttrg + ecmod , data =dm, family = poisson(link = "log"))


mrin <- glm(hboth ~age+ group + group:ttrg + sigm + ttrg + sigm:ttrg + ecmod , data =dm, family = poisson(link = "log"))
#mrin is winning byt aic 428 vs aic 433
#
