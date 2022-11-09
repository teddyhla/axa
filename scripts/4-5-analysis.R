# SOURCE DATA -------------------------------------------------------------
load(file="data/clean/finalout.RData")


# LIBRARIES ---------------------------------------------------------------
library(survival)
library(tidyverse)
library(survminer)



# NOTE --------------------------------------------------------------------



#need to adjust for 
#age, apache, weight, bmi, median_platelets, fibrinogen , neutrophils, ferritin
#ck,#crp #pct #albumin ,creat, eGFR, bicarb, ph,rrt

#targets
#1. ttr - improvement in favour of axa but no statistical significance
#2. var - significant improvement in fav axa and stats significance 
#3. heparin per kilogram per day - in favour of apttr but needs looking 
#4. no of heparin prescription changes - in favour of axa and stats significance 
#5. total blood products
#6. time to first complication
#7. time to first circuit change
#8. should test "duration" and should test "death"



# FINAL -------------------------------------------------------------------


# 6. Time to 1st Complication ANY complication --------------------------------

t2cmp <- t1cmp
#the levels are both, only h, only t and no comp
#both should be event and only h should be event and others are non events
levels(t2cmp$value) <- c(1,1,1,0)
t2cmp$value <- as.numeric(as.character(t2cmp$value))

#na here is medianinfgul -- because na medians event does not happen.

t2cmp <- left_join(
        t2cmp,
        dfcore %>% select(mrn, ecmoh),
        by = "mrn"
)

t2cmp$ecmoh <- as.numeric(t2cmp$ecmoh)
##important this code needs to be run in this current order
##1
t2cmp <- t2cmp %>%
        mutate(t = case_when(
                is.na(value) ~ ecmoh,
                !is.na(value) ~ duhr
                
        ))

##2 order no 2 

t2cmp$value[is.na(t2cmp$value)] <- 0

t2cmp <- t2cmp %>% filter(!is.na(t) & t> 0)

drrt <- t2cmp
drrt <- left_join(
        drrt,
        dfcore %>% select(mrn,ecmo_finish),
        by = "mrn"
)

drrt <- drrt %>% 
        mutate(t2 = case_when(
                is.na(time)~ ecmo_finish,
                !is.na(time)~ time
        ))

drrt <- left_join(
        daki,
        drrt %>% select(mrn,t2),
        by = "mrn"
)

drrt <- drrt %>%
        group_by(mrn) %>%
        filter(dates < t2) %>% 
        summarise (aki = sum(RRT)) %>%
        mutate(rrt = case_when(
                aki >0 ~ "yes",
                aki == 0 ~ "no"
        )) %>%
        ungroup()

drrt$rrt <- as.factor(drrt$rrt)
        
t2cmp <- left_join(
        t2cmp,
        drrt %>% select(mrn,rrt),
        by = "mrn"
)
rm(drrt)

t2cmp <- t2cmp %>% select(mrn,group,value,t,rrt)
t2cmp <- left_join(
        t2cmp,
        d1tr %>% select(mrn,ttrg),
        by = "mrn"
)

t2cmp <- left_join(
        t2cmp,
        d1sig %>% select(mrn,sigm),
        by = "mrn"
)

t2cmp <- left_join(
        t2cmp,
        dfcore %>% select(mrn,age,sex,apache),
        by = "mrn"
)

t2cmp <- left_join(
        t2cmp,
        dm %>% select(mrn,ph_median,ferritin_median),
        by = "mrn"
)


# model fit for ANY complications -----------------------------------------


so <- coxph(Surv(t,value)~ttrg + group , data= t2cmp)
s1 <- coxph(Surv(t,value)~sigm + group + sigm:group, data= t2cmp)
s2 <- coxph(Surv(t,value)~sigm + ttrg + group + sigm:ttrg, data= t2cmp)
s3 <- coxph(Surv(t,value)~sigm + ttrg + group + sigm:ttrg + age + sex + apache, data= t2cmp)

s4 <- coxph(Surv(t,value)~sigm + ttrg + group + age + sex + rrt + apache, data= t2cmp)

s5 <- coxph(Surv(t,value)~sigm + ttrg + group + age + sex + rrt + apache + sigm:ttrg , data= t2cmp)
s5i <- coxph(Surv(t,value)~sigm + ttrg + group  + age + sex + rrt + apache + group:sigm, data= t2cmp)
s7 <- coxph(Surv(t,value)~sigm + ttrg + group + age + sex + rrt + apache + ttrg:group , data= t2cmp)
s8 <- coxph(Surv(t,value)~sigm + ttrg + group + age + sex + rrt + apache + ttrg:group + sigm:ttrg + group:sigm , data= t2cmp)

s6 <- coxph(Surv(t,value)~group + sigm + age + rrt + sex + apache  ,data= t2cmp)
anova(s5,s5i,s7,s8)
#s8 is useless
#s5i and s7

anova(s4,s5i,s7)

sfit <- survfit(Surv(t,value)~group, data= t2cmp)
sjPlot::plot_model(s7,title = "Time to first ANY complication")

#testing for cox's assumptions

# 6.1. 1st Haemorrhagic complication --------------------------------------

oh2cmp <- oh1cmp
levels(oh2cmp$value) <- c(1,1,0,0)
oh2cmp$value <- as.numeric(as.character(oh2cmp$value))

oh2cmp <- left_join(
        oh2cmp,
        dfcore %>% select(mrn,ecmoh),
        by = "mrn"
        
)

oh2cmp$ecmoh <- as.numeric(oh2cmp$ecmoh)

oh2cmp <- oh2cmp %>%
        mutate( t= case_when(
                is.na(value) ~ ecmoh,
                !is.na(value) ~ duhr
        ))

##this code neeeds to be run in this order 
oh2cmp$value[is.na(oh2cmp$value)] <- 0

##addd in rrt information 
drrt <- oh2cmp
drrt <- left_join(
        drrt,
        dfcore %>% select(mrn,ecmo_finish),
        by = "mrn"
)

drrt <- drrt %>% 
        mutate(t2 = case_when(
                is.na(time)~ ecmo_finish,
                !is.na(time)~ time
        ))

drrt <- left_join(
        daki,
        drrt %>% select(mrn,t2),
        by = "mrn"
)

drrt <- drrt %>%
        group_by(mrn) %>%
        filter(dates < t2) %>% 
        summarise (aki = sum(RRT)) %>%
        mutate(rrt = case_when(
                aki >0 ~ "yes",
                aki == 0 ~ "no"
        )) %>%
        ungroup()

drrt$rrt <- as.factor(drrt$rrt)

t2cmp <- left_join(
        t2cmp,
        drrt %>% select(mrn,rrt),
        by = "mrn"
)
rm(drrt)

# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


oh2cmp <- oh2cmp %>% select(mrn,group,value,t)
oh2cmp <- left_join(
        oh2cmp,
        o1tr %>% select(mrn,ttrg),
        by = "mrn"
)

oh2cmp <- left_join(
        oh2cmp,
        o1sig %>% select(mrn,sigm),
        by = "mrn"
)

oh2cmp <- left_join(
        oh2cmp,
        dfcore %>% select(mrn,age,sex,apache),
        by = "mrn"
)

oh2cmp <- left_join(
        oh2cmp,
        dm %>% select(mrn,ph_median,ferritin_median),
        by = "mrn"
)





# model fit only hemorrhagic ----------------------------------------------



sh <- coxph(Surv(t,value)~group + sigm + ttrg + sigm:ttrg,data= oh2cmp)
sh1 <- coxph(Surv(t,value)~group + sigm + ttrg + sigm:ttrg + age + sex + apache, data= oh2cmp)


sh4 <- coxph(Surv(t,value)~sigm + ttrg + group + sigm:ttrg + age + sex + apache + ph_median + ferritin_median, data= oh2cmp)


# 7. Circuit change -------------------------------------------------------
dxc2 <- dxc %>% filter(xc == 1 & !is.na(time))

sum(duplicated(dxc2$mrn))
#code works

dxc2 <- left_join(
        dfcore %>% select(mrn,group,ecmo_start,ecmoh),
        dxc2,
        by = "mrn"
)

dxc2$ecmoh <- as.numeric(dxc2$ecmoh)

#lets calc time interval
dxc2$duhr <- difftime(dxc2$time,dxc2$ecmo_start,units = "hours")
dxc2$duhr <- round(as.numeric(dxc2$duhr),2)

dxc2 <- dxc2 %>% 
        mutate(t = case_when(
                is.na(xc) ~ ecmoh,
                !is.na(xc) ~ duhr
        ))

#order of code running is important , this has to be run after above code
dxc2$xc[is.na(dxc2$xc)] <- 0

sx <- coxph(Surv(t,xc)~group, data= dxc2)
####

