# SOURCE DATA -------------------------------------------------------------
load(file="data/clean/finalout.RData")


# LIBRARIES ---------------------------------------------------------------
library(survival)

library(survminer)

# NOTE --------------------------------------------------------------------



#need to adjust for 
#age, apache, weight, bmi, mean_platelets, fibrinogen , neutrophils, ferritin
#ck,#crp #pct #albumin ,creat, eGFR, bicarb, ph,rrt

#targets
#1. ttr - improvement in favour of axa but no statistical significance
#2. var - significant improvement in fav axa and stats significance 
#3. heparin per kilogram per day - in favour of apttr but needs looking 
#4. no of heparin prescription changes - in favour of axa and stats significance 
#5. total blood products
#6. time to first complication
#7. time to first circuit change


# TTR ---------------------------------------------------------------------


# FINAL -------------------------------------------------------------------


# 6. Time to 1st Complication ---------------------------------------------

t2cmp <- t1cmp
#the levels are both, only h, only t and no comp
#both should be event and only h should be event and others are non events
levels(t2cmp$value) <- c(1,1,0,0)
t2cmp$value <- as.numeric(as.character(t2cmp$value))

#na here is meaninfgul -- because na means event does not happen.

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
so <- coxph(Surv(t,value)~group, data= t2cmp)


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

