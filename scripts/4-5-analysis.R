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

#Requirements
#key var : time and event
# events need to be cross checked
# time need to be cut for "pre-event" 

#all other vars need to satisfy this.

#t1cmp is ANY COMP
#oh1cmp IS ONLY BLEEDING COMP

#basically in df - t1cmp - value is NA if there is no event, 
#value is not NA if there is an event, and time is the time of that event.
#duhr is time in that duration

t2cmp <- t1cmp
#the levels are both, only h, only t and no comp
#all events are assigned as "1" and others "0"
levels(t2cmp$value) <- c(1,1,1,0)
t2cmp$value <- as.numeric(as.character(t2cmp$value))

#na here is meaningful -- because na means event does not happen.

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
# t is the variable we want. 


drrt <- t2cmp
drrt <- left_join(
        drrt,
        dfcore %>% select(mrn,ecmo_finish),
        by = "mrn"
)

#remember time is the time of the event, thus NA if no event, and not na if there is event.
#thus if there is NO event - > time is NA - > thus we will capture down to ecmo fin
#thus if there is an event !is.na - then the time of event will be used to subset.
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

#potentially could use the absolute number here to show days on aki
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
        dfcore %>% select(mrn,age,sex,apache,bmi),
        by = "mrn"
)

t2cmp <- left_join(
        t2cmp,
        dm %>% select(mrn,ph_median,ferritin_median),
        by = "mrn"
)

t3 <- t2cmp
bt <- t2cmp


# bt ----------------------------------------------------------------------

#what we are trying to do here is we are trying to censor at 500 hours
#so all time would be 500 or less. [0,500]
#any events that happened afte 500, would be 0 because they happen after 500.

# bt %>% count(value)
# this showed that there is 114 events 

# bt %>% filter(t > 500) %>% count(value)
# this showed there are 17 events that happen after 500 hrs.
# so after the code treatment, there should 114 - 17 = 97 events

bt$cen <- ifelse(bt$t > 500, "cen", NA)
bt$cen <- as.factor(bt$cen)

bt$e <- ifelse(bt$t > 500 & bt$value >0 , 0 , bt$value )
bt$t2 <- ifelse(bt$t > 500 , 500, bt$t)
# bt %>% count(e)
# 1 is 97 thus it tallies.
#

# identical(t3,t2cmp)

bsx <- coxph(Surv(t2,e)~age + sex + rrt + bmi + group + apache + ttrg + sigm, data = bt )

bsfit <- survfit(Surv(t2,e)~group, data= bt)


bsp1 <- ggsurvplot(bsfit,surv.median.line = "hv",pval=TRUE,conf.int = TRUE,title = "bsTime to first any BTE")



ftbsx <-cox.zph(bsx)

ggcoxzph(ftbsx)
ggcoxdiagnostics(bsx,type = ,linear.predictions = TRUE)
ggcoxdiagnostics(bsx,type = "dfbeta" ,linear.predictions = TRUE)


bsxtt <- coxph(Surv(t2,e)~ age + sex + rrt + bmi + group + apache + ttrg + sigm + tt(ttrg),
              data = bt, tt=function(x,t,...)x*t)


# --------- different treatment
#249 by 12
t2cmp <- t2cmp %>% filter(t<500)

#198 by 12

# need to examine 
# 1061663N likely an extreme outlier
#comp1 dtm is very wrong 2022-03-04





# model fit for ANY complications -----------------------------------------
it <- survdiff(Surv(t,value) ~ group, data = t2cmp)



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

s9 <- coxph(Surv(t,value)~sigm + ttrg + group + age + sex + rrt + apache + ttrg:group + apache + ferritin_median , data= t2cmp)

sx <- coxph(Surv(t,value)~age + sex + rrt + bmi + group + apache + ttrg + sigm, data = t2cmp )


sx03 <- coxph(Surv(t,value)~age + sex + rrt + bmi + group + apache + ttrg + sigm, data = t3 )


sxi <- coxph(Surv(t,value)~age + sex + rrt + bmi + group + apache + ttrg + exp(sigm), data = t2cmp )


s7ii <- coxph(Surv(t,value)~ log(sigm) + ttrg + group + age + sex + rrt + apache + ttrg:group , data= t2cmp)

sfit <- survfit(Surv(t,value)~group, data= t2cmp)
sjPlot::plot_model(sx,title = "Time to first ANY complication")
sjPlot::tab_model(sx,title = "Time to first ANY complication")
#testing for cox's assumptions

# plotting

ftest <-cox.zph(sx)



ggcoxzph(ftest)
ggcoxdiagnostics(sx,type = ,linear.predictions = TRUE)
ggcoxdiagnostics(sx,type = "dfbeta" ,linear.predictions = TRUE)

sp1 <- ggsurvplot(sfit,surv.median.line = "hv",pval=TRUE,conf.int = TRUE,title = "Time to first any BTE")

sxtt <- coxph(Surv(t,value)~ age + sex + rrt + bmi + group + apache + ttrg + sigm + tt(ttrg),
              data = t2cmp, tt=function(x,t,...)x*t)

sxt3 <- coxph(Surv(t,value)~ age + sex + rrt + bmi + group + apache + ttrg + tt(sigm) + tt(ttrg),
              data = t2cmp, tt=function(x,t,...)x*t)


sxt2 <- coxph(Surv(t,value)~ age + sex + rrt + bmi + group + apache + ttrg + sigm 
              +ph_median + tt(ttrg),
              data = t2cmp, tt=function(x,t,...)x*t)

TIME <- seq(50,500,10)
BETA <- coef(sxtt)["ttrg"]
BETAtt <- coef(sxtt)["tt(ttrg)"]
AHR <- exp(BETA + BETAtt*TIME)


plot(TIME,AHR,type = "l",main = "Effects of TTR over time", xlab = "Time in hours", ylab = "adjusted Hazards Ratio")
abline(h = 1, lty = 2, col = "darkgray")
abline(v = -1*BETA/BETAtt,lty= 2, col = "blue")
plt1 <- recordPlot()
# 
fm <- survfit(Surv(t,value)~ group, data = t2cmp)


##
gtest <- cox.zph(sx03)
ggcoxzph(gtest)

ggcoxdiagnostics(sx03,type = ,linear.predictions = TRUE)
ggcoxdiagnostics(sx03,type = "dfbeta" ,linear.predictions = TRUE)

sxtt3 <- coxph(Surv(t,value)~ age + sex + rrt + bmi + group + apache + ttrg + sigm + tt(ttrg),
              data = t3, tt=function(x,t,...)x*t)

#t2 <- seq(50,1100,10)
b2 <- coef(sxtt3)["ttrg"]
bt2 <- coef(sxtt3)["tt(ttrg)"]
ahr3 <- exp(b2 + bt2*t2)

plot(t2,ahr3)
abline(h= 1)



#winner is sx
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

oh2cmp <- left_join(
        oh2cmp,
        drrt %>% select(mrn,rrt),
        by = "mrn"
)
rm(drrt)

# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


oh2cmp <- oh2cmp %>% select(mrn,group,value,t,rrt)
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
        dfcore %>% select(mrn,age,sex,apache,bmi),
        by = "mrn"
)

oh2cmp <- left_join(
        oh2cmp,
        dm %>% select(mrn,ph_median,ferritin_median),
        by = "mrn"
)





# model fit only hemorrhagic ----------------------------------------------
oh2cmp <- oh2cmp %>% filter(t<520)

#there are 7 missing data in ttrg and sigm

sh0 <- coxph(Surv(t,value)~1, data= oh2cmp)
sh <- coxph(Surv(t,value)~group + sigm + ttrg + sigm:ttrg,data= oh2cmp)
sh1 <- coxph(Surv(t,value)~group + sigm + ttrg + sigm:ttrg + age + sex + apache, data= oh2cmp)


sh4 <- coxph(Surv(t,value)~sigm + ttrg + group + age + sex + bmi + apache + ph_median + ferritin_median + rrt, data= oh2cmp)

sh5 <- coxph (Surv(t,value)~ group + ttrg + sigm:ttrg + ttrg:group + sigm:ttrg + age + sex + apache + rrt + ph_median + ferritin_median, data = oh2cmp)


anova(sh,sh1,sh4,sh5)

otest <- cox.zph(sh4)
ggcoxzph(otest)

szfit <- survfit(Surv(t,value)~group, data= oh2cmp)

ggsurvplot(szfit,surv.median.line = "hv",pval=TRUE,conf.int = TRUE,title="restricted to bleeding complications only")


sjPlot::plot_model(sh4,title = "Time to hemorrhagic  complication")
sjPlot::tab_model(sh4,title = "Time to hemorrhagic  complication")

it2 <- survdiff(Surv(t,value) ~ group, data = oh2cmp)

# 7. Circuit change -------------------------------------------------------

dx2 <- left_join(
        dx2,
        dfcore %>% select(mrn,age,sex,apache,bmi),
        by = "mrn"
)

dx2 <- left_join(
        dx2,
        dm %>% select(mrn,ph_median,ferritin_median),
        by = "mrn"
)

#there are some problem with left hand censoring.

dx2 <- dx2 %>% filter(duhr >0)

dx2$cx <- ifelse(dx2$cx == "yes",1,0)

xt <- survdiff(Surv(dud,cx) ~ group, data = dx2)

#dxc2 <- dxc %>% filter(xc == 1 & !is.na(time))
#
#sum(duplicated(dxc2$mrn))
##code works
#
#dxc2 <- left_join(
#        dfcore %>% select(mrn,group,ecmo_start,ecmoh),
#        dxc2,
#        by = "mrn"
#)
#
#dxc2$ecmoh <- as.numeric(dxc2$ecmoh)
#
##lets calc time interval
#dxc2$duhr <- difftime(dxc2$time,dxc2$ecmo_start,units = "hours")
#dxc2$duhr <- round(as.numeric(dxc2$duhr),2)
#
#dxc2 <- dxc2 %>% 
#        mutate(t = case_when(
#                is.na(xc) ~ ecmoh,
#                !is.na(xc) ~ duhr
#        ))
#
##order of code running is important , this has to be run after above code
#dxc2$xc[is.na(dxc2$xc)] <- 0
#
#dxc2 <- dxc2 %>% filter(t<500)
#
x0 <- coxph(Surv(duhr,cx)~group, data= dx2 )

#xf <- coxph(Surv(duhr,cx)~ age + sex + bmi + apache + ph_median + ferritin_median + ttrg + sigm 
#              +ph_median + tt(ttrg),
#              data = t2cmp, tt=function(x,t,...)x*t)
#
x1 <- survfit(Surv(duhr,cx)~group, data=dx2 )


x2 <- coxph(Surv(duhr,cx)~
                    age + sex + rrt + bmi + group + apache + ttrg + sigm, data = dx2 )


x3 <- coxph(Surv(duhr,cx)~
                    age + sex + rrt + bmi + group + apache + ttrg + sigm + ph_median + ferritin_median, data = dx2 )


x4 <- coxph(Surv(duhr,cx)~
                    age + sex + rrt + group + apache + ttrg + sigm, data = dx2 )

xtest <-cox.zph(x3)
ggcoxzph(xtest)

ggsurvplot(x1,surv.median.line = "hv",pval=TRUE,conf.int = FALSE,title="circuit changes only")

anova(x0,x2,x3)

####

exp <- c(
        "fm",
        "plt1",
        "s7",
        "sp1",
        "sx"
      
)
##
save(list = exp,file = "data/clean/cout.RData")
##
##

