# SOURCE DATA -------------------------------------------------------------
load(file="data/clean/finalout.RData")


# LIBRARIES ---------------------------------------------------------------


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



mb1 <- glm(bldtot ~ group, data =dm, family = poisson(link = "log"))
mb2 <- glm(bldtot ~ group + age + apache + wkg + ttrg + sigm + cohort, family = poisson(link="log"),data=dm)
mb3 <- glm(bldtot ~ group + sex + age + apache + wkg + ttrg + sigm + cohort, family = poisson(link="log"),data=dm)
mb4 <- glm(bldtot ~ group + sex + apache + wkg + ttrg + sigm + cohort, family = poisson(link="log"),data=dm)

mb5 <- glm(bldtot ~ group + sex + apache + wkg + ttrg + ecmosb + cohort, family = poisson(link="log"),data=dm)
mb6 <- glm(bldtot ~ group + sex + apache + wkg + ttrg + rl_day, family = poisson(link = "log"),data = dm)
anova(mb1,mb3,mb4,test="Chisq")

# FINAL -------------------------------------------------------------------



