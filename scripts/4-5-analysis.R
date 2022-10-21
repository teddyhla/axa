# SOURCE DATA -------------------------------------------------------------
load(file="data/clean/finalout.RData")


# LIBRARIES ---------------------------------------------------------------


# TTR ---------------------------------------------------------------------



mb1 <- glm(bldtot ~ group, data =dm, family = poisson(link = "log"))
mb2 <- glm(bldtot ~ group + age + apache + wkg + ttrg + sigm + cohort, family = poisson(link="log"),data=dm)
mb3 <- glm(bldtot ~ group + sex + age + apache + wkg + ttrg + sigm + cohort, family = poisson(link="log"),data=dm)
mb4 <- glm(bldtot ~ group + sex + apache + wkg + ttrg + sigm + cohort, family = poisson(link="log"),data=dm)

mb5 <- glm(bldtot ~ group + sex + apache + wkg + ttrg + ecmosb + cohort, family = poisson(link="log"),data=dm)
mb6 <- glm(bldtot ~ group + sex + apache + wkg + ttrg + rl_day, family = poisson(link = "log"),data = dm)
anova(mb1,mb3,mb4,test="Chisq")

# FINAL -------------------------------------------------------------------


