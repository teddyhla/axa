# SOURCE DATA -------------------------------------------------------------
load(file="data/clean/finalout.RData")


# LIBRARIES ---------------------------------------------------------------
library(ggplot2)
library(lme4)
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


# MOULDING ----------------------------------------------------------------

dm <- left_join(
        dm,
        dfcore %>% select(mrn,aki),
        by = "mrn"
)

dm <- left_join(
        dm,
        dcumhep %>% select(mrn,cudose),
        by = "mrn"
)

# TTR ---------------------------------------------------------------------
#no missing values
#ttr between the two is the same.

#no statistical difference.

dm$ttrgf <- ((dm$ttrg * 253) + 0.5)/254  

f1 <- ggplot(data= dm, aes(x= ttrgf,color = group)) + 
        geom_histogram(aes(y=..density..),fill = "white")+
        geom_vline(aes(xintercept=mean(ttrgf)),
                   color="blue", linetype="dashed", size=1)+
        facet_wrap(~group)


bm1 <- betareg::betareg(ttrgf ~  group + age + wkg + apache +sex + ferritin_mean+ ecmod +1|ecmosb, data = dm)

bm3 <- betareg::betareg(ttrgf ~  group + age + wkg + apache +sex + ecmod +1|ecmosb, data = dm)
#pseudo R2 0.146
 

# 2. VARIABILITY ----------------------------------------------------------
dk <- dm %>% filter(sigm < 100)
mo1 <- lm(sigm ~ age + apache + sex + group + ttrg + ferritin_mean + wkg + ecmod,data= dm)


# 3. HEPARIN  -------------------------------------------------------------
h1 <- lm(cudose ~ ttrg + sigm + group + age + wkg + ecmod, data= dm)
h2 <- lm(cudose ~ ttrg + sigm + group + age + wkg + group:sigm + group:ttrg + ttrg:sigm + ecmod, data= dm)

h3 <- lm(cudose ~  sigm + group + age + wkg + group:sigm + group:ttrg + ttrg:sigm + ecmod, data= dm)

# 4. PRESCRIPTION CHANGES -------------------------------------------------


# 5. BLOOD PRODUCTS ----------------------------------------------------------

#age, apache, weight, bmi, mean_platelets, fibrinogen , neutrophils, ferritin
#ck,#crp #pct #albumin ,creat, eGFR, bicarb, ph,rrt

bp1 <- glm(bldtot ~ age + apache + wkg + aki + ferritin_mean + ph_mean + cudose , data= dm, family = quasipoisson(link = "log"))

bp2 <- glm(bldtot ~ apache + aki + cudose + ttrg + sigm + group + ttrg:sigm + group:sigm + offset(log(ecmod)), data= dm, family = poisson(link="log"))

#mb2 <- glm(bldtot ~ group + sex + age + apache + wkg + ttrg + sigm + ecmod, data = dm, family = quasipoisson(link="log"))
#mb1 <- glm(bldtot ~ group + sex + age + apache + wkg + ttrg + sigm + ecmod, data = dm, family = poisson(link="log"))
#mb3 <- glm(bldtot ~ group + sex + age + apache + wkg + sigm + ecmod, data = dm, family = poisson(link="log"))
#mb4 <- glm(bldtot ~ group + sex + age + apache + wkg + sigm + group:sigm + ecmod, data = dm, family = poisson(link="log"))
#mb5 <- glm(bldtot ~ group + sex + age + apache + wkg +ttrg+ group:ttrg+ sigm + group:sigm + ecmod, data = dm, family = poisson(link="log"))
#mb6 <- glm(bldtot ~ group + sex + age + apache + wkg +ttrg+ group:ttrg+ sigm + group:sigm + ecmod, data = dm, family = quasipoisson(link="log"))


