# SOURCE DATA -------------------------------------------------------------
load(file="data/clean/finalout.RData")


# LIBRARIES ---------------------------------------------------------------
library(ggplot2)
library(lme4)
# NOTE --------------------------------------------------------------------

#need to adjust for 
#age, sex,  apache, weight, bmi, ecmod, 
# mean_platelets, mean_neutrophils,ferritin_mean, mean_CRP,mean_procalcitonin
# mean_albumin , mean _creat, minimum bicarb, mean pH 

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


# CORRELATED VALUES -------------------------------------------------------
labt <- dm %>% 
        select(
                plt_mean, neut_mean,ferritin_mean,crp_mean,pct_mean,
                alb_mean,creat_mean,bicarb_mean,ph_mean,ck_mean,apache
        )


plcor1 <- GGally::ggpairs(labt)

# TTR ---------------------------------------------------------------------
#no missing values
#ttr between the two is the same.
#should include no of tests as feature.
#no statistical difference.

dm$ttrgf <- ((dm$ttrg * 253) + 0.5)/254  #this is as per Smithson and Vekuilen 2006

f1 <- ggplot(data= dm, aes(x= ttrgf,color = group)) + 
        geom_histogram(aes(y=..density..),fill = "white")+
        geom_vline(aes(xintercept=mean(ttrgf)),
                   color="blue", linetype="dashed", size=1)+
        facet_wrap(~group)

bm0 <- betareg::betareg(ttrgf ~ 1, data= dm)

bm1 <- betareg::betareg(ttrgf ~  group + age + wkg + apache +sex + ferritin_mean+ crp_mean+ ph_mean +bicarb_mean + ecmod, data = dm)

bm3 <- betareg::betareg(ttrgf ~ group + age + wkg + apache + group:ecmod +sex + ferritin_mean + ecmod +1|ecmod , data = dm)

lmtest::lrtest(bm0,bm1,bm3)
AIC(bm0,bm1,bm3)
 
#now lets fit with link = log log
bm4 <- betareg::betareg(ttrgf ~ group + age + wkg + apache + group:ecmod+sex + ferritin_mean + ecmod +1|ecmod , data = dm, link = "loglog")
summary(bm3)$pseudo.r.squared
#0.08
summary(bm4)$pseudo.r.squared
#0.06
#doesnt make an improvement.
#thus stick with original logit.

bm6 <- betareg::betareg(ttrgf ~ group + age + wkg + apache + group:ecmod +sex + ferritin_mean + ecmod +1|group:ecmod , data = dm)
summary(bm6)$pseudo.r.squared
AIC(bm3,bm6)


bm7 <- betareg::betareg(ttrgf ~ group + age + wkg + apache + group:ecmod + lactate_mean +sex + ferritin_mean + ecmod +1|group:ecmod , data = dm)
summary(bm7)$pseudo.r.squared

lmtest::lrtest(bm6,bm7)
plot(bm6) #not bad
#better AIC 
#lets plot for ferritin and ecmod

ttr_finalmod <- bm6

car::vif(ttr_finalmod)
#no VIF

ttrpl1 <- ggplot(data = dm, aes(x=ecmod,y=ttrgf))+
        geom_point()+
        geom_smooth(aes(y=predict(bm6,dm)))+
        facet_wrap(~group)

ttrpl2 <- ggplot(data = dm, aes(x=ferritin_mean,y=ttrgf))+
        geom_point()+
        geom_smooth(aes(y=predict(bm6,dm)))+
        facet_wrap(~group)

ttrpl3 <- ggplot(data = dm, aes(x=age,y=ttrgf))+
        geom_point()+
        geom_smooth(aes(y=predict(bm6,dm)))+
        facet_wrap(~group)
#should cap the outlier values!
# 2. VARIABILITY ----------------------------------------------------------

mo1 <- lm(sigm ~ age + apache + sex + group + ttrg + ferritin_mean + wkg + ecmod,data= dm)
#model diagnstics showed "21" is a big problem
dk <- dm %>% filter(sigm < 100)
dk2 <- dk %>% select(-mrn) %>% drop_na()
dk2 <- dk %>%
        mutate(lact2 = case_when(
                lactate_mean < 7 ~ lactate_mean,
                lactate_mean > 7 ~ 7
        ))

dkr <- dk[-c(1,8,60),]

##


# tests -------------------------------------------------------------------



mo2 <- lm(sigm ~ age + apache + sex + group + ttrg + ferritin_mean + wkg + ecmod,data= dk)
#mo

mo3 <- lm(sigm ~ age + apache + sex + group + crp_mean + ph_mean + wkg + ecmod, data = dk)

mo4 <- lm(sigm ~ apache + group + group:apache, data = dk)
#Rsq 0.18

mo5 <- lm(sigm ~., data =dk2)

#seems need to add "aki " , latate mean , bicarb mean , platelet mean, fib, wkg, platelet
mo6 <- MASS::stepAIC(mo5)
#multipleR2 0.49, p val sig

mo7 <- lm(sigm ~ age + aki + apache + group + sex + wkg + lactate_mean + bicarb_mean + plt_mean + alb_mean + group:apache, data= dk)


mo8 <- lm(sigm ~ aki + group + wkg + lactate_mean + bicarb_mean + alb_mean + group:lactate_mean, data= dk)
#mo
mo9 <- lm(sigm ~ wkg + group + lactate_mean + alb_mean + group:lactate_mean + group:wkg , data = dk)

#observations 60,20,8

Hmisc::describe(dk)

#this output showed that 

mox <- lm(sigm ~ wkg + group + lact2 + alb_mean + group:lact2 , data= dk)
mop <- lm (sigm ~ wkg + group + alb_mean + plt_mean + ph_mean + group:ph_mean, data = dk)

moh <- lm(sigm ~ wkg + group + plt_mean + ferritin_mean + alb_mean + aki , data= dk)
#mo


moh2 <- lm(sigm ~ wkg + group + plt_mean + ferritin_mean + alb_mean + aki , data= dkr)
#mo
moh3 <- lm(sigm ~wkg  + group + plt_mean + ferritin_mean + alb_mean + aki +ecmod , data= dkr)

mof <- lm(sigm ~ age + wkg + apache + sex + ferritin_mean + group + alb_mean + group:alb_mean + group:ferritin_mean, data= dk)

mog<- lm(sigm ~ age + wkg + apache + sex  + group + alb_mean + group:alb_mean , data= dk)


moh<- lm(sigm ~ age + wkg + apache + sex + group + alb_mean + lactate_mean + plt_mean + creat_mean + ferritin_mean + ecmod, data= dk)


car::vif(moh)

sigm_finalmod <- moh
#no vif 
sigpl1 <- ggplot(data = dk, aes(x=alb_mean,y=sigm))+
        geom_point()+
        geom_smooth(aes(y=predict(mo2,dk)))+
        coord_cartesian(ylim = c(0,10),xlim=c(15,25))+
        facet_wrap(~group)



# 3. HEPARIN  -------------------------------------------------------------
h0 <- lm(cudose ~ ., data= dk2)

h1 <- lm(cudose ~ age + wkg + apache + sex + group + alb_mean + lactate_mean + ecmod, data = dm)

h2 <- lm(cudose ~ age + wkg + apache + sex + group + alb_mean + lactate_mean + ecmod + , data = dm)

h3 <- lm(cudose~ age + wkg + group + ecmod + lactate_mean + ttrg + sigm , data= dm)

h5 <- lm(cudose~ age + wkg + group + ecmod + lactate_mean + ttrg + sigm +ecmod:wkg, data= dm)


h7 <- lm(log(cudose)~ age + wkg + group + ecmod + lactate_mean + ttrg + sigm , data= dm)


hpl1 <- ggplot(data = dk, aes(x=ttrg,y=cudose))+
        geom_point()+
        geom_smooth(aes(y=predict(h3,dk)))+
        #coord_cartesian(ylim = c(0,10),xlim=c(15,25))+
        facet_wrap(~group)

hpl2 <- ggplot(data = dk , aes(x = cudose))+
        geom_density()+
        facet_wrap(~group)

hpl3 <- ggplot(data = dk, aes(x=wkg, y= cudose))+
        geom_point()+
        geom_smooth(aes(y=predict(h3,dk)))+
        facet_wrap(~group)

#looks like a gamma distribution

h4 <- glm(cudose ~ age + wkg + group + ecmod + lactate_mean + ttrg + sigm , family = Gamma, data= dm)


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


