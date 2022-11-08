# SOURCE DATA -------------------------------------------------------------
load(file="data/clean/finalout.RData")


# LIBRARIES ---------------------------------------------------------------
library(ggplot2)
library(lme4)
# NOTE --------------------------------------------------------------------

#need to adjust for 
#age, sex,  apache, weight, bmi, ecmod, 
# median_platelets, median_neutrophils,ferritin_median, median_CRP,median_procalcitonin
# median_albumin , median _creat, minimum bicarb, median pH 

#targets
#1. ttr - improvement in favour of axa but no statistical significance
#2. var - significant improvement in fav axa and stats significance 
#3. heparin per kilogram per day - in favour of apttr but needs looking 
#4. no of heparin prescription changes - in favour of axa and stats significance 
#5. total blood products


# MOULDING ----------------------------------------------------------------

dm <- left_join(
        dm,
        dfcore %>% select(mrn,rrt),
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
                plt_median, neut_median,ferritin_median,crp_median,pct_median,
                alb_median,creat_median,bicarb_median,ph_median,ck_median,apache
        )


plcor1 <- GGally::ggpairs(labt)

# TTR ---------------------------------------------------------------------
#no missing values
#ttr between the two is the same.
#should include no of tests as feature.
#no statistical difference.

dm$ttrgf <- ((dm$ttrg * 253) + 0.5)/254  #this is as per Smithson and Vekuilen 2006

f1 <- ggplot(data= dm, aes(x= ttrgf,color = group)) + 
        geom_density()+
        geom_vline(aes(xintercept=median(ttrgf)),
                   color="blue", linetype="dashed", size=1)+
        facet_wrap(~group)

f2 <- ggplot(data = dm, aes(x= group, y= ttrgf))+
        geom_point()+
        geom_jitter()

f3 <- ggplot(data = dm, aes(x=group, y= ttrgf))+
        ggdist::stat_halfeye(
                adjust = .5, 
                width = .6,
                justification = - .3,
                .width = 0,
                point_colour = NA
        ) +
        geom_boxplot(
                width = .25,
                outlier.color = NA,
        ) +
        geom_point(
                size = 1.3,
                alpha = .3,
                position = position_jitter(
                        seed = 1, width = .1
                )
        )

bm0 <- betareg::betareg(ttrgf ~ 1, data= dm)

bm1 <- betareg::betareg(ttrgf ~  group + age + wkg + apache +sex + ferritin_median+ crp_median+ ph_median +bicarb_median + ecmod, data = dm)

bm3 <- betareg::betareg(ttrgf ~ group + age + wkg + apache + group:ecmod +sex + ferritin_median + ecmod +1|ecmod , data = dm)

lmtest::lrtest(bm0,bm1,bm3)
AIC(bm0,bm1,bm3)
 
#now lets fit with link = log log
bm4 <- betareg::betareg(ttrgf ~ group + age + wkg + apache + group:ecmod+sex + ferritin_median + ecmod +1|ecmod , data = dm, link = "loglog")
summary(bm3)$pseudo.r.squared
#0.08
summary(bm4)$pseudo.r.squared
#0.06
#doesnt make an improvement.
#thus stick with original logit.

bm6 <- betareg::betareg(ttrgf ~ group + age + wkg + apache + group:ecmod +sex + ferritin_median + ecmod +1|group:ecmod , data = dm)
summary(bm6)$pseudo.r.squared
AIC(bm3,bm6)


bm7 <- betareg::betareg(ttrgf ~ group + age + wkg + apache + group:ecmod + lactate_median +sex + ferritin_median + ecmod +1|group:ecmod , data = dm)

bm8 <- betareg::betareg(ttrgf ~ age + sex + wkg + apache + group + group:ecmod + ferritin_median + ecmod , data=dm)

summary(bm7)$pseudo.r.squared
summary(bm8)$pseudo.r.squared
#0.093

bm9 <- betareg::betareg(ttrgf ~ age + sex + wkg + apache + group + rrt + group:ecmod + ferritin_median + ecmod, data= dm)

lmtest::lrtest(bm6,bm7)

lmtest::lrtest(bm6,bm8)

lmtest::lrtest(bm8,bm9)


bmx <- betareg::betareg(ttrgf ~ age + sex + wkg + apache + group + rrt + group:ecmod + ferritin_median + ecmod + ph_median, data= dm)
lmtest::lrtest(bm9,bmx)
#plot(bm6) #not bad
#better AIC 
#lets plot for ferritin and ecmod

ttr_finalmod <- bm9

bmu <- betareg::betareg(ttrgf ~ group, data= dm)


car::vif(ttr_finalmod)
#no VIF

ttrpl1 <- ggplot(data = dm, aes(x=ecmod,y=ttrgf,color = rrt))+
        geom_point(alpha = 0.3)+
        geom_smooth(method = lm,aes(y=predict(bm9,dm)))+
        facet_wrap(~group)

ttrpl2 <- ggplot(data = dm, aes(x=ferritin_median,y=ttrgf))+
        geom_point()+
        geom_smooth(method = lm,aes(y=predict(bm8,dm)))+
        coord_cartesian(xlim = c(0,5000))+
        facet_wrap(~group )

ttrpl3 <- ggplot(data = dm, aes(x=age,y=ttrgf))+
        geom_point()+
        geom_smooth(aes(y=predict(bm9,dm)))+
        facet_wrap(~group)

ttrpl4 <- ggplot(data = dm, aes(x=ecmod,y=ttrgf,color = rrt))+
        geom_point(alpha = 0.3)+
        geom_smooth(method = lm,aes(y=predict(bm9,dm)))+
        facet_wrap(~sex)
#should cap the outlier values!


# 2. VARIABILITY ----------------------------------------------------------

mo1 <- lm(sigm ~ age + apache + sex + group + ttrg + ferritin_median + wkg + ecmod,data= dm)
#model diagnstics showed "21" is a big problem
dk <- dm %>% filter(sigm < 100)
dk2 <- dk %>% select(-mrn) %>% drop_na()
dk2 <- dk %>%
        mutate(lact2 = case_when(
                lactate_median < 7 ~ lactate_median,
                lactate_median > 7 ~ 7
        ))

dkr <- dk[-c(1,8,60),]

##


# tests -------------------------------------------------------------------



mo2 <- lm(sigm ~ age + apache + sex + group + ttrg + ferritin_median + wkg + ecmod,data= dk)
#mo

mo3 <- lm(sigm ~ age + apache + sex + group + crp_median + ph_median + wkg + ecmod, data = dk)

mo4 <- lm(sigm ~ apache + group + group:apache, data = dk)
#Rsq 0.18

mo5 <- lm(sigm ~., data =dk2)

#seems need to add "rrt " , latate median , bicarb median , platelet median, fib, wkg, platelet
#mo6 <- MASS::stepAIC(mo5)
#multipleR2 0.49, p val sig

mo7 <- lm(sigm ~ age + rrt + apache + group + sex + wkg + lactate_median + bicarb_median + plt_median + alb_median + group:apache, data= dk)


mo8 <- lm(sigm ~ rrt + group + wkg + lactate_median + bicarb_median + alb_median + group:lactate_median, data= dk)
#mo
mo9 <- lm(sigm ~ wkg + group + lactate_median + alb_median + group:lactate_median + group:wkg , data = dk)

#observations 60,20,8

Hmisc::describe(dk)

#this output showed that 

#mox <- lm(sigm ~ wkg + group + lact2 + alb_median + group:lact2 , data= dk)
mop <- lm (sigm ~ wkg + group + alb_median + plt_median + ph_median + group:ph_median, data = dk)

moh <- lm(sigm ~ wkg + group + plt_median + ferritin_median + alb_median + rrt , data= dk)
#mo


moh2 <- lm(sigm ~ wkg + group + plt_median + ferritin_median + alb_median + rrt , data= dkr)
#mo
moh3 <- lm(sigm ~wkg  + group + plt_median + ferritin_median + alb_median + rrt +ecmod , data= dkr)

mof <- lm(sigm ~ age + wkg + apache + sex + ferritin_median + group + alb_median + group:alb_median + group:ferritin_median, data= dk)

mog<- lm(sigm ~ age + wkg + apache + sex  + group + alb_median + group:alb_median , data= dk)


moh<- lm(sigm ~ age + wkg + apache + sex + group + alb_median + lactate_median + plt_median + creat_median + ferritin_median + ecmod, data= dk)


mox <- lm(sigm ~ age + wkg + apache + sex + group + alb_median + lactate_median + plt_median + rrt + ferritin_median + ecmod, data= dk)


moxi <- lm(sigm ~ age + wkg + apache + sex + group + alb_median + lactate_median + group:alb_median + lactate_median:alb_median + rrt  + ecmod, data= dk)



moxii <- lm(sigm ~ age + wkg + apache + sex + group + alb_median + lactate_median + group:alb_median + group:lactate_median + lactate_median:alb_median + rrt  + ecmod, data= dk)

AIC(moh,mox)
anova(mop,mox,test="Chisq")
anova(mox,moxi,moxii,test="Chisq")

#thus mox is the winner 
car::vif(mox)

sigm_finalmod <- mox
#no vif 
sigpl1 <- ggplot(data = dk, aes(x=lactate_median,y=sigm,color = sex))+
        geom_point()+
        geom_smooth(method = lm,aes(y=predict(mox,dk)))+
        coord_cartesian(ylim = c(0,10),xlim= c(0,5))+
        facet_wrap(~group)



# 3. HEPARIN  -------------------------------------------------------------
h0 <- lm(cudose ~ ., data= dk2)
ho2 <- lm(cudose ~ 1,data= dk2)

h1 <- lm(cudose ~ age + wkg + apache + sex + group + alb_median + lactate_median + ecmod, data = dm)

h2 <- lm(cudose ~ age + wkg + apache + sex + group + alb_median + lactate_median + ecmod , data = dm)

h3 <- lm(cudose~ age + wkg + group + ecmod + lactate_median + ttrg + sigm , data= dm)

h5 <- lm(cudose~ age + wkg + group + ecmod + lactate_median + ttrg + sigm +ecmod:wkg, data= dm)


h7 <- lm(log(cudose)~ age + wkg + group + ecmod + lactate_median + ttrg + sigm , data= dm)

h8 <- lm(sqrt(cudose)~ age + wkg + group + ecmod + lactate_median + ttrg + sigm , data= dm)

#need to think about sigm

hpl1 <- ggplot(data = dk, aes(x=sigm,y=log(cudose)))+
        geom_point()+
        geom_smooth(method = lm,aes(y=predict(h7,dk)))+
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

h4 <- glm(cudose ~ age + wkg + group + ecmod + lactate_median + ttrg + sigm , family = Gamma, data= dm)


# 4. PRESCRIPTION CHANGES -------------------------------------------------

dr <- dm

dr <- left_join(
        dr,
        dheprl %>% select(mrn,runl),
        by = "mrn"
)

r0 <- glm(runl ~ 1 , data= dr,family = poisson(link="log"))

r1 <- glm(runl ~ age + sex + wkg + group + ecmod + lactate_median + ttrg + sigm, family = poisson(link="log"),data= dr)


r2 <- glm(runl ~ age + sex + wkg + group + ecmod + lactate_median + ttrg + sigm + group:sigm + group:ttrg + ttrg:sigm, family = poisson(link="log"),data= dr)

r3 <- glm (runl ~ age + sex + wkg + group + ecmod + lactate_median + ttrg + sigm, offset = log(ecmod), family = poisson(link = "log"),data=dr)
anova(r0,r1,r2,r3,test= "Chisq")

rpl1 <- ggplot(data = dr, aes(x=sigm, y= runl,color))+
        geom_point()+
        geom_smooth(method = lm,aes(y=predict(r2,dr)))+
        coord_cartesian(xlim = c(0,3),ylim = c(0,20))+
        facet_wrap(~group)

#r2 is the winner

# 5. BLOOD PRODUCTS ----------------------------------------------------------

#age, apache, weight, bmi, median_platelets, fibrinogen , neutrophils, ferritin
#ck,#crp #pct #albumin ,creat, eGFR, bicarb, ph,rrt

bp1 <- glm(bldtot ~ age + apache + wkg + rrt + ferritin_median + ph_median + cudose , data= dm, family = quasipoisson(link = "log"))

bp2 <- glm(bldtot ~ apache + rrt + cudose + ttrg + sigm + group + ttrg:sigm + group:sigm + offset(log(ecmod)), data= dm, family = poisson(link="log"))

#mb2 <- glm(bldtot ~ group + sex + age + apache + wkg + ttrg + sigm + ecmod, data = dm, family = quasipoisson(link="log"))
#mb1 <- glm(bldtot ~ group + sex + age + apache + wkg + ttrg + sigm + ecmod, data = dm, family = poisson(link="log"))
#mb3 <- glm(bldtot ~ group + sex + age + apache + wkg + sigm + ecmod, data = dm, family = poisson(link="log"))
#mb4 <- glm(bldtot ~ group + sex + age + apache + wkg + sigm + group:sigm + ecmod, data = dm, family = poisson(link="log"))
#mb5 <- glm(bldtot ~ group + sex + age + apache + wkg +ttrg+ group:ttrg+ sigm + group:sigm + ecmod, data = dm, family = poisson(link="log"))
#mb6 <- glm(bldtot ~ group + sex + age + apache + wkg +ttrg+ group:ttrg+ sigm + group:sigm + ecmod, data = dm, family = quasipoisson(link="log"))


