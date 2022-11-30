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

# Function

r <- function (x) {
        return ((exp(x)-1)*100) 
}

Vectorize(r)


# MOULDING ----------------------------------------------------------------

#dm <- left_join(
#        dm,
#        dfcore %>% select(mrn),
#        by = "mrn"
#)

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


#plcor1 <- GGally::ggpairs(labt)

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

bmx <- betareg::betareg(ttrgf ~ 
                                age + sex + bmi + apache + group + rrt +ecmod + ph_median, 
                        data =dm)

bmxi <- betareg::betareg(ttrgf ~
                                 age + sex + bmi + apache + group + rrt + ecmod + ph_median + ferritin_median,
                         data= dm)

lmtest::lrtest(bm6,bm7)

lmtest::lrtest(bm6,bm8)

lmtest::lrtest(bm8,bm9)


bmxii <- betareg::betareg(ttrgf ~ age + sex + wkg + apache + group + rrt + group:ecmod + ferritin_median + ecmod + ph_median, data= dm)
lmtest::lrtest(bm9,bmx)
#plot(bm6) #not bad
#better AIC 
#lets plot for ferritin and ecmod

bmr <- betareg::betareg(ttrgf ~ sex + rrt + ecmod , data= dm)
bmxp <- betareg::betareg(ttrgf ~ 
                                age + sex + bmi + group + rrt +ecmod + ph_median, 
                        data =dm)
bmxa <- betareg::betareg(ttrgf ~ 
                                age + sex + bmi + apache + group + rrt +ecmod, 
                        data =dm)

bmxphi <- betareg::betareg(ttrgf ~ 
                                 age + sex + bmi + apache + group + rrt +ecmod|ecmod, 
                         data =dm)

ttr_finalmod <- bmxphi


bmu <- betareg::betareg(ttrgf ~ group, data= dm)



car::vif(ttr_finalmod)

exp(cbind(coef(bmu),confint(bmu)))

exp(cbind(coef(bmxphi),confint(bmxphi)))

broom::tidy(bmxphi)
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

dm$ttrgf2 <- 100 * dm$ttrgf
ttrpl4 <- ggplot(data = dm, aes(x=ecmod,y=ttrgf,color = rrt))+
        geom_point(alpha = 0.3)+
        geom_smooth(method = MASS::rlm,aes(y=predict(bmxphi,dm)))+
        facet_grid(cols = vars(group))+
        labs(
                x = "Duration on ECMO (days)",
                y = "Time in Therapeutic Range (TTR)",
                title = "Variables affecting Time in Therapeutic Range",
                subtitle = "5-part-1analysis:ttrpl4",
                color = "Renal replacement"
        ) 
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

dk <- dk %>% mutate(
        sigm2 = case_when(
                sigm == 0 ~ 0.01,
                sigm != 0  ~ sigm
        )
)

dk$sigm2 <- log(dk$sigm2)



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


moxbmi <- lm(sigm ~ 
                     age + bmi + apache + sex + group + alb_median + lactate_median + plt_median + rrt + ferritin_median + ecmod, data= dk)

#bmi doesnt matter


moxred <- lm(sigm ~ 
                     group + alb_median + lactate_median, data = dk)

AIC(moh,mox)
anova(mop,mox,test="Chisq")
anova(mox,moxi,moxii,test="Chisq")

AIC(mox,moxred)
anova(mox,moxred,test="Chisq")
#this showed that reduced model was not significantly superior

mox02 <- lm(sigm2~
                    age + bmi + apache + sex + group + alb_median +lactate_median + plt_median + rrt
            + ecmod, data= dk)

mox02red <- lm(sigm2 ~
                       group + ecmod + lactate_median + lactate_median:group,
               data= dk )

mox03 <- lm(sigm2 ~
                    age + bmi + sex + apache + group + lactate_median + rrt + ecmod, data = dk)

mox04 <- lm(sigm2 ~
                    age + sex + bmi + apache +rrt + ecmod + lactate_median + group , data = dk)

AIC(mox02,mox02red)

AIC(mox02,mox04)

lmtest::lrtest(mox02,mox03)
lmtest::lrtest(mox02,mox04)

#thus mox02 is the winner 

car::vif(mox02)

sigm_finalmod <- mox02

moxua <- lm(sigm2 ~ group, data = dk)
#only indepdent variable is tranformed 
#tuhs reported as 

broom::tidy(moxua,conf.int=TRUE)

rep_sig0 <- broom::tidy(mox04,conf.int=TRUE)

rep_sig0 <- rep_sig0 %>%
        mutate(
                cl = (exp(conf.low)-1)*100,
                cm = (exp(estimate)-1)*100,
                ch = (exp(conf.high)-1)*100
        )


exp(cbind(coef(mox04),confint(mox04)))


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
#h7 showed us ecmod, wkg, lactate, ttrg 
h8 <- lm(sqrt(cudose)~ age + wkg + group + ecmod + lactate_median + ttrg + sigm , data= dm)
#h8 showed us wkg, ecmod, lactate, ttrg

h9 <- lm((cudose/wkg)~ age + group + ecmod + lactate_median + ttrg + sigm, data= dm)

dm$pw <- dm$cudose/dm$wkg
h10 <- lm(pw ~ age + sex + group + ecmod + lactate_median + ttrg + sigm , data= dm)
#ecmod or ttrg

#h9 showed us ecmod, ttrg 
#need to think about sigm


#really cudose is quite skewed
hist(dm$cudose)
#takign a log does solves it

dm$cud2 <- log(dm$cudose)

hn <- lm(
        cud2 ~ 
                age + sex + bmi + group + lactate_median + ecmod + ttrg + sigm,
        data = dm
)

hn2 <- lm(
        cud2 ~ 
                age + sex + bmi + group + lactate_median + ecmod + ttrg + sigm + ttrg:group,
        data = dm
)

lmtest::lrtest(hn,hn2)
AIC(hn,hn2)

#UNADJUSTED MODEL

hn0.0 <- lm(cud2 ~ group, data = dm)
hn0.1 <- lm(cud2 ~ ttrg , data = dm)
hn0.2 <- lm(cud2 ~ sigm, data = dm)

hn0.0u <- broom::tidy(hn0.0,conf.int=TRUE)
hn0.1u <- broom::tidy(hn0.1,conf.int=TRUE)
hn0.2u <- broom::tidy(hn0.2,conf.int = TRUE)

hn0.0u <- hn0.0u %>% mutate(
        c1 = r(conf.low),
        cm = r(estimate),
        c2 = r(conf.high)
)

hn0.1u <- hn0.1u %>% mutate(
        c1 = r(conf.low),
        cm = r(estimate),
        c2 = r(conf.high)
)

hn0.2u <- hn0.2u %>% mutate(
        c1 = r(conf.low),
        cm = r(estimate),
        c2 = r(conf.high)
)

#ADJUSTED MODEL

hep_fm <- hn
hep_fm <- broom::tidy(hep_fm,conf.int=TRUE)
hep_fm <- hep_fm %>% mutate(
        c1 = r(conf.low),
        cm = r(estimate),
        c2 = r(conf.high)
)


hpl1 <- ggplot(data = dk, aes(x=sigm,y=log(cudose)))+
        geom_point()+
        geom_smooth(method = lm,aes(y=predict(h7,dk)))+
        #coord_cartesian(ylim = c(0,10),xlim=c(15,25))+
        facet_wrap(~group)

hpl2 <- ggplot(data = dm , aes(x = ttrg , y = pw,color = lactate_median))+
        geom_point()+
        geom_smooth(method = lm)+
        facet_wrap(~group)

hpl3 <- ggplot(data = dm, aes(x=lactate_median, y= pw))+
        geom_point()+
        geom_smooth(aes(y=predict(h3,dm)))+
        facet_wrap(~group)+
        coord_cartesian(xlim = c(0,5))

#looks like a gamma distribution

h4 <- glm(cudose ~ age + wkg + group + ecmod + lactate_median + ttrg + sigm , family = Gamma, data= dm)


# 4. PRESCRIPTION CHANGES -------------------------------------------------

dr <- dm

dr <- left_join(
        dr,
        dheprl %>% select(mrn,runl),
        by = "mrn"
)
dr$runl[is.na(dr$runl)] <- 0

dr3 <- dr
dr3$runl2 <- ifelse(dr3$runl > 25,25,dr3$runl)

r0 <- glm(runl ~ 1 , data= dr,family = poisson(link="log"))

r1 <- glm(runl ~ age + sex + wkg + group + ecmod + lactate_median + ttrg + sigm, family = poisson(link="log"),data= dr)


r2 <- glm(runl ~ age + sex + wkg + group + ecmod + lactate_median + ttrg + sigm + group:sigm + group:ttrg + ttrg:sigm, family = poisson(link="log"),data= dr)

r3 <- glm (runl ~ age + sex + wkg + group + ecmod + lactate_median + ttrg + sigm, offset = log(ecmod), family = poisson(link = "log"),data=dr)
anova(r0,r1,r2,r3,test= "Chisq")

r4 <- glm (runl ~ age + sex + wkg + group + lactate_median + ttrg + sigm, offset = log(ecmod), family = poisson(link = "log"),data=dr)

r6 <- MASS::stepAIC(r4)

r6 <- glm(runl2 ~ age + wkg + group + lactate_median + ttrg + sigm , offset = log(ecmod),family = quasipoisson(link="log"),data=dr3)

r7 <- glm(runl2 ~ age + wkg + group + lactate_median + ttrg + sigm , offset = log(ecmod),family = quasipoisson(link="log"),data=dr3)

r8 <- glm(runl2 ~ age + wkg + group + lactate_median + ttrg + sigm + (sigm*sigm) , offset = log(ecmod),family = quasipoisson(link="log"),data=dr3)

#5974944J leverage

#dr2 <- dr %>% filter(!mrn == "5974944J")
#r7 <- glm(runl ~ age + wkg + group + lactate_median + ttrg + sigm, offset = log(ecmod),family = quasipoisson(link="log"),data=dr2)
#doesnt make a huge difference. so let's just fit a normal

r5 <- glm (runl ~ age + sex + wkg + group + lactate_median + ttrg + sigm + group:ttrg, offset = log(ecmod), family = quasipoisson(link = "log"),data=dr)


r50 <- glm (runl ~ age + sex + bmi + group + lactate_median + ttrg + sigm + group:ttrg, offset = log(ecmod), family = quasipoisson(link = "log"),data=dr)




dr %>% 
        select(group,runl) %>% 
        group_by(group) %>% 
        summarise(
                one = quantile(runl,prob= c(0.25)),
                med = median(runl),
                two = quantile(runl,prob = c(0.75)))

rpl1 <- ggplot(data = dr, aes(x=sigm, y= runl))+
        geom_point()+
        geom_smooth(method = loess,aes(predict.glm(r7,dr3)))+
        coord_cartesian(xlim = c(0,2),ylim=c(0,40))+
        facet_wrap(~group)+
        labs(
                title = "Cumulative prescrition changes is increased with higher variability"
        )

m <- sjPlot::plot_model(r7,
                   title = "Cumulative Prescription changes adjusted for duration",
                   show.p = TRUE,
                   value.size = 3,digits = 3)

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
#mb6 <- glm(bldtot ~ group + sex + age + apache + wkg +ttrg+ groupgr:ttrg+ sigm + group:sigm + ecmod, data = dm, family = quasipoisson(link="log"))


# toth --------------------------------------------------------------------

dh <- dm %>% select(-cudose)

h0 <- glm (toth ~ age + sex + apache + wkg + rrt + ferritin_median + group + sigm, offset = log(ecmod), data= dh, family = quasipoisson(link="log"))


h1 <- glm (toth ~ ttrg + group + sigm + ttrg:sigm + group:ttrg + group:sigm, offset = log(ecmod), data= dh, family = quasipoisson(link="log"))


h2 <- glm (toth ~ ttrg + group + sigm + age + sex, offset = log(ecmod), data= dh, family = quasipoisson(link="log"))

h2i <- glm (toth ~ ttrg + group + sigm + age + apache +sex, offset = log(ecmod), data= dh, family = poisson(link="log"))
h2ii <- glm(toth ~ age + sex + apache + group + age:group + ttrg + group:ttrg + sigm + sigm:group + sigm:age, 
            offset = log(ecmod),
            family = poisson(link = "log"),
            data = dh)

ht <- glm(toth ~ ttrg, offset = log(ecmod),data= dh, family = poisson(link="log"))

h <- sjPlot::plot_model(h2i,title = "Model Coefficients for Haemorrhagic Complications ")

h<- h+ylim(0,1.5)
anova(h0,h1,h2,h2i)


#make graph 1

# G1 ----------------------------------------------------------------------

dz <- dm %>% select(group,ttrg,sigm)
dz$ttrg <- 100 * dz$ttrg
levels(dz$group) <- c("aPTTr monitoring group","Anti-Xa monitoring group")



g1 <- ggplot(data = dz, aes(x = group, y = ttrg,color=group)) +
        ggdist::stat_halfeye(
                adjust = .5,
                width = .6,
                .width = 0,
                justification = -.2,
                point_colour = NA
        )+
        geom_boxplot(
                width= .15,
                outlier.shape = NA
        )+ gghalves::geom_half_point(
                side = "1",
                range_scale = .4,
               # alpha = .3
        ) +
        coord_cartesian(xlim= c(1.2,NA),clip = "off")+
        scale_x_discrete()+
        scale_y_continuous(breaks = seq(0,100,10))+
        labs(
                title = "Time in Therapeutic Ranges",
                x = "\n Monitoring Group",
                y = "\n Time in Therapeutic Range (%)"
        )+
        theme_minimal()+
        theme(legend.position ="none",
              text = element_text(size = 15))

g2 <- ggplot(data = dz, aes(x = group, y = sigm,color=group)) +
        ggdist::stat_halfeye(
                adjust = .5,
                width = .6,
                .width = 0,
                justification = -.2,
                point_colour = NA
        )+
        geom_boxplot(
                width= .15,
                outlier.shape = NA
        )+ gghalves::geom_half_point(
                side = "1",
                range_scale = .4,
                #alpha = .3
        ) +
        coord_cartesian(xlim= c(1.2,NA),clip = "off")+
        scale_x_discrete()+
        scale_y_continuous(limits = c(0,2))+
        labs(
                title = "Variability of Anticoagulation",
                x = "\n Monitoring Group",
                y = "\n Variability of Anticoagulation"
        )+
        theme_minimal()+
        theme(legend.position ="none",
              text = element_text(size = 15))

fig1 <- cowplot::plot_grid(g1,g2)


ggsave("products/manuscript/fig1.eps",plot = fig1, device = "eps",dpi = 1200)


#
