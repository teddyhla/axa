
# DEPENDENCIES ------------------------------------------------------------


# DATA SOURCE -------------------------------------------------------------
load(file="data/clean/out.RData")
library(ggplot2)
# STRUCTURE OF ANALYSIS ---------------------------------------------------

#Note we will use person-days

# Fihn's method A (Ibrahim 2013)

#Variance growth rate Fihn (method A)
#lhs = sigma 2 = variance
#$$\sigma^2 = \frac{1}{n}* \sum \limits_{i=1}^{n}\frac {(INR_i - target_i)^2}{\tau_i}$$


#Quality markers are defined as :-
# time to first threapeutic range
# time in therapeutic range but chosen for - time "above"
# both individually independently and as a feature
# percent time in TTR as well as proportion in range
# INR variability using Fihn method 
# how about scaling and centering as per 


# PERSON DAYS IN GROUPS ---------------------------------------------------

dfcore %>% 
        group_by(group)%>% 
        summarise(n= n(),persondays = sum(ecmod))

# BLOOD PRODUCTS COMPARISON -----------------------------------------------
## 4.1. Products transfused table -----------------------------------------

#let's demonstrate that there are differences in blood products tx using simple 
# table - labelled as "pt1", ctotcryo = cumulative total cryo and so on 

#Assumption : each row is for 1 unit of blood products ONLY.

sum(tf$t_form < 1 ,na.rm=T)
sum(tf$t_form > 1, na.rm = T)
#assumption passed 


pt1 <- tf %>%
        select(mrn,group,s_label,nd) %>%
        group_by(group) %>% 
        summarise(
                ctotcryo = sum(s_label == "cryo"),
                ctotffp = sum(s_label == "ffp"),
                ctotplt = sum(s_label == "plt"),
                ctotrbc = sum(s_label == "prbcs")
        )

#lets perform statistical test to confirm pt1 
#chisq test due to proportions in group.
chisq.test(xtabs(~group + s_label,data = tf))
#lets test specifically for rbc by preparing a new datframe "t1" for testing1
t1 <- tf %>% select(group,s_label) %>% filter(s_label == "prbcs")
#lets check this t1 and it checked out with findings from pt1
summary(t1) 

### Statistical Test ------------------------------------------------
#note chi sq doesnt like small things 
test1 <- chisq.test(table(t1$s_label == "prbcs",t1$group))

## 4.2. ADJUSTED products table -------------------------------------------

#We need to adjust as numbers and duration on ecmo VASTLY differ in the two group.

tf$ecmod <- as.numeric(tf$ecmod)

pt2 <- tf %>% 
        select(mrn,s_label,group,ecmod)%>% 
        group_by(mrn) %>% 
        #add all the blood products transfused for each type per patient
        summarise (
                cryopp = sum(s_label == "cryo"),
                ffppp = sum(s_label == "ffp"),
                pltpp = sum(s_label == "plt"),
                rbcpp = sum(s_label == "prbcs")
        ) %>% 
        ungroup()

pt2 <- left_join(
        pt2,
        dfcore %>% select(mrn,ecmod,group),
        by = "mrn"
)

pt2$ecmod <- as.numeric(pt2$ecmod)

#devide this by the duratin of each patient ecmo run but to make small
#no visible, we multiple by 1000
pt2 <- pt2 %>% 
        group_by(mrn) %>% 
        transmute(
                cryopp = (cryopp / ecmod)*1000,
                ffppp = (ffppp / ecmod)*1000,
                pltpp = (pltpp / ecmod)*1000,
                rbcpp = (rbcpp / ecmod)*1000
        ) %>% 
        ungroup () 

pt2 <- left_join (
        pt2,
        dfcore %>% select(mrn,group),
        by = "mrn"
)


pt2 %>% group_by(group) %>% group_map(~summary(.x))

#lets check this by groupwise 
sum(pt2$rbcpp[pt2$group == "gapt"])/63
#essentially summing all the rbcpp for group apt and dividing by n.
#it checks out


### Statistical Test  -----------------------------------------------
test2 <- wilcox.test(rbcpp ~ group, data = pt2)


## 4.3. UNIT "TIME" ---------------------------------------------------------

#Is it justifiable to use "calendar day" as a unit of time?
pt3 <- dg %>% 
        group_by(mrn) %>% 
        summarise(
                maxd = max(ecmod),
                dtx = sum(totall != 0)
        ) %>% 
        mutate(prpn = dtx/maxd)

quantile(pt3$prpn)
#this showed that over 75% doesnt receive daily blood products. 
#thus is okay to use calendar day as a unit of time.




## 4.4.  COMPLICATIONS  -------------------------------------------------

#lets look at if complications are the same between groups. 
br <- tdf %>% 
        select(mrn,group,tot,tot_circhange,ecmod)%>% 
        mutate( ecmod = as.numeric(ecmod)) %>% 
        mutate(tot = as.numeric(tot)) %>%
        mutate(tot_circhange = as.numeric(tot_circhange)) %>%
        mutate(across(where(is.numeric),~replace_na(.x,0))) %>% 
        group_by(mrn)%>%
        mutate(
                bter = tot / ecmod,
                ccr = tot_circhange / ecmod
        ) %>% 
        ungroup()

xtabs(~tot+group , data = tdf)
#this code showed that there are 
br %>% group_by(group) %>% summarise(mean(bter),median(bter),mean(ccr),median(ccr))

## Blood products model ----------------------------------------------------
# 5.0. MODEL 1 --------------------------------------------------------------


#however Var seems to be greater than Mean in where 0 are counted and not counted
#likely OVERDISPERSION 
m1 <- glm(totall ~ group, data =dg, family = poisson(link = "log"))
m2 <- glm(totall ~ group, data =dg, family = quasipoisson(link = "log"))
#univariate analysis looks promising.

#lets try a logistic regression to see if it stands.
dm <- dg
dm$y <- ifelse(dm$totall == 0 , 0,1)
dm <- left_join(
        dm,
        dfcore %>% select(mrn,age,surv_ecmo,wkg,apache),by="mrn"
)

m3 <- glm (y ~ group + age + surv_ecmo + wkg + apache + group*surv_ecmo, data = dm, family = binomial)


#lets also see if this remains true for RBCs 

m4 <- glm(totrbc ~ group, data = dg, family = poisson(link="log"))
#looking good so far 

#KEY ASSUMPTIONS OF MODELS
#independence - a bit dififcult

#checking overdispersion
#residual deviance is great than degrees of freedom then overdispersion exists

# Multivariate regression----

#lets' look at dfcore variables such as ethnic, weight, gender, age, apache
# lets look at dfbl variables such as hb, min, max, mean, plt min max mean, lactate
#crp min max mean
#also as admission variable and also as a delta ! 
dt <- left_join(dg, dfcore %>% select(mrn,age,ethnic,apache,wkg,sex), by = "mrn")
m5 <- glm(totall ~ group + age + ethnic + apache + wkg + sex, data =dt, family = poisson(link = "log"))
#note ethnic is problematic as too many factor levels.
m5mod <- glm(totall ~ group + age + apache + wkg + sex + ecmod + ecmod*group, data =dt, family = poisson(link = "log"))


#let's look at "rate" 
dgm <- dg %>% 
        select(mrn,ecmod,totall,group)%>% 
        group_by(mrn) %>% 
        summarise(maxecmod = max(ecmod),sumtx = sum(totall))

dgmt <- left_join(
        dgm,
        dfcore %>% select(mrn,age,ethnic,apache,sex,group,wkg),
        by = "mrn"
)

dgmt$rate <- log(dgmt$maxecmod)

#trial of univariate "rate" pois regression 
m6 <- glm(sumtx ~ group + age + offset(rate),family = poisson(link="log"),data = dgmt)
m7 <- glm(sumtx ~ group + age + apache + wkg + sex + offset(rate),family = poisson(link="log"),data = dgmt)




# TTR COMPARISONS ---------------------------------------------------------


# 4.2. 1.  TTR traditional OR proportion ----------------------------------

#in this approach, we count number of blood tests as denominator and numerator
#numerator = no of blood tests in range. 
#using targets of 0.3 to 0.7 for axa
#using targets of 1.5 to 2.5 for apttr 

#to ensure fair comparison, we should work out the person time in each group.
dfcore %>% 
        group_by(group)%>% 
        summarise(n= n(),persondays = sum(ecmod))
#this showed the person days
message("above showed person days")

tco %>% 
        filter(group == "gapt") %>% 
        select(apttr)%>%
        summarise(
                total_apttr_tests=sum(!is.na(apttr)),
                total_above_range= sum(apttr>2.5,na.rm=TRUE),
                total_in_range = sum(apttr>=1.5 & apttr <= 2.5,na.rm=TRUE)
        )
#this showeed for APPTTR group , raw or traditional TTR values at aggregate level 
tco %>% 
        filter(group == "gaxa") %>% 
        select(axa)%>%
        summarise(
                total_axa_tests=sum(!is.na(axa)),
                total_above_range= sum(axa>0.7,na.rm=TRUE),
                total_in_range = sum(axa>=0.3 & axa <= 0.7,na.rm=TRUE)
        )
#this showeed for AXA group , raw or traditional TTR values at aggregate level 


# 4.2.2. Statistical Tests for traditional TTR ----------------------------

tco %>% 
        select(mrn,group,axa)%>% 
        filter(group=="gaxa")%>%
        drop_na()%>%
        group_by(mrn)%>%
        summarise(
                no_tests_per_pts = n(),
                no_tests_above_range = sum(axa>0.7,na.rm=TRUE),
                no_tests_in_range = sum(axa>=0.3 & axa <= 0.7,na.rm=TRUE)
        ) -> gaxatrad

gaxatrad$g <- 'gaxa'
gaxatrad$prop <- gaxatrad$no_tests_in_range/gaxatrad$no_tests_per_pts

tco %>% 
        select(mrn,apttr,group)%>% 
        filter(group=="gapt")%>%
        drop_na()%>%
        group_by(mrn)%>%
        summarise(
                no_tests_per_pts = n(),
                no_tests_above_range = sum(apttr>2.5,na.rm=TRUE),
                no_tests_in_range = sum(apttr>=1.5 & apttr <= 2.5,na.rm=TRUE)
        ) -> gapttrad

gapttrad$g <- "gapt"
gapttrad$prop <- gapttrad$no_tests_in_range/gapttrad$no_tests_per_pts

pl_ttr_trad <- rbind(
        gaxatrad %>% select(mrn,g,prop),
        gapttrad %>% select(mrn,g,prop)
)

pl_ttr_trad$g <- as.factor(pl_ttr_trad$g)

#undertake Statistical test
wilcox.test(prop~ g , data = pl_ttr_trad)
t.test(prop~g,data = pl_ttr_trad)


#ggplot code
p1 <- ggplot(data = pl_ttr_trad, aes(x=g,y=prop,color=g)) + 
        geom_boxplot()+
        theme_bw()+
        labs(
                x = "Monitoring Groups",
                y = "proportion of blood tests  in range",
                title = "Proportion of blood tests in desired range",
                subtitle = "Welch Two Sample t- test : p > 0.005"
                
        )

#ggsave("products/presentations/sept22_01",plot=p1,device ="png",dpi=320)



# 4.3.2. Statistical Tests for TTRrose --------------------------------------------

wilcox.test(ttrg ~ group, data = dttr)        
t.test(ttrg ~ group, data = dttr)


# 4.3.3. Ggplot2 output for above---------------------------------------------------

p2 <- ggplot(data = dttr, aes(x=group,y=ttrg,color=group)) + 
        geom_boxplot()+
        theme_bw()+
        labs(
                x = "Monitoring Groups",
                y = "% of tests in range",
                title = "Proportion of blood tests in range using Rosendaal method",
                subtitle = "Welch Two Sample t- test : p <0.005"
                
        )

#ggsave("products/presentations/sept22_01_p2",plot=p2,device ="jpeg",dpi=320)


# 4.3.4. calculating t high's ---------------------------------------------

dttr$prophi <- dttr$thi / dttr$totalhr


p3 <- ggplot(data = dttr, aes(x=group,y=prophi,color=group)) + 
        geom_boxplot()+
        theme_bw()+
        labs(
                x = "hi Groups",
                y = "% of tests in range",
                title = "Proportion of blood tests in range using Rosendaal method",
                subtitle = "Welch Two Sample t- test : p <0.005"
        )



# statistical test for TTR ------------------------------------------------



dtest <- left_join(
        dfcore %>% select(mrn,age,apache,ethnic,sex,surv_ecmo,group),
        dttr %>% select(mrn,ttrg),
        by = "mrn"
)

dtest <- dtest %>% select(-mrn)

mz <- glm(surv_ecmo ~  ttrg + age + sex + apache, data = dtest,family = binomial(link="logit"))

#I think there isa  dispersion in this model
p41 <- ggplot(data = dtest,aes(x=group,y=ttrg,color = surv_ecmo))+
        geom_boxplot()+
        geom_jitter(width = 0.15,alpha = 0.45)+ 
        theme_bw()+
        labs(
                x= "Monitoring Groups",
                y= "Time in therapeutic range(Rosendaal)",
                title = "ECMO survival as grouped by monitoring group"
                
        )

#ggsave("products/presentations/sept22_01_p41",plot=p41,device ="jpeg",dpi=320)

p42 <- ggplot(data=dtest, aes(x=group,y=ttrg,color =surv_ecmo))+
        geom_boxplot()+
        geom_jitter(width = 0.15,alpha = 0.45)+
        facet_wrap(~surv_ecmo)+
        theme_bw()+
        labs(
                x= "Monitoring Groups",
                y= "Time in therapeutic range(Rosendaal)",
                title = "ECMO survival as grouped by monitoring group"
        )

#ggsave("products/presentations/sept22_01_p42",plot=p42,device ="jpeg",dpi=320)


# ecmo_timeline -----------------------------------------------------------
#
#milestone <- c(
#        "acute respiratory failure",
#        "intubation",
#        "ARDS",
#        "VV-ECMO Retrieval",
#        "Bleeding and Thrombotic Complications",
#        "Successful Wean from ECMO"
#        
#)
#
#timel <- c(
#        "day1",
#        "day3",
#        "day5",
#        "day6",
#        "day12",
#        "day18"
#)
#
#dataval <- c(
#        "no",
#        "no",
#        "no",
#        "no",
#        "yes",
#        "yes"
#)
#
#status_colors <- c(
#        "#C00000",
#        "#C00000",
#        "#C00000",
#        "#C00000",
#        "#00B050",
#        "#00B050"
#        
#)
#
#dtl <- data.frame(milestone,timel,dataval)
#
#dtl$dataval <- as.factor(dtl$dataval)
#
#plot_dt<- ggplot2::ggplot(dtl,aes(x=timel,y=0,col=milestone,label=dataval)) +
#        labs(col = "Milestones")+
#        scale_color_manual(values=status_colors, labels=milestone, drop = FALSE)+
#        theme_classic()+
#        geom_hline(yintercept =  0,color = "black",size = 0.5)+
#        theme(axis.line.y = element_blank(),
#              axis.text.y = element_blank(),
#              axis.title.y = element_blank(),
#              axis.title.x = element_blank(),
#              axis.ticks.y = element_blank()
#              )
#        
#     

dtl <- data.frame(
        event = c("acute resp failure","intubation_ards","vv_ecmo","complication","wean_from_ecmo"),
        start = c("2020-06-06","2020-06-08","2020-06-10","2020-06-12","2020-06-25"),
        end = c("2020-06-08","2020-06-10","2020-06-25","2020-06-13","2020-6-26"),
        datav = c("no","no","yes","yes","yes")
)


p_dt <- ggplot(dtl)+
        geom_segment(aes(x=start,xend = end,y=0,yend=0,color = datav,size = 10 ))+
        labs(x = "Time(days)")+
        scale_y_discrete()+
        theme(axis.line.y = element_blank(),
              axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks.y = element_blank())



#calculate dose differences

t.test(hep_wkgday ~ group, data = dcumhep)
wilcox.test(hep_wkgday ~ group, data = dcumhep)

# explore complications

df3 <- left_join(
        df3,
        dfcore %>% select(mrn,group,ecmod,surv_ecmo),
        by = "mrn"
)

df3$ecmod <- as.numeric(df3$ecmod)
df3$all <- df3$toth + df3$totboth + df3$totthr
df3$all_day <- df3$all / df3$ecmod


df3 %>% filter(group == "gaxa") %>% summarise(mean = mean(all))

mcomp <- glm(surv_ecmo ~ all_day, data = df3,family = binomial(link="logit"))


t.test(all_day ~ group, data = df3)
wilcox.test(all_day ~ group, data = df3)

#lets explore circuit changes

dfci <- df %>% select(mrn,tot_circhange)
dfci$totc <- as.numeric(dfci$tot_circhange)
dfci$totc[is.na(dfci$totc)] <- 0

dfci <- left_join(
        dfci,
        dfcore %>% select(mrn,group,ecmod,surv_ecmo),
        by = "mrn"
)
dfci$ecmod <- as.numeric(dfci$ecmod)

dfci$cday <- dfci$totc / dfci$ecmod

dfci %>% filter(group == "gapt") %>% summarise(mean = mean(totc))

dfci %>% filter(group == "gapt") %>% summarise(med = median(totc))

dfci %>% filter(group == "gaxa") %>% summarise(mean = mean(totc))

dfci %>% filter(group == "gaxa") %>% summarise(med = median(totc))

##

dfci %>% filter(group == "gapt") %>% summarise(mean = mean(cday))

dfci %>% filter(group == "gapt") %>% summarise(med = median(cday))

dfci %>% filter(group == "gaxa") %>% summarise(mean = mean(cday))

dfci %>% filter(group == "gaxa") %>% summarise(med = median(cday))

##

wilcox.test(cday ~ group, data = dfci)

mci <- glm(surv_ecmo ~ cday ,family = binomial(link = "logit"), data = dfci)


#3

dttr <- left_join(dttr, dfcore %>% select(age,mrn,apache,wkg,sex),by ="mrn")



#4 
dttr <- left_join(
        dttr,
        dsig %>% select(mrn,sigm),
        by = "mrn"
)

mo <- lm(ttrg ~ age + apache + wkg + sex + group, data = dttr)

myo <- lm(ttrg ~ age + apache + wkg + sex + group + group*apache + sigm, data = dttr)
my2 <- MASS::stepAIC(myo)

my3 <- lm(ttrg ~ group + age + apache, data = dttr)

my4 <- betareg::betareg(ttrg ~ age + apache , data = dttr)

#ero-inflated beta regression

my5 <- gamlss::gamlss(ttrg ~ age + apache +group, data = dttr)

my5ans <- emmeans::emmeans(my5, "group",type = "response")

#https://stats.stackexchange.com/questions/309047/zero-inflated-beta-regression-using-gamlss-for-vegetation-cover-data
#m



## bmi

dfcore %>% 
        filter(group == "gaxa") %>% 
        select(bmi) %>% summarise (median = median(bmi),iqrbmi = IQR(bmi))

dfcore %>% 
        filter(group == "gapt") %>% 
        select(bmi) %>% summarise (median = median(bmi),iqrbmi = IQR(bmi))


grz <- ggplot(data = dheprl, aes(x=group, color = group,y=runl)) +
        geom_boxplot()+
        coord_cartesian(ylim = c(0,60))+
        labs(
                title = "Unadjusted total prescription changes",
                x = "Monitoring group",
                y = "Number of Prescription changes",
                subtitle = "median for AXA = 11.00, median for APTTR = 16.00, p-value >0.05"
        )

grz2 <- ggplot(data = dgrhep[dgrhep$group == "gaxa",], aes(group =nd,x = nd,y =rlnd)) + 
        geom_boxplot()

grz4 <- ggplot(data = dheprl, aes(x=group, color = group,y=rl_day)) +
        geom_boxplot() +
        coord_cartesian(ylim = c(0,3))+
        labs(
                title = "Prescription changes ADJUSTED FOR DURATION",
                x = "Monitoring group",
                y = "Number of Prescription changes / durationof ecmo",
                subtitle = "median for AXA = 0.75, median for APTTR = 1.26, *p-value <0.05*"
        )

ggsave("products/presentations/final_presentations/src/grz2.png",plot = grz, device = "png",dpi = 320)
ggsave("products/presentations/final_presentations/src/grz4.png",plot = grz4, device = "png",dpi = 320)

##
#
#p7<-ggplot(data = vin[vin$group == "gaxa",], aes(x = ecmod, y = axa)) +
#        geom_boxplot(outlier.shape = NA)+
#        geom_hline(yintercept = c(0.3,0.7),colour = "blue")+
#        geom_hline(yintercept = 1.0,colour ="red")+
#        coord_cartesian(ylim = ylim1 *1.05)+
#        scale_x_discrete(breaks = seq(0,80,10))+
#        theme(axis.text.x = element_text(angle = 45))+
#        theme_minimal()+
#        labs(title= "AXA values in AXA group",
#             x = "Days on Ecmo",y ="anti-Xa levels",
#             subtitle = "Normal Range 0.3 to 0.7, VTE Range 0.6 to 1.0")
#




##
