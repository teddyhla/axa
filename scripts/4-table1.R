# SOURCE DATA -------------------------------------------------------------
load(file="data/clean/finalout.RData")


# LIBRARIES ---------------------------------------------------------------
library("table1")
library("tidyverse")




# CUSTOM FUNCTION FOR p-value ---------------------------------------------

pvalue <- function(x,...){
        y <- unlist(x)
        g <- factor(rep(1:length(x), times=sapply(x, length)))
        if(is.numeric(y)){
                p <- kruskal.test(y ~ g)$p.value
        } else {
                p <- chisq.test(table(y,g))$p.value
        }
        
        c("",sub("<","&lt;",format.pval(p,digits= 3, eps = 0.001)))
}

my.render.cont <- function(x) {
        with(stats.apply.rounding(stats.default(x), digits=2), 
             c("","Median (IQR)"=sprintf("%s (%s,%s)", MEDIAN, 
                                         round(quantile(x,na.rm=TRUE,prob=c(.25,.75))[[1]],1),
                                         round(quantile(x,na.rm=TRUE,prob=c(.25,.75))[[2]],1)
                                         )))
}
my.render.cat <- function(x) {
        c("", sapply(stats.default(x), function(y) with(y,
                                                        sprintf("%d (%0.0f %%)", FREQ, PCT))))
}

# TABLE 1 -----------------------------------------------------------------

setdiff(names(dfcore),names(dm))

#there are still some 

dttr$ttrhi <- round(dttr$thi / dttr$totalhr,2)

dm3 <- dm

dm3 <- left_join(
        dm3,
        dfcore %>% select(mrn,mode,ecmoconfig,pe,aki,ethnic,pmh),
        by = "mrn"
)

dm3 <- left_join(
        dm3,
        dttr %>% select(mrn,ttrhi),
        by = "mrn"
)

dm3 <- dm3 %>% mutate(
        ethnic2 = case_when(
                ethnic %in% c("A","B","C") ~ "White",
                ethnic %in% c("D","E","F","G") ~ "Mixed",
                ethnic %in% c("H","J","K","L") ~ "Asian",
                ethnic %in% c("M","N","P","R") ~ "Black",
                ethnic %in% c("S") ~ "Other",
                ethnic %in% c("Z") ~ "Not stated"
        )
)

dm3$ethnic2 <- forcats::fct_relevel(dm3$ethnic2, "White","Mixed","Asian","Black","Other","Not stated")

levels(dm3$pmh)<- c(
        "Liver cirrhosis",
        "Immunodeficiency",
        "Immunodeficiency",
        "None",
        "Severe respiratory disease",
        "Corticosteroid Usage"
)
dm3$pmh <- forcats::fct_relevel(
        dm3$pmh, 
        "None","Corticosteroid Usage","Severe respiratory disease","Liver cirrhosis",
        "Immunodeficiency"
        )


dm3$aki <- as.factor(dm3$aki)
levels(dm3$aki) <- c("no","yes")

label(dm3$ethnic2) <- "Ethnicity"

label(dm3$pmh) <- "Past Medical History"

label(dm3$aki) <- "Renal Replacement Therapy"
label(dm3$cohort) <- "Cohorts within assigned group"

label(dm3$hb_max) <- "Max Haemoglobin"
units(dm3$hb_max) <- "g/L"

label(dm3$hb_median) <- "Median Haemoglobin"
units(dm3$hb_median) <- "g/L"

label(dm3$hb_min) <- "Minimum Haemoglobin"
units(dm3$hb_min) <- "g/L"

label(dm3$plt_median) <- "Median Platelets"
units(dm3$plt_median) <- "needed"

label(dm3$neut_median) <- "Median Neutrophils"
units(dm3$neut_median) <- "needed"

label(dm3$neut_min) <- "Minimum neutrophils"
units(dm3$neut_min) <- "needed"

label(dm3$neut_max) <- "Maximum neutrophils"
units(dm3$neut_max) <- "needed"

label(dm3$fib_median) <- "Median Fibrinogen levels"
units(dm3$fib_median) <- "unit needed"

label(dm3$ck_median) <- "Median Creatinine Kinase levels"
units(dm3$ck_median) <- "unit needed"

label(dm3$crp_median) <- "Median C-Reactive Protein"
units(dm3$crp_median) <- "unit needed"

label(dm3$pct_median) <- "Median Procalcitonin"
units(dm3$pct_median) <- "unit needed"

label(dm3$bili_median) <- "Median Bilirubin"
units(dm3$bili_median) <- "unit needed"

label(dm3$alb_max) <- "Max Albumin"
units(dm3$alb_max) <- "needed"

label(dm3$alb_median) <- "Median Albumin"
units(dm3$alb_median) <- "needed"

label(dm3$alb_min) <- "Min Albumin"
units(dm3$alb_min) <- "needed"

label(dm3$creat_median) <- "Median Creatinine"
units(dm3$creat_median) <- "needed"

label(dm3$creat_max) <- "Maximum Creatinine"
units(dm3$creat_max) <- "needed"

label(dm3$creat_min) <- "Minimum Creatinine"
units(dm3$creat_min) <- "needed"

label(dm3$gfr_median) <- "median eGFR"
units(dm3$gfr_median) <- "needed"

label(dm3$ca_median) <- "median Calcium"
units(dm3$ca_median) <- "needed"

label(dm3$corr_ca_median) <- "Median Corrected Calcium"
units(dm3$corr_ca_median) <- "needed"

label(dm3$bicarb_max) <- "Max Bicarb "
units(dm3$bicarb_max) <- "needed"

label(dm3$bicarb_median) <- "Median Bicarb"
units(dm3$bicarb_median) <- "needed"

label(dm3$bicarb_min) <- "Minimum Bicarb"
units(dm3$bicarb_min) <- "needed"

label(dm3$lactate_max) <- "Maximum Lactate"
units(dm3$lactate_max) <- "needed"

label(dm3$lactate_median) <- "Median Lactate"
units(dm3$lactate_median) <- "needed"

label(dm3$lactate_min) <- "Minimum Lactate"
units(dm3$lactate_min) <- "needed"

label(dm3$ldh_median) <- "Median Lactatedehydrogenase levels(units)"

label(dm3$ph_median) <- "Median pH"

label(dm3$tlow) <- "Time in therapeutic range BELOW target"
units(dm3$tlow) <- "%"

label(dm3$ttrg) <- "Time in Therapeutic Range"
units(dm3$ttrg) <- "%"

label(dm3$ttrhi) <- "Time ABOVE therapeutic Range"
units(dm3$ttrhi) <- "%"

label(dm3$sigm) <- "Variability of anticoagulation"

label(dm3$hep_wkgday) <- "Heparin per kilogram per day"
units(dm3$hep_wkgday) <- "international units"

label(dm3$rl_day) <- "No of heparin prescription changes"

label(dm3$totc) <- "Total number of circuit changes"

label(dm3$bldtot) <- "Total blood products "
units(dm3$bldtot) <- "Units"

label(dm3$ecmoconfig) <- "ECMO Cannula configuration"

label(dm3$pe) <- "Pulmonary Embolism on admission"

label(dm3$ferritin_median) <- "Median Ferritin(unit needed)"

label(dm3$age) <- "Age"
units(dm3$age) <- "Years"

label(dm3$surv_ecmo) <- "Survival on ECMO"
label(dm3$apache) <- "APACHE II score"
label(dm3$sex) <- "Sex"
label(dm3$wkg) <- "Weight"
units(dm3$wkg) <- "kg"

label(dm3$bmi) <- "Body Mass Index"
units(dm3$bmi) <- "kg/m^2"

label(dm3$ecmod) <- "Duration on ECMO"
units(dm3$ecmod) <- "Days"

label(dm3$surv_ecmo) <- "Survival on ECMO"

# TABLE 3 -----------------------------------------------------------------

dm4 <- dfcore %>% select(mrn,group,ecmod)

dm4$ecmod <- as.numeric(dm4$ecmod)
dm4 <- left_join(
        dm4,
        dcumhep %>% select(mrn,cudose,hep_wkgday),
        by = "mrn"
)

dm4 <- left_join(
        dm4,
        dheprl %>% select(mrn,runl,rl_day),
        by = "mrn"
)

dm4 <- left_join(
        dm4,
        dm %>% select(mrn, totc,toth, bldtot,hboth,cday,bldtot_day),
        by= "mrn"
)


dm4[is.na(dm4)]<- 0

label(dm4$cudose) <- "Cumulative Dosage of Heparin"
units(dm4$cudose) <- "Units"

label(dm4$runl) <- "Cumulative Prescription changes of heparin"
label(dm4$ecmod) <- "Duration on ECMO"
units(dm4$ecmod) <- "Days"

label(dm4$totc) <- "Cumulative ECMO Circuit Changes"

label(dm4$hep_wkgday) <- "Heparin dose per kilogram per day"
units(dm4$hep_wkgday) <- "Units/kg/day"

label(dm4$rl_day) <- "No of prescription changes per day"

label(dm4$bldtot) <- "Cumulative blood products"
units(dm4$bldtot) <- "units"

label(dm4$toth) <- "Total Hemorrhagic Complications"
units(dm4$toth) <- "event"


# TABLE 1 generation ------------------------------------------------------


table1(~age + sex + apache + wkg + bmi + cohort + ecmod + tlow + ttrhi + ttrg + sigm + hep_wkgday + rl_day + totc + cday + toth + aki + bldtot  + ecmoconfig + pe |group, data = dm3
       ,overall = F,
       render.continuous = my.render.cont,
       render.categorical = my.render.cat,
       topclass = "Rtable1-zebra",
       extra.col = list("P-value"=pvalue))

table1(~age + sex + apache + wkg + bmi + cohort + ethnic2 + pmh +  aki  + ecmoconfig + hb_median + plt_median + neut_median + fib_median + ldh_median + ferritin_median + ck_median + crp_median + pct_median +
               bili_median + alb_median + creat_median +
               corr_ca_median + bicarb_median + lactate_median + ph_median + ecmod + surv_ecmo|group, data = dm3
       ,overall = F,
       render.continuous = my.render.cont,
       render.categorical = my.render.cat,
       topclass = "Rtable1-zebra",
       extra.col = list("P-value"=pvalue))

table1( ~ hb_median + hb_min + hb_max + plt_median + neut_median +
        neut_min + neut_max + fib_median + ldh_median + ferritin_median + ck_median + crp_median + pct_median +
        bili_median + alb_median + alb_min + alb_max + creat_median + creat_min + creat_max + gfr_median + ca_median +
        corr_ca_median + bicarb_median + bicarb_min + bicarb_max + lactate_median + lactate_min + lactate_max + ph_median | group, data = dm3
       ,overall = F, extra.col = list("P-value"=pvalue))



dm3 %>% 
        select(group,ph_median) %>% 
        group_by(group) %>% 
        summarise(
                one = quantile(ph_median,prob= c(0.25)),
                med = median(ph_median),
                two = quantile(ph_median,prob = c(0.75)))

table1 (~  ecmod +cudose + hep_wkgday + runl + rl_day + totc + cday+ toth + bldtot + bldtot_day  | group, data = dm4,
        overall = F,
        render.continuous = my.render.cont,
        render.categorical = my.render.cat,
        topclass = "Rtable1-zebra",
        extra.col = list("P-value"=pvalue))

dm4 %>% 
        select(group,runl) %>% 
        group_by(group) %>% 
        summarise(
                one = quantile(runl,prob= c(0.25)),
                med = median(runl),
                two = quantile(runl,prob = c(0.75)))


dm4 %>% 
        select(group,bldtot_day) %>% 
        group_by(group) %>% 
        summarise(
                one = quantile(bldtot_day,prob= c(0.25)),
                med = median(bldtot_day),
                two = quantile(bldtot_day,prob = c(0.75)))


table1 (~  bldtot_day  | group, data = dm4,
        overall = F,
        render.continuous = my.render.cont,
        render.categorical = my.render.cat,
        topclass = "Rtable1-zebra",
        extra.col = list("P-value"=pvalue))

dbl1 <- dm4 %>% select(group,bldtot_day)
dbl1$any <- as.factor(ifelse(dbl1$bldtot_day == 0,"no","yes"))

table1 (~  any  | group, data = dbl1,
        overall = F,
        render.continuous = my.render.cont,
        render.categorical = my.render.cat,
        topclass = "Rtable1-zebra",
        extra.col = list("P-value"=pvalue))

# dm5 <- dm4 %>% 
#         select(group,toth,ecmod) %>% 
#         group_by(group) %>% 
#         summarise(
#                 personday = sum(ecmod),
#                 numbers = n(),
#                 zeroc = sum(toth == 0),
#                 onec = sum(toth == 1),
#                 twoc = sum(toth == 2),
#                 threc = sum(toth==3),
#                 fourc = sum(toth==4)
#                 )
# 


d6 <- dm4 %>% select(mrn,group,toth,ecmod)%>%
        group_by(mrn) %>% 
        mutate(tothpd = toth /ecmod)%>%
        ungroup() 

d6$hbin <- as.factor(ifelse(d6$toth == 0 , "no","yes"))


ptwoprd <- dfcore %>% select(mrn,group) %>% filter(mrn %in% pt_without_bldproducts)
ptwoprd$prd <- 0

#patients without any hemorrhagic outcome, so we will use hboth

dm %>% select(group,hboth) %>%group_by(group)%>% tally(hboth == 0)

dh <- dm %>% select(group,hboth)
dh$hemc <- as.factor(ifelse(dh$hboth == 0,"no","yes"))

table1 (~  hemc  | group, data = dh,
        overall = F,
        render.continuous = my.render.cont,
        render.categorical = my.render.cat,
        topclass = "Rtable1-zebra",
        extra.col = list("P-value"=pvalue))


#patient without any complication
df3 <- left_join(df3,dfcore %>% select(mrn,group),by = "mrn")
dh2 <- df3

dh2$anyc <- as.factor(ifelse(dh2$toth == 0 & dh2$totthr == 0 & dh2$totboth == 0,"no","yes"))

table1 (~  anyc  | group, data = dh2,
        overall = F,
        render.continuous = my.render.cont,
        render.categorical = my.render.cat,
        topclass = "Rtable1-zebra",
        extra.col = list("P-value"=pvalue))

df3 %>% 
        filter(toth == 0 & totthr == 0 & totboth == 0) %>%
        group_by(group) %>%
        tally()

#ecmo circuit change

dm$circ <- as.factor(ifelse(dm$totc == 0,"no","yes"))
table1 (~  circ  | group, data = dm,
        overall = F,
        render.continuous = my.render.cont,
        render.categorical = my.render.cat,
        topclass = "Rtable1-zebra",
        extra.col = list("P-value"=pvalue))

## blood products per day

table1 (~ bldtot_day | group, data = dm,extra.col = list("P-value"=pvalue))
table1 (~  bldtot  | group, data = dm,
        overall = F,
        render.continuous = my.render.cont,
        render.categorical = my.render.cat,
        topclass = "Rtable1-zebra",
        extra.col = list("P-value"=pvalue))

table1 (~  bldtot_day  | group, data = dm,
        overall = F,
        render.continuous = my.render.cont,
        render.categorical = my.render.cat,
        topclass = "Rtable1-zebra",
        extra.col = list("P-value"=pvalue))
#


# single queries ----------------------------------------------------------

dttr %>% group_by(group) %>% group_map(~summary(.x))

dsig %>% group_by(group) %>% group_map(~summary(.x))
#
