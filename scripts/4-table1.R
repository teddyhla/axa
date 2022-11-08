# SOURCE DATA -------------------------------------------------------------
load(file="data/clean/finalout.RData")


# LIBRARIES ---------------------------------------------------------------
library("table1")




# CUSTOM FUNCTION FOR p-value ---------------------------------------------

pvalue <- function(x,...){
        y <- unlist(x)
        g <- factor(rep(1:length(x), times=sapply(x, length)))
        if(is.numeric(y)){
                p <- wilcox.test(y ~ g)$p.value
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
        dfcore %>% select(mrn,mode,ecmoconfig,pe,aki),
        by = "mrn"
)

dm3 <- left_join(
        dm3,
        dttr %>% select(mrn,ttrhi),
        by = "mrn"
)

dm3$aki <- as.factor(dm3$aki)
levels(dm3$aki) <- c("no","yes")

label(dm3$aki) <- "Renal Replacement Therapy"
label(dm3$cohort) <- "Cohorts within assigned group"

label(dm3$hb_max) <- "Max Haemoglobin"
units(dm3$hb_max) <- "g/L"

label(dm3$hb_median) <- "median Haemoglobin"
units(dm3$hb_median) <- "g/L"

label(dm3$hb_min) <- "Minimum Haemoglobin"
units(dm3$hb_min) <- "g/L"

label(dm3$plt_median) <- "median Platelets"
units(dm3$plt_median) <- "needed"

label(dm3$neut_median) <- "median Neutrophils"
units(dm3$neut_median) <- "needed"

label(dm3$neut_min) <- "Minimum neutrophils"
units(dm3$neut_min) <- "needed"

label(dm3$neut_max) <- "Maximum neutrophils"
units(dm3$neut_max) <- "needed"

label(dm3$fib_median) <- "median Fibrinogen levels"
units(dm3$fib_median) <- "unit needed"

label(dm3$ck_median) <- "median Creatinine Kinase levels"
units(dm3$ck_median) <- "unit needed"

label(dm3$crp_median) <- "median C-Reactive Protein"
units(dm3$crp_median) <- "unit needed"

label(dm3$pct_median) <- "median Procalcitonin"
units(dm3$pct_median) <- "unit needed"

label(dm3$bili_median) <- "median Bilirubin"
units(dm3$bili_median) <- "unit needed"

label(dm3$alb_max) <- "Max Albumin"
units(dm3$alb_max) <- "needed"

label(dm3$alb_median) <- "median Albumin"
units(dm3$alb_median) <- "needed"

label(dm3$alb_min) <- "Min Albumin"
units(dm3$alb_min) <- "needed"

label(dm3$creat_median) <- "median Creatinine"
units(dm3$creat_median) <- "needed"

label(dm3$creat_max) <- "Maximum Creatinine"
units(dm3$creat_max) <- "needed"

label(dm3$creat_min) <- "Minimum Creatinine"
units(dm3$creat_min) <- "needed"

label(dm3$gfr_median) <- "median eGFR"
units(dm3$gfr_median) <- "needed"

label(dm3$ca_median) <- "median Calcium"
units(dm3$ca_median) <- "needed"

label(dm3$corr_ca_median) <- "median Corrected Calcium"
units(dm3$corr_ca_median) <- "needed"

label(dm3$bicarb_max) <- "Max Bicarb "
units(dm3$bicarb_max) <- "needed"

label(dm3$bicarb_median) <- "median Bicarb"
units(dm3$bicarb_median) <- "needed"

label(dm3$bicarb_min) <- "Minimum Bicarb"
units(dm3$bicarb_min) <- "needed"

label(dm3$lactate_max) <- "Maximum Lactate"
units(dm3$lactate_max) <- "needed"

label(dm3$lactate_median) <- "median Lactate"
units(dm3$lactate_median) <- "needed"

label(dm3$lactate_min) <- "Minimum Lactate"
units(dm3$lactate_min) <- "needed"

label(dm3$ph_median) <- "median pH"

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


label(dm3$age) <- "Age"
units(dm3$age) <- "Years"

label(dm3$surv_ecmo) <- "Survival on ECMO"
label(dm3$apache) <- "APACHE score"
label(dm3$sex) <- "Sex"
label(dm3$wkg) <- "Weight"
units(dm3$wkg) <- "kg"

label(dm3$bmi) <- "Body Mass Index"
units(dm3$bmi) <- "kg/m^2"

label(dm3$ecmod) <- "Duration on ECMO"
units(dm3$ecmod) <- "Days"


# TABLE 1 generation ------------------------------------------------------


table1(~age + sex + apache + wkg + bmi + cohort + ecmod + tlow + ttrhi + ttrg + sigm + hep_wkgday + rl_day + totc + cday + toth + aki + bldtot  + ecmoconfig + pe |group, data = dm3
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

