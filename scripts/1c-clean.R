#### 1. LOAD DATA -----
library(tidyverse)
###import new dataset
s0 <- readRDS("data/axa_NONanonymised_data_20220506.rds")


#turned out s0 primary data source is a nested list of 8 lists.
#

s1 <- dplyr::as_tibble(s0[1][1])
#patient list
s2 <- dplyr::as_tibble(s0[2][1])
#id_lookups
s3 <- dplyr::as_tibble(s0[3][1])
#heparin administration
s4 <- dplyr::as_tibble(s0[4][1])
#coag results
s5 <- dplyr::as_tibble(s0[5][1])
#other labs
s6 <- dplyr::as_tibble(s0[6][1])
#other meds
s7 <- dplyr::as_tibble(s0[7][1])
#hydrocortisone
s8 <- dplyr::as_tibble(s0[8][1])
#txa
s9 <- dplyr::as_tibble(s0[9][1])
#blood products


s1 <- lapply(s1,data.frame,stringsAsFactors = FALSE)
s2 <- lapply(s2,data.frame,stringsAsFactors = FALSE)
s3 <- lapply(s3,data.frame,stringsAsFactors = FALSE)
s4 <- lapply(s4,data.frame,stringsAsFactors = FALSE)
s5 <- lapply(s5,data.frame,stringsAsFactors = FALSE)
s6 <- lapply(s6,data.frame,stringsAsFactors = FALSE)
s7 <- lapply(s7,data.frame,stringsAsFactors = FALSE)
s8 <- lapply(s8,data.frame,stringsAsFactors = FALSE)
s9 <- lapply(s9,data.frame,stringsAsFactors = FALSE)
#apply as df 

s1 <- plyr::rbind.fill(s1)
s2 <- plyr::rbind.fill(s2)
s3 <- plyr::rbind.fill(s3)
s4 <- plyr::rbind.fill(s4)
s5 <- plyr::rbind.fill(s5)
s6 <- plyr::rbind.fill(s6)
s7 <- plyr::rbind.fill(s7)
s8 <- plyr::rbind.fill(s8)
s9 <- plyr::rbind.fill(s9)
#use rbind fill 

##########SECRETS
s2 <- dplyr::left_join(s2,
                       s1 %>% select(mrn, name),
                       by = "mrn")



###### 2.0. DATA CLEAN and VERIFY ####---------------------------
names(s1) <- tolower(names(s1))
#convert to lower case col names
s1 <- select(s1,-(name))

s1$mrn <- toupper(s1$mrn)
fts1 <-
        c(
                "cohort",
                "diagnosis",
                "diagnosis_category",
                "mode",
                "venous_access_1",
                "venous_access_2",
                "venous_return",
                "arterial_return",
                "survived_ecmo",
                "survived_icu",
                "ethnic",
                "sex",
                "biopsy_proven_cirrhosis",
                "portal_hypertension",
                "hepatic_encephalopathy",
                "very_severe_cardiovascular_disease",
                "severe_respiratory_disease",
                "home_ventilation",
                "chronic_renal_replacement_therapy",
                "hiv_aids",
                "steroid_treatment",
                "radiotherapy",
                "chemotherapy",
                "metastatic_disease",
                "acute_myelogenous_lymphocytic_leukaemia_or_multiple_myeloma",
                "chronic_myelogenous_lymphocytic_leukaemia",
                "lymphoma",
                "congenital_immunohumoral_or_cellular_immune_deficiency_state"
        )

s1[fts1]<- lapply(s1[fts1],factor)
#convert to factors for factor classes.

#names are too long so lets make it shorter 
colnames(s1) <- c(
        "cohort",
        "admission_date",
        "mrn",
        "age",
        "dx",
        "dx_cat",
        #for diagnosis_category,
        "mode",
        "v_1",
        "v_2",
        "v_return",
        "art_return",
        #for venous access 1 and 2,arterial return
        "surv_ecmo",
        "surv_icu",
        "date_icu_discharge",
        "ecmo_start",
        "ecmo_finish",
        "date_can",
        "date_de_can",
        #for survived ecmo/icu/date cannulated and decannulate
        "acute_phys_score",
        "age_score",
        "chron_health_score",
        "apache",
        "ethnic",
        "sex",
        "hcm",
        "wkg",
        #for acute physiology score,agescore,chronic health score,apache2score
        "cirrhosis",
        "portal_htn",
        "hepatic_enceph",
        "s_cvd",
        "s_respd",
        #for biopsy proven cirrhosis, portal hypertension,hepatic encephalopathy, severe cardiovasc dis, severe resp dis
        "dom_vent",
        "chron_rrt",
        "hiv",
        "steroids_hx",
        "rt",
        "chemo",
        "mets",
        ##for home ventilation,chronic renal replacement,hiv_aids,history of steroid treatment,radiotherapy,chemotherapy,"metastatic disease"
        "aml_mm",
        "cml",
        "lymphoma",
        "cong_immuno_def"## aml or multiple myeloma, cml , lymphoma, congenital immunodeficiency state
)

#### 2.1. Factor reassignment-------------------------------
#now lets check all the levels for factor vars are appropriate.
levels(s1$mode) <- c("vv","vv")
#reassign as there are cap vv and VV 

#reassign cannula  va_1 column
levels(s1$v_1) <- c(
        "21fr_lfv",
        "21fr_rfv",
        "25fr_lfv",
        "25fr_lfv",
        "25fr_lfv",
        "25fr_lfv",
        "25fr_rfv",
        "25fr_rfv",
        "31fr_d_rij",
        "lfv",
        "rfv"
)

#reassign cannula v_return column
levels(s1$v_return) <- c(
        "17fr_rij",
        "19fr_rij",
        "21fr_rfv",
        "21fr_rij",
        "21fr_rfv",
        "21fr_rij",
        "21fr_rij",
        "23fr_rfv",
        "23fr_lfv",
        "23fr_lfv",
        "23fr_rfv",
        "23fr_rfv",
        "23fr_rfv",
        "23fr_rij",
        "25fr_lfv",
        "25fr_lfv",
        "25fr_rfv",
        "rfv",
        "rij",
        "unknown"
)

s1$ecmoconfig <- paste(s1$v_1,s1$v_return)
s1$ecmoconfig <- as.factor(s1$ecmoconfig)
levels(s1$ecmoconfig) <- c(
        "fem-fem",
        "fem-fem",
        "fem-ij",
        "fem-ij",
        "fem-fem",#5
        "fem-ij",
        "fem-fem",#7
        "fem-fem",
        "fem-ij",
        "fem-fem",#10
        "fem-fem",
        "fem-unknown",
        "fem-fem",#13
        "fem-ij",
        "fem-ij",#15
        "fem-ij",#16
        "fem-unknown",#17
        "fem-ij",#18
        "fem-unknown",#19
        "dual-ij",#20
        "fem-fem",#21
        "fem-unknown",#22
        "fem-ij"#23
)

levels(s1$surv_ecmo) <- c("no","no","yes","yes")
levels(s1$surv_icu) <- c("no","no","yes","yes")
#clean up levels for surv ecmo and surv icu columsn


s1 <- subset(
        s1,
        select = -c(
                v_2,
                art_return,
                portal_htn,
                hepatic_enceph,
                s_cvd,
                dom_vent,
                chron_rrt,
                rt,
                chemo,
                aml_mm,
                cml,
                lymphoma
        )
)
#remove all the empty columns

s1[s1$mrn == "2553305Z","mets"] <- "metoz"


s1 <- s1 %>% mutate(pmh = case_when(
        cirrhosis == "1" ~ "cirrhosis",
        s_respd == "1" ~ "s_respd",
        hiv == "H" ~ "hiv",
        steroids_hx == "1" ~ "steroids_hx",
        mets == "metoz"  ~ "MODTEDS",
        cong_immuno_def =="1" ~ "cong_immunodef",
        TRUE ~ "nilpmh"
))

# for some reason the code is broken here in above case when for 'mets'. 
#this is due to patient having both s_respd and mets

#s1 %>% filter(!is.na(cirrhosis) | !is.na(s_respd) | !is.na(hiv) | !is.na(steroids_hx) |!is.na(cong_immuno_def)) %>% View()
#this above code showed that all other patients dont have concurrent pmh
s1[s1$mrn == "2553305Z","pmh"] <- "s_respd + mets"


s1$pmh <- as.factor(s1$pmh)
#these are confirmed and clarified
s1 <- subset(
        s1,
        select = - c(cirrhosis,s_respd,hiv,steroids_hx,mets,cong_immuno_def)
)
#CODE FOR CHECK
# s1 %>% select(id,pmh) %>% filter(!is.na(pmh)) %>% View()
s1$dxcont <- s1$dx
levels(s1$dxcont) <- c(
        "flu", # ? flu 
        "flu_b + aspergillus", # ?aspergillus and flu a 
        "non cov viral pneumonitis + cap", # cap and viral pneumonitis 
        "non cov viral pneumonitis + bilat pe", #right basal cap and flu and bilat pe
        "covid pneumonitis",
        "covid pneumonitis",
        "covid pneumonitis",
        "covid pneumonitis + pe",
        "covid pneumonitis",
        "covid pneumonitis",
        "covid pneumonitis + hlh",
        "covid pneumonitis + cap",
        "covid pneumonitis",
        "covid pneumonitis",
        "covid pneumonitis",
        "covid pneumonitis",
        "covid pneumonitis",
        "covid pneumonitis + preg",
        "covid pneumonitis",
        "covid pneumonitis",
        "covid pneumonitis",
        "covid pneumonitis + post part",
        "covid pneumonitis + ischemic leg",
        "covid pneumonitis",
        "covid pneumonitis",
        "covid pneumonitis + post part",
        "covid pneumonitis + pe",
        "covid pneumonitis",
        "covid pneumonitis + pe",
        "covid pneumonitis",
        "covid pneumonitis",
        "covid pneumonitis",
        "covid pneumonitis + cap",
        "covid pneumonitis + cap",
        "covid pneumonitis",
        "covid pneumonitis + preg",
        "covid pneumonitis + preg",
        "covid pneumonitis",
        "flu",
        "flu_a",
        "flu_a + cap",
        "flu_a + h1n1",
        "flu_a",
        "flu_b",
        "flu_b + cap",
        "flu_b",
        "flu_b + cap",
        "flu_b + cap",
        "flu_b + cap",
        "h1n1",
        "h1n1 + cap",
        "h1n1 + cap",
        "h1n1 + cap",
        "h1n1 + cap",
        "h1n1 + cap",
        "h1n1 + cap",
        "h1n1 + cap",
        "flu_a",
        "h1n1",
        "flu_b",
        "flu_b + cap",
        "flu_b + cap",
        "flu_b + cap",
        "flu_b + cap",
        "flu_b + cap",
        "flu_b + cap",
        "flu_b + cap + pe",
        "parainfluenza",
        "parainfluenza + measles",
        "pcp + cmv",
        "viral pneumonitis",
        "rsv",
        "flu_a + cap",
        "cap",
        "flu_a",
        "flu_b"
 )

## make a new column for patients with pe 

s1$pe <-
        ifelse(
                s1$dxcont %in% c(
                        "flu_b + cap + pe",
                        "covid pneumonitis + pe",
                        "non cov viral pneumonitis + bilat pe"
                ),
                "yes",
                "no"
        )

s1$pe <-as.factor(s1$pe)

#### 2.2. Num vars check ------------------------------------

#CODE CHECK sum of duplicated items is 0 . i.e., no duplicates
# sum(duplicated(s1$id)) 
#now let's check the numerical variables
#actually all the numerical variables look fairly clean.

ptid <- unique(s1$mrn)
rm(fts1)
message("Cleaning s1 is complete.")

dfcore <- s1 
rm(s1)
message ("s1 is now dfcore")

##### 3.1 Cleaning s3-----------------------------------------

names(s3) <- tolower(names(s3))
colnames(s3) <- c(
        "mrn","chart_t","short_label","terse_form","unit","kg"
)

s3$mrn <- toupper(s3$mrn)

#correct col names
# now let's check all the patient are in this heparin prescrition

pt_without_heparin<- setdiff(ptid,unique(s3$mrn))
#this showed that these patients dont have any heparin prescribed.

##### 3.2. Col classes for s3-----------------------------------

s3$short_label <- as.factor(s3$short_label)
s3$terse_form <- as.double(s3$terse_form)
s3$unit <- as.factor(s3$unit)

message("Cleaning s3 is complete.")
dfhep <- s3
rm(s3)

message("Dataframe s3 is now dfhep")

###### 4.1. Cleaning s4---------------------------------

colnames(s4) <- c("mrn","chart_t","axa","apttr")
#change col names
s4$mrn <- toupper(s4$mrn)

#CODE CHECK
# sum(ptid %in% unique(s3$id))
#setdiff(ptid,unique(s4$mrn))
# this show that all patients have blood results.

s4$axa <- as.double(s4$axa)
s4$apttr <- as.double(s4$apttr)

message("Cleaning s4 is complete.")
dfcoag <- s4
rm(s4)
message("s4 is now dfcoag")

#####-5.1. Cleaning s5 ------------------------------------

s5$mrn <- toupper(s5$mrn)

colnames(s5) <- c(
        "mrn","chart_t","hb","plt","neut","fib","ldh","ferritin",
        #haemoglobin grams per litre, platelets 10 e9,neutrophils 10 e9
        #fibrinogen g per litre, ldh units per litre,ferritin ug per l,
        "ck","crp","pct","bili","alb","creat","gfr","ca","corr_ca",
        #ck in iu_l, crp in mg_l, pct ug_l, bili_umol_l, alb_g_l,
        #creat _umol_l,gfr in ml min, cal mmol_l
        "bicarb","lactate","ph"
        #bicarb_actual_mmol_l,lactate_mmol_l,
        
)

#CODE CHECK
# sum(ptid %in% (unique(s5$id))) 
# this is 254 showed that all patients have blood results.

#all cols are in character so lets turn into double.
nums4 <- names(s5)
nums4 <- nums4[3:20]

s5[nums4] <- lapply(s5[nums4],as.double)

message("Cleaning s4 is complete.")

#checking with this code

#s5 %>% summarise(across(
#        everything(),
#        .f = list(
#                mean = mean,
#                max = max,
#                min = min,
#                sd = sd
#        ),
#        na.rm = TRUE
#))
# this code suggests values are appropriate
dfbl <- s5
rm(s5)
message("s5 is now dfbl")

###### ----- 6.1 Cleaning s6 -------------------------------

s6$mrn <- toupper(s6$mrn)

colnames(s6) <- c(
        "mrn","chart_t","aspirin","aspirin_supp","clopi","warfarin","rivarox",
        #aspirin,aspirin suppository,clopidogrel,warfarin,rivaroxaban
        "apix","dalteparin_sc","ibuprof_liq","ibuprof_tab","diclof_inj","diclof_supp",
        #apixaban,dalteparin units,ibuprofen liquid, tablet, diclofenac injection and suppostiory
        "dexa_liq","dexa_inj","dexa_tab","prednis_tab","hydrocort_inj","hydrocort_tab",
        #dexamethasone liquid, dexamethasone injection, dexa tablet, prednisolone tablet,
        #hydrocortisone injection ,hydrocortisone tablets
        "methylpred_inj","methylpred_puls","methylpresd_maint","ca_cavitd","ca_sando","cacl_mmol",
        #methylpred injection, pulsed methylpred, matienancce methylpred,calcium with vit d, sandocal, 
        #calcium chloride in mmols
        "cacl_mg","cagluc_mmol","txa_mouthwash","txa_inj","txa_nebs","txa_tab","txa_top"
        #calcium chloride in mg,calcium gluconate in mmol,tranexemic acid mouth wash
        #txa injection, nebulised, tablets and topical forms
)

#CODE CHECK 
pt_without_extradrugs <- setdiff(ptid, unique(s6$mrn))

#lets change to numerical forms.
nums5 <- names(s6)
nums5 <- nums5[3:32]

s6[nums5] <- lapply(s6[nums5],as.double)

#no warfarin, no rivaroxabab,no apixaban, diclofena inj or suppr, 
#hydrocort tab, txa top 
s6 <- subset(
        s6,
        select = -c(
                warfarin,
                rivarox,
                apix,
                diclof_inj,
                diclof_supp,
                hydrocort_tab,
                txa_top
        )
)

#need to drop columns that has 0  or all NA.
message("Cleaning s6 is complete.")
pt_without_extra_drugs <- setdiff(ptid,unique(s6$mrn))

dfrx <- s6
rm(s6)
message("s6 is now dfrx")

##### 7.1 Cleaning s7-----------------------------------------

s7$mrn <- toupper(s7$mrn)
colnames(s7) <- c("mrn","chart_t","hydrocort_inf_mghr")

pt_without_hydrocortinfus <- setdiff(ptid,unique(s7$mrn))
#only 44 patients have hydrocortisone infusion

s7$hydrocort_inf_mghr <- as.double(s7$hydrocort_inf_mghr)

message("Cleaning s7 is complete. ")
dfhydrocortinf <- s7
rm(s7)
message("s7 is now dfhydrocortinf")

#### 8.1 Cleaning s8------------------------------------

s8$mrn <- toupper(s8$mrn)
colnames(s8) <- c("mrn","chart_t","txa_inf_mghr")
pt_without_txainfus <- setdiff(ptid,unique(s8$mrn))
s8$txa_inf_mghr <- as.double(s8$txa_inf_mghr)

message("Cleaning s8 is complete. ")
dftxa <- s8
rm(s8)
message("s8 is now dftxa")


#### 9.1 Cleaning s9-------------------------------------

s9$mrn <- toupper(s9$mrn)

colnames(s9) <- c("mrn","chart_t","s_label","t_form","unit")
pt_without_bldproducts <- setdiff(ptid,unique(s9$mrn))

s9$s_label <- as.factor(s9$s_label)
levels(s9$s_label) <- c("cryo", "cryo", "cryo", "ffp", "plt", "prbcs")
s9$t_form <- as.double(s9$t_form)
#this needs checking as data looks a bit confusing. 75 units is it major hemorrhage
#?
s9$unit <- as.factor(s9$unit)
levels(s9$unit) <- c("unit")

message("Cleaning s9 is complete. ")
dfprd <- s9
rm(s9)
message("s9 is now dfprd ")

#### 10.1 correcting blood products as per Barney -----------

rm(s0)
rm(s2)
message("master cleaning is complete and dataframes ready." )




