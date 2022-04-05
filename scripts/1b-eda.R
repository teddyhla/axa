#### 1. LOAD DATA -----

###import new dataset
s0 <- readRDS("data/axa_anonymised_data_20220402.rds")

#turned out s0 primary data source is a nested list of 8 lists.
#

s1 <- dplyr::as_tibble(s0[1][1])
#patient list
s2 <- dplyr::as_tibble(s0[2][1])
#heparin administration
s3 <- dplyr::as_tibble(s0[3][1])
#coag lab result
s4 <- dplyr::as_tibble(s0[4][1])
#other lab result
s5 <- dplyr::as_tibble(s0[5][1])
#other meds
s6 <- dplyr::as_tibble(s0[6][1])
#hydrocortisone infusion
s7 <- dplyr::as_tibble(s0[7][1])
#tranexemic acid
s8 <- dplyr::as_tibble(s0[8][1])
#blood products


s1 <- lapply(s1,data.frame,stringsAsFactors = FALSE)
s2 <- lapply(s2,data.frame,stringsAsFactors = FALSE)
s3 <- lapply(s3,data.frame,stringsAsFactors = FALSE)
s4 <- lapply(s4,data.frame,stringsAsFactors = FALSE)
s5 <- lapply(s5,data.frame,stringsAsFactors = FALSE)
s6 <- lapply(s6,data.frame,stringsAsFactors = FALSE)
s7 <- lapply(s7,data.frame,stringsAsFactors = FALSE)
s8 <- lapply(s8,data.frame,stringsAsFactors = FALSE)
#apply as df 

s1 <- plyr::rbind.fill(s1)
s2 <- plyr::rbind.fill(s2)
s3 <- plyr::rbind.fill(s3)
s4 <- plyr::rbind.fill(s4)
s5 <- plyr::rbind.fill(s5)
s6 <- plyr::rbind.fill(s6)
s7 <- plyr::rbind.fill(s7)
s8 <- plyr::rbind.fill(s8)
#use rbind fill 


###### 2.0. DATA CLEAN and VERIFY ####
names(s1) <- tolower(names(s1))
#convert to lower case col names

fts1 <- c("cohort","diagnosis_category","mode","venous_access_1","venous_return","arterial_return","survived_ecmo","survived_icu","ethnic","sex",
          "biopsy_proven_cirrhosis","portal_hypertension","hepatic_encephalopathy","very_severe_cardiovascular_disease",
          "severe_respiratory_disease","home_ventilation","chronic_renal_replacement_therapy","hiv_aids",
          "steroid_treatment","radiotherapy","chemotherapy","metastatic_disease","acute_myelogenous_lymphocytic_leukaemia_or_multiple_myeloma",
          "chronic_myelogenous_lymphocytic_leukaemia","lymphoma","congenital_immunohumoral_or_cellular_immune_deficiency_state")

s1[fts1]<- lapply(s1[fts1],factor)
#convert to factors for factor classes.

#names are too long so lets make it shorter 
colnames(s1) <- c(
        "id","cohort","admission_date","age","dx_cat",#for diagnosis_category,
        "mode","va_1","va_2","v_return","art_return",#for venous access 1 and 2,arterial return 
        "surv_ecmo","surv_icu","date_icu_discharge","ecmo_start","ecmo_finish","date_can","date_de_can",#for survived ecmo/icu/date cannulated and decannulate
        "acute_phys_score","age_score","chron_health_score","apache","ethnic","sex","hcm","wkg",#for acute physiology score,agescore,chronic health score,apache2score
        "cirrhosis","portal_htn","hepatic_enceph","s_cvd","s_respd", #for biopsy proven cirrhosis, portal hypertension,hepatic encephalopathy, severe cardiovasc dis, severe resp dis
        "dom_vent","chron_rrt","hiv","steroids_hx","rt","chemo","mets", ##for home ventilation,chronic renal replacement,hiv_aids,history of steroid treatment,radiotherapy,chemotherapy,"metastatic disease"
        "aml_mm","cml","lymphoma","cong_immuno_def"## aml or multiple myeloma, cml , lymphoma, congenital immunodeficiency state 
)

#### 2.1. Factor reassignment----
#now lets check all the levels for factor vars are appropriate.
levels(s1$mode) <- c("vv","vv")
#reassign as there are cap vv and VV 

#reassign cannula  va_1 column
levels(s1$va_1) <- c(
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

levels(s1$surv_ecmo) <- c("no","no","yes","yes")
levels(s1$surv_icu) <- c("no","no","yes","yes")
#clean up levels for surv ecmo and surv icu columsn


s1 <- subset(
        s1,
        select= -c(va_2,portal_htn,hepatic_enceph,s_cvd,dom_vent,
                   chron_rrt,rt,chemo,aml_mm,cml,lymphoma)
        )
#remove all the needless columns

s1 <- s1 %>% mutate(pmh = case_when(
        cirrhosis == "1" ~ "cirrhosis",
        hiv == "H" ~ "hiv",
        steroids_hx == "1" ~ "steroids_hx",
        mets == "1"  ~ "mets+s_respd",
        cong_immuno_def =="1" ~ "cong_immunodef"
))

s1$pmh <- as.factor(s1$pmh)
#these are confirmed and clarified

#CODE FOR CHECK
# s1 %>% select(id,pmh) %>% filter(!is.na(pmh)) %>% View()

#### 2.2. Num vars check ----

#CODE CHECK sum of duplicated items is 0 . i.e., no duplicates
# sum(duplicated(s1$id)) 
#now let's check the numerical variables
#actually all the numerical variables look fairly clean.

s1 <- subset(
        s1, 
        select= - c(
        art_return,cirrhosis,s_respd,hiv,steroids_hx,mets,cong_immuno_def
))

rm(fts1)
message("Cleaning s1 is complete.")

##### 3.1 Cleaning s2-----
colnames(s2) <- c(
        "id","chart_t","short_label","terse_form","unit","kg"
)
ptid <- unique(s1$id)
#correct col names
# now let's check all the patient are in this heparin prescrition

pt_without_heparin<- setdiff(ptid,unique(s2$id))
#this showed that these patients dont have any heparin prescribed.

##### 3.2. Col classes for s2----

s2$short_label <- as.factor(s2$short_label)
s2$terse_form <- as.double(s2$terse_form)
s2$unit <- as.factor(s2$unit)

message("Cleaning s2 is complete.")

###### 4.1. Cleaning s3-----

colnames(s3) <- c("id","chart_t","axa","apttr")
#change col names

#CODE CHECK
# sum(ptid %in% unique(s3$id))
#setdiff(ptid,unique(s3$id))
# this show that all patients have blood results.

s3$axa <- as.double(s3$axa)
s3$apttr <- as.double(s3$apttr)

message("Cleaning s3 is complete.")

#####-5.1. Cleaning s4 ------

colnames(s4) <- c(
        "id","chart_t","hb","plt","neut","fib","ldh","ferritin",
        #haemoglobin grams per litre, platelets 10 e9,neutrophils 10 e9
        #fibrinogen g per litre, ldh units per litre,ferritin ug per l,
        "ck","crp","pct","bili","alb","creat","gfr","ca","corr_ca",
        #ck in iu_l, crp in mg_l, pct ug_l, bili_umol_l, alb_g_l,
        #creat _umol_l,gfr in ml min, cal mmol_l
        "bicarb","lactate","ph"
        #bicarb_actual_mmol_l,lactate_mmol_l,
        
)

#CODE CHECK
# sum(ptid %in% (unique(s4$id))) 
# this is 254 showed that all patients have blood results.

#all cols are in character so lets turn into double.
nums4 <- names(s4)
nums4 <- nums4[3:20]

s4[nums4] <- lapply(s4[nums4],as.double)

message("Cleaning s4 is complete.")

###### ----- Cleaning s5 ----

colnames(s5) <- c(
        "id","chart_t","aspirin","aspirin_supp","clopi","warfarin","rivarox",
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
pt_without_extradrugs <- setdiff(ptid, unique(s5$id))

#lets change to numerical forms.
nums5 <- names(s5)
nums5 <- nums5[3:32]

s5[nums5] <- lapply(s5[nums5],as.double)

#need to drop columns that has 0  or all NA.

##### Cleaning s6------

colnames(s6) <- c("id","chart_t","hydrocort_inf_mghr")

pt_without_hydrocortinfus <- setdiff(ptid,unique(s6$id))
#only 44 patients have hydrocortisone infusion
        
s6$hyrocort_inf_mghr <- as.double(s6$hydrocort_inf_mghr)

message("Cleaning s6 is complete. ")
#### Clearning s7----
colnames(s7) <- c("id","chart_t","txa_inf_mghr")
pt_without_txainfus <- setdiff(ptid,unique(s7$id))
s7$txa_inf_mghr <- as.double(s7$txa_inf_mghr)

message("Cleaning s7 is complete. ")

#### Cleaning s8----
colnames(s8) <- c("id","short_label","t_form","unit","chart_t")
pt_without_bldproducts <- setdiff(ptid,unique(s8$id))

message("Cleaning s8 is complete. ")

#### Renaming dataframes to something sensible -----

message("master cleaning is complete.")



