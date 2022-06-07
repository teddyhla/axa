#####################
##OUTCOMES CLEANING##
#####################


# SOURCE ------------------------------------------------------------------

#3 source files reflecting how data collection happened. 
#1 and 2 are for the first 46 patients. 

library(tidyverse)

s00 <- readxl::read_excel("data/46circuitloss.xlsx")
s01 <- readxl::read_excel("data/46outcome1.xlsx",sheet = "Form1")

key <- readxl::read_excel("data/key.xlsx",sheet = "Sheet1")
s02 <- readxl::read_excel("data/ecmo_data_collection_final.xlsx")

#prove keys are identical between s00 and s01 
identical(s00$unique_id, s01$unique_pid)


# CLEANING S00 ------------------------------------------------------------


#compact s00 column names

names(s00) <- c(
        "id", #unique_id
        "cir_change", #does this patient have any circuit change?
        "tot_circhange",#total number of circuit changes in entire ecmo run
        "first_cc_d",#1st circuit change date
        "first_cc_t",#1st circuit change time
        "first_cc_reason",#1st circuit chang reason
        "first_cc_other",#other reasons 1st ccc
        "sec_cc_d",#2nd circuit change date
        "sec_cc_t",#2nd circuit change time
        "sec_cc_reason",#2nd circuit change reason
        "sec_cc_other",#2nd cc other reason
        "third_cc_d",#3rd cc date
        "third_cc_t",#3rd cc time
        "third_cc_reason"#3rd cc reason 
)

#noted that timestamps dont parse if there is an error in time input. i.e., time value should be 4 digits
#id_037 
s00[s00$id == "id_037","first_cc_t"] <- as.character("1900")
s00[s00$id == "id_023","sec_cc_t"] <- as.character("0941")

#make a new time merged xircuit change timestamps 
s00$firstxc_dtm <- lubridate::ymd_hm(paste(s00$first_cc_d,s00$first_cc_t))
s00$secxc_dtm <- lubridate::ymd_hm(paste(s00$sec_cc_d,s00$sec_cc_t))
s00$thirdxc_dtm <- lubridate::ymd_hm(paste(s00$third_cc_d,s00$third_cc_t))

#combine the reasons 
s00$firstxc_reason <- paste(s00$first_cc_reason,s00$first_cc_other)
s00$firstxc_reason <- as.factor(s00$firstxc_reason)
levels(s00$firstxc_reason) <- c(
        "gas_xchange+mech",#gasexchange and rising TMP/noise mechanical
        "gas_xchange+mech",#as per above
        "gas_xchange_only",#gas exchange
        "mech_only",#mechanical (rising tmp, noise)
        NA,#NA
        "haemolysis",#haemolysis
        "thrombus_accu",#thrombus accumulation
        "unknown"
)

s00$secxc_reason <- paste(s00$sec_cc_reason,s00$sec_cc_other)
s00$secxc_reason <- as.factor(s00$secxc_reason)
levels(s00$secxc_reason) <- c(
        "gas_xchange+thrombus_accu",#gas exchange and thrombus accumultion
        "gas_xchange+xc_dic",#gas exchange circuit dic 
        "gas_xchange_only",#gas exchange
        NA,#NA
        "haemolysis",#others haemolysis
        "unknown"
)

s00$thirdxc_reason <- s00$third_cc_reason
s00$thirdxc_reason <- as.factor(s00$thirdxc_reason)
levels(s00$thirdxc_reason) <- "mech_only"

#now  into final clean df.

s00 <-  s00 %>% 
        select(
                id,
                cir_change,
                tot_circhange,
                firstxc_dtm,
                secxc_dtm,
                thirdxc_dtm,
                firstxc_reason,        
                secxc_reason,
                thirdxc_reason
        )


# CLEANING S01 ------------------------------------------------------------



#change to single column name for merge.

names(s01) <- c(
        "date_ecmo_can",#date ecmo cannulation1
        "date_ecmo_DE_can",#date ecmo decan2
        "covid",#received ecmo for covid3
        "id",
        "time_ecmo_can",#time ecmo cannulation5
        "time_ecmo_DE_can",#time ecmo de cannulation6
        "admn_ct",#admission CT completed?7
        "admn_ct_results",#admn ct findings8
        "admn_ct_thrombotic",#admn ct thrombotic complications 9
        "admn_ct_hem",#admn ct heorrhagic comp 10
        "admn_ct_d",#admn ct date [11]
        "admn_ct_t",#admn ct time [12]
        "int_ct",#int ct completed yes or no[13]
        "int_ct_d",#int ct date[14]
        "int_ct_t",#int ct time [15]
        "int_ct_results",#int ct results [16]
        "int_ct_thrombotic",#int ct thrombotic com [17]
        "int_ct_hem",#int ct hemorrhagic comp [18]
        "other_comp",#additional comps not caputred in imaging [19]
        "other_comp_1",#additional comps not captured in imaging [20]
        "other_comp_2",#additional comps 2 not captured in imaging[21]
        "comp_1",#complication 1 [22]
        "comp_1_d",#complication 1 date [23]
        "comp_1_t",#complication 1 time[24]
        "comp_1_quali",#qualify complication 1 [25]
        "decan_uss",#decan uss completed or not [26]
        "decan_uss_results",#findings of decn uss [27]
        "ecmo_outcome",#ecmo utcome [28]
        "total_events_not_imaged"#tot events not caputred in imaging [29]
        
)

#lets merge date and time columns together 


s01$ecmo_dtm<-lubridate::ymd_hm(paste(s01$date_ecmo_can,s01$time_ecmo_can))

#lets fix s01$time_ecmo_de_can values 305,315,230

s00[s00$id == "id_007","time_ecmo_DE_can"] <- as.character("0305")
s00[s00$id == "id_021","time_ecmo_DE_can"] <- as.character("0315")
s00[s00$id == "id_028","time_ecmo_DE_can"] <- as.character("0230")

s01$ecmo_end_dtm <- lubridate::ymd_hm(paste(s01$date_ecmo_DE_can,s01$time_ecmo_DE_can))
s01$admct_dtm <- lubridate::ymd_hm(paste(s01$admn_ct_d,s01$admn_ct_t))
s01$intct_dtm <- lubridate::ymd_hm(paste(s01$int_ct_d,s01$int_ct_t))
s01$comp1_dtm <- lubridate::ymd_hm(paste(s01$comp_1_d,s01$comp_1_t))

s01$covid <- as.factor(s01$covid)

levels(s01$covid) <- c("no","yes")
s01$admn_ct <- as.factor(s01$admn_ct)
levels(s01$admn_ct)<- c("no","yes")

s01$int_ct <- as.factor(s01$int_ct)
levels(s01$int_ct) <- c("no","yes")
s01$decan_uss <- as.factor(s01$decan_uss)
levels(s01$decan_uss) <- c(
        "died_on_ecmo",
        "died_on_ecmo",
        "no",
        "yes"
)
s01$ecmo_outcome <- as.factor(s01$ecmo_outcome)
s01$total_events_not_imaged <- as.numeric(s01$total_events_not_imaged)

s01$admn_ct_results <- as.factor(s01$admn_ct_results)
levels(s01$admn_ct_results) <- c(
        "both_haem_and_throm",#both haemorrhagic and thrombotic
        "only_haem",#haemorrhagic complications
        "no_comp",#no complications
        "only_throm"
)
s01$admn_ct_thrombotic <- as.factor(s01$admn_ct_thrombotic)
levels(s01$admn_ct_thrombotic) <- c(
        "dvt_unilat",
        "lv_thrombus",
        NA,
        "non_mass_pe(seg+subseg)",
        "no_comp",
        "renal_vein_thrombus",
        "splenic_infarcts"
)

s01$admn_ct_hem <- as.factor(s01$admn_ct_hem)
levels(s01$admn_ct_hem) <- c(
        "intracranial_bleed_wo_midlineshift",
        NA,
        "none",
        "other_surg_hemorrhage"#(cardiac tamponade,insertion site, surgical site)
)

s01$int_ct_results <- as.factor(s01$int_ct_results)
levels(s01$int_ct_results) <- c(
        "only_haem",#only haemorrhagic complications
        "no_comp",#no complications
        "only_throm"#only thrombotic complications
)

s01$int_ct_thrombotic <- as.factor(s01$int_ct_thrombotic)
s01$int_ct_hem <- as.factor(s01$int_ct_hem)

levels(s01$int_ct_thrombotic) <- c(
        "dvt_unilat",
        NA,
        "non_mass_pe(seg+subseg)",
        "none",
        "none"
)

levels(s01$int_ct_hem) <- c(
        "intracranial_bleed_wo_midlineshift",
        "large_rectussheath_haematoma",
        "med_hem",#pulmonary,GI,cutaneous,airway
        NA,
        "none",
        "spont_retropertioneal_bleed_lumbar_art",#spontbleed from lunbar art
        "increase_known_icb"#same intracranial foci,some larger
)

s01$other_comp <- as.factor(s01$other_comp)
levels(s01$other_comp) <- c(
        "none",
        "only_hemrr",#only additional hemorrhagic complications
        "only_thromb"#only additional thrombotic complications
)
s01$other_comp_1 <- as.factor(s01$other_comp_1)
s01$other_comp_2 <- as.factor(s01$other_comp_2)
s01$comp_1_quali <- as.factor(s01$comp_1_quali)
levels(s01$comp_1_quali) <- c(
        "dvt",
        "major_airway/pulm/cut_hem",#major airway chest cutaneous hemorrhage
        "major_gi_hem",#major gi hem
        "nonmajor_hem",#nonmajor hemm
        "thromb_dic",#thrombotic dic with left big, 4th toe, tip of nose
        "thromb_dic",#thrombotic dic
        "thromb_dic"
)

s01$decan_uss_results <- as.factor(s01$decan_uss_results)
levels(s01$decan_uss_results) <- c(
        "died_on_ecmo",
        "dvt_post_decan",
        "normal",
        "not_done",
        "not_done",
        "died_on_ecmo"
)

s01 <- s01 %>% select(
        -c(date_ecmo_can,
           time_ecmo_can,
           date_ecmo_DE_can,
           time_ecmo_DE_can,
           admn_ct_d,
           admn_ct_t,
           int_ct_d,
           int_ct_t,
           comp_1_d,
           comp_1_t
           )
)


##lets reorder s01
#col_order <- c(
#        "id",
#        "covid",
#        "admct_dtm",
#        "admn_ct",
#        "admn_ct_results",
#        "admn_ct_thrombotic",
#        "admn_ct_hem",
#        "intct_dtm",
#        "int_ct",
#        "int_ct_results",
#        "int_ct_thrombotic",
#        "int_ct_hem",
#        "other_comp",
#        "total_events_not_imaged",
#        "other_comp_1",
#        "other_comp_2",
#        "comp_1",
#        "comp1_dtm",
#        "comp_1_quali",
#        "decan_uss",
#        "decan_uss_results",
#        "ecmo_outcome",
#        "ecmo_dtm",
#        "ecmo_end_dtm"
#        
#)
#
#s01 <- s01[,col_order]

## NEED TO MERGE S01 and S00 

s01 <- left_join(s01,s00,by="id")
rm(s00)

# CLEANING S02 ------------------------------------------------------------



names(s02) <- c(
        "id",#[1]
        "date_ecmo_can",
        "time_ecmo_can", #[3]
        "date_ecmo_DE_can",#[4]
        "time_ecmo_DE_can",#[5]
        "covid",#[6]
        "ecmo_outcome",#[7]
        "admn_ct",#[8]
        "admn_ct_d",#[9]
        "admn_ct_t",#[10]
        "admn_ct_results",#[11]
        "admn_ct_thrombotic",#[12]
        "admn_ct_hem",#[13]
        "int_ct",#[14]
        "int_ct_d",#[15]
        "int_ct_t",#[16]
        "int_ct_results",#[17]
        "int_ct_thrombotic",#[18]
        "int_ct_hem",#[19]
        "other_comp",#[20]
        "total_events_not_imaged",#[21]
        "other_comp_1",#[22]complications_1
        "other_comp_2",#[23]
        "other_comp_3",#[24]
        "other_comp_4",#[25]
        "comp_1",#[26]
        "comp_1_d",#[27]
        "comp_1_t",#[28]
        "comp_1_quali",#[29]
        "comp_2",
        "comp_2_d",
        "comp_2_t",
        "comp_2_quali",#[33]
        "comp_3",
        "comp_3_d",
        "comp_3_t",
        "comp_3_quali",#[37]
        "comp_4",
        "comp_4_d",
        "comp_4_t",
        "comp_4_quali",#[41]
        "decan_uss",
        "decan_uss_results",#[43]
        "tot_circhange",#[44]
        "cir_change",#[45]
        "first_cc_t",#[46]
        "first_cc_d",
        "firstxc_reason",
        "sec_cc_d",#[49]
        "sec_cc_t",
        "secxc_reason",#[51]
        "firstxc_other",#52
        "secxc_other",#53
        "third_cc_d",#54
        "third_cc_t",#55
        "thirdxc_other",#56
        "thirdxc_reason",#57
        "fourth_cc_d",
        "fourth_cc_t",
        "fourthxc_other",
        "fourthxc_reason"#[61]
)

#changing time related columns

s02$ecmo_dtm  <- lubridate::ymd_hm(paste(s02$date_ecmo_can,s02$time_ecmo_can))
s02$ecmo_end_dtm <- lubridate::ymd_hm(paste(s02$date_ecmo_DE_can,s02$time_ecmo_DE_can))
s02$admct_dtm <- lubridate::ymd_hm(paste(s02$admn_ct_d,s02$admn_ct_t))
s02$intct_dtm <- lubridate::ymd_hm(paste(s02$int_ct_d,s02$int_ct_t))
s02$comp1_dtm <- lubridate::ymd_hm(paste(s02$comp_1_d,s02$comp_1_t))
s02$comp2_dtm <- lubridate::ymd_hm(paste(s02$comp_2_d,s02$comp_2_t))
s02$comp3_dtm <- lubridate::ymd_hm(paste(s02$comp_3_d,s02$comp_3_t))
s02$comp4_dtm <- lubridate::ymd_hm(paste(s02$comp_4_d,s02$comp_4_t))
s02$firstxc_dtm <- lubridate::ymd_hm(paste(s02$first_cc_d,s02$first_cc_t))
s02$secxc_dtm <- lubridate::ymd_hm(paste(s02$sec_cc_d,s02$sec_cc_t))

# s02$third_Cc_t 808 needs fixing 
s02[s02$id == "id_198","third_cc_t"] <- as.character("0808")
s02$thirdxc_dtm <- lubridate::ymd_hm(paste(s02$third_cc_d,s02$third_cc_t))
s02$fourthxc_dtm <- lubridate::ymd_hm(paste(s02$fourth_cc_d,s02$fourth_cc_t))

#remove the other time date columns.

s02 <- s02 %>% select (
        - c(
                date_ecmo_can,
                time_ecmo_can,
                date_ecmo_DE_can,
                time_ecmo_DE_can,
                admn_ct_d,
                admn_ct_t,
                int_ct_d,
                int_ct_t,
                comp_1_d,
                comp_1_t,
                comp_2_d,
                comp_2_t,
                comp_3_d,
                comp_3_t,
                comp_4_d,
                comp_4_t,
                first_cc_d,
                first_cc_t,
                sec_cc_d,
                sec_cc_t,
                third_cc_d,
                third_cc_t,
                fourth_cc_d,
                fourth_cc_t
        )
)

colfactors <- c(
        "covid",
        "ecmo_outcome",
        "admn_ct",
        "admn_ct_results",
        "admn_ct_thrombotic",
        "admn_ct_hem",
        "int_ct",
        "int_ct_results",
        "int_ct_thrombotic",
        "int_ct_hem",
        "other_comp",
        "other_comp_1",
        "other_comp_2",
        "other_comp_3",
        "other_comp_4",
        "comp_1",
        "comp_1_quali",
        "comp_2",
        "comp_2_quali",
        "comp_3",
        "comp_3_quali",
        "comp_4",
        "comp_4_quali",
        "decan_uss",
        "decan_uss_results",
        "tot_circhange",
        "total_events_not_imaged",
        "cir_change",
        "firstxc_reason",
        "secxc_reason",
        "firstxc_other",
        "secxc_other",
        "thirdxc_other",
        "thirdxc_reason",
        "fourthxc_other",
        "fourthxc_reason"
)

s02[colfactors] <- lapply(s02[colfactors],factor)

#sapply(s02[colfactors],summary)
#this code show summary of most situations

#Let's clean s02 factors into some sensible leels.
levels(s02$covid) <- c("no","yes")

levels(s02$admn_ct) <- c("no","yes")
levels(s02$int_ct) <- c("no","yes")
levels(s02$decan_uss) <- c("no","yes")
levels(s02$cir_change) <- c("no","Yes")

levels(s02$admn_ct_results)<- c(
        "both_haem_and_throm",
        "only_haem",
        "isch_bowel_lung",#ischaemic bowel and lung
        "no_comp",#no complications
        "only_throm"#thrombotic complications
)


# clean up and sync with key. 


# MERGE FINAL DF ----------------------------------------------------------



intersect(s02$id,s01$id)
df <- plyr::rbind.fill(s02,s01)

rm(s01,s02)

length(df$id)
length(unique(df$id))
#both matched at 253.
sapply(df,summary) 
#lets check all the factors are appropraite.

# CLEAN FINAL DF FACTORS --------------------------------------------------
#levels(df$admn_ct_thrombotic) <- c(
#        "dvt_unilat",#1 dvt unilat
#        "llimb_art_throm",#2 fem art thrombus
#        "llimb_art_throm",#3 ilia art thrombus
#        "isch_stroke",#4 ischaemic stroke
#        "isch_stroke+non_mass_pe_segsubseg",#5 ischaemic stroke and non massive pe -segmental and subsegmental
#        "isch_stroke+splen_renal_infarc",#6 isch st + splenic renal infarcts
#        "ivc_throm",#7 ivc thrombus
#        "lung_infarc",#8 lung infarcts
#        "mass_pe",#9 mass pe (saddle and haemodynamic disturbance)
#        "mass_pe+isch_stroke",#10 mass pe + isch stroke
#        "non_mass_pe_segsubseg",#11 non mass pe seg subseg
#        "non_mass_pe_+isch_stroke_splen_renal_infarc",#12 nmpe + isch + splen inf
#        "non_mass_pe+isch_stroke_splen_renal_infarct",#13 nmpe +isch + splen infarct
#        "non_mass_pe+lung_infarc",#14nmpe and lung infarct
#        "non_mass_pe+splen_renal_infarc",#15 nmpe +renal infarct
#        ""
#)

#levels(df$int_ct_results) <- c(
#        "both_haem_and_throm",
#        "only_haem",
#        "no_comp",
#        "only_throm"
#)
#
#levels(df$other_comp) <- c(
#        "both_haem_and_throm",
#        "no_comp",
#        "only_haem",
#        "only_throm",
#        "no_comp",
#        "only_haem",
#        "only_throm"
#)

df$total_events_not_imaged <- as.factor(df$total_events_not_imaged)
df$comp_1 <- as.factor(df$comp_1)
df$tot_circhange <- as.factor(df$tot_circhange)
df$cir_change <- as.factor(df$cir_change)

##as per andy R re: circuit change reasons
#### circuit change reasons - simplified to mechanical issues, gas exchange deterioration
#,haematologial  (combination of thrombus, dic, haemolysis)
#combinatinon

#NOW WE SHALL APPEND mrn' to df outcome.

key$mrn <- key$patient_hospital_no 
key$id <- key$unique_id
df <- left_join (df, key %>% select(mrn,id), by = "id")
rm(key)

## as per discussion with A RETTER, will summarise into 
## non arterial thrombus, arterial thrombus, multiple 
## pulmonary hemorrhage, GI hemorrhage,muscular, others
## DIC and HIT as it is 

levels(df$admn_ct_results) <- c(
        "both_haem_throm",
        "only_haem",
        "only_throm",
        "no_comp",
        "only_throm"
)

levels(df$admn_ct_thrombotic) <- c(
        "non_art_throm", #dvt unilat
        "art_throm", #fem art throm
        "art_throm", #iliar art throm
        "art_throm", #isch stroke
        "multiple", # isch stroke , non massive pe,
        "multiple", #isch stroke, splenic and renal infarcts
        "non_art_throm",# ivc thrombus
        "art_throm",#lung infarcts
        "art_throm",#massive pe
        "multiple",#massive pe and stroke
        "art_throm",#nmpe [11]
        "multiple_throm",#nmpe + stroke and renal infarct [12]
        "multiple_throm", #nmpe + stroke + splenic
        "multiple_throm",# similar to above [14]
        "multiple_throm",#[15]
        "multiple_throm",#[16]
        "multiple_throm",#[17]
        "non_art_throm",#18
        "art_throm",#[19]
        "non_art_throm",#[20]
        "art_throm",
        "art_throm",
        "no_comp",
        "non_art_throm",
        "non_art_throm" #[25 splenic]
)

levels(df$admn_ct_hem) <- c(
        "icb_wo",#[1] icb without midline shift
        "multiple",#[2] multiple 
        "medhem",#medical hemorrhage
        "other_haem",#other surgical hemorrhage
        "icb_wo",#[5] icb without midline shift
        "none",
        "other_haem"
)

levels(df$int_ct_results) <- c(
        "both_haem_throm",#[1]
        "only_haem",
        "no_comp",
        "only_throm",
        "only_haem",
        "no_comp",
        "only_throm" #[7]
)

levels(df$int_ct_thrombotic) <- c(
        "art_throm",#[1] renal colon
        "non_art_throm",#[2]
        "non_art_throm",#[3]
        "non_art_throm",#[4] hepartic infarcts
        "art_throm",#[5] ischaemic bowel
        "art_throm",#[6] lung infarct
        "art_throm",#[7] nmpe
        "non_art_throm",#[8] rij throm
        "non_art_throm",#[9] dvt unilat
        "non_art_throm",#[10] non mass pe
        "none"
)

levels(df$int_ct_hem) <- c(
        "icb_wo",#[1] icb without midline shift
        "icb_WITH_midlineshift",#[2] icb with midline shift
        "medhem",#[3]medical hemorrhages
        "other_haem",#[4] other surg hemo
        "icb_wo",#[5]
        "medhem",#[6 ]rectus sheath
        "med_hem",
        "none",#[8]
        "other_haem",#spont retropertineal knoeel
        "icb_wo"#10
)

levels(df$other_comp) <- c(
        "both_haem_throm",
        "no_comp",
        "only_haem",
        "only_throm",
        "none",
        "only_haem",
        "only_throm"#[7]
)

levels(df$other_comp_1) <- c("haem","throm")
levels(df$other_comp_2) <- c("haem","throm")
levels(df$other_comp_3) <- c("haem","throm")
levels(df$other_comp_4) <- "haem"

levels(df$comp_1) <- c("haem","throm")
levels(df$comp_1_quali) <- c(
        "non_art_throm",#1 cv clot
        "non_art_throm",#[2 ] dvt
        "non_art_throm",#[3] groin haematoma
        "hit",
        "hit",
        "non_art_throm",#[6] ij throm
        "art_throm",#[7] isch stroke
        "multiple_throm",#[8] ivc and right atrial 
        "pulm_haem",#[9]
        "gi",#10 gi hem
        "art_throm",#11 nmpe
        "other_haem",#12 other haem non major 
        "none",#13 non occlusive lijv
        "other_haem",#14 other surgical hemorrhage
        "muscular_haem",#15 psoas haematoma
        "art_throm",#16 radial art throm
        "art_throm",#17 radial art throm
        "none",#18 rij mural thrombus
        "art_throm",#19 right ventricle thrombus
        "none",#20 superficial thrombo
        "non_art_throm",#21 dvt
        "pulm_haem",#22 pulmonary haem
        "gi",#23 gi haem
        "other_haem",#24 other haem
        "dic"
)

levels(df$comp_2)<-c("haem","throm")
levels(df$comp_2_quali) <- c(
        "non_art_throm",#[1] cephalic vein throm
        "non_art_throm",#[2] dvt
        "hit",
        "gi",#4 gi haemo
        "art_throm",#5 nmpe
        "other_haem",#6 non major haem
        "other_haem",#7 other surg haem
        "none"
)

levels(df$comp_3) <- c("haem","throm")
levels(df$comp_3_quali)<-c(
        "hit",
        "pulm_haem",
        "art_throm",
        "other_haem"
)

levels(df$comp_4) <- "haem"
levels(df$comp_4_quali) <- "other_haem"

levels(df$decan_uss_results) <- c(
        "dvtpostdecan",#1 dvt post decan
        "minor",#2
        "minor",
        "none",
        "minor",
        "died_on_ecmo",
        "dvtpostdecan",
        "none",
        "not_done"
        
)

levels(df$cir_change) <- c("no","no","yes")

#simplying circuti causes to
# mechanical issues, gas exchange deterioration, haematological, combination

levels(df$firstxc_reason) <- c(
        "gas_xch",#gas exchange[1]
        "combo",#combination
        "mech",
        "combo",
        "combo",#[5]
        "combo",
        "combo",
        "combo",#"[8] others
        "haematol",#9 thrombus accumu
        "combo",#[10]
        "combo",#[11]
        "unknown",
        "combo",
        "gas_xch",#14 gas exchange only
        "mech",
        "haematol",
        "haematol",
        "unknown"#[18]
)

levels(df$firstxc_other) <- c(
        "mech",#1 acces pressure not reading
        "haematol",
        "haematol",
        "haematol",
        "haematol",#5
        "haematol",
        "haematol",
        "haematol",
        "haematol",
        "haematol"
)

levels(df$secxc_reason) <- c(
        "gas_xch",#[1] gas exchabnge
        "mech",#[2] mech
        "combo",#[3] mech and others
        "combo",
        "combo",
        "combo",#6 others
        "haematol",
        "combo",
        "combo",
        "combo",#[9] gas exchange and thrombus 
        "combo",
        "gas_xch",
        "haematol",
        "unknown"#13 unknown
)

levels(df$secxc_other)<- c(
        "haematol",#1
        "haematol",
        "haematol",
        "haematol",#4
        "haematol",
        "haematol",
        "haematol"
)

levels(df$thirdxc_other) <- "haematol"
levels(df$thirdxc_reason) <- c(
        "mech",
        "combo",
        "combo",
        "unknown",
        "mech"
        
)

levels(df$fourthxc_reason) <- "mech"

#need to make a total bleeding event, total clotting event, total all events, 



#best would be to combine reason + other and then re-level.

# CHECK FINAL DF against df0 ----------------------------------------------

#will generate a csv file for andy and silvana to help merge
# x <- sapply(df,levels)
# xx <- lapply(x,unlist)
# max <- max(sapply(x,length))
# y <- do.call(cbind, lapply(xx, function(z)c(z, rep(NA, max-length(z)))))
# y <- as.data.frame(y)
# write.csv(y,file="outcomereview.csv")
# 