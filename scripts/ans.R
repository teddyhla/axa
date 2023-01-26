dans$r <- as.character(dans$admn_ct_thrombotic)
dfcore
df <- left_join(df,dfcore %>% select(mrn,group), by = "mrn")
rlang::last_error()
dfcore
dfcore$cohort
dans <- left_join( df %>% select(mrn,admn_ct_results),dfcore %>% select(mrn,cohort), by = "mrn")
dans
dans <- left_join( df %>% select(mrn,admn_ct_thrombotic),dfcore %>% select(mrn,cohort,date_can), by = "mrn")
dans
str(dans)
dans$group <- ifelse(dans$date_can < as.Date("2019-12-01"),"gapt","gaxa")
dans$group <- as.factor(dans$group)
dans
str(dans)
dans$r <- as.character(dans$admn_ct_thrombotic)

dans$massive <- str_detect(dans$r, regex("saddle",ignore_case = TRUE))