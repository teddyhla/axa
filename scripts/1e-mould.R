
#load the clean file from 1d-dtf.R
load(file="data/clean/out.RData")
source(file = "scripts/utils.R")

#libraries
library(tidyverse)
#library(tidylog)
#load other data
tp <- readRDS("data/axa_NONanonymised_data_20220714.rds")


####sort aki rrt data

daki <- tp$rrt_data
#setdiff(dfcore$mrn,daki$mrn)
#length(intersect(dfcore$mrn,daki$mrn))
#so thats all checks out and is reassuring.

daki2 <- daki %>%
        group_by(mrn) %>%
        summarise(temp = sum(RRT)) %>%
        mutate(aki = case_when(
                temp >0 ~ 1,
                temp == 0 ~ 0
        )) %>%
        mutate(rrt = case_when(
                temp >0 ~ "yes",
                temp == 0 ~ "no"
        ))
#clean RRT data ever been on RRT vs never been on RRT

dfcore <- left_join(
        dfcore,
        daki2 %>% select(mrn,aki,rrt),
        by = "mrn"
)

dfcore$rrt <- as.factor(dfcore$rrt)

####
#make a final ready to use dataframe "dm"
dm <- dfcore %>% 
        select(
                mrn,
                age,
                surv_ecmo,
                surv_icu,
                acute_phys_score,
                age_score,
                chron_health_score,
                apache,
                ethnic,
                sex,
                wkg,
                group,
                ecmod,
                bmi,
                ecmo_start,
                cohort
        )

#254 * 13
dm$ecmos <- as.Date(dm$ecmo_start)

dm <- dm %>% 
        mutate(ecmosb = case_when(
                ecmo_start < as.Date("2015-04-01") ~ "15",
                (as.Date("2015-04-01") < ecmo_start) & (ecmo_start < as.Date("2016-04-01")) ~ "15-16",
                (as.Date("2016-04-01") < ecmo_start) & (ecmo_start < as.Date("2017-04-01")) ~ "16-17",
                (as.Date("2017-04-01") < ecmo_start) & (ecmo_start < as.Date("2018-04-01")) ~ "17-18",
                (as.Date("2018-04-01") < ecmo_start) & (ecmo_start < as.Date("2019-04-01")) ~ "18-19",
                (as.Date("2019-04-01") < ecmo_start) & (ecmo_start < as.Date("2020-04-01")) ~ "19-20",
                (as.Date("2020-04-01") < ecmo_start) & (ecmo_start < as.Date("2021-04-01")) ~ "20-21",
                as.Date("2021-04-01") < ecmo_start ~ "21-22"
        ))

#ecmo is a winter game so treated as winter intervals
dm$ecmosb <- as.factor(dm$ecmosb)

dm$ecmod <- as.numeric(dm$ecmod)

#add blood test variables
dm <- left_join(
        dm,
        d1bl %>% select(-c(nd_median,nd_min,nd_max)),
        by = "mrn"
)

# 254 *67 

#add ttrg related variables
dm <- left_join(
        dm,
        dttr %>% select(mrn,tlow,thi,ttrg),
        by = "mrn"
)

#add variability related variables
dm <- left_join(
        dm,
        dsig %>% select(mrn,sigm),
        by = "mrn"
)

#add hep day per dose relatd variables
dm <- left_join(
        dm,
        dcumhep %>% select(mrn,hep_wkgday),
        by = "mrn"
)

#add no of prescription changes
dm <- left_join(
        dm,
        dheprl %>% select(mrn,rl_day),
        by = "mrn"
)

#add circuitchange 
dm <- left_join(
        dm,
        dfci %>% select(mrn,totc,cday),
        by = "mrn"
)

##adding haemorhhagic comps

df3$hboth <- df3$toth + df3$totboth

dm <- left_join(
        dm,
        df3 %>% select(mrn,toth,hboth),
        by = "mrn"
)

# add blood products transfused

dm <- left_join(
        dm,
        dprd,
        by = "mrn"
)

dm$bldtot_day <- dm$bldtot/as.numeric(dm$ecmod)


#remove messy repeated ones
dm2 <- dm

dm <- dm %>% select(
        - c(
                ethnic,
                surv_icu,
                acute_phys_score,
                age_score,
                chron_health_score,
                
        )
)

#remove missing datas
dm <- dm %>% select(
        - c(
                plt_min,
                plt_max,
                fib_min,
                fib_max,
                ldh_min,
                ldh_max,
                ferritin_min,
                ferritin_max,
                ck_min,
                ck_max,
                crp_min,
                crp_max,
                pct_min,
                pct_max,
                bili_min,
                bili_max,
                gfr_min,
                gfr_max,
                ca_min,
                ca_max,
                corr_ca_min,
                corr_ca_max,
                ph_min,
                ph_max
                
        )
)

###

###NEED TO CORRECT Na's

## 
dm[is.na(dm)] <- 0



# COMPLICATIONS LONGFORM --------------------------------------------------


####use custom function to churn out complications in long form
tx1 <- shp("admn_ct_results","admct_dtm")
tx2 <- shp("int_ct_results","intct_dtm")
tx3 <- shp("comp_1","comp1_dtm")
tx4 <- shp("comp_2","comp2_dtm")
tx5 <- shp("comp_3","comp3_dtm")
tx6 <- shp("comp_4","comp4_dtm")


#bind by row and re-organise using dplyr
dcmp <- rbind(tx1,tx2,tx3,tx4,tx5,tx6)
dcmp$value[is.na(dcmp$value)] <- "no_comp"

levels(dcmp$value) <- c(
        "both",
        "only_h",
        "only_t",
        "no_comp",
        "only_h",
        "only_t"
)

dcmp <- dcmp %>% 
        group_by(mrn)%>%
        arrange(mrn) %>% 
        ungroup()

dcmp <- drs(dcmp)

t1cmp <- dcmp %>%
        filter(!events == "admn_ct_results")%>%
        filter(!value %in% c("no_comp","none"))%>%
        group_by(mrn)%>%
        slice(1)%>%
        ungroup()


t1cmp <- left_join(
        dfcore %>% select(mrn,group),
        t1cmp,
        by = "mrn"
)

#t1cmp on review

# 6419229C comp1 is 8 days older (?) [/] likely mistake comp1_dtm "2019-05-17"
# 5096268E comp1 is several days older (?) [/] likely mistake comp1 dtm "2020-05-25"
# 2442798A interval ct is significantly older [] this is way depper. discrepancy bitime but dont trust the one from 
# 244 looks too complicated. likely an error. so ignore.
# 6803879M comp1 is 1 day older []

#  
oh1cmp <- dcmp %>%
        filter(!events == "admn_ct_results") %>%
        filter(value %in% c("both","only_h")) %>%
        group_by(mrn)%>%
        slice(1) %>%
        ungroup()

oh1cmp <- left_join(
        dfcore %>% select(mrn,group),
        oh1cmp,
        by = "mrn"
)




####do the same for circuit change
ty1 <- xcs("firstxc_dtm",1)
ty2 <- xcs("secxc_dtm",2)
ty3 <- xcs("thirdxc_dtm",3)
ty4 <- xcs("fourthxc_dtm",4)


dxc <- rbind(ty1,ty2,ty3,ty4)

dxc <- dxc %>%
        select(mrn,xc,time) %>%
        group_by(mrn)%>%
        arrange(mrn) %>%
        ungroup()

####

# NOW USE THIS FOR TTR PRE 1stcomp ----------------------------------------

#we will use t1cmp$time & oh1cmp

d1tr <- t1cmp %>% select(mrn,value,time)
d1tr <- left_join(
        d1tr,
        dfcore %>% select(mrn,ecmo_finish),
        by = "mrn"
)
d1tr <- d1tr %>% 
        mutate(tn = case_when(
                is.na(time) ~ ecmo_finish,
                !is.na(time) ~ time
        ))

d1tr <- d1tr %>% select(mrn,tn)

#d1tr is a dataframe that has times for "ANY" event
###

o1 <- oh1cmp %>% select(mrn,value,time)
o1 <- left_join(
        o1,
        dfcore %>% select(mrn,ecmo_finish),
        by = "mrn"
)
o1 <- o1 %>% 
        mutate(tn = case_when(
                is.na(time) ~ ecmo_finish,
                !is.na(time) ~ time
        ))

o1 <- o1 %>% select(mrn,tn)
#o1 is a dataframe that has times for "ONLY HAEM" event


# now lets use it to subset from "tco" ------------------------------------

#for verification 
#temp <- d1tr

d1tr <- left_join(
        tco,
        d1tr %>% select(mrn,tn),
        by = "mrn"
)
#here is 254 patients

d1tr <- d1tr %>% 
        group_by(mrn) %>%
        filter(chart_t >= ecmo_start & chart_t <= tn) %>%
        ungroup()
#here it drops the other patients it drops 4 patients.
#lets find out who they are
#"6419229C" "6803879M" "5096268E" "2442798A"
#prob1 <- t1cmp %>% filter(mrn %in% c("6419229C","6803879M","5096268E","2442798A"))
#1103375H - checked out

#up to here is 250 pt

d1trx <- d1tr %>%
        select(mrn,chart_t,ecmo_start,tn,axa,group) %>%
        filter(group == "gaxa") %>% #177 up to here 
        group_by(mrn) %>% #here drop na results in dropping 6 patients.
        drop_na(axa) %>% 
        arrange(chart_t) %>%
        group_split()

#setdiff((unique(d1tr$mrn[d1tr$group == "gaxa"])),d1trx$mrn)
#prob2 <- setdiff((unique(d1tr$mrn[d1tr$group == "gaxa"])),d1trx$mrn)
#this showed the 6 patients lost - why?
#171

d1trp <- d1tr %>%
        select(mrn,chart_t,ecmo_start,tn,apttr,group) %>%
        filter(group == "gapt") %>% #73 
        group_by(mrn) %>%
        drop_na(apttr) %>%
        arrange(chart_t) %>%
        group_split()
#73 

#treatment for gaxa 
temp1 <- map(d1trx,wz)
temp1 <- map(temp1,lwg)
temp3 <- map(temp1,mwt)
temp3 <- map(temp3,rap)
temp3 <- map(temp3,edd)

temp3 <- plyr::ldply(temp3,data.frame)
temp3 <- as.tibble(temp3)

## treatment for gapt 

temp2 <- map(d1trp,wz)
temp2 <- map(temp2,lwg)
temp4 <- map(temp2,mwt)
temp4 <- map(temp4,rap)
temp4 <- map(temp4,edd)

temp4 <- plyr::ldply(temp4,data.frame)
temp4 <- as.tibble(temp4)

d1tr <- rbind(temp3,temp4)

### Fihns variability

temp5 <- map(temp1,mwtv)
temp5 <- map(temp5,fnm)

temp6 <- map(temp2,mwtv)
temp6 <- map(temp6,fnm)

temp5 <- plyr::ldply(temp5,data.frame)
temp5 <- as.tibble(temp5)
temp6 <- plyr::ldply(temp6,data.frame)
temp6 <- as.tibble(temp6)

d1sig <- rbind(temp5,temp6)

rm(temp1,temp2,temp3,temp4,temp5,temp6)

###

o1 <- left_join(
        tco,
        o1 %>% select(mrn,tn),
        by = "mrn"
)

o1 <- o1 %>% 
        group_by(mrn) %>%
        filter(chart_t >= ecmo_start & chart_t <= tn) %>%
        ungroup()

o1x <- o1 %>%
        select(mrn,chart_t,ecmo_start,tn,axa,group) %>%
        filter(group == "gaxa") %>%
        group_by(mrn) %>%
        drop_na(axa) %>%
        arrange(chart_t) %>%
        group_split()

o1p <- o1 %>%
        select(mrn,chart_t,ecmo_start,tn,apttr,group) %>%
        filter(group == "gapt") %>%
        group_by(mrn) %>%
        drop_na(apttr) %>%
        arrange(chart_t) %>%
        group_split()


#treatment for gaxa 
temp1 <- map(o1x,wz)
temp1 <- map(temp1,lwg)
temp3 <- map(temp1,mwt)
temp3 <- map(temp3,rap)
temp3 <- map(temp3,edd)

temp3 <- plyr::ldply(temp3,data.frame)
temp3 <- as.tibble(temp3)

## treatment for gapt 

temp2 <- map(o1p,wz)
temp2 <- map(temp2,lwg)
temp4 <- map(temp2,mwt)
temp4 <- map(temp4,rap)
temp4 <- map(temp4,edd)

temp4 <- plyr::ldply(temp4,data.frame)
temp4 <- as.tibble(temp4)

o1tr <- rbind(temp3,temp4)

##

### Fihns variability

temp5 <- map(temp1,mwtv)
temp5 <- map(temp5,fnm)

temp6 <- map(temp2,mwtv)
temp6 <- map(temp6,fnm)

temp5 <- plyr::ldply(temp5,data.frame)
temp5 <- as.tibble(temp5)
temp6 <- plyr::ldply(temp6,data.frame)
temp6 <- as.tibble(temp6)

o1sig <- rbind(temp5,temp6)

rm(temp1,temp2,temp3,temp4,temp5,temp6)

# EXPORTAGE ---------------------------------------------------------------


####
l <- setdiff(ls(),lsf.str())
sl <- c(
        "blvars",
        "dcumhep",
        "dttr",
        "dfcore",
        "df",
        "dg",
        "dheprl",
        "dcumhep",
        "dprd",
        "dsig",
        "pt_without_bldproducts",
        "pt_without_heparin",
        "dm",
        "dm2",
        "t1cmp",
        "dcmp",
        "dxc",
        "oh1cmp",
        "d1tr",
        "d1sig",
        "o1tr",
        "o1sig",
        "daki"
)

#l <- l[!l %in% frm]

save(list = sl,file = "data/clean/finalout.RData")

message("SUCCCESS 1e-mould")


#things need to be fixed and reviewed (very comprehensive)
# 1419834R (because of admn ct time is one year before ecmo start time, 
# 6862925K because interval ct result is one month from ecmo start time, 
# 8102549N 8 days between ecmo start and admn ct,
# 2442798A is one month apart from admn ct and ecmo start, same with 
# 6419229C because comp1  is earlier than ecmo start, 
# 5096268E comp1 is earlier than ecmo start, 
# 2442798A interval CT is 9 days earlier than ecmo start, 
# 6539653Y admn ct is 2 days earlier than ecmo_start, 
# 6803879M comp_1 is 23 hours earlier. 
# 5765901X admin ct is 21 hours earlier,
# 6860416Y admn ct is 20 hrs earlier ,
# 6859559U  admn ct is 7.9 hours earlier, 
# 4707786A admn ct is , 
# 4169075Q 6.3 hrs earlier admn ct, 
# 8102549N int ct is 4 hours earlier , admn ct is 4 hours earlier ,

#