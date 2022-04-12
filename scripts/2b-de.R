####TASKS----
####1. calculate time on ecmo in minutes, days, append this to relv df's
####2. summarise the drug information into one as multiple factor
####3. assign groups for PE and non-PE
####4. assign
library(tidyverse)
source("scripts/1b-eda.R")


#SENSECHECK
#(df_m$ecmo_finish - df_m$ecmo_start)/ 24
#matches with
# df_m$date_de_can - df_m$date_can

df_m$runt_hr <- df_m$ecmo_finish - df_m$ecmo_start
df_m$runt_day <- df_m$date_de_can - df_m$date_can

df_m$icuday <- df_m$date_icu_discharge - df_m$admission_date
#above codes are there to insert a runtime in day and hours.

# inserting runtimes and days to df's -------------------------------------



df_coag <-left_join(df_coag,df_m %>% select(id, date_can, date_de_can, ecmo_start, ecmo_finish),
                by = "id")

df_bl <-right_join(df_m %>% select(id, date_can, date_de_can, ecmo_start, ecmo_finish),df_bl,
        by = "id")

df_h <-left_join(df_h,df_m %>% select(id, date_can, date_de_can, ecmo_start, ecmo_finish),
        by = "id")

df_hydrocort <-left_join(df_hydrocort,df_m %>% select(id, date_can, date_de_can, ecmo_start, ecmo_finish),
                by = "id")

df_prd <- left_join(df_prd,df_m %>% select(id, date_can, date_de_can, ecmo_start, ecmo_finish),
                    by = "id")

df_rx <- left_join(df_rx,df_m %>% select(id, date_can, date_de_can, ecmo_start, ecmo_finish),
                   by = "id")

df_txa <- left_join(df_txa,df_m %>% select(id, date_can, date_de_can, ecmo_start, ecmo_finish),
                    by = "id")


# calculating it for each df ----------------------------------------------


df_bl %<>% group_by(id) %>% mutate(runt_min = (chart_t - ecmo_start) / 60) %>%
        mutate(ecmo_day = ceiling(runt_min / (60 * 24))) %>%
        mutate(runt_min = as.numeric(runt_min)) %>%
        mutate(ecmo_day = as.numeric(ecmo_day)) %>%
        ungroup() 

df_coag <- df_coag %>%
        group_by(id) %>%
        mutate(runt_min = (chart_t - ecmo_start) / 60) %>%
        mutate(ecmo_day = ceiling(runt_min / (60 * 24))) %>%
        mutate(runt_min = as.numeric(runt_min)) %>%
        mutate(ecmo_day = as.numeric(ecmo_day)) %>%
        ungroup()


df_h <- df_h %>%
        group_by(id) %>%
        mutate(runt_min = (chart_t - ecmo_start) / 60) %>%
        mutate(ecmo_day = ceiling(runt_min / (60 * 24))) %>%
        mutate(runt_min = as.numeric(runt_min)) %>%
        mutate(ecmo_day = as.numeric(ecmo_day)) %>%
        ungroup()

df_hydrocort <- df_hydrocort %>%
        group_by(id) %>%
        mutate(runt_min = (chart_t - ecmo_start) / 60) %>%
        mutate(ecmo_day = ceiling(runt_min / (60 * 24))) %>%
        mutate(runt_min = as.numeric(runt_min)) %>%
        mutate(ecmo_day = as.numeric(ecmo_day)) %>%
        ungroup()

df_prd <- df_prd %>%
        group_by(id) %>%
        mutate(runt_min = (chart_t - ecmo_start) / 60) %>%
        mutate(ecmo_day = ceiling(runt_min / (60 * 24))) %>%
        mutate(runt_min = as.numeric(runt_min)) %>%
        mutate(ecmo_day = as.numeric(ecmo_day)) %>%
        ungroup()

df_rx <- df_rx %>%
        group_by(id) %>%
        mutate(runt_min = (chart_t - ecmo_start) / 60) %>%
        mutate(ecmo_day = ceiling(runt_min / (60 * 24))) %>%
        mutate(runt_min = as.numeric(runt_min)) %>%
        mutate(ecmo_day = as.numeric(ecmo_day)) %>%
        ungroup()

df_txa <- df_txa %>%
        group_by(id) %>%
        mutate(runt_hr = (chart_t - ecmo_start) * 24) %>%
        mutate(ecmo_day = ceiling(runt_hr / 24)) %>%
        mutate(runt_hr = as.numeric(runt_hr)) %>%
        mutate(ecmo_day = as.numeric(ecmo_day)) %>%
        ungroup()

message("2b-de is loaded.")


# further cleaning --------------------------------------------------------

df_bl[df_bl$chart_t < df_bl$ecmo_start,]
#this code rerutnred zero. which showed that all blood values are at the time of ecmo starting.
#none for after

#running this for df_coag

df_coag[df_coag$chart_t < df_coag$ecmo_start,] %>% arrange(chart_t)
#this showed that there are over 163 values but most are on the same day more or less.
#there are some values way out of range - 2009 to 2000. i.e,why?

df_coag[df_coag$chart_t > df_coag$ecmo_finish,]
#this showed there are lots of values. some are ridiculous e.g., ID_052 has 181.
#this showed that may be we have subsetted with icu discharge. let's look at this on icu discharge time
#coag values used icu discharge not ecmo discharge.

#Please note median ecmo run time is 14 days. 
#please note median icu stay is 24 days.

df_h[df_h$chart_t < df_h$ecmo_start,]
#this showed that there is no heparin prescription prior to commencement of ecmo as far sa this 
#data extraction is concerned.

df_h[df_h$chart_t > df_h$ecmo_finish,]
#this showed the same about psot ecmo.


df_hydrocort[df_hydrocort$chart_t <df_hydrocort$ecmo_start,]

df_hydrocort[df_hydrocort$chart_t > df_hydrocort$ecmo_finish,] 
#likewise for hydrocort 

df_prd[df_prd$chart_t<df_prd$ecmo_start,]
#looking at this showed that ther are 19 patients. some receive products way before ecmo can
#is it significant?
df_prd[df_prd$chart_t > df_prd$ecmo_finish,]
#this showed quite a lot of patients receive products after ecmo. need to consider.

#checked df_rx - there is no before or after. 
#checked df_txa - there is no before or after.
message("df_bl is more or less clean and has no values before or ater ecmo, df_coag showed there are soeme vaues before ecmo, and there are some values after ecmo as it seems to be icu discharge was used. df_h showed vals are within ecmo run times. df_hydrocort is the same. df_prd have products before ecmo and also receive products after ecmo. df_rx and df_txa are the same as in clean in between ecmo runtimes")

