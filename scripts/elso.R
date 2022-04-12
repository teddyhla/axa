source("scripts/2b-de.R")

library(kableExtra)
df_m %>% 
        select(cohort,dx_cat,age,mode,surv_ecmo,surv_icu,age_score,chron_health_score,apache,wkg,pmh,runt_hr,runt_day,sex) %>%
        group_by(cohort) %>% 
        summarise(
                no_pts = n(),
                mean_age = mean(age),
                diagnosis = sum(dx_cat == "Pneumonia - Viral"),
                modeecmo = sum(mode == "vv"),
                deaths_ecmo = sum(surv_ecmo == "no"),
                deaths_icu = sum(surv_icu == "no"),
                mean_age_score = mean(age_score),
                median_age_score = median(age_score),
                mean_chron_score = mean(chron_health_score),
                median_apache = median(apache),
                median_wkg = median(wkg),
                median_runt = median(runt_day),
                sex = sum(sex == "m"),
                crude_mortality_rate = round(deaths_ecmo/no_pts,digits = 2),
        ) %>%
        kbl(format= "html",full_width = F) %>% 
        kable_styling()

t1 <- df_m %>% 
        select(cohort,dx_cat,age,mode,surv_ecmo,surv_icu,age_score,chron_health_score,apache,wkg,pmh,runt_hr,runt_day,sex) %>%
        group_by(cohort) %>% 
        summarise(
                no_pts = n(),
                mean_age = mean(age),
                diagnosis = sum(dx_cat == "Pneumonia - Viral"),
                modeecmo = sum(mode == "vv"),
                deaths_ecmo = sum(surv_ecmo == "no"),
                deaths_icu = sum(surv_icu == "no"),
                mean_age_score = mean(age_score),
                median_age_score = median(age_score),
                mean_chron_score = mean(chron_health_score),
                median_apache = median(apache),
                median_wkg = median(wkg),
                median_runt = median(runt_day),
                sex = sum(sex == "m"),
                crude_mortality_rate = round(deaths_ecmo/no_pts,digits = 2),
        )

## here is a rough table of features 

#let's run an anova

baset <- as.data.frame(t(as.data.frame(t1)))
names(baset) <- c("cov1","cov2","cov3","non-cov")
baset <- baset[-c(1),]

baset[,1:4] <- sapply(baset[,1:4],as.numeric)

pl <- df_m
pl$cov <- ifelse(pl$cohort != "historical_viral_pneumonia","cov","ncov")

#drawing insights
