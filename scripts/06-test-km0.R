load(file="data/clean/finalout.RData")


#

dc <- dm %>%
        select(
                -c( mrn, surv_ecmo, group, ecmod, totc, rl_day, hep_wkgday,hboth,bldtot,bldtot_day)
        )