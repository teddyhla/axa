#### loading source ----
source("scripts/1-eda.R")
message ("0 - necessary packages loaded.")

message ("1 - source script run and loaded.")

#### 1.0. calculating time in therapeutic range ----

#There are multiple ways of calculating time in therapeutic range.

# traditional approach  
# vs. modified rosendaal 


#### 1.1. traditional method ----
cttraxa <- clab %>% 
        select(mrn,axarange,g2,cohort) %>% 
        filter(g2 == "axa")%>%  
        group_by(mrn) %>% 
        count(axarange) %>% 
        ungroup()

cttraxa <- left_join (cttraxa, clabna %>% select(mrn,erunt,no_lab),by = "mrn")

cttraxa <- cttraxa %>% 
        filter(axarange == "in") %>%
        group_by(mrn) %>% 
        mutate(tradttr = n/no_lab) %>%
        ungroup()


cttrapt <- clab %>% 
        select(mrn,apttrrange,g2,cohort) %>% 
        filter(g2 == "apt")%>%  
        group_by(mrn) %>% 
        count(apttrrange) %>% 
        ungroup()

cttrapt <- left_join (cttrapt, clabna %>% select(mrn,erunt,no_lab),by = "mrn")

cttrapt <- cttrapt %>% 
        filter(apttrrange == "in") %>%
        group_by(mrn) %>% 
        mutate(tradttr = n/no_lab) %>%
        ungroup()


cttraxa$g <- "axa"
cttrapt$g <- "apt"

cttr <- rbind (
        cttraxa %>% select(mrn,g,tradttr),
        cttrapt %>% select(mrn,g,tradttr)
)

cttr$g <- as.factor(cttr$g)

# to ensure we are using g2 definition.
names(cttr) <- c("mrn","g2","tradttr")
# all 

#### 1.2. modified Rosendaal method

#note ecmo_min is actually in 'seconds'

## note on original Rosendaal method
## need original citation

## Essentially if a patient has INR 2.4 on day 1 and INR 3.2 on day 10. Target NR is 2 to 3. 
## so between day 1 and day 10 patient has some time in therapeutic range and some above therapeutic range.
## 
## To calculate as per Rosendaal.
## first - calculate 3.2 - 2.4 = 0.8 [actual shift]
## second - calculate 3.0 - 2.4 = 0.6 [shift that is within range]
## third  - no of days that is within shift = (0.6/0.8)*9 = 0.75*9 = 6.75 days in range. 

#def ttrcalc(v1, v2, upper=3.0001, lower=1.999):
#        # formula for time in therapeutic range calculation using linear interpol
#        # variables upper and lower are target ranges.
#        # this calculator should be able to handle 9 possible scenarios
#        upper = upper
#lower = lower
#vdiff = abs(v2 - v1)
## vdiff allows absolute difference between two values.
#res = 0
## result object.
#
#if v1 < lower and v2 < lower:
#        # situation 1 where both vals are lower than range.
#        return "low"
#
#elif v1 < lower and lower < v2 < upper:
#        # situation 2
#        res = (v2 - lower) / vdiff
#return res
#
#elif v1 < lower and v2 > upper:
#        # situation 3
#        res = (upper - lower) / vdiff
#return res
#
#elif lower < v1 < upper and v2 < lower:
#        # situation 4
#        res = (v1 - lower) / vdiff
#return res
#
#elif lower < v1 < upper and lower < v2 < upper:
#        # situation 5 where both vals are in range.
#        return 1
#
#elif lower < v1 < upper and v2 > upper:
#        # situation 6
#        res = (upper - v1) / vdiff
#return res
#
#elif v1 > upper and v2 < lower:
#        # situation 7
#        res = (upper - lower) / vdiff
#return res
#
#elif v1 > upper and lower < v2 < upper:
#        # situation 8
#        res = (upper - v2) / vdiff
#return res
#
elif v1 > upper and v2 > upper:
        # situation 9 where both vals are higher than range.
        return " higher"


