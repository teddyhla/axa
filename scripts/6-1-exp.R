# SOURCE DATA -------------------------------------------------------------
load(file="data/clean/finalout.RData")


# LIBRARIES ---------------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(lme4)


#  LEVELS -----------------------------------------------------------------

lmm2 <- glmer(
        formula = bldtot ~ scale(age) + scale(sigm) + scale(ttrg) + scale(lactate_median) + scale(apache) + group + bmi 
        + (1|ecmosb),data= dm, family = poisson, verbose = FALSE
                
)

summary(lmm2)

plm <- glm(bldtot ~ age + scale (sigm) + scale(ttrg) 
           + ecmosb + offset(ecmod) , data = dm, family = poisson
)