#########################

########################


# SOURCE DATA -------------------------------------------------------------
load(file="data/clean/out.RData")

#clean data sourced from 1d-dtf


# ANALYSIS PLAN -----------------------------------------------------------


#
#1. Time TO therapeutic range
# 2. Time IN therapeutic range 
#3. less variability 
#4. Frequency of changes
#5. total per person per day dose of heparin ?
#6. Patient outcomes
#7. time to first complication
#8. poisson regressions for blood products
#


# ASSUMPTIONS -------------------------------------------------------------


# 1.0. Using 1st 24 hr lab values --------------------------------------------
dfcore %>% select(ecmod) %>% count(ecmod)
dfcore %>% select(ecmoh) %>% count(ecmoh)
#This code show that there is only 2 patient who are on ecmo for 1 day.
#looking at hours - it is at 18.8 hours and 29.8 hours


# 2.0. WRANGLING ---------------------------------------------------------------

#Features we will use.

# 2.1.  Blood results features -----------------------------------------------------
#Haemoglobin, Platelet, Neutrophils, Fibrinogen, LDH, Ferritin, CK, CRP, PCT,Bilirubin
#Albumin, Creatining, corrected calcium, bicarbonate, lactate, ph
#16 variables

# 2.2. Anticoagulation Features ------------------------------------------
#globa_ttr,time_to_first_ttr,
#variability fihn, variability standardised

# 2.3. Heparin related features -------------------------------------------
#cumulative heparin doses
#no of prescription changes

# 2.4. Circuit Changes / Complications ?----------------------------------

# 2.5. Prescription related features -------------------------------------


# 2.6. Demographic features -----------------------------------------------


# 2.7. Blood products features  --------------------------------------------

#transfusion in 1st 24 hour, cumulative 

# 2.8. Final df -----------------------------------------------------------





