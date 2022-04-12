library(tidyverse)
#### Custom function for calculation of TTR ----
ttrs<- function (x,y, lower = 1.9999 , upper = 3.00001,na.rm = TRUE){
        #formula for ttr calculation using linear interpol
        #variables lower and upper are target ranges
        # this calculator should be able to handle all 9 poss scenarios.
        
        upper <- upper
        lower <- lower
        vdiff <- abs (y-x)
        #vdiff var allows absolute diff between two vals.
        
        res <- 0
        #result object
        
        if (is.na(x)|is.na(y)){
                #NA values
                return(NA)
                
        } else if (x < lower & y <lower) {
                #sit 1 both vals are lower
                return("low")
                
        } else if (x < lower & (y> lower & y<upper)) {
                #sit2 y is in  range.
                res = (y - lower)/vdiff
                return (res)
                
        } else if (x < lower & y > upper) {
                #sit3 y above range
                res = (upper - lower)/vdiff
                return(res)
                
        } else if ((x>lower & x<upper) & y <lower ) {
                #sit4 x in range, y lower
                res = (x-lower)/vdiff
                return (res)
                
        } else if ((x>lower & x < upper) & (y>lower & y<upper)) {
                #sit 5 both in range
                res = 1
                return(res)
                
        } else if ((x>lower & x <upper) & y>upper) {
                #sit 6 x in rang,y upper
                res = (upper - x)/vdiff
                return (res)
                
        } else if (x>upper & y < lower ) {
                #sit 7 x is above and y is low
                res = (upper - lower)/vdiff
                return (res)
                
        } else if (x>upper & (y>lower & y<upper)) {
                #sit 8 x is above and y in range
                res = (upper - y)/vdiff
                return (res)
                
        } else if (x >upper & y >upper) {
                #situation 9 both upper
                return ("high")
                
        } 
        
}

#### Vectorising this custom function ----
#lets test these code works
ttrcalc <- Vectorize(ttrs,c("x","y"))

#this function is working as a scalar function. Vectorize from base R converts it and voila

### Testing custom function in all possible numerical scenarios ----
testdf <- data.frame(
        situation = sprintf("situation-%s",1:9),
        v1 = c(1,1,1,2.5,2.5,2.5,5,5,5),
        v2 =c(1,2.5,5,0.9,2.5,5,0.5,2.5,7)
)
# this test dataframe would cover all possible scenario's without NA. can it handle it ?
testdf %>% mutate(ans = ttrcalc(v1,v2))
# the output seems correct i.e., it produces right output for all these possible scenarios. 

#####---- Testing custom function in situations with NA ----
#Now let's try NA values in this custom function.
testna <- data.frame (
        situation = sprintf("situation-%s",1:5),
        v1 = c(NA,1.1,2.0,3.0,5.0),
        v2 = c(1.5,2.0,NA,2.5,NA)
)
       
#it works with NA