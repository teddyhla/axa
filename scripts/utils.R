

## 2.3. CUSTOM FUNCTIONS  ---------------------------------------------------
source("scripts/ttr_rose.R")
#custom rosenthaal calculation function
#this function requires data in the format
#time interval, value 1, value 2 to work.

#custom function 1. this is basically to append ecmo finish and start time in corect
#formats to each data frames
wr <- function (x,dfcore) {
        #func to append ecmo start finish columns to each dataframes
        x <- left_join(
                x,
                dfcore %>% select(mrn,ecmo_start,ecmo_finish,group,ecmod),
                by = "mrn"
        )
        x<- x %>% 
                group_by(mrn) %>% 
                filter(chart_t >= ecmo_start & chart_t <= ecmo_finish)%>%
                ungroup()
        
        x$chart_t2 <- as.Date(x$chart_t,format = "%Y-%m-%d")
        x$es2 <- as.Date(x$ecmo_start, format = "%Y-%m-%d")
        
        x$nd <- round(difftime(x$chart_t2,x$es2,units = "days"))
        x$nd <- as.numeric(x$nd)
        #makes it numeric to make it easier then lets add 1 to prevent issue of day 0 on ecmo
        x$nd <- x$nd + 1 
        
        return(x)
}

#2nd custom function
#this is basically function add begin and end times in blood tests
wo <- function(x){
        #function to add begin and end times in blood tests
        x <- x %>%
                add_row(
                        mrn = sample(.$mrn,size =1),
                        chart_t = sample(.$ecmo_start,size=1),
                        group = sample(.$group,size=1)
                ) %>% 
                add_row(
                        mrn = sample(.$mrn,size = 1),
                        chart_t = sample(.$ecmo_finish,size=1),
                        group = sample(.$group,size=1)
                )%>%
                arrange(chart_t)
        return(x)
}

wz <- function(x){
        #function to add begin and end times in blood tests
        x <- x %>%
                add_row(
                        mrn = sample(.$mrn,size =1),
                        chart_t = sample(.$ecmo_start,size=1),
                        group = sample(.$group,size=1)
                ) %>% 
                add_row(
                        mrn = sample(.$mrn,size = 1),
                        chart_t = sample(.$tn,size=1),
                        group = sample(.$group,size=1)
                )%>%
                arrange(chart_t)
        return(x)
}

#3rd custom function
#function to essentially lag the blood results and times so can make a interval


lwg <- function (x){
        #function to add lagged values both times and blood,
        #need to select axa or apt
        
        if ("axa" %in% names(x)){
                x <- x %>%
                        select(mrn,chart_t,axa,group) %>%
                        mutate(
                                t2=lag(chart_t),
                                v2 = lag(axa)
                        ) %>%
                        select(mrn,chart_t,t2,axa,v2,group)
                return(x)
        } else {
                x <- x %>%
                        select(mrn,chart_t,apttr,group) %>%
                        mutate(
                                t2=lag(chart_t),
                                v2 = lag(apttr)
                        ) %>%
                        select(mrn,chart_t,t2,apttr,v2,group)
                return(x)
        }
        
}

#4th custom function
#function to calculate time intervals in hours 
mwt <- function (x){
        #fuction calculates time intervals in hours mainly for TTR
        x<- x %>% 
                mutate(ivethr = as.numeric(difftime(t2,chart_t,units = "hours")))
        x$ivethr <- x$ivethr * -1
        #this line converts secs to hours
        return(x)
}

mwtv <- function (x){
        #fuction calculates time intervals in hours mainly for Fihn var
        x<- x %>% 
                mutate(ivethr = as.numeric(difftime(t2,chart_t,units = "days")))
        x$ivethr <- x$ivethr * -1
        #this line converts secs to hours
        return(x)
}

#5th custom function
#lets apply the ttr function 

rap <- function (a){
        #function to apply ttrose function
        #need to select axa or apt
        
        if ("axa" %in% names(a)){
                l = 0.2999
                u = 0.7001
                a$ttrose <- ttrcalc(lower = l,upper = u, x=a$axa,y=a$v2)
                return(a)
        } else {
                l = 1.49999
                u = 2.50001
                a$ttrose <- ttrcalc(lower = l, upper = u, x=a$apttr,y=a$v2)
                return(a)
        }
        
}

rpe <- function (a){
        #FOR PE function to apply ttrose function
        #need to select axa or apt
        
        if ("axa" %in% names(a)){
                l = 0.5999
                u = 1.0001
                a$ttrose <- ttrcalc(lower = l,upper = u, x=a$axa,y=a$v2)
                return(a)
        } else {
                l = 1.99999
                u = 3.00001
                a$ttrose <- ttrcalc(lower = l, upper = u, x=a$apttr,y=a$v2)
                return(a)
        }
        
}

#6th custom function
#here the goal is to generate a summary value for each patient.
edd <- function (a){
        mrn <- sample(a$mrn,size=1)
        group <- sample(a$group,size =1)
        totalhr <- sum(a$ivethr,na.rm=TRUE)
        tlow <- sum(a[which(a$ttrose == "low"),"ivethr"])
        thi <- sum(a[which(a$ttrose == "high"),"ivethr"])
        
        a$ttrn<- as.numeric(a$ttrose)
        a$it <- a$ttrn * a$ivethr
        sumttr <- sum(a$it,na.rm=TRUE)
        ttrg <- sum(a$it,na.rm = TRUE)/sum(a$ivethr,na.rm = TRUE)
        
        df <- data.frame(mrn,group,totalhr,tlow,thi,sumttr,ttrg)
        return(df)
}

#7th custom function
#fihn's method

fnm <- function(a){
        #this is a function to calculate variability using Fihn's method
        #variability is a summary feature, so we can ignore individual NA's
        
        if ("axa" %in% names(a)){
                z = 0.5 #mid point of axa target
                a <- a %>% 
                        select(mrn,axa,group,ivethr) %>%
                        drop_na(axa) %>%
                        filter(ivethr>0) %>%
                        mutate(
                                temp1 = (axa - 0.5)^2,
                                temp2 = temp1 / ivethr
                        )
                mrn <- sample(a$mrn,size=1)
                group <- sample(a$group,size =1)
                sigm <- (sum(a$temp2))/(dim(a)[1])
                df <- data.frame(mrn,group,sigm)
                return(df)
                
        } else {
                z = 2 #midpoint of apttr target
                a <- a %>%
                        select(mrn,apttr,group,ivethr) %>%
                        drop_na(apttr)%>%
                        filter(ivethr > 0) %>%
                        mutate(
                                temp1 = (apttr - 2)^2 ,
                                temp2 = temp1 / ivethr
                        )
                
                mrn <- sample(a$mrn,size=1)
                group <- sample(a$group,size =1)
                sigm <- (sum(a$temp2))/(dim(a)[1])
                df <- data.frame(mrn,group,sigm)
                return(df)
                
        }
        
}

fpe <- function(a){
        #FOR PE this is a function to calculate variability using Fihn's method
        #variability is a summary feature, so we can ignore individual NA's
        
        if ("axa" %in% names(a)){
                z = 0.8 #mid point of axa target
                a <- a %>% 
                        select(mrn,axa,group,ivethr) %>%
                        drop_na(axa) %>%
                        filter(ivethr>0) %>%
                        mutate(
                                temp1 = (axa - 0.8)^2,
                                temp2 = temp1 / ivethr
                        )
                mrn <- sample(a$mrn,size=1)
                group <- sample(a$group,size =1)
                sigm <- (sum(a$temp2))/(dim(a)[1])
                df <- data.frame(mrn,group,sigm)
                return(df)
                
        } else {
                z = 2.5 #midpoint of apttr target
                a <- a %>%
                        select(mrn,apttr,group,ivethr) %>%
                        drop_na(apttr)%>%
                        filter(ivethr > 0) %>%
                        mutate(
                                temp1 = (apttr - 2.5)^2 ,
                                temp2 = temp1 / ivethr
                        )
                
                mrn <- sample(a$mrn,size=1)
                group <- sample(a$group,size =1)
                sigm <- (sum(a$temp2))/(dim(a)[1])
                df <- data.frame(mrn,group,sigm)
                return(df)
                
        }
        
}



hti <- function (x){
        #function to add lagged values for heparin
        
        
        x <- x %>%
                select(mrn,chart_t,s_label,t_form,u,wkg,dose,group) %>%
                mutate(t2=lag(chart_t)) %>%
                mutate(tinv = as.numeric(difftime(t2,chart_t,units = "hours")))%>%
                mutate(tinv = -1 * tinv) %>%
                mutate(tidose = tinv * dose)
        
        mrn <- sample(x$mrn,size=1)
        group <- sample(x$group,size =1)
        cudose <- sum(x$tidose,na.rm = TRUE)
        cumtime <- sum(x$tinv,na.rm = TRUE)
        df <- data.frame(mrn,group,cumtime,cudose)
        return(df)
}

hr <- function(x){
        #function to count run length and tell how many changes 
        eg <- rle(x$t_form)
        runl <- length(eg$lengths)
        mrn <- sample(x$mrn,size =1)
        group <- sample(x$group,size =1)
        
        df<- data.frame(mrn,group,runl)
        return(df)
}

blz <- function(x) {
        #function to detect and find missing values for blood vars
        if (sum(x$test %in% blvars) < 18){
                status <- "missing"
                no_m <- (24 - sum(x$test %in% blvars))
                col_m <- setdiff(blvars,x$test)
        } else {
                status <- "no_missing"
                no_m <- 0
                col_m <- "none"
        }
        
        mrn <- sample(x$mrn, size = 1)
        df <- data.frame(mrn,status,no_m,col_m)
        return(df)
}

ht <- function(x){
        x$nd <- round(difftime(x$chart_t,x$ecmo_start,units = "days"))
        x$nd <- as.numeric(x$nd)
        x<- x %>% 
                group_by(nd) %>%
                arrange(nd,chart_t)%>% 
                summarise(mrn = mrn, nd = nd, rlnd = length(rle(t_form)$lengths)) %>%
                group_by(nd)%>%
                ungroup()
        
        x<-distinct(x)
        return(x)        
}




####CUSTOM FUNCTIONS for shaping long form for bleeding and throm comps####



shp <- function(a,b){
        #function to reshape into long forms
        #need to fill as "admn_ct" etc
        x <- df %>% select(mrn,a)
        x <- x %>% pivot_longer(!mrn, names_to = "events")
        
        y <- df %>% select(mrn,b)
        y <- y %>% pivot_longer(!mrn,values_to = "time" )
        
        z <- left_join(
                x, 
                y %>% select(mrn,time),
                by = "mrn"
        )
        
        return(z)
        
}


xcs <- function (a,b){
        #custom func for re-shaping long form of circuit changes
        x <- df %>% select(mrn,a)
        x$xc <- b
        names(x)[2] <- "time"
        return(x)
}

drs <- function(x){
        #function to calculate time elapsed
        x <- left_join(
                x,
                dfcore %>% select(mrn,ecmo_start),
                by = "mrn"
        )
        x$duhr <- difftime(x$time,x$ecmo_start,units="hours")
        x$duhr <- round(as.numeric(x$duhr),2)
        
        x$dud <- difftime(x$time,x$ecmo_start,units="days")
        x$dud <- round(as.numeric(x$dud),2)
        
        #x <- x %>% select(-ecmo_start)
        
        return(x)
        
}