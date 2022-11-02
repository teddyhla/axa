

####CUSTOM FUNCTION####



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