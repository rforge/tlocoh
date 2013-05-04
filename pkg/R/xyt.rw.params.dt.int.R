xyt.rw.params.dt.int <- function(id, xy, dt, dt.int.round.to=0.1, tau.diff.max=0.02, step.len.central.tendency=c("median","mean")[1]) {

    ## Called by lxy.subset, and xyt.lxy
    
    ## Given a factor id, xy data frame, and dt vector, this will compute a data frame for dt.int (a frequency table of the point-to-point time difference)
    ## and a rw.params. 
    ## No cleaning or sorting is done, presumes all objects passed have already been cleaned
    ## 
    ## The function specified by step.len.central.tendency will be used to find the step length (part of the random walk model)
    ## dt.int.round.to is the proportion of the median sampling frequency that time intervals will be rounded to when computing
    ##                 the frequency table of sampling intervals (no change is made to the time stamps)
    ## tau.diff.max is the maximum deviation from tau (expressed as a proportion of tau) that a point-to-point time difference must 
    ##                 fall within for the point-to-point distance to be included in the calculation of the median step length
    ##
    ## Returns a list with two named elements, dt.int and rw.params

    rw.params <- NULL
    dt.int <- NULL

    for (idVal in levels(id)) {
        
        ## Filter xy and dt
        thisid.idx <- which(id == idVal)
        xys.thisid <- xy[thisid.idx,]
        dt.thisid.int <- as.numeric(dt[thisid.idx])
        n <- length(thisid.idx)

        ## Compute the time step for idVal.idx points from i to i+1
        ## dt.idVal <- diff(lxy.dt.int[idVal.idx])
        
        ## Calculate the time difference between points
        dt.thisid.diff <- diff(dt.thisid.int)
        
        #print("lets compare"); browser()
        #dt.thisid <- dt[thisid.idx]
        #dt.thisid.diff.orig <- as.numeric(difftime(dt.thisid[2:n], dt.thisid[1:(n-1)], units="secs"))

        ## Calculate the median time interval
        tau <- median(dt.thisid.diff)

        ## Compute a frequency table of the time intervals for this individual
        ## First we round the time intervals to the nearest n
        if (dt.int.round.to > 0) {
            round.to <- max(1, dt.int.round.to * tau)
            dt.thisid.diff.4tab <- round.to * round(dt.thisid.diff / round.to)
        } else {
            round.to <- 1
            dt.thisid.diff.4tab <- dt.thisid.diff
        }
        
        ## Next compute the frequency table
        dt.thisid.diff.tbl.df <- as.data.frame(table(dt.thisid.diff.4tab))
        
        ## Convert the frequency table to a data frame
        dt.thisid.diff.freq <- data.frame(id=idVal, interval=as.numeric(levels(dt.thisid.diff.tbl.df[,1]))[dt.thisid.diff.tbl.df[,1]], 
                                          count=dt.thisid.diff.tbl.df$Freq, rounded.to.nearest=round.to)        
        
        ## Append to the master data frame 
        dt.int <- rbind(dt.int, dt.thisid.diff.freq)
        
        ## In preparation for finding the median step length, identify those pairs of consecutive points whose time interval 
        ## falls within the acceptable the acceptable range of tau
        thisid.idx.good <- which(dt.thisid.diff >= tau * (1 - tau.diff.max) & dt.thisid.diff <= tau * (1 + tau.diff.max))

        ## Compute the step length from i to i+1                            
        ## xys.step.length <- sqrt((xys.idVal[2:length(idVal.idx), 1] - xys.idVal[1:(length(idVal.idx)-1), 1])^2  + (xys.idVal[2:length(idVal.idx), 2] - xys.idVal[1:(length(idVal.idx)-1), 2])^2)
        
        ## Identify the time steps that are within tct of tau
        ## dt.idVal.good <- (dt.idVal >= tau * (1 - tct)) & (dt.idVal <= tau * (1 + tct))

        ## Calculate the distances for pairs of consecutive point sampled within the maximum time.gap
        thisid.dist.steps <- sqrt((xys.thisid[2:n,1] - xys.thisid[1:(n-1),1])^2 + (xys.thisid[2:n,2] - xys.thisid[1:(n-1),2])^2)[thisid.idx.good]
        
        ## Compute the mean or median step length
        thisid.dist.bar <- get(step.len.central.tendency)(thisid.dist.steps)

        ## Compute the maximum point-to-point velocity
        thisid.vel.max <- max( thisid.dist.steps / dt.thisid.diff[thisid.idx.good] )
        
        rw.params <- rbind(rw.params, data.frame(id=idVal, time.step.median=tau, d.bar=thisid.dist.bar, vmax=thisid.vel.max))
          
    }
    return(list(dt.int=dt.int, rw.params=rw.params))
}
