dateticks <- function(dt, max.ticks=13, dtint.unit=c(NA, "sec", "min", "hour", "day", "week", "month")[1], buff=0.4, enclose=TRUE) {
    # Given a vector of dates, returns a sensibily rounded set of values that be used for the tick marks on an axis label
    # This is called by lhs.exp.mov and lxy.exp.mov to set up the date bar

    dtlim <- range(dt)
    dtlim.diff <- as.numeric(difftime(time1=dtlim[2], time2=dtlim[1], units="secs")) 
    
    if (is.na(dtint.unit)) dtint.unit <- time.unit.from.val(dtlim.diff / max.ticks, buff = buff)
    
    if (dtint.unit == "sec") {
        dtint.secs <- 1
        tick.initial <- trunc(dtlim[1], units="secs") + ifelse(enclose, 0, dtint.secs)
        format.str.expr <- expression("%H:%M:%S")
    
    } else if (dtint.unit == "min") {
        dtint.secs <- 60
        tick.initial <- trunc(dtlim[1], units="mins") + ifelse(enclose, 0, dtint.secs)
        format.str.expr <- expression(if (as.POSIXlt(tick.all[1])$yday != as.POSIXlt(tick.all[num.int.use + 1])$yday) "%H:%M\n%b-%d" else "%H:%M")
    
    } else if (dtint.unit == "hour") {
        dtint.secs <- 3600
        tick.initial <- trunc(dtlim[1], units="hours") + ifelse(enclose, 0, dtint.secs)
        format.str.expr <- expression(if (as.POSIXlt(tick.all[1])$yday != as.POSIXlt(tick.all[num.int.use + 1])$yday) "%H:%M\n%b-%d" else "%H:%M")
    
    } else if (dtint.unit == "day") {
        dtint.secs <- 3600*24
        tick.initial <- trunc(dtlim[1], units="days") + ifelse(enclose, 0, dtint.secs)
        format.str.expr <- expression("%m/%d")

    } else if (dtint.unit == "week") {
        dtint.secs <- 3600*24*7
        ## Initial tick will still be the first day
        tick.initial <- trunc(dtlim[1], units="days") + ifelse(enclose, 0, 3600*24)
        format.str.expr <- expression("%m/%d")
    
    } else if (dtint.unit == "month") {
        dtint.secs <- 3600*24*30
        tick.initial <- as.POSIXlt(trunc(dtlim[1], units="days"))
        tick.initial$mday <- 1        
        tick.initial$mon  <- (tick.initial$mon + ifelse(enclose, 0, 1)) %% 12
        
        dt.last <- as.POSIXlt(trunc(dtlim[2], units="days"))
        dt.last$mday <- 1        
        dt.last$mon  <- dt.last$mon + ifelse(enclose, 1, 0)
        if (dt.last$mon >= 12) {
            dt.last$mon <- dt.last$mon %% 12
            dt.last$year <- dt.last$year + 1
        }
        
        dtlast.minus.tick.initial <- as.numeric(difftime(dt.last, time2=tick.initial, units="secs")) 
        num.int <- round(dtlast.minus.tick.initial / (3600 * 24 * 30))
        
        format.str.expr <- expression(if (as.POSIXlt(tick.all[1])$year != as.POSIXlt(tick.all[num.int.use + 1])$year) "%b-%y" else "%b")
    }
    
    ## Compute the number of intervals needed 
    if (dtint.unit != "month") {
        ## Compute the total time from the initial tick date to the last dt 
        dtlast.minus.tick.initial <- as.numeric(difftime(time1=dtlim[2], time2=tick.initial, units="secs")) 

        ## The number of intervals (before any skipping) is the integer portion of this interval divided by dtint.secs
        if (enclose) {
            num.int <- ceiling(dtlast.minus.tick.initial / dtint.secs)
        } else {
            num.int <- trunc(dtlast.minus.tick.initial / dtint.secs)
        }
    }
    
    ## If there are more intervals than allowed, redefine the number of intevals
    if (num.int > (max.ticks - 1)) {
        inter.tick.quant <- ceiling(num.int / (max.ticks - 1))
        if (enclose) {
            if (dtint.unit == "month") {
                num.int.use  <- round(dtlast.minus.tick.initial / (dtint.secs * inter.tick.quant))
                tick.all <- seq(tick.initial, length.out=num.int.use + 1, by=paste(inter.tick.quant, dtint.unit, sep=" "))
                if (tick.all[length(tick.all)] < dtlim[2]) num.int.use <- num.int.use + 1
                
            } else {
                num.int.use  <- ceiling(dtlast.minus.tick.initial / (dtint.secs * inter.tick.quant))
            }
        } else {
            num.int.use  <- trunc(dtlast.minus.tick.initial / (dtint.secs * inter.tick.quant))
        }
    } else {
        num.int.use <- num.int
        inter.tick.quant <- 1
    }
    
    #if (enclose && (tick.initial + dtint.secs * num.int.use * inter.tick.quant < dtlim[2])) num.int.use <- num.int.use + 1
    
    ## Compute the full range of ticks
    tick.all <- seq(tick.initial, length.out=num.int.use + 1, by=paste(inter.tick.quant, dtint.unit, sep=" "))
    
    #   "sec", "min", "hour", "day", "DSTday", "week", "month" or "year"
    
    format.str <- eval(format.str.expr)
    
    return(list(tick.all=tick.all, format.str=format.str))

}
