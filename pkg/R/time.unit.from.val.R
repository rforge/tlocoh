time.unit.from.val <- function(time.val, buff = 0) {
    ## Given a unit of time in seconds, returns a unit of time as a string that would be a sensible scaling factor
    ## Buff is an lower extension on the upper limit given as a proportion of the extact interval 
    
    if (time.val <= 60 * (1 - buff)) {
        time.unit <- "sec"
    } else if (time.val <= 3600 * (1 - buff)) {
        time.unit <- "min"
    } else if (time.val <= 3600 * 24 * (1 - buff)) {
        time.unit <- "hour"
    } else if (time.val <= 3600 * 24 * 7  * (1 - buff)) {
        time.unit <- "day"
    } else if (time.val <= 3600 * 24 * 30 * (1 - buff)) {
        time.unit <- "week"
    } else if (time.val <= 3600 * 24 * 30 * 365 * (1 - buff)) {
        time.unit <- "month"
    } else {
        time.unit <- "year"
    }        

    return(time.unit)
}

