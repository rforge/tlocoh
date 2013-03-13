time.factor <- function(time.unit) {
    
    ## Given a string unit of time, returns the number of seconds in that unit
    if (time.unit == "sec") {
        time.factor <- 1
    } else if (time.unit == "min") {
        time.factor <- 60
    } else if (time.unit == "hour") {
        time.factor <- 3600
    } else if (time.unit == "day") {
        time.factor <- 3600 * 24
    } else if (time.unit == "week") {
        time.factor <- 3600 * 24 * 7
    } else if (time.unit == "month") {
        time.factor <- 3600 * 24 * 30
    } else {
        stop("Time unit unknown")
    }

    return(time.factor)
}
