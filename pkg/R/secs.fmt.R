#' Format a number of seconds as minutes, hours or days
#'
#' Returns a formated version of a time value
#'
#' @param secsVal a time value (in seconds)
#' @param round.pos The number of decimal places in the formatted string
#' @return a character string
#' @note This function only works with a single value. To format a multiple time values stored in a vector, use sapply or lapply
#'
#' Based on the value of secsVal, the formatted version will be converted to days, hours, minutes, or seconds
#' @export

secs.fmt <- function(secsVal, round.pos=1) {

    ############################################################
    ############################################################

        ## Returns a formatted character version of secsVal,
        ## which is presumed to be the time window in seconds
        ## 

        if (!is.numeric(secsVal)) {
                return(NULL)
                
        #} else if (secsVal >= 3600*24*365*2) {
        #        return(paste(round(secsVal/3600*24*365, round.pos), "year", sep=""))
        #} else if (secsVal >= 3600*24*61) {
        #        return(paste(round(secsVal/3600*24*30, round.pos), "month", sep=""))
        } else if (secsVal >= 86400) {
                return(paste(round(secsVal/86400, round.pos), "day", sep=""))
        } else if (secsVal >= 3600) {
                return(paste(round(secsVal/3600, round.pos), "hs", sep=""))
        } else if (secsVal >= 60) {
                return(paste(round(secsVal/60, round.pos), "min", sep=""))
        } else {
                return(paste(secsVal, "sec", sep=""))
        }

}
