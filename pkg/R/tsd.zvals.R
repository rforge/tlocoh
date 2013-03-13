#' tsd.zvals
#'
#' returns the values for the "time axis" of the TSD distance metric
#'
#' @param delta.t is the time difference in seconds
#' @param sVal is the value of S
#' @param d.bar is the median step length (for the entire dataset)
#' @param tau is the median sampling interval (for the entire dataset)

tsd.zvals <- function(delta.t, sVal, type, d.bar=NULL, tau=NULL, vmax=NULL) {

    if (type=="dif") {
        ## Diffusion
        if (is.null(d.bar) || is.null(tau)) stop("d.bar and tau are required paramters")
        return(d.bar * sqrt(sVal * abs(delta.t) / tau))
        
    } else if (type=="vmax") {
        if (is.null(vmax)) stop("vmax is a required parameter")
        return(sVal * vmax * abs(delta.t))
        
    } else {
        stop(paste("Unknown value for type:", type))
        
    }

}
