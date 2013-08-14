#' Parameters to select a reasonable value of 'a' 
#'
#' Returns a list of parameters that other functions accept for the 'a' parameter
#'
#' @param meth The name of the method to use
#' @param ptp The proportion of total points [0..1]
#' @param nnn The number of nearest neighbors
#' @param tct The temporal continuity thresshold, used to filter out of the calculation points that are isolated in time to avoid bias from internal time gaps. Expressed as a proportion of the median sampling frequency, such that if a point is temporally disconnected from its a.nn / 2 nearest temporal neighbors by more thanthan a.tct times the median sampling frequency, the point will be be excluded from the computation of a
#'
#' @note
#' If multiple values are passed for any of the parameters, other parameters will be recycled as needed.
#'
#' This function does not actually find nearest neighgors. Rather it returns a list of parameters that can be used as input
#' in other functions. To actually find nearest neighbors, use \code{\link{lxy.nn.add}}.
#'
#' @return A data frame with columns meth, ptp nnn, and tct
#'
#' @seealso \code{\link{lxy.nn.add}}
#'
#' @examples
#' # lxy <- lxy.nn.add(lxy, ptsh=0.5, a=auto.a())
#'
#' @export

#auto.a <- function(a.pp=0.98, a.nn=2, a.h=1, a.tct=1.05, a.meth = "nn") {

auto.a <- function(meth=c("enc", "nn")[1], ptp=0.98, nnn=2, tct=1.05) {

    if (min(ptp)<=0 || max(ptp)>1) stop("ptp must be between 0 and 1")
    if (!meth %in% c("enc", "nn")) stop("Unknown value for 'meth'")
                                                                   
    cat("Need to update documentation for auto.a \n")
                                                                   
    ## Returns a data frame with the settings that define how a value for 'a' should be automatically selected. Columns include
    ##    a.meth  The name of the method for finding a (default is "nn" method for 'nearest neighbor' method.
    ##            Someday I may also use a value of a as a function of c.e.
    ##      a.pp  the proportion of parent points that should have at least a.nn neighbors (default 0.98)
    ##      a.nn  the number of nearest neighbors (default 2, so each point is in a triangle)
    ##       a.h   A hull extensivity coefficient, which will be multiplied times the value of a which satisfies p% of points having n nearest neighbors
    ##     a.tct  a time threshhold (expressed as a percentage of the median sampling frequency), such that
    ##            if a point is temporally disconnected from its a.nn / 2 nearest temporal neighbors by more than
    ##            than a.tct times the median sampling frequency, the point will be be excluded from the computation of a
    ##            This is to avoid bias from internal time gaps (default 1.05)

    return(data.frame(meth=meth, ptp=ptp, nnn=nnn, tct=tct))

}




