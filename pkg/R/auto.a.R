#' Parameters to select a reasonable value of 'a' 
#'
#' Returns a list of parameters that other functions accept for the 'a' parameter
#'
#' @param meth The name of the method for finding a (default is "nn" method for 'nearest neighbor' method.
#' @param ptp The proportion of total points [0..1]
#' @param nnn The number of nearest neighbors
#' @param tct The temporal continuity thresshold, see details.
#'
#' @details
#' If multiple values are passed for any of the parameters, the resulting data frame will have multiple rows 
#' and other parameters will be recycled as needed.
#'
#' This function does not actually find nearest neighgors. Rather it returns a list of parameters that can be used as input
#' in other functions such as \code{\link{lxy.nn.add}}.
#'
#' @return A data frame with columns \code{meth}, \code{ptp}, \code{nnn}, and \code{tct} which are the settings
#' that other functions will use to find a value for 'a' should be automatically selected. Columns include:
#'
#' \itemize{
#'     \item \emph{meth} The name of the method for finding a
#'     \item \emph{ptp} The proportion of parent points that should have at least a.nn neighbors (default 0.98)
#'     \item \emph{nnn} The minimum number of nearest neighbors that each point will get (default 2, so each point can be part of a triangle)
#'     \item \emph{tct} A time threshhold (expressed as a percentage of the median sampling frequency), such that if a point is temporally 
#'                      disconnected from its 2 nearest temporal neighbors by more than than \code{tct} times the median sampling frequency, 
#'                      the point will be be excluded from the computation of a. This is to avoid bias from internal time gaps (default 1.05)
#'     }
#'
#' @seealso \code{\link{lxy.nn.add}}
#'
#' @examples
#' \dontrun{
#' lxy <- lxy.nn.add(lxy, ptsh=0.5, a=auto.a())
#' }
#' @export

auto.a <- function(meth="nn", ptp=0.98, nnn=2, tct=1.05) {

    if (min(ptp)<=0 || max(ptp)>1) stop("ptp must be between 0 and 1")
    if (!meth %in% c("enc", "nn")) stop("Unknown value for 'meth'")
                                                                   
    return(data.frame(meth=meth, ptp=ptp, nnn=nnn, tct=tct))

}




