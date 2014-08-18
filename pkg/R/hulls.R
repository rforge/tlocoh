#' Extract hulls from a LoCoH-hullset
#'
#' @param lhs A LoCoH-hullset object
#' @param id The name(s) of individuals
#' @param k The k value of hullsets
#' @param r The r value of hullsets
#' @param a The a value of hullsets
#' @param s The s value of hullsets 
#' @param hs.names The name(s) of saved hullsets
#'
#' @return list of SpatialPolygonDataFrame objects
#'
#' @details 
#' This function returns a list of SpatialPolygonDataFrame objects that contain hulls. There will be one list element 
#' for each hullset in \code{lhs}. The data table will contain the hull metrics. 
#'
#' @seealso \code{\link{isopleths}}, \code{\link{lhs.exp.shp}}, \code{\link{lhs.exp.csv}}, Vignette on T-LoCoH data classes
#' @export

hulls <- function(lhs, id=NULL, k=NULL, r=NULL, a=NULL, s=NULL, hs.names=NULL) {

    if (!inherits(lhs, "locoh.lhs")) stop("lhs should be of class \"locoh.lhs\"")
    
    if (is.null(id) && is.null(r) && is.null(k) && is.null(a) && is.null(s) && is.null(hs.names)) {
        hs <- lhs
    } else {    
        hs <- lhs.select(lhs, id=id, r=r, k=k, a=a, s=s, hs.names=hs.names)
    }
    if (length(hs)==0) stop("No hullsets found matching those criteria")
    
    res <- lapply(hs, function(x) x$hulls)

    if (length(res)==0) warning("No hulls found")
    return(res)
     
     
}
