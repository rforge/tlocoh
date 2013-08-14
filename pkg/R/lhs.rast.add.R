#' Convert isopleths to rasters
#'
#' Adds rasterized version of isopleth(s) to a LoCoH-hullset object 
#'
#' @param lhs A \link{LoCoH-hullset} object
#' @param id The id(s) of the hullsets to create isopleths for 
#' @param k The k value of hullsets to create isopleths for
#' @param r The r value of hullsets to create isopleths for
#' @param a The a value of hullsets to create isopleths for
#' @param s The s value of hullsets to create isopleths for
#' @param hs.names The name(s) of saved hullsets to create isopleths for
#' @param sort.metric The name of a hull metric that will be used to sort the hulls prior to merging into isopleths
#' @param iso.method The method used to define isopleths. Default is "pt.quantiles" which defines isopleths as containing a quantile of points (e.g., 
#' the 0.1th isopleth contains 10\% of the points). Can also be "hm.vals", in which case the isopleth level represents not a proportion of points enclosed 
#' but a value of the hull metric (e.g., the 0.1 isopleth contains hulls whose hull metric is <= 0.1)
#' @param status Show status messages. T/F
#' 
#' @return A LoCoH-hullset object
#'
#' @details
#' This will take exising isopleths and create raster versions of them.
#'
#' @examples
#' \dontrun{
#' lhs <- lhs.iso.add(lhs)
#' lhs <- lhs.rast.add(lhs)
#' }
#'
#' @export
#' @seealso \code{\link{lhs.iso.add}}


lhs.rast.add <- function(lhs, id=NULL, k=NULL, r=NULL, a=NULL, s=NULL, hs.names = NULL, 
                        sort.metric=c("auto", hm.expr(names.only=T, desc=F, print=F))[1], 
                        iso.method=c("pt.quantiles", "hm.vals")[1], 
                        cell.size=NULL, ll.round=TRUE, status=TRUE) {

    # taken out @param ivg Value(s) for inter-visit gap, required if the sort metric is a time-use metric (e.g., visitation). 
    # An inter-visit gap is the period of time (in second) which must pass before another occurrence 
    # of the individual in the hull is considered a separate visit.
     
    if (!inherits(lhs, "locoh.lhs")) stop("lhs should be of class \"locoh.lhs\"")
    if (!is.null(lhs[["xys"]])) stop("Old data structure detected")
    if (!require(sp)) stop("package sp required")
    if (!require(raster)) stop("package raster required")
    #if (!require(pbapply)) stop("package pbapply required")
    #if (anyDuplicated(iso.levels)) stop("Duplicate iso.levels detected")

    start.time <- Sys.time()
    if (is.null(id) && is.null(r) && is.null(k) && is.null(a) && is.null(s) && is.null(hs.names)) {
        hs.matching.idx <- 1:length(lhs)
    } else {    
        hs.matching.idx <- lhs.select.which(lhs, id=id, r=r, k=k, a=a, s=s, hs.names=hs.names)
    }
    if (length(hs.matching.idx)==0) stop("No sets of hulls found matching those criteria")
    
    isos.converted <- 0
    if (status) {cat("Converting isopleths to rasters\n"); flush.console()}
    
    for (hs.idx in hs.matching.idx) {
        if (status) {cat(names(lhs)[hs.idx] , "\n", sep=""); flush.console()}
        ## lhs[[hs.idx]][["pts"]][[anvVal]] <- NULL
        if (!is.null(lhs[[hs.idx]][["isos"]])) {
            for (iso.idx in 1:length(lhs[[hs.idx]][["isos"]])) {
                iso.rast <- iso2raster(lhs[[hs.idx]][["isos"]][[iso.idx]][["polys"]], cell.size=cell.size, ll.round=ll.round, status=status)
                lhs[[hs.idx]][["isos"]][[iso.idx]][["rast"]] <- iso.rast
                isos.converted <- isos.converted + 1
            }
        
        }

            
    }
    
    if (status) {
        time.taken = difftime(Sys.time(), start.time, units="auto")
        cat("Total time:", round(time.taken,1), units(time.taken), "\n", sep = " ")    
    }
    
    return(lhs)  

}
