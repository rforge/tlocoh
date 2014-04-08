#' Extract isopleths from a LoCoH-hullset
#'
#' @param lhs A LoCoH-hullset object
#' @param id The name(s) of individuals
#' @param k The k value of hullsets
#' @param r The r value of hullsets
#' @param a The a value of hullsets
#' @param s The s value of hullsets 
#' @param hs.names The name(s) of saved hullsets
#' @param iso.idx The index(s) of the isopleths to extract. Use \link{summary.locoh.lhs} to see the indices of the isopleths.
#' @param iso.sort.metric The name(s) of sort metric(s) for the isopleths that will be extracted. Character. See \link{hm.expr}.
#'
#' @return list of SpatialPolygonDataFrame objects
#'
#' @details 
#' This function returns a list of SpatialPolygonDataFrame objects that contain isopleths. There will be one list element 
#' for each isopleth in \code{lhs}. Note that a LoCoH-hullset object can contain multiple hullsets, and a single hullset 
#' can have multiple isopleths. Accordingly, the names of elements in the returned list are a combination of a hullset name 
#' and an isopleth name.
#'
#' @seealso \code{\link{hulls}}, \code{\link{lhs.exp.shp}}, \code{\link{lhs.exp.csv}}, Vignette on T-LoCoH data classes
#' @export

isopleths <- function(lhs, id=NULL, k=NULL, r=NULL, a=NULL, s=NULL, hs.names=NULL, iso.idx=NULL, iso.sort.metric=NULL) {

    if (!inherits(lhs, "locoh.lhs")) stop("lhs should be of class \"locoh.lhs\"")
    if (!require(sp)) stop("package sp required")
    
    if (is.null(id) && is.null(r) && is.null(k) && is.null(a) && is.null(s) && is.null(hs.names)) {
        hs <- lhs
    } else {    
        hs <- lhs.select(lhs, id=id, r=r, k=k, a=a, s=s, hs.names=hs.names)
    }
    if (length(hs)==0) stop("No hullsets found matching those criteria")
    
    if (!is.null(iso.sort.metric)) {
        if (FALSE %in% (iso.sort.metric %in% hm.expr(names.only=TRUE, print=FALSE, desc=FALSE))) {
            stop(paste("Unknown sort metric: ", paste(iso.sort.metric[!iso.sort.metric %in% hm.expr(names.only=TRUE, print=FALSE, desc=FALSE)], collapse=", ", sep=""), sep=""))
        }
    }
    
    res <- list()
    
    for (i in 1:length(hs)) {
        if (!is.null(length(hs[[i]][["isos"]]))) {
            if (is.null(iso.idx)) {
                iso.idx.use <- 1:length(hs[[i]][["isos"]])
            } else {
                iso.idx.use <- intersect(iso.idx, 1:length(hs[[i]][["isos"]]))
            }
            
            if (!is.null(iso.sort.metric)) {
                iso.idx.use <- intersect(iso.idx.use, which(sapply(hs[[i]][["isos"]], function(x) x[["sort.metric"]]) %in% iso.sort.metric))
            }
            
            if (length(iso.idx.use) > 0) {
                isos.lst <- lapply(hs[[i]][["isos"]][iso.idx.use], function(x) x[["polys"]])                                        
                if (length(isos.lst) > 0) {
                    names(isos.lst) <- paste(names(hs)[i], names(isos.lst), sep="_")
                    res <- c(res, isos.lst)
                }
            }
        }
    }
    
    if (length(res)==0) warning("No isopleths found")
    return(res)
     
}
