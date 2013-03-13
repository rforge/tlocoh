#' Compute hull bounding ellipses 
#'
#' Creates bounding ellipses around each hull and computes the eccentricity as a metric of ellongation
#'
#' @param lhs A LoCoH-hullset object
#' @param save.ellipses Save ellipses in the lhs object? (T/F). If False, only the eccentricity values of the ellipses are saved as a hull metric.
#' @param existing.ellipses Whether to overwrite or abort if existing ellipses are saved
#' @param id The name(s) of individuals to analyze
#' @param k The k value of hullsets to analyze
#' @param r The r value of hullsets to analyze
#' @param a The a value of hullsets to analyze
#' @param s The s value of hullsets to analyze
#' @param hs.names The name(s) of saved hullsets to analyze
#' @param status Show status messages (T/F)
#' @param beep Beep when done (T/F)
#'
#' @note
#' Saving the ellipses is optional. If ellipses are not saved, the eccentricity values will still be computed and
#' saved as a hull metric. The only real reason to save ellipses is if you wish to plot them.
#'
#' Note creating ellipses is computationally intensive and can take a long time. 
#'
#' @return A LoCoH-hullset object
#'
#' @export

lhs.ellipses.add <- function(lhs, id=NULL, k=NULL, r=NULL, a=NULL, s=NULL, hs.names = NULL, status=TRUE, beep=FALSE,
                             save.ellipses=TRUE, existing.ellipses=c("overwrite", "abort")[1]) {

    ## Using compiler package on mvee speeds this up by about 10%, not enough of a gain to implement
    ## This could be a good candidate to parallelize over multiple cores

    if (!inherits(lhs, "locoh.lhs")) stop("lhs should be of class \"locoh.lhs\"")
    if (!is.null(lhs[["xys"]])) stop("Old data structure detected")
    if (!require(pbapply)) stop("package pbapply required")
    if (!require(sp)) stop("package sp required")
    if (!existing.ellipses %in% c("overwrite", "abort")) stop("Unknown value for existing.ellipses")
    
    start.time <- Sys.time()
    if (is.null(id) && is.null(r) && is.null(k) && is.null(a) && is.null(s) && is.null(hs.names)) {
        hs.matching.idx <- 1:length(lhs)
    } else {    
        hs.matching.idx <- lhs.select.which(lhs, id=id, r=r, k=k, a=a, s=s, hs.names=hs.names)
    }
    if (length(hs.matching.idx)==0) stop("No sets of hulls found matching those criteria")
    
    for (hs.idx in hs.matching.idx) {
        
        if (!is.null(lhs[[hs.idx]][["ellipses"]])) {
            if (existing.ellipses == "abort") stop("Ellipses already exist. Try setting existing.ellipses='overwrite'")
        }
        
        if (status) cat(names(lhs)[hs.idx], "\n  Calculating enclosing ellipses \n", sep="")
        
        checks <- (anyDuplicated(coordinates(lhs[[hs.idx]]$pts)) != 0)
                
        ## Get minimum volumne enclosing ellipsoid for each hull
        ellipse.params.lst <- pblapply(lhs[[hs.idx]][["hulls"]]@polygons, function(p) mvee(p@Polygons[[1]]@coords[-nrow(p@Polygons[[1]]@coords),], plotme=FALSE, tolerance = 0.005, no.ellipse.val=2, checks=checks))
        
        ## Convert list of ellipse parameters to data frame
        hulls.ellipses <- do.call(rbind, lapply(ellipse.params.lst, function(x) data.frame(cx=x$c[1], cy=x$c[2], a=x$ab[1], b=x$ab[2], alpha=x$alpha)))

        ## Add the pts.idx column to the data frame
        hulls.ellipses <- cbind(pts.idx=lhs[[hs.idx]][["hulls"]][["pts.idx"]], hulls.ellipses)

        ## Update hull metrics for ecc and bearing
        lhs[[hs.idx]][["hulls"]][["ecc"]] <- with(hulls.ellipses, sqrt(1 - (b^2/a^2)))
        lhs[[hs.idx]][["hm"]][["ecc"]] <- list(type="ecc", aux=NULL)
        
        ## Save ellipses if needed
        if (save.ellipses) lhs[[hs.idx]][["ellipses"]] <- hulls.ellipses 
    
    }
    time.taken = difftime(Sys.time(), start.time, units="auto")
    if (status) cat("Total time:", round(time.taken,1), units(time.taken), "\n", sep = " ")    
    if (beep) {
        flush.console()
        for (i in 1:3) {
            alarm()
            Sys.sleep(0.8)
        }
    }

    return(lhs)
    
}
