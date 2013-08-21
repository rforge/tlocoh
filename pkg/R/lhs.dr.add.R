#' Define directional routes
#'
#' Defines 'directional routes' by identifying parent points that are 1) temporally contiguous and 2) have hulls in the top n% of elongation values
#'
#' @param lhs A LoCoH-hullset object
#' @param metric The name of the hull metric used as a proxy for directionality
#' @param thresh.val The threshhold above which a hull is considered part of a directional route
#' @param thresh.type The type of thresshold used. If \code{'q'} for quantile, then \code{thresh.val} is taken to be a percentile of the 
#' full range of the directionality metric values (i.e., 0  < \code{thresh.val} < 1). If \code{'v'}, \code{thresh.val} is taken to be an actual value of the threshhold metric.
#' @param smooth The amount of temporal smoothing applied, expressed as the number of points on either side (temporally) of the parent 
#' point whose average of the directionality metric is used for determining if the hull constructed around the parent point should be considered a part of a directional route.
#' For no smoothing, set \code{smooth = 0}.
#' @param status Display status messages
#' @param show.elong.hist Whether to display histogram(s) of the distribution of the directionality metric before and after smoothing. T/F
#'
#' @note
#' Directional routes are line segments defined by connecting temporally contiguous hull parent-points that are in the top N percent of hull elongation.
#' Hull elongation is proxied by one of two hull metrics: perimeter:area ratio ('par') or the eccentricity of the bounding ellipse ('ecc'). Hull perimeter:area ratios
#' are automatically computed when a hullset is created; bounding ellipses must be computed separately using \code{\link{lhs.ellipses.add}}. 
#' Once computed, you can plot directional routes by passing \code{dr=TRUE} to \code{\link{plot.locoh.lhs}}.
#'
#' @return A LoCoH-hullset object
#'
#' @export


lhs.dr.add <- function(lhs, metric=c("ecc","par")[2], thresh.val=0.95, thresh.type=c("q","v")[1], smooth=1, status=TRUE, show.elong.hist=FALSE) {

    #cat("Still need to implement a temporal connectivity threshhold (proportion of median sampling frequency) for smoothing \n")

    if (!inherits(lhs, "locoh.lhs")) stop("lhs should be of class \"locoh.lhs\"")
    if (thresh.type=="q" && (thresh.val > 1 || thresh.val < 0)) stop("thresh.val is the proportion of points, must be between 0 and 1")
    if (!metric %in% c("ecc","par")) stop("Unknown value for 'metric'")
    
    for (hs.name in names(lhs)) {
        if (is.null(lhs[[hs.name]][["dr"]])) lhs[[hs.name]][["dr"]] <- list()
        
        for (thresh.val.use in thresh.val) {
        for (smooth.use in smooth) {
        for (metric.use in metric) {
            if (metric.use=="ecc" && !metric.use %in% names(lhs[[hs.name]][["hulls"]]@data)) stop("eccentricity values not found. Please compute with lhs.ellipses.add.")
        
            ## Derive a name for this directional route
            dr.name <- paste("dr.", metric.use, ".", thresh.type, thresh.val.use, ".sm", smooth.use, sep="")
            
            if (metric.use == "par") {
                elong.vals <- with(lhs[[hs.name]][["hulls"]]@data, perim / area)
            } else {
                elong.vals <- lhs[[hs.name]][["hulls"]][[metric.use]]
            }
            if (is.null(elong.vals)) stop("That hull metric not found")
            
            ## Use merge() to grab the index of each hull parent point 
            #ptids.idx <- cbind(ptid=lhs[[hs.name]]$ptid, idx=1:length(lhs[[hs.name]]$ptid))
            #hulls.ptid.idx <- merge(data.frame(ptid=as.numeric(names(lhs[[hs.name]][["hulls.polys"]]))), ptids.idx)
            #if (nrow(hulls.ptid.idx) != length(lhs[[hs.name]][["hulls.polys"]])) stop("For some reason could not find a parent point index for all hulls")
            
            pts.idx <- lhs[[hs.name]][["hulls"]][["pts.idx"]]
            
            ## Next we need to sort the elongation values and pts.idx by date
            dt.ord <- order(lhs[[hs.name]][["pts"]][["dt"]][pts.idx])
            elong.vals <- elong.vals[dt.ord]
            pts.idx <- pts.idx[dt.ord]
            
            #hulls.ptid.idx <- hulls.ptid.idx[dt.ord,]
            
            if (smooth.use > 0) {
                ## Smooth by taking the average
                elong.vals.count <- length(elong.vals)
                if (show.elong.hist) xhist <- hist(elong.vals, main="elong.vals before smoothing", breaks=20)
                elong.vals <- sapply(1:elong.vals.count, function(i) mean(elong.vals[seq(max(1, i - smooth.use), min(elong.vals.count, i + smooth.use))]))
                if (show.elong.hist) hist(elong.vals, main="elong.vals after smoothing", breaks=xhist$breaks)
            }
            
            # Next thing is to sort then find a thresshold
            if (thresh.type == "q") {
                elong.vals.min <- sort(elong.vals)[length(elong.vals) * thresh.val.use]
            } else if (thresh.type == "v") {
                elong.vals.min <- thresh.val.use
            } else {
                stop("Dont know how to handle that thresh.type")
            }
            
            ## Define groups of adjacent hulls that are all above or below the thresshold
            elong.vals.above.thresh <- (elong.vals >= elong.vals.min)
            elong.vals.groups <- cumsum(c(1, diff(elong.vals.above.thresh) != 0))
    
            ## Split both idx and above.or.below.thresh by the groups
            #elong.idx.lst <- split(hulls.ptid.idx[, "idx"], elong.vals.groups)
            
            elong.idx.lst <- split(pts.idx, elong.vals.groups)
            
            elong.vals.above.thresh.lst <- split(elong.vals.above.thresh, elong.vals.groups)
    
            ## Identify which of these groups are above the threshhold       
            elong.vals.above.thresh.lst.above.only <- sapply(elong.vals.above.thresh.lst, function(x) x[1])
            
            ## Pull out just those groups where elong.vals are above the thresshold
            elong.idx.lst <- elong.idx.lst[elong.vals.above.thresh.lst.above.only]
            
            ## Filter out any groups that have only one point
            elong.idx.lst <- elong.idx.lst[sapply(elong.idx.lst, length) > 1]
            
            lhs[[hs.name]][["dr"]][[dr.name]] <- list(metric=metric.use, thresh.val=thresh.val.use, thresh.type=thresh.type, smooth=smooth.use, lines=elong.idx.lst)
            
            if (status) cat("    Saved '", dr.name, "' to hull set '", hs.name, "'\n", sep="")
        }
        }
        }
    
    }
    return(lhs)

}
