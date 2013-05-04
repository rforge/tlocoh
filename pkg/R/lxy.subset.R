#' Take a subset of a LoCoH-xy object
#'
#' Returns a subset of a LoCoH-xy object based on the id, ptid, idx, or dates
#'
#' @param lxy A \link{LoCoH-xy} object
#' @param id The id value(s) to include in the subset
#' @param ptid A vector of ptid values for the subset of points
#' @param idx A vector of indices for the subset of points
#' @param dt.start A starting date for the subset. POSIXct object (or something that can be coered to POSIXct)
#' @param dt.end An ending date for the subset. POSIXct object (or something that can be coered to POSIXct)
#' @param dt.int.round.to The proportion of the median sampling frequency that time intervals will be rounded to when computing the frequency table of sampling intervals (no change is made to the time stamps)
#' @param tau.diff.max The maximum deviation from tau (expressed as a proportion of tau) that a point-to-point time difference must fall within for the point-to-point distance to be included in the calculation of the median step length
#'
#' @return A \link{LoCoH-xy} object
#'
#' @seealso \code{\link{xyt.lxy}}, \code{\link{lxy.repair}}, \code{\link{lxy.thin.byfreq}}, \code{\link{lxy.thin.bursts}}, \code{\link{lxy.merge}}
#' @export

lxy.subset <- function(lxy, id=NULL, ptid=NULL, idx=NULL, dt.start=NULL, dt.end=NULL, dt.int.round.to=0.1, tau.diff.max=0.02) {

    if (!inherits(lxy, "locoh.lxy")) stop("lxy should be of class \"locoh.lxy\"")
    if (!require(sp)) stop("package sp required")
    if (is.null(id) && is.null(ptid) && is.null(idx) && is.null(dt.start) && is.null(dt.end)) stop("no parameters for filtering passed")
    
    num.pts <- nrow(lxy[["pts"]])

    if (is.null(id)) {
        idx.ids <- 1:num.pts        
    } else {
        idx.ids <- which(lxy[["pts"]][["id"]] %in% id)
    }
    
    if (is.null(ptid)) {
        idx.ptid <- 1:num.pts        
    } else {
        idx.ptid <- which(lxy[["pts"]][["ptid"]] %in% ptid)
    }

    if (is.null(dt.start)) {
        idx.dt.start <- 1:num.pts        
    } else {
        if (!is(dt.start, "POSIXct")) stop("dt.start must be of class POSIXct")
        if (is.null(lxy[["pts"]][["dt"]])) stop("No timestamps saved")
        idx.dt.start <- which(lxy[["pts"]][["dt"]] >= dt.start)
    }
    
    if (is.null(dt.end)) {
        idx.dt.end <- 1:num.pts        
    } else {
        if (!is(dt.end, "POSIXct")) stop("dt.end must be of class POSIXct")
        if (is.null(lxy[["pts"]][["dt"]])) stop("No timestamps saved")
        idx.dt.end <- which(lxy[["pts"]][["dt"]] <= dt.end)
    }

    if (is.null(idx)) {
        idx.idx <- 1:num.pts
    } else {
        idx.idx <- idx
    }

    "%i%" <- intersect
    idx.filter <- idx.ids %i% idx.ptid %i% idx.dt.start %i% idx.dt.end %i% idx.idx
    
    if (length(idx.filter) == num.pts) {
        stop("These parameters do not filter out any locations \n")
    } else if (length(idx.filter) == 0) {
        stop("No locations meeting these criteria were found  \n")
    } else {
        lxy[["pts"]] <- lxy[["pts"]][idx.filter, ]
        
        ## Drop unused factors
        for (colname in names(lxy[["pts"]]@data)[sapply(lxy[["pts"]]@data, is.factor)]) {
            lxy[["pts"]][[colname]] <- lxy[["pts"]][[colname]][ , drop=TRUE]
        }

        lxy[["nn"]] <- NULL
        
        ## Calculate frequency table and median value of time interval for each id
        rwp.dti.lst <- xyt.rw.params.dt.int(id=lxy[["pts"]][["id"]], xy=coordinates(lxy[["pts"]]), dt=lxy[["pts"]][["dt"]], dt.int.round.to=dt.int.round.to, tau.diff.max=tau.diff.max) 
        lxy[["dt.int"]] <- rwp.dti.lst[["dt.int"]]
        lxy[["rw.params"]] <- rwp.dti.lst[["rw.params"]]
        
        ## Construct a 'comment'
        comment <- list()
        ids.tab <- table(lxy[["pts"]][["id"]])
        for (id.name in names(ids.tab)) {
            comment[[id.name]] <- paste(id.name, ".n", ids.tab[[id.name]], ".", 
                  format(min(lxy[["pts"]][["dt"]][lxy[["pts"]][["id"]]==id.name]), format = "%Y-%m-%d", tz = ""), ".", 
                  format(max(lxy[["pts"]][["dt"]][lxy[["pts"]][["id"]]==id.name]), format = "%Y-%m-%d", tz = ""), sep="")
        }
        lxy[["comment"]] <- comment
        return (lxy)                
    }
}
