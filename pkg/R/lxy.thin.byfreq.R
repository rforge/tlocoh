#' Delete locations to harmonize the sampling frequency and time duration
#'
#' Standardize the sampling frequency and duration across individuals in a \link{LoCoH-xy} object by deleting points
#'
#' @param lxy A \link{LoCoH-xy} object
#' @param id The id value(s) to be harmonized
#' @param trim.ends Truncate points from either end of the timeline to achieve a common time window, T/F
#' @param dt.start The starting date-time that all individual trajectories will be truncated to. If \code{NULL}, the first date-time that 
#' all points have in common will be used.
#' @param dt.end The end date-time that all individual trajectories will be truncated to. If \code{NULL}, the last date-time that 
#' all points have in common will be used.
#' @param byfreq Delete points to achieve a common sampling frequency (\code{samp.freq}), T/F
#' @param samp.freq The common time step for the output (in seconds). Can also be set to \code{"lcm"}, in which case the least common multiple
#' of the median time step of each individual will be computed
#' @param lcm.round When \code{samp.freq="lcm"}, the median time step for each individual will be rounded to the nearest interval of \code{lcm.round} 
#' (in seconds)
#' @param lcm.max.iter The maximum number of iterations to be used in the algorithm that finds the least common multiple of the median time steps
#' @param dt.int.round.to The proportion of the median sampling frequency that time intervals will be rounded 
#' to when computing the frequency table of sampling intervals (no change is made to the time stamps)
#' @param tau.diff.max The maximum deviation from tau (the median delta.t of the entire dataset), expressed 
#' as a proportion of tau, that time difference between two points must fall for the distance between those 
#' two points to be included in the calculation of the median step length
#' @param status Show messages, T/F
#'
#' @note This function processes a \link{LoCoH-xy} object that contains movement data for several individuals, and removes points
#' such that the output contains a fixed start and end date for each individual, as well as an approximately uniform sampling frequency (time step).
#' 
#' Before using this function, you should clean your data of all abnormally short time intervals (e.g., bursts). See \code{\link{lxy.thin.bursts}}.
#'
#' If you know the time interval the data was *supposed* to be sampled (e.g., every 20 minutes), you should 
#' pass that value for \code{samp.freq} (expressed in seconds). If \code{samp.freq="lcm"}, the function will 
#' automatically compute the common time step for the individuals by taking the least common multiple 
#' of the median time steps of each individual. You can deal with noise by rounding the median sampling interval to 
#' the value of \code{lcm.round} (expressed in seconds).
#'
#' The function \code{\link{lxy.plot.freq}} can help you see the 'actual' sampling intervals in the data (set \code{by.date=TRUE}).
#'
#' Because this function deletes points, the nearest-neighbors lookup table of the \link{LoCoH-xy} object (if any) will be deleted.
#'
#' @return a \link{LoCoH-xy} object
#' @export
#' @seealso \code{\link{xyt.lxy}}, \code{\link{lxy.plot.freq}}, \code{\link{lxy.thin.bursts}}

# You can thin by dt.start / end, deltat, or both

lxy.thin.byfreq <- function (lxy, id=NULL, trim.ends=TRUE, dt.start=NULL, dt.end=NULL, 
                             byfreq=TRUE, samp.freq="lcm", lcm.round=120, lcm.max.iter=300,
                             status=TRUE, dt.int.round.to=0.1, tau.diff.max=0.02) {

    if (!inherits(lxy, "locoh.lxy")) stop("lxy should be of class \"locoh.lxy\"")
    if (is.null(lxy[["pts"]][["dt"]])) stop("Can't harmonize the temporal frequency without date-time values")
    if (!trim.ends && !byfreq) stop("Don't know what to do. trim.ends and/or byfreq must be TRUE")
    
    if (is.null(id)) {
        id <- levels(lxy[["pts"]][["id"]])
    } else {
        if (FALSE %in% (id %in% levels(lxy[["pts"]][["id"]]))) stop("id value(s) not found")
    }
    idx.comb <- which(lxy[["pts"]][["id"]] %in% id)
    
    idx.not.analyzed <- (1:nrow(lxy[["pts"]]))[-idx.comb]

    ## Take note of the original time zone so we can restore it later
    tz.lxy <- attr(lxy[["pts"]][["dt"]], "tzone")
    
    ## Find the dt.start and dt.end
    if (trim.ends) {
        idx.comb.orig.len <- length(idx.comb)
        
        ## Make a list of the indices of each id
        ids.idx.lst <- lapply(id, function(x) which(lxy[["pts"]][["id"]] == x))

        if (is.null(dt.start)) {
            dt.start.use <- as.POSIXct(max(as.numeric(sapply(ids.idx.lst, function(x) min(lxy[["pts"]][["dt"]][x])))), origin="1970-01-01", tz="UTC")
            if (!identical(tz.lxy, "UTC")) dt.start.use <- as.POSIXct(format(dt.start.use, tz=tz.lxy), tz=tz.lxy)
        } else {
            dt.start.use <- dt.start
        }

        if (is.null(dt.end)) {
            dt.end.use <- as.POSIXct(min(as.numeric(sapply(ids.idx.lst, function(x) max(lxy[["pts"]][["dt"]][x])))), origin="1970-01-01", tz="UTC")
            if (!identical(tz.lxy, "UTC")) dt.end.use <- as.POSIXct(format(dt.end.use, tz=tz.lxy), tz=tz.lxy)
        } else {
            dt.end.use <- dt.end
        }
        
        ## Error check that dt.start <- dt.end
        if (dt.start.use >= dt.end.use) {
            if (is.null(dt.start) || is.null(dt.end)) {
                stop("No temporal overlap detected")
            } else {
                stop("dt.start must be earlier than dt.end")
            }
        }
        
        ## Keep only points that fall within the common date range
        idx.comb <- idx.comb[lxy[["pts"]][["dt"]][idx.comb] >= dt.start.use & lxy[["pts"]][["dt"]][idx.comb] <= dt.end.use]
        
        if (length(idx.comb)==0) stop("No points fall within those date ranges")
        
        if (length(idx.comb) == idx.comb.orig.len) {
            if (status) cat(" - no points thinned out due to the start and end dates \n")        
        } else {
            if (status) cat(" - trimming date-times to ", format(dt.start.use, format="%Y-%m-%d"), " to ", format(dt.end.use, format="%Y-%m-%d"), "\n", sep="")
        }
    }
    
    if (byfreq) {
        
        ## Error check samp.freq
        if (is.null(samp.freq)) stop("Missing required parameter: samp.freq")

        ## Start a vector of the indices of points that will be kept in the lxy object returned
        idx.keepers <- NULL
        num.dups.lst <- list()
        
        if (trim.ends) {
            ## Since we truncated some points, need to recompute rw.params (to get new taus)
            rwp.df <- xyt.rw.params.dt.int(id=lxy[["pts"]][["id"]][idx.comb], xy=coordinates(lxy[["pts"]])[idx.comb,], dt=lxy[["pts"]][["dt"]][idx.comb], dt.int.round.to=dt.int.round.to, tau.diff.max=tau.diff.max)[["rw.params"]] 
        } else {
            rwp.df <- lxy[["rw.params"]]
        }
        taus <- rwp.df[rwp.df[["id"]] %in% id, "time.step.median"]
        
        ## Compute the common samp.freq (if needed)
        if (identical(samp.freq, "lcm")) {
            if (lcm.round != 1) taus <- lcm.round * round(taus / lcm.round)
            samp.freq <- least.common.multiple(taus, max.iter=lcm.max.iter, show.err=FALSE)
            if (is.null(samp.freq)) {
                stop(cw("Unable to find the least common multiple of the sampling frequencies. Make sure you've removed all extremely short-interval points (bursts), then either provide a numeric value for samp.freq, or increase lcm.round and/or lcm.max.iter \n", final.cr=F))
            } 
            if (status) cat(" - thinning to achieve a sampling frequency of ", secs.fmt(samp.freq), "\n", sep="")
        } else {
            if (samp.freq < min(taus)) stop("samp.freq can not be less than the median time step")
        }

        ## Loop through idVals
        for (idVal in id) {
            idx.idVal <- idx.comb[lxy[["pts"]][["id"]][idx.comb] == idVal]
            
            dts.int <- as.numeric(lxy[["pts"]][["dt"]][idx.idVal])
            
            ## Define a series of target times based on samp.freq
            dts.target <- seq(from=dts.int[1], to=dts.int[length(dts.int)], by=samp.freq)
            if (dts.target[length(dts.target)] < dts.int[length(dts.int)]) {
                dts.target <- c(dts.target, dts.target[length(dts.target)] + samp.freq)
            }
            
            ## For each element of dts.target, identify the nearest element of dts.int
            dts.target.nn.idx <- get.knnx(data=dts.int, query=dts.target, k=1)[["nn.index"]]
            
            ## Record the number of duplicates, which are most likely due to sampling gaps > samp.freq 
            num.dups.lst[[idVal]] <- sum(duplicated(dts.target.nn.idx))
            
            ## Add the indices of the closest points to the 'keepers' list
            idx.keepers <- c(idx.keepers, idx.idVal[unique(dts.target.nn.idx)])

        }
        
        idx.comb <- idx.keepers
        

    }
    
    if (status && byfreq) {
        cat(" - time periods without points (probably due to gaps in the sampling): \n")
        print(formatdf4print(as.data.frame(num.dups.lst)), row.names=FALSE)
    }
    
    idx.return <- sort(c(idx.not.analyzed, idx.comb))
    return(lxy.subset(lxy, idx=idx.return, dt.int.round.to=dt.int.round.to, tau.diff.max=tau.diff.max))

}
