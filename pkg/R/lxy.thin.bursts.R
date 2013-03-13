#' Thins out the 'bursts' in a GPS dataset
#'
#' Thin out closely-timed bursts of locations
#'
#' @param lxy A \code{\link{LoCoH-xy}} object
#' @param id The id value(s) to be thinned
#' @param thresh The threshhold for delta.t below which a pair of points is considered to be part of a burst, expressed either as a proportion of the median sampling frequency (0..1) or an absolute unit of time (in seconds)
#' @param replace The burst replacement method ('mean' or 'median')
#' @param info.only Show information about number of bursts (only)
#' @param dt.int.round.to The proportion of the median sampling frequency that time intervals will be rounded to when computing the frequency table of sampling intervals (no change is made to the time stamps)
#' @param tau.diff.max The maximum deviation from tau (the median delta.t of the entire dataset), expressed as a proportion of tau, that time difference between two points must fall for the distance between those two points to be included in the calculation of the median step length
#'
#' @note This function processes 'bursts' of locations, where a 'burst' is a series of locations captured 
#' close together in time. Each group of points in a burst is replaced with a single point. This of course presumes the
#' burst of locations is an artifact of data collection and not desirable.
#'
#' Many GPS devices have a feature to save 'bursts' of points close together in time (relative to the dominant sampling frequency)
#' The 'burst' feature should not be confused with point averaging, whereby a GPS device internally averages locations 
#' for a period of time (e.g., 2 minutes) but saves a single location. 
#' 
#' \code{thresh} is a value for the sampling interval for identifying which points should be considered part of 
#' a burst. \code{thresh} can be a proportion of the median sampling frequency (0..1) or an absolute unit of time (in seconds).
#'
#' To identify whether there are bursts in a \code{\link{LoCoH-xy}} dataset, and the sampling frequency of those bursts (i.e., the value 
# 'you should use for \code{thresh}), run \code{\link{lxy.plot.freq}} with \code{cp=TRUE}.
#'
#' The two replacement methods are /code{replace="mean"}, in which case a burst of locations is replaced by a single point consisting of the 
#' spatial and temporal average of the burst, or /code{replace="median"} in which case a burst of locations is replaced by the location
#' at the temporal median of the burst. If /code{replace="mean"}, then any ancillary variables for the 'new' points will be set to \code{NA}.
#'
#' Because this function deletes points, the nearest neighbor lookup table of the \code{\link{LoCoH-xy}} object (if any) will be deleted, 
#' and the parameters for random walk model will be recomputed.
#'
#' @return a \code{\link{LoCoH-xy}} object
#' @seealso \code{\link{lxy.plot.freq}}, \code{\link{lxy.thin.freq}}
#' @export

lxy.thin.bursts <- function (lxy, id=NULL, thresh=NULL, replace=c("mean","median")[2], info.only=FALSE, status=TRUE,
                             dt.int.round.to=0.1, tau.diff.max=0.02) {

    if (!require(sp)) stop("package sp required")
    if (!inherits(lxy, "locoh.lxy")) stop("lxy should be of class \"locoh.lxy\"")
    if (is.null(lxy[["pts"]][["dt"]])) stop("Can't thin bursts without date-time values")
    if (!replace %in% c("mean","median")) stop("Unknown value for 'replace'")
    if (is.null(thresh)) stop("Please provide a value for thresh [0..1]. Tip: run lxy.plot.freq with cp=T")
    
    if (is.null(id)) {
        id <- levels(lxy[["pts"]][["id"]])
    } else {
        if (FALSE %in% (id %in% levels(lxy[["pts"]][["id"]]))) stop("id value(s) not found")
    }
    
    idx.remove <- NULL
    xys.new <- NULL
    dt.new <- NULL
    anv.new <- NULL
    ids.new <- NULL
    info.lst <- list()

    ## Create a data frame that we can use to add rows of NA to anv.new
    #if (!is.null(lxy[["anv"]]) && replace=="mean") anv.na <- tail(rbind(lxy[["anv"]], rep(NA, length(lxy[["anv"]]))), n=1)
    if (!is.null(lxy[["anv"]]) && replace=="mean") anv.na <- rbind(lxy[["pts"]]@data[1,as.character(lxy$anv$anv)], rep(NA, nrow(lxy[["anv"]])))[2,]
  
    for (idVal in id) {
        idx <- which(lxy[["pts"]][["id"]] == idVal)
        tau <- lxy[["rw.params"]][lxy[["rw.params"]][["id"]] == idVal, "time.step.median"]
        
        ##dt.int is the difference in seconds between time stamps, starting from dt[2] - dt[1]
        dt.int <- diff(as.numeric(lxy[["pts"]][["dt"]][idx]))

        if (thresh < 1) {
            thresh.use <- thresh * tau
        } else {
            thresh.use <- thresh
        }

        #cat("thresh.use = ", thresh.use, "\n", sep="")
        
        ## Construct a factor that will put the short-interval points into separate groups
        dtbt <- (dt.int <= thresh.use)
        dtbt.diff <- abs(c(0, diff(dtbt)))
        dtbt.diff.cs <- cumsum(dtbt.diff)
        dtbt.diff.cs.no0 <- dtbt * dtbt.diff.cs
        dtbt.grps.vec <- ceiling(dtbt.diff.cs.no0 / 2)

        ## Create a list object with the indices of the short-interval points
        dtbt.grps.lst <- split(1:length(dtbt), dtbt.grps.vec)[-1]
        
        if (length(dtbt.grps.lst) == 0) {
            cat("No bursts were found for id ", idVal, "\n")
            info.lst[[idVal]] <- NA
        } else {
            ## Add one more index to the end of each burst group
            dtbt.grps.lst <- lapply(dtbt.grps.lst, function(x) c(x, x[length(x)]+1))
            
            ## Save information about the number of bursts and number of points per burst
            dtbt.grps.len <- sapply(dtbt.grps.lst, length) 
            info.lst[[idVal]] <- as.data.frame(table(dtbt.grps.len))
            names(info.lst[[idVal]]) <- c("pts.in.burst","freq")
            
            ## Record the points the need to get deleted
            idx.remove <- c(idx.remove, idx[unlist(dtbt.grps.lst)])
        
            ## Create replacement points
            if (replace=="mean") {
                xys.new <- rbind(xys.new, do.call(rbind, lapply(dtbt.grps.lst, function(x) colMeans(coordinates(lxy[["pts"]])[idx[x], ]))))
                dt.new <- c(dt.new, sapply(dtbt.grps.lst, function(x) mean(lxy[["pts"]][["dt"]][idx[x]])))
                if (!is.null(lxy[["anv"]])) anv.new <- rbind(anv.new, anv.na[rep(1,length(dtbt.grps.lst)), ])
            } else if (replace=="median") {
                xys.new <- rbind(xys.new, do.call(rbind, lapply(dtbt.grps.lst, function(x) coordinates(lxy[["pts"]])[idx[ceiling(median(x))], ])))
                if (!is.null(lxy[["anv"]])) anv.new <- rbind(anv.new, do.call(rbind, lapply(dtbt.grps.lst, function(x) lxy[["pts"]]@data[idx[ceiling(median(x))], as.character(lxy[["anv"]][["anv"]]), drop=FALSE])))
                dt.new <- c(dt.new, sapply(dtbt.grps.lst, function(x) lxy[["pts"]][["dt"]][idx[ceiling(median(x))]]))
            }
            ids.new <- c(ids.new, rep(idVal, length(dtbt.grps.lst)))
        }
        
        
    }

    if (info.only) {
        print(info.lst)
        return(invisible(NULL))
    } else {
        # Get rid of points marked for deletion
        lxy[["pts"]] <- lxy[["pts"]][-idx.remove,]

        ## Save the local time zone, we will need it later
        tz.local <- attr(lxy[["pts"]][["dt"]], "tz")

        ## In preparation for merging, convert dt to numeric
        dt.orig <- lxy[["pts"]]@data[["dt"]]
        lxy[["pts"]]@data[["dt"]] <- as.numeric(lxy[["pts"]]@data[["dt"]])

        ## Create ptid values for the new points
        ptid.new <- (1:length(ids.new))+ max(lxy[["pts"]][["ptid"]])
        
        ## Put the new objects in a data frame
        data <- data.frame(ptid=ptid.new, id=ids.new, dt=dt.new)
        if (!is.null(anv.new)) data <- data.frame(data, anv.new)
        
        ## Create a SpatialPointsDataFrame for the new points
        pts.new <- SpatialPointsDataFrame(coords=xys.new, data=data, proj4string=lxy[["pts"]]@proj4string, match.ID=FALSE)
        
        ## Merge the new objects
        pts.comb <- rbind(lxy[["pts"]], pts.new)
        
        ## Convert the time values, which are now numeric and presumably in UTC, to a POSIXct object specifying the time zone "UTC"
        dt.gmt <- as.POSIXct(pts.comb[["dt"]], origin="1970-01-01", tz="UTC")
        
        ## Convert the UTC times back to local time
        dt.local <- as.POSIXct(format(dt.gmt, tz=tz.local), tz=tz.local)
        
        return(xyt.lxy(xy=coordinates(pts.comb), proj4string=pts.comb@proj4string, id=pts.comb[["id"]], ptid=pts.comb[["ptid"]], dt=dt.local, anv=pts.comb@data[,as.character(lxy[["anv"]][["anv"]]),drop=FALSE], anv.desc=lxy[["anv"]][["desc"]], dt.int.round.to=dt.int.round.to, tau.diff.max=tau.diff.max))

    }

}
