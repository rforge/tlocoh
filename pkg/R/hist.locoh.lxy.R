#' Plot histograms of properties of a LoCoH-xy object
#'
#' Displays histogram(s) of point-to-point step length, velocity, and sampling frequency for a LoCoH-xy object
#'
#' @param lxy A \link{LoCoH-xy} object
#' @param id The id value(s) to be plotted
#' @param dt Include a histogram of the number of locations over time) (T/F)
#' @param d Include a histogram of distance travelled per adjacent points (i.e., step length) (T/F)
#' @param delta.t Include a histogram of the time between points (i.e., sampling frequency) (T/F)
#' @param v Include a histogram of point velocity (distance over delta.t) (T/F)
#' @param dt.bins.base Where to start the first bin ((for histogram of number of points over time): "secs", "mins", "hours", "days"
#' @param dt.bins.width The width of the time bins (for histogram of number of points over time). In seconds.
#' @param figs.per.page Number of plots per page
#' @param col The color of the bars
#' @param d.tct temporal connectivity thsesshold for the distance between two points to be included in the histogram of step length. In other words, detlta.t must be <= median delta.t * d.tct). Ignored if d=F.
#' @param delta.t.num.sd Number of standard deviations for delta.t to be included in the histogram. To omit outliers from appearing in the histogram (which can make the central data more difficult to discern, set delta.t.num.sd to ~2. Ignored if delta.t=F.
#' @param time.unit The unit of time on the x-axis (character). Ignored if delta.t=F.
#' @param overlay.median Plot the median value on the histogram (T/F)
#' @param breaks Argument passed to the \code{\link{hist}} function, see \code{\link{hist}} 
#'
#' @return A list of frequencies with one element for each of the histograms plotted.
#'
#' @method hist locoh.lxy
#' @export
#' @import sp

hist.locoh.lxy <- function(lxy, id=NULL, dt=TRUE, d=TRUE, delta.t=TRUE, v=TRUE, figs.per.page=NULL, col="gray80",
                           dt.bins.base=c("secs", "mins", "hours", "days")[4], dt.bins.width=3600*24*7,
                           delta.t.num.sd=NULL, d.tct=1.2, time.unit="auto", overlay.median=TRUE, breaks=20) {

    #lxy <- x; rm(x)
    if (!inherits(lxy, "locoh.lxy")) stop("lxy should be of class \"locoh.lxy\"")
    if (delta.t && is.null(lxy[["pts"]][["dt"]])) stop("No time stamps in this dataset, can't plot delta.t")
    if (dt && is.null(lxy[["pts"]][["dt"]])) stop("No time stamps in this dataset, can't plot dates over time")
    if (v && is.null(lxy[["pts"]][["dt"]])) stop("No time stamps in this dataset, can't plot velocity")
    if (!d && !delta.t && !v && !dt) stop("nothing to plot")

    if (is.null(id)) {
        id <- levels(lxy[["pts"]][["id"]])
    } else {
        if (FALSE %in% (id %in% levels(lxy[["pts"]][["id"]]))) stop("id value(s) not found")
    }
    
    opar <- par(mfrow = n2mfrow(if (is.null(figs.per.page)) d + delta.t + v + dt else figs.per.page), mar=c(4,3.2,3.2,0.5), mgp=c(1.2, 0.5, 0), bg="white")
    on.exit(par(opar))
    res <- list()
    
    for (idVal in id) {
    
        id.idx <- which(lxy[["pts"]][["id"]] == idVal)
        p1 <- id.idx[-length(id.idx)]
        p2 <- id.idx[-1]

        if (dt) {
            dt.bins.start <- as.POSIXct(trunc(min(lxy[["pts"]][["dt"]][id.idx]), dt.bins.base))
            dt.bins.start.int <- as.numeric(dt.bins.start)
    
            # Calculate the full range of starting dates for each bin
            dt.bins.all <- dt.bins.start + (dt.bins.width * 0:(ceiling((as.numeric(max(lxy[["pts"]][["dt"]][id.idx])) - as.numeric(dt.bins.start)) / dt.bins.width) - 0))
            
            # Setup the labels for the x-axis. Can't have more than ~10 tick marks
            bins.max <- 10
            bins.skip <- ceiling(length(dt.bins.all) / bins.max)
            bins.idx <- seq(from=1, to=length(dt.bins.all), by=bins.skip)
            
            ## Define a list containing the format strings that will be used to display the date-times on the x-axis
            dt.format <- list(days="%m/%d", hours="%H:%M", mins="%M:%S", secs="%M:%S")

            ## Produce the histogram and x-axis
            hist.obj <- hist(lxy[["pts"]][["dt"]][id.idx], breaks=dt.bins.all, freq=TRUE, xlab="date", ylab="num pts", xlim=range(dt.bins.all), col=col, yaxt="n", xaxt="n", main=paste(idVal, ": Num Locations Over Time", sep=""), sub=paste("bin width=", secs.fmt(dt.bins.width), sep=""), cex.sub=0.9)
            axis(side=1, at=dt.bins.all[bins.idx], labels=format(dt.bins.all[bins.idx], dt.format[[dt.bins.base]]), cex.axis=0.9, pos=0)
            axis(2, pos=as.numeric(dt.bins.start), cex.axis=0.9)
            res[[paste(idVal, ".dt", sep="")]] <- hist.obj
        }
        
        if (d) {
            if (is.null(d.tct) || is.null(lxy[["pts"]][["dt"]])) {
                p1.idx <- p1
                p2.idx <- p2            
            } else {
                delta.t.thisid <- as.numeric(difftime(lxy[["pts"]][["dt"]][p2], lxy[["pts"]][["dt"]][p1], units="secs"))
                tau <- median(delta.t.thisid)                                                     
                p1.idx <- which(delta.t.thisid <= tau * d.tct)
                p2.idx <- p1.idx + 1
            } 
            
            dist <- sqrt((coordinates(lxy[["pts"]])[p2.idx, 1] - coordinates(lxy[["pts"]])[p1.idx, 1])^2 + (coordinates(lxy[["pts"]])[p2.idx, 2] - coordinates(lxy[["pts"]])[p1.idx, 2])^2)
            hist.obj <- hist(dist, xlab="step length", ylab="freq", freq=TRUE, axes=FALSE, main=paste(idVal, ": Distance Travelled", sep=""), breaks=breaks, col=col)
            axis(side=1, cex.axis=0.9, pos=0)
            axis(side=2, cex.axis=0.9, pos=hist.obj[["breaks"]][1])
            if (overlay.median) points(x=median(dist), y=0, pch="|", col="red", cex=4)
            res[[paste(idVal, ".d", sep="")]] <- hist.obj
        }

        if (delta.t) {
            delta.t.thisid <- as.numeric(difftime(lxy[["pts"]][["dt"]][p2], lxy[["pts"]][["dt"]][p1], units="secs"))
            tau <- median(delta.t.thisid)                                                     
            if (time.unit == "auto") time.unit <- time.unit.from.val(tau) 
            time.factor.int <- time.factor(time.unit)
            if (is.null(delta.t.num.sd)) {
                delta.t.idx <- 1:length(delta.t.thisid)
                subtitle <- NULL
            } else {
                delta.t.idx <- (delta.t >= (tau - delta.t.num.sd * sd(delta.t.thisid)) & delta.t <= (tau + delta.t.num.sd * sd(delta.t.thisid)))
                subtitle <- paste("showing dts within ", delta.t.num.sd, " sd(s) of median", sep="")
            }
    
            hist.obj <- hist(delta.t.thisid[delta.t.idx] / time.factor.int, xlab=paste("time interval between samples (", time.unit, "s)", sep=""), ylab="freq",
                                                            freq=TRUE, axes=F, col=col, main=paste(idVal, ": Time Interval", sep=""), sub=subtitle, breaks=breaks)
            axis(side=1, cex.axis=0.9, pos=0)
            axis(side=2, cex.axis=0.9, pos=hist.obj[["breaks"]][1])
            if (overlay.median) points(x=tau / time.factor.int, y=0, pch="|", col="red", cex=4)
            res[[paste(idVal, ".delta.t", sep="")]] <- hist.obj
        }
    
    
        if (v) {
            if (is.null(d.tct) || is.null(lxy[["pts"]][["dt"]])) {
                p1.idx <- p1
                p2.idx <- p2            
            } else {
                delta.t.thisid <- as.numeric(difftime(lxy[["pts"]][["dt"]][p2], lxy[["pts"]][["dt"]][p1], units="secs"))
                tau <- median(delta.t.thisid)                                                     
                p1.idx <- which(delta.t.thisid <= tau * d.tct)
                p2.idx <- p1.idx + 1
            } 
            
            v.vals <- sqrt((coordinates(lxy[["pts"]])[p2.idx, 1] - coordinates(lxy[["pts"]])[p1.idx, 1])^2 + (coordinates(lxy[["pts"]])[p2.idx, 2] - coordinates(lxy[["pts"]])[p1.idx, 2])^2) / delta.t.thisid[p1.idx]
            hist.obj <- hist(v.vals, xlab="velocity", ylab="freq", freq=TRUE,  main=paste(idVal, ": Velocity", sep=""), breaks=breaks, col=col, axes=FALSE)
            axis(side=1, cex.axis=0.9, pos=0)
            axis(side=2, cex.axis=0.9, pos=hist.obj[["breaks"]][1])
            if (overlay.median) points(x=median(v), y=0, pch="|", col="red", cex=4)
            res[[paste(idVal, ".v", sep="")]] <- hist.obj
        }
        
    }  # for idVal
    
    return(invisible(res))

}
