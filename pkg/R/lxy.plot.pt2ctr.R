#' Plot distance to centroid over time
#'
#' Produces a plot of the distance of each point to the centroid over time to see 'natural' periodicities in the data
#'
#' @param lxy A \link{LoCoH-xy} object
#' @param id The id value(s) to include on the plot
#' @param idx An optional vector of indices of points that the plot will be restricted to. See notes
#' @param vline An interval of time (in seconds) for the vertical grid lines
#' @param vline.trunc.units.to A character value: "secs", "mins", "hours", or "days". Determines where the first vertical grid lines will appear
#' @param figs.per.page The number of figures per page
#' @param ... Additional parameters that will be passed to the \code{\link{plot}} function
#'
#' @note
#' The purpose of this graph is to help see the 'natural' movement cycles in a dataset, when no \emph{a priori} information exists that 
#' helps identify the time scale of interest. In T-locoh, defining the timescale of interest helps to select a value for \code{s}.
#'
#' To 'zoom in' to a section of the timeline, pass a value for \code{idx}. For example, if there are 5000 points in the dataset 
# and you want to zoom in to about 500 of those so you can better see the temporal cycles, set idx=1:500
#' 
#' If a value is passed for vline, the first vertical line will coincide with a multiple of \code{vline.trunc.units.to}. For example, 
#' if \code{vline=3600*24}(24 hours), and \code{vline.trunc.units.to="days"}, the first vertical line will be the beginning of a day regardless 
#' of when the point series started. This can be helpful for seeing cycles of a specific frequency.
#' 
#' @export
#' @import sp
#' @seealso \code{\link{lxy.plot.sfinder}}

lxy.plot.pt2ctr <- function(lxy, id=NULL, idx=NULL, vline=NULL, vline.trunc.units.to = c("secs", "mins", "hours", "days")[4], figs.per.page=NULL, ...) {
    
    if (!inherits(lxy, "locoh.lxy")) stop("lxy should be of class \"locoh.lxy\"")
    if (!is.null(id) && !is.null(idx)) stop("only one of the parameters 'id' and 'idx' should be passed")
    if (is.logical(vline)) stop("Improper value for vline. Should be a unit of time in seconds")
    
    if (is.null(idx)) idx <- 1:nrow(lxy[["pts"]])    
    if (is.null(id)) {
        id <- unique(as.character(lxy[["pts"]][["id"]][idx]))
    } else {
        if (FALSE %in% (id %in% levels(lxy[["pts"]][["id"]][idx]))) stop("id value(s) not found")
    }
    if (is.null(figs.per.page)) figs.per.page <- length(id)
    par(mfrow=n2mfrow(figs.per.page))

    for (idVal in id) {
        
        idx.use <- intersect(which(lxy[["pts"]][["id"]]==idVal), idx)
        ctr <- colMeans(coordinates(lxy[["pts"]])[idx.use,])
        d <- sqrt((coordinates(lxy[["pts"]])[idx.use,1] - ctr[1])^2 + (coordinates(lxy[["pts"]])[idx.use,2] - ctr[2])^2)
        plot(x=lxy[["pts"]][["dt"]][idx.use], y=d, type="l", xlab="date", ylab="dist to centroid", main=paste(idVal, ": distance to centroid vs. time", sep=""), ...)
        if (!is.null(vline)) {
              if (vline < lxy[["rw.params"]][lxy[["rw.params"]]$id==idVal, "time.step.median"]) {
                  cat("Improper value for 'vline', skipping vertical lines \n")
              } else {
                  dt.int.start <- as.numeric(as.POSIXct(trunc(min(lxy[["pts"]][["dt"]][idx.use]), vline.trunc.units.to)))
                  abline(v=seq(from=dt.int.start, to=as.numeric(max(lxy[["pts"]][["dt"]][idx.use])), by=vline), lty=3, col="gray70", lwd=0.1)
              }
        }
    }

}
