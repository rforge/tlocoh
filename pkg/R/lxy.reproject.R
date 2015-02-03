#' Project or reproject coordinates
#'
#' Project or reproject coordinates of a \link{LoCoH-xy} object
#'
#' @param lxy A \link{LoCoH-xy} object
#' @param proj Projection object of class \code{\link{CRS-class}}
#' @param dt.int.round.to The proportion of the median sampling frequency that time intervals will be rounded to when computing the frequency table of sampling intervals (no change is made to the time stamps)
#' @param tau.diff.max The maximum deviation from tau (expressed as a proportion of tau) that a point-to-point time difference must fall within for the point-to-point distance to be included in the calculation of the median step length
#' @param status Show status messages (T/F)
#'
#' @details This function will reproject the locations to a new coordinate system. In order for this to work,
#'
#' \itemize{
#'     \item The coordinate system of the current locations must be recorded. To see the current coordinate system, run \code{\link{summary.locoh.lxy}}. To add projection information (which you need to get from the source of the data), use \code{\link{lxy.proj.add}}.
#'     \item Argument \code{proj4string} which is an object of class \code{\link{CRS-class}} containing information about the new projection system. A good place to find the proj4 strings for a projection system is \url{http://www.spatialreference.org}.
#' }
#'
#' Note that not all coordinate systems can be projected into all other coordinate systems.
#' If you get an error message like \emph{'non finite transformation detected'}, the coordinate system
#' you are trying to project into may not be compatible with the existing coordinate system.
#'
#' @examples
#' \dontrun{
#' lxy.utm <- lxy.reproject(lxy, CRS("+proj=utm +north +zone=18 +ellps=WGS84"))
#' }
#'
#' @seealso \code{\link{lxy.proj.add}}, \code{\link{summary.locoh.lxy}}, \code{\link{CRS}}
#' @export
#' @import sp

lxy.reproject <- function(lxy, proj, dt.int.round.to=0.1, tau.diff.max=0.02, status=TRUE) {

    if (!requireNamespace("rgdal")) stop("package rgdal required")
    if (!inherits(lxy, "locoh.lxy")) stop("lxy should be of class \"locoh.lxy\"")
    if (identical(lxy[["pts"]]@proj4string, CRS(as.character(NA)))) stop(cw("Coordinates can not be reprojected because the current coordinate system is not known. Specify the current projection system with lxy.proj.add and try again.", final.cr=FALSE))
    
    if (is(proj, "CRS")) {
        crs.good <- rgdal::checkCRSArgs(proj@projargs)
        if (!crs.good[[1]]) stop(crs.good[[2]])
    } else if (is.character(proj)) {
        crs.good <- rgdal::checkCRSArgs(proj)
        if (!crs.good[[1]]) stop(crs.good[[2]])
        proj <- CRS(proj)
    } else {
        stop("proj must be class 'CRS' or 'character'")
    } 

    ## Project the spatial points data frame
    lxy[["pts"]] <- spTransform(lxy[["pts"]], proj)

    if (!is.null(lxy[["pts"]]@data[["dt"]])) {
        ## Calculate frequency table and median value of time interval for each id
        rwp.dti.lst <- xyt.rw.params.dt.int(id=lxy[["pts"]]@data[["id"]], xy=coordinates(lxy[["pts"]]), dt=lxy[["pts"]]@data[["dt"]], dt.int.round.to=dt.int.round.to, tau.diff.max=tau.diff.max)
        lxy[["rw.params"]] <- rwp.dti.lst[["rw.params"]]
    }
    
    if (!is.null(lxy[["ptsh"]])) {
        if (status) cat(cw(" - proportion of time-selected hulls table must be recreated", final.cr=T))
        lxy[["ptsh"]] <- NULL
    }
    
    if (!is.null(lxy[["nn"]])) {
        if (status) cat(cw(" - nearest neighbor lookup table must be recreated", final.cr=T))
        lxy[["nn"]] <- NULL
    }

    return(lxy)

}

