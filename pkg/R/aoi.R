#' Select an area of interest on a plot using the mouse
#'
#' Allows the user to select an area of interest (box) by clicking on the active plot window
#'
#' @param draw.poly Whether to draw the box on the plot. T/F.
#' @param draw.poly.col The color to use if drawing the box on the plot. Color value. Ignored if \code{draw.poly=FALSE}.
#' @param status Show messages to the user
#'
#' @note
#' This function requires a plot window to be open
#'
#' @return A two-column xy data frame containing the upper left point and the lower right point in the first and second rows respectively
#'
#' @export

aoi <- function(draw.poly=TRUE, draw.poly.col="black", status=TRUE) {

    if (dev.cur()==1) stop("No plot window open")
    if (status) cat("Click *two* points on the active plotting window that define a box \n" )
    flush.console()

    pts <- locator(n=2, type="n")
    if (is.null(pts)) stop("Didn't get a point, try again.")
    pts <- data.frame(x=pts$x, y=pts$y)
    
    ul.x <- min(pts[,"x"])
    ul.y <- max(pts[,"y"])
    lr.x <- max(pts[,"x"])
    lr.y <- min(pts[,"y"])
    
    if (draw.poly) {
      bbox <- as.data.frame(rbind(c(ul.x,ul.y), c(lr.x,ul.y), c(lr.x,lr.y), c(ul.x,lr.y), c(ul.x,ul.y)))
      polygon(bbox, border=draw.poly.col)
    }
    return(data.frame(x=c(ul.x,lr.x), y=c(ul.y,lr.y)))
}
