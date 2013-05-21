#' Identify the points that fall within a bounding box
#'
#' @param lxy A LoCoH-xy object
#' @param aoi Area-of-interest, a two-column xy data frame containing the upper left point and the lower right point in the first and second rows respectively.
#'
#' @return A dataframe containing the ptid values and indices of the points from \code{lxy} that fall within \code{aoi}
#'
#' @note This function can be used to identify specific points on a plot. For example, you see a cluster of points on a locoh-xy plot,
#' and want to know when those points were recorded, and so on. To do this, you can use the \code{\link{aoi}} function to define an area of interest box
#' on the plot window, and then use \code{\link{lxy.identify.aoi}} to grab the indices and ptid values for that cluster of points.
#' You can then use this information as a parameter in \code{\link{lxy.subset}} function to create a \code{locoh-xy} object containing just that cluster of points.
#'
#' @seealso \code{\link{aoi}}, \code{\link{plot.locoh.lxy}}, \code{\link{lxy.subset}}
#' @export

lxy.identify.aoi <- function (lxy, aoi) {

    if (!inherits(lxy, "locoh.lxy")) stop("lxy should be of class \"locoh.lxy\"")
    if (!is.data.frame(aoi)) stop("aoi must be a two column data frame")
    
    lxy.coord <- lxy[["pts"]]@coords
    rx <- range(aoi[,1])
    ry <- range(aoi[,2])
    pib.idx <- which(lxy.coord[,1] >= rx[1] & lxy.coord[,1] <= rx[2] & lxy.coord[,2] >= ry[1] & lxy.coord[,2] <= ry[2])

    if (length(pib.idx) > 0) {
        return(data.frame(idx=pib.idx, ptid=lxy[["pts"]][["ptid"]][pib.idx]))
    } else {
        return(NULL)
    }
    
}
