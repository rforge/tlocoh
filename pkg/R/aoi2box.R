#' Converts upper-left / lower-right to a closed polygon
#' @export
aoi2box <- function(aoi) {
    ## Takes a two-column xy data frame containing the upper left point of an aoi in row 1 and the lower right point in row 2, 
    ## and returns a xy data frame with five points of a closed rectangle that can be plotted using points() or polygons()
    return(data.frame(x=c(aoi[1,1], aoi[2,1], aoi[2,1], aoi[1,1], aoi[1,1]), y=c(aoi[1,2], aoi[1,2], aoi[2,2], aoi[2,2], aoi[1,2])))
}
