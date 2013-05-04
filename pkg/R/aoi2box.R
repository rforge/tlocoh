#' Converts a dataframe with upper-left / lower-right coordinates of a box to a closed polygon
#'
#' @param aoi A two-column two-row data frame or matrix with x in the first column and y in the second columm. The upper left point and the lower right point in the first and second rows respectively
#'
#' @return Two column dataframe containing the xy coordinates of a closed rectangle that can be plotted using \code{\link{points}} or \code{\link{polygons}}
#'
#' @note
#' You can use the \code{aoi} function to select two coordinates on the active plot window with the mouse. This function facilitates plotting that rectangle.
#'
#' @seealso \code{\link{aoi}}
#'
#' @examples
#' \dontrun{
#' myaoi <- aoi()
#' polygons(myaoi)
#' }
#'
#' @export

aoi2box <- function(aoi) {
    return(data.frame(x=c(aoi[1,1], aoi[2,1], aoi[2,1], aoi[1,1], aoi[1,1]), y=c(aoi[1,2], aoi[1,2], aoi[2,2], aoi[2,2], aoi[1,2])))
}

