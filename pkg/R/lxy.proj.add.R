#' Add ancillary variable(s) to a LoCoH-xy object from a raster
#'
#' @param lxy \link{LoCoH-xy} object
#' @param proj4string An object of class "CRS", projection string 
#'
#' @return A \link{LoCoH-xy} object 
#'
#' @seealso \code{\link{CRS-class}}
#' @examples
#' lxy <- lxy.proj.add(lxy, CRS("+proj=utm +south +zone=34"))
#' @export

lxy.proj.add <- function(lxy, proj4string=CRS(as.character(NA))) {

    if (!inherits(lxy, "locoh.lxy")) stop("lxy should be of class \"locoh.lxy\"")
    lxy[["pts"]]@proj4string <- proj4string 
    
    return(lxy)
}
