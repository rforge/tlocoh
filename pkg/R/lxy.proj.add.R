#' Specify the projection system for a LoCoH-xy object
#'
#' @param lxy \link{LoCoH-xy} object
#' @param proj4string An object of class "CRS", projection string 
#'
#' @return A \link{LoCoH-xy} object 
#'
#' @details This function will add projection information to a \link{LoCoH-xy} object. Note this does 
#' not reproject coordinates, it only adds or changes the projection information. Any existing projection
#' information will be overwritten.
#' 
#' @seealso \code{\link{CRS-class}} for arguments accepted by the \code{\link{CRS}} function.
#' @examples
#' # lxy <- lxy.proj.add(lxy, CRS("+proj=utm +south +zone=34"))
#'
#' @export

lxy.proj.add <- function(lxy, proj4string=CRS(as.character(NA))) {

    if (!inherits(lxy, "locoh.lxy")) stop("lxy should be of class \"locoh.lxy\"")
    if (!is(x, "CRS")) stop("proj4string should be a class 'CRS'. Type ?lxy.proj.add for more info.")
    lxy[["pts"]]@proj4string <- proj4string 
    
    return(lxy)
}
