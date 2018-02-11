#' Clean slivers
#'
#' Remove 'slivers' from a SpatialPolygons* object
#'
#' @param sp A SpatialPolygons or SpatialPolygonsDataFrame object
#' @param status Show messages. T/F 
#' @param min_nodes The minimum number of unique nodes a polygon must have to *not* be labeled a sliver and deleted. 
#' 
#' @details
#' As defined here, a 'sliver' is polygon or hole that has < \code{min_nodes} (default=3) unique nodes
#' (perhaps due to a data type conversion or rounding error). 
#' This seems to be a particularly common type of geometry error that 
#' crops in when unioning hulls into isopleths.
#' 
#' @return 
#' Returns a list with two elements:
#'
#'   sp - the (cleaned) SpatialPolygons* object
#'   results - a dataframe of the polygon and Polygon indices of removed slivers
#'
#' @export

clean_slivers <- function(sp, status=TRUE, min_nodes = 3) {
    
    if (!inherits(sp, "SpatialPolygons")) stop("sp should be a SpatialPolygons* object")

    results <- NULL
    for (i in 1:length(sp)) {
        bad_Poly_idx <- which(unlist(lapply(sp@polygons[[i]]@Polygons, function(x) dim(unique(x@coords))[1] < min_nodes)))
        
        if (length(bad_Poly_idx) > 0) {
          results <- rbind(results, data.frame(polygon_idx=i, Polygon_idx=bad_Poly_idx))
          ## Remove them backwards, because when you take out an element from a list the subsequent indices get messed up
          for (Poly_idx in sort(bad_Poly_idx, decreasing=TRUE)) {
              sp@polygons[[i]]@Polygons[[Poly_idx]] <- NULL ##
              if (status) cat("Getting rid of Polygon ", Poly_idx, " from polygon ", i, "\n", sep="")
          }
        }
    }
    
    return(list(sp=sp, results=results))

}
