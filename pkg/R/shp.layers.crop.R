shp.layers.crop <- function(shp.layers.lst, bbox) {
    ## Crops the spatial data frame layer in shp.layers.lst to the extent of the bbox
    ## This is used primarily for faster drawing on plots
    ##
    ## shp.layers.lst is a list of spatialdataframe objects and symbology parameters (see shp.layers)
    ## Bbox is a 2x2 xy matrix containing two opposite corner points of a box
    
    ## Create a spatial polygons object for the bounding box
    bb.rx <- range(bbox[,1])
    bb.ry <- range(bbox[,2])
    bb.df <- data.frame(x=bb.rx[c(1,2,2,1,1)], y=bb.ry[c(2,2,1,1,2)])
    bb.spatpoly <- SpatialPolygons(list(Polygons(list(Polygon(bb.df)), ID="a")))
    
    layers.no.intersection <- NULL
    for (i in 1:length(shp.layers.lst)) {
        shp.layer.sdf.cropped <- gIntersection(bb.spatpoly, shp.layers.lst[[i]][["sdf"]])
        if (is.null(shp.layer.sdf.cropped)) {
            layers.no.intersection <- c(layers.no.intersection, i)
        } else {
            shp.layers.lst[[i]][["sdf"]] <- shp.layer.sdf.cropped
        }
    }
    
    ## Get rid of any layers where there was no intersection
    if (length(layers.no.intersection) > 0) shp.layers.lst[layers.no.intersection] <- NULL
    return(shp.layers.lst)
}
