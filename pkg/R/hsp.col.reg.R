hsp.col.reg <- function(x,y, regions, col="gray50", return.reg.num.only=FALSE) {

    ## Returns a single column vector of colors based on which polygon points fall in
    ## regions is a list of unnamed elements. Each element of this list is a two-element sub-list with items:
    ##      $poly.pts is a two-column data frame of xy coordinates the define a polygon in xy space
    ##      $col is a color object. All points that fall within the polygon defined by $xys will be assigned this color

    ## col is a single color value or a vector of color values (same length as x & y) for the default or incoming colors for all points
    ## only points within defined regions will have their color changed to that of the region
    ## return.reg.num.only return only the region number, not its color code
    
    if (length(x) != length(y)) stop("length of x and y must be equal")
    
    ## Define the default values (for those points that don't fall in any region)
    if (return.reg.num.only) {
        res <- rep(0, times=length(x)) 
    } else {
        ## Expand col if needed
        if (length(col)==1) {
            res <- rep(col, times=length(x)) 
        } else {
            if (length(col) != length(x)) stop("Length of col must be equal to the number of points")
            res <- col
        }
    
        ## Get a vector of the colors of the regions
        reg.cols <- sapply(regions, function(x) x$col)
        ## spc.reg.cols[!spc.reg.col.view] <- NA
    
    }
    
    for (i in 1:length(regions)) {
        poly.pts <- regions[[i]][["poly.pts"]]
        poly.pts.x.range <- range(poly.pts[,1])
        poly.pts.y.range <- range(poly.pts[,2])
        
        pts.idx.in.this.poly.short.list <- which(x >= poly.pts.x.range[1] & x <= poly.pts.x.range[2] & y >= poly.pts.y.range[1] & y <= poly.pts.y.range[2])                                                          
        pts.idx.in.this.poly <- pts.idx.in.this.poly.short.list[sapply(pts.idx.in.this.poly.short.list, function(i) point.in.polygon(point.x=x[i], point.y=y[i], pol.x=poly.pts[,1], pol.y=poly.pts[,2]))!=0]
        res[pts.idx.in.this.poly] <- if (return.reg.num.only) i else reg.cols[i]
    }
    
    return(res)
}
