#' Converts a SpatialPolygonsDataFrame to a raster
#' This presumes the SPDF contains isopleths ordered from lowest level to highest
#'  

iso2raster <- function(polys, ext=NULL, cell.size=NULL, ll.round=TRUE, status=TRUE) {

    if (!require(raster)) stop("raster package required")
    if (!require(sp)) stop("raster package required")

    if (!is(polys, "SpatialPolygonsDataFrame")) stop("polys must be class SpatialPolygonsDataFrame")
    
    if (is.null(ext)) {
        ext <- extent(polys)
    } else {
        print("need to run checks on ext");browser()        
    }
    
    # Define the cell size
    if (is.null(cell.size)) {
        target.num <- 100
        cell.size = signif(min((ext@xmax - ext@xmin) / target.num, (ext@ymax - ext@ymin) / target.num), digits = 2)
        ## cat("cell.size=", cell.size, "\n")
    } 
    
    # Next, in order to get nice neat coordinates for our cells, we'll round xmin and ymin
    # *down* to the nearest multiple of cell.size, and round xmax and ymax *up* to the nearest
    # multiple of cell.size.
    if (ll.round) {
        ext@xmin <- floor(ext@xmin / cell.size) * cell.size
        ext@ymin <- floor(ext@ymin / cell.size) * cell.size
        ext@xmax <- ceiling(ext@xmax / cell.size) * cell.size
        ext@ymax <- ceiling(ext@ymax / cell.size) * cell.size
    }
    
    # Now we can compute the number of rows and columns needed.
    nrow <- (ext@ymax - ext@ymin) / cell.size
    ncol <- (ext@xmax - ext@xmin) / cell.size
    ## cat("nrow=", nrow, ", ncol=", ncol, "\n")
    
    # Create a blank raster, the cell values will be NA (missing data).
    rast.iso <- raster(ext, ncol=ncol, nrow=nrow, crs=polys@proj4string)
    rast.mask <- rast.iso
    rast.blank <- rast.iso

    ## Assign starting values
    rast.iso[] <- 0
    rast.mask[] <- 1
    
    ## Keep track of proportion of total points that have been added to rast.iso
    ptp.total <- 0
    if (status) pb <- txtProgressBar(min = 0, max = nrow(polys), style = 3)
    
    print("lets look at this"); browser()
    
    for (i in 1:nrow(polys)) {
    
      if (status) setTxtProgressBar(pb, i)  
      #print("ok ready to build this layer");browser()

      this.iso.rast <- rasterize(polys[i,], rast.blank, field=1, fun='count', background=0, silent=TRUE)
      cat("lets look at ths iso for level", i, "\n")
      print(this.iso.rast)
      
      if (i==3) browser()
      
      ## Mask out any cells that already have values                  
      this.iso.rast <- this.iso.rast * rast.mask
      
      ## Update mask for next iteration of loop
      rast.mask <- rast.mask - this.iso.rast
      
      ## Get the number of cells not masked out by previous isopleths
      this.iso.sum <- cellStats(this.iso.rast, stat="sum")
      
      if (this.iso.sum==0) {print('were screwed');browser()}
      
      ## Compute the total of proportion of total points that should be distributed within the remaining cells
      this.iso.level <- polys@data[i, "ptp"]
      ptp.this.layer <- this.iso.level - ptp.total
      
      ## Update ptp for next pass
      ptp.total <- this.iso.level

      ## Assign values to the non-zero cells
      ## PROBLEM IS HERE, THIS CAN BE INFINITE IF NO NEW CELLS INVOLVED
      this.iso.rast <- this.iso.rast * (ptp.this.layer / this.iso.sum)
      
      ## Add values to rast.iso
      rast.iso <- rast.iso + this.iso.rast
      
      cat(i, ". rast.iso looks like \n")
      print(rast.iso)
    
    }
    if (status) close(pb)
    
    return(rast.iso)
    
    #print("done with that, lets look");browser()
    
    #sbok.rast.ptcount <- rasterize(pol.spdf, sbok.rast.blank, field=1, fun='count')
    
    #plot(sbok.rast.ptcount)
    #summary(sbok.rast.ptcount)
    
    ## Need a check the iso represents ptp, not something else



}