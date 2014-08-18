readpartgdal <- function(fn, xlim = NULL, ylim = NULL, silent=TRUE, status=TRUE, band=NULL, ...) {
    ## Reads part of an image file defined by xlim and ylim (two-element numeric vectors)
    ## adapted from function by Michael Sumner
    ## https://stat.ethz.ch/pipermail/r-sig-geo/2010-July/008854.html
    
  	if (!requireNamespace("rgdal")) stop("package 'rgdal' required")
  	if (status) cat("   reading ", fn, "...\n", sep="")
  	info <- GDALinfo(fn, silent=silent)
  	offs <- info[c("ll.x", "ll.y")]
  	scl <- info[c("res.x", "res.y")]
  	dimn <- info[c("columns", "rows")]
  	numBands <- info[["bands"]]
  	
  	if (numBands==1) {
 	     band <- 1
  	} else {
    	if (max(band) > numBands) {
        cat(cw(paste("Oh dear...", fn, " only has ", numBands, " bands, and you want to display bands ", paste(band, collapse=",", sep=""), ". That won't work. Change the value of tiff.bands.", sep=""), final.cr=T, indent=3, exdent=3))
       	return(NULL)
    	}
  	}
  	
  	## brain dead, but easy
  	xs <- seq(offs[1], by = scl[1], length = dimn[1]) + scl[1]/2
  	ys <- seq(offs[2], by = scl[2], length = dimn[2]) + scl[2]/2
  
  	if (!is.null(xlim)) {
  		if (!is.numeric(xlim)) stop("xlim must be numeric")
  		if (!length(xlim) == 2) stop("xlim must be of length 2")
  		if (!diff(xlim) > 0) stop("xlim[1] must be less than xlim[2]")
  		xind <- which(xs >= xlim[1] & xs <= xlim[2])
  	}
  
  	if (!is.null(ylim)) {
  		if (!is.numeric(ylim)) stop("ylim must be numeric")
  		if (!length(ylim) == 2) stop("ylim must be of length 2")
  		if (!diff(ylim) > 0) stop("ylim[1] must be less than ylim[2]")
  		yind <- which(ys >= ylim[1] & ys <= ylim[2])
  	}
  	## probably need a sign check for info["ysign"]
  	## reverse for y/x order in readGDAL
  	
  	if (length(xind)==0 || length(yind)==0) {
      cat(cw(paste("Oh dear!\n", fn, " falls outside the plot area", sep=""), final.cr=T, indent=3, exdent=3))
     	return(NULL)
    } else {  	
  	  rgdal.offset <- rev(c(min(xind), dimn[2] - max(yind)))
  	  rgdal.dim <- rev(c(length(xind), length(yind)))
      return(readGDAL(fn, offset = rgdal.offset, region.dim = rgdal.dim, silent=silent, band=band, ...))
    }
}
