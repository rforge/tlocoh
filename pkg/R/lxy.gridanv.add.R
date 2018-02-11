#' Add ancillary variable(s) to a LoCoH-xy object from a raster
#'
#' @param lxy \link{LoCoH-xy} object
#' @param band Numeric vector of which band(s) to read. If omitted, all bands will be read.
#' @param dtfn A two-column data frame containing a date and filename
#' @param fn Filename of a raster
#' @param anv.name The name(s) of the ancillary variable(s) that will be added to the LoCoH-xy object (one per band)
#' @param anv.desc A character vector of short descriptions of each ancillary variable
#' @param date.match How to match dates when \code{dtfn} is used: \code{'closest'} or \code{'before'}. See note.
#'
#' @note
#' This function will create new ancillary variables and fill it with the value of the pixel at each point.
#' The image should be in the same coordinate system as the points (the script however does not check this).
#' Any points that fall beyond the edges of the image will be given 'NA' values. If an ancillary variable with the
#' same name already exists, it will be overwritten.
#'
#' The raster image specified by \code{fn} must be in a format supported by the
#' \code{readGDAL} function in the rgdal package (run \code{gdalDrivers} for a list of supported formats).
#' Multi-band formats are supported. If the image has multiple bands, you can
#' specify which band(s) to read using the optional argument \code{band}. A
#' separate ancillary variable will be created for each band. You can provide
#' name(s) for the ancillary variable(s) with the \code{anv.name} parameter.
#'
#' You can also pass a time-series of images, and the function will use the image from the closest date
#' for each point. To do this, pass a two-column data frame as the argument \code{dtfn}. The first
#' column of \code{dtfn} must be a date (in one of the POSIXt date-time classes), and the second column
#' must be a valid raster filename associated with that date. If each image is a time composite, (e.g.,
#' two-week NDVI), the date should either be the mid-point or the beginning date. Two options for time
#' matching are available through the argument \code{date.match}. When \code{date.match='closest'}, the
#' image whose date is closest in time (before or after) will be used. When \code{date.match='before'},
#' it will pick the first image that comes before the location.
#'
#' @return A \link{LoCoH-xy} object 
#'
#' @export

lxy.gridanv.add <- function(lxy, band=NULL, dtfn=NULL, fn=NULL, anv.name=NULL, anv.desc=NULL, date.match=c("closest", "before")[1]) {

    if (!inherits(lxy, "locoh.lxy")) stop("lxy should be of class \"locoh.lxy\"")
    if (is.null(lxy[["pts"]])) stop("Old data structure detected, please update with lxy.repair()")
    if (!is.null(anv.name) && !is.null(band) && (length(band) != length(anv.name))) stop("Please provide an ancillary variable name (anv.name) for each band")
    if (is.null(fn) + is.null(dtfn) != 1) stop("fn OR dtfn is required (but not both)")
    if (is.null(fn) && length(fn) != 0) stop("fn must be of length 1. To grab values from multiple image by date, use dtfn")
    if (!date.match %in% c("closest", "before")) stop("Unknown value for date.match")
    
    if (!requireNamespace("rgdal")) stop("package rgdal is required for this function")

    ## Do some error checking on dtfn
    dtfn.err <- "dtfn should be a two-column data frame containing a date and filename"
    if (!requireNamespace("rgdal")) stop("package rgdal required to read raster image")
    if (!is.null(dtfn)) {
        if (!is.data.frame(dtfn)) stop(dtfn.err)
        if (ncol(dtfn) != 2) stop(dtfn.err)
        if (!inherits(dtfn[[1]], "POSIXt")) stop(dtfn.err)
        if (is.null(lxy[["pts"]][["dt"]])) stop("Time stamps not found in lxy, can't match by date")
        dtfn <- dtfn[order(dtfn[,1]), ]
    }

    ## Get the bounding box of the points, and expand by 0.01 for a little buffer
    ## we could optimize this a bit by creating the bounding box within the loop below, but I'm not ure
    ## the performance gained in reading small number of pixels would offset the time it takes to
    ## find the bounding box for subsets of points
    ##lxy.bbox <- lxy[["pts"]]@bbox * matrix(c(0.99, 1.01), ncol=2, nrow=2, byrow=TRUE)
    lxy.bbox <- lxy[["pts"]]@bbox
    lxy.bbox.buff.dist <- 0.02 * max(diff(lxy.bbox[1,]), diff(lxy.bbox[2,]))
    lxy.bbox.buff.mat <- lxy.bbox.buff.dist * matrix(data=c(-1,1,-1,1), nrow=2, ncol=2, byrow=T)
    lxy.bbox <- lxy.bbox + lxy.bbox.buff.mat

    ## Compile a list of points that go with each image    
    if (is.null(dtfn)) {
        ptdt.lst <- list(1:nrow(lxy[["pts"]]))
    } else {
        ## Match images to points by date
        
        ## First convert both the point time stamps and the image dates to numeric values, which 
        ## converts them to a common time zone
        lxy.dtint <- as.numeric(lxy[["pts"]][["dt"]])
        img.dtint <- as.numeric(dtfn[,1])
        
        if (date.match == "closest") {
            ## For each point in lxy, find the closest date
            idx <- as.numeric(FNN::get.knnx(data=img.dtint, query=lxy.dtint, k=1)$nn.index)
            
        } else if (date.match == "before") {
            idx <- findInterval(lxy.dtint, img.dtint)
        }
        
        ## Make these into a list
        ptdt.lst <- lapply(1:nrow(dtfn), function(i) which(idx==i))
        fn <- as.character(dtfn[,2])
    }

    ## Deal with a NULL value for the band argument
    if (is.null(band)) {
      ## We're supposed to use all the bands. Get the number of bands from the first imaage.
      ## We'll eheck that every other image has the same number of bands
      band <- 1:rgdal::GDALinfo(fn[1], silent=TRUE, returnStats=FALSE)[3]
      if (!is.null(anv.name) && (length(band) != length(anv.name))) stop("Length of 'anv.name' does not match number of bands")
    }

    ## Initialize anv.desc
    if (is.null(anv.desc)) anv.desc <- rep(NA, length(band))

    ## Create a blank matrix to store the results
    gridvals.mat <- matrix(NA, ncol=length(band), nrow=length(lxy[["pts"]]))

    ## Loop through the images(s), and for each set of points associated with that image grab the pixel value(s)
    use.pb <- length(ptdt.lst) > 2
    if (use.pb) pb <- txtProgressBar(min=0, max=length(ptdt.lst), style = 3)
    for (i in 1:length(ptdt.lst)) {
        if (use.pb) setTxtProgressBar(pb, i)
        if (length(ptdt.lst[[i]]) > 0) {
            if (!file.exists(fn[i])) stop(paste(fn[i], "not found"))
            
            ## Get the bounding box of the image, because we don't want to read the whole thing
            fn.info <- rgdal::GDALinfo(fn[i], silent=TRUE)
            fn.bbox <- matrix(data=c(fn.info[4], fn.info[4] + fn.info[2] * fn.info[6], fn.info[5], fn.info[5] + fn.info[1] * fn.info[7]), ncol=2, byrow=T, dimnames=list(c("x","y"), c("min","max")))
            if (length(band) > fn.info[3]) stop(paste(fn[i], "doesn't have enough bands"))
            
            ## Define the bounding box of the region of the image to read, making sure it isn't bigger than the image itself
            lxy.bbox.use <- matrix(0, ncol=2, nrow=2)
            lxy.bbox.use[,1] <- pmax(lxy.bbox[,1], fn.bbox[,1])
            lxy.bbox.use[,2] <- pmin(lxy.bbox[,2], fn.bbox[,2])

            ## Read in the image, just the part that overlap and just the bands
            img.sgdf <- tlocoh:::readpartgdal(fn[i], xlim=lxy.bbox.use[1,], ylim=lxy.bbox.use[2,], band=band, silent=TRUE, status=FALSE)
            
            ## Use the over function to grab the pixel values of each location,
            ## returning a data frame, which one attribute field for each band
            overlayvals.df <- sp::over(lxy[["pts"]][ptdt.lst[[i]], ], img.sgdf)

            ## Put the results in the holding frame
            ## as.matrix(data frame) preserves the column labels
            gridvals.mat[ptdt.lst[[i]], ] <- as.matrix(overlayvals.df)

        }
    }
    if (use.pb) close(pb)

    if (sum(is.na(gridvals.mat))> 0) warning(paste("Couldn't get a raster value for ", sum(is.na(gridvals.mat)), " locations. Check if the data and image(s) are in the same coordinate system.", sep=""))

    if (is.null(anv.name)) {
        ## Use the band names from the last image loaded
        anv.name.use <- names(overlayvals.df)
    } else {
        anv.name.use <- anv.name
    }

    ## Add the variables to the data frame of lxy$pts
    for (i in 1:length(anv.name.use)) {
        ## Add / replace column in lxy$pts
        lxy[["pts"]]@data[[anv.name.use[i]]] <- gridvals.mat[,i]
        lxy[["anv"]] <- unique(rbind(lxy[["anv"]], data.frame(anv=anv.name.use[i], desc=anv.desc[i], stringsAsFactors=FALSE)))
    }
    return(lxy)
}
