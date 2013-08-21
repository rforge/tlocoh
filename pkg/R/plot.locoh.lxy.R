#' Plot LoCoH-xy object
#'
#' Multi-purpose plotting function for a LoCoH-xy object
#'
#' @param x A \link{LoCoH-xy} object
#' @param id A vector of the id value(s) to plot
#' @param cex Character expansion factor for the points
#' @param show.start Whether to highlight the starting location (if time stamps are present) in green. T/F.
#' @param show.end Whether to highlight the end location (if time stamps are present) in red. T/F.
#' @param col A single value or vector of color values. Can also be 'auto' in which case the colors saved in \code{lxy} will be used (which are rainbow by default).
#' @param connect.dots Whether to draw line segments between consecutive locations. T/F.
#' @param overlay Whether to overlay the plots of all individuals in \code{lxy} on one pair of axes (map). T/F.
#' @param status Display status messages. T/F.
#' @param title The title to be displayed. Character. If NULL a title will be constructed.
#' @param title.show Whether to show the title. T/F.
#' @param axes.show Whether to show the axes. T/F.
#' @param axes.titles Whether to show axes titles. T/F.
#' @param axes.ticks Whether to show the tick marks and labels on the axes. T/F.
#' @param mar The plot margins. A four item numeric vector
#' @param mgp The distance away from the edge of the plot for the 1) label, 2) tick marks, and 3) axis line. A three-item numeric vector
#' @param lo.save Whether to save and reset the plot device margin settings (some wrapper functions that call this function don't want device settings reset). T/F.
#' @param panel.num A number or letter to display in the upper left hand corner of the plot when the plot will be used as part of a multi-frame graphic (as in publications). Character
#' @param panel.num.inside.plot Whether to display panel.num inside the plot area itself, as opposed to the title area. T/F
#' @param png.fn The path and name of the PNG file to create (instead of displaying in a plot window)
#' @param png.dir The directory for a PNG file (filename will be constructed automatically). Ignored if png.fn is passed
#' @param png.dir.make Whether to create png.dir if it doesn't exist. T/F
#' @param png.width The width of the PNG image
#' @param png.height The height of the PNG image
#' @param png.overwrite Whether to overwrite an existing PNG file if it exists. T/F
#' @param png.pointsize The pointsize (in pixels) for the PNG image, equivalent to the height or width of a character in pixels (increase to make labels appear larger)
#' @param tiff.fn The path and name of a GeoTIFF file (e.g., satellite image) that will be displayed in the background. See notes.
#' @param tiff.bands A vector of threee integers corresponding to the bands of the GeoTIFF image that will be mapped to the red, green and blue color guns respectively.
#' @param tiff.pct Whether or to convert the GeoTIFF to an indexed 256 color RGB image, which may speed up drawing. T/F.
#' @param tiff.buff A numeric buffer distance that the range of the plot will be expanded so the points are not right on the edge of the GeoTIFF.
#' @param tiff.fill.plot Whether to fill the entire plot area with the GeoTIFF. T/F.
#' @param layers The name(s) of layers in shp.csv to display in the background. Will be displayed using the symbology in shp.csv. Character vector or comma delimited string
#' @param shp.csv The path and filename of a csv file that contains information about shapefiles, including layer names, file, and symbology.
#' @param xlim The lower and upper limit of the x-axis, two-element numeric vector
#' @param ylim The lower and upper limit of the y-axis, two-element numeric vector
#' @param ... Additional parameters that will be passed to the \code{\link{plot}} function
#'
#' @export
#' @method plot locoh.lxy

plot.locoh.lxy <- function(x, id=NULL, cex=0.8, show.start=TRUE, show.end=TRUE, col=c("auto","gray80")[1], connect.dots=TRUE,
                           overlay=FALSE, status=TRUE, title=NULL, title.show=TRUE, 
                           axes.show=TRUE, axes.titles=axes.show, axes.ticks=axes.show,
                           mar=c(if (axes.titles || axes.ticks) 3.3 else 0.5, if (axes.titles || axes.ticks) 3.2 else 0.5, if (title.show) 3.2 else 0.5, 0.5), 
                           mgp=c(2, 0.7, 0), lo.save=TRUE, panel.num=NULL, panel.num.inside.plot=!title.show,
                           png.fn=NULL, png.dir=NULL, png.dir.make=TRUE, png.width=800, png.height=png.width, png.overwrite=TRUE, png.pointsize=12+(png.width-480)/80, 
                           tiff.fn=NULL, tiff.bands=c(3,2,1), tiff.pct=FALSE, tiff.buff=500, tiff.fill.plot=TRUE, 
                           layers=NULL, shp.csv=NULL, xlim=NULL, ylim=NULL, ...) {

    ## taken out: figs.per.page (wasn't being used)
    
    ## creates a plot (map) of lxy
    ## if there are multiple individuals, if overlay=T it will overlay them, otherwise will display in separate plots
    ## tiff.fn is the name of a geotiff, which will be displayed in the background. Presumed to be a 3-band image, pre-stretched, with bands 123 corresponding to RGB
    ## if tiff.pct = TRUE, will convert the TIFF to a 256 indexed color image (might have quicker drawing time particularly if several plots)
    ## tiff.buff is an additional buffer (in map units) on all sides
    ## layers is the name(s) (can be comma separated) of a shapefile layer specified in shp.csv. 
    ## NEW: layers can also be a list of lists, see comments in shp.layers
    ## If provided, these layers will be displayed in the background according to the symbology listed in shp.csv
    
    lxy <- x; rm(x)
    if (!inherits(lxy, "locoh.lxy")) stop("lxy should be of class \"locoh.lxy\"")
    if (!require(sp)) stop("package sp required")
    
    if (is.null(id)) {
        id <- levels(lxy[["pts"]][["id"]])
    } else {
        if (FALSE %in% (id %in% levels(lxy[["pts"]][["id"]]))) stop("id not found")
    }
    
    ## Prepare the tiff file
    if (is.null(tiff.fn)) {
        range.expand.tiff <- 0    
    } else {
        if (!require(rgdal)) stop("package rgdal required to display a tiff in the background")
        if (!file.exists(tiff.fn)) stop(paste(tiff.fn, "not found"))
        range.expand.tiff <- tiff.buff
        tiff.sgdf <- NULL
        if (length(tiff.bands) > 3) stop("tiff.bands can not be longer than 3")
    }

    ## Prepare GIS features
    gis.features <- if (is.null(layers)) list() else shp.layers(layers, shp.csv=shp.csv)
    
    tick.show <- if (axes.ticks) "s" else "n"
    
    ## Create a vector to hold the names of png files created, and create the output folder if needed
    if (!is.null(png.dir)) {
        if (!file.exists(png.dir)) {
            if (png.dir.make) {
                dir.made <- dir.create(png.dir)
                if (!dir.made) stop(paste("Couldn't make output folder ", png.dir, sep=""))            
            } else {
                stop(paste(png.dir, " doesn't exist and png.dir.make is False, so can not continue."))
            }
        }
        first.png.made <- FALSE
        pngs.made <- list()
    }
    
    if (length(id) > 1 && !overlay) {
        if (lo.save) {
            opar <- par(mfrow = n2mfrow(length(id)), mar=mar, mgp=mgp)
            on.exit(par(opar))
        }
        multiple.plots <- TRUE
    } else {
        ## There will only be a single plot (but might have several individuals overlaid on it)
        multiple.plots <- FALSE

        ## Create png device if needed
        if (!is.null(png.dir)) {
            if (is.null(png.fn)) {
                fn <- file.path(png.dir, paste(strTrim(paste(unlist(lxy[["comment"]][id]), collapse = ".", sep = "")), ".xy.png", sep=""))
            } else {
                fn <- png.fn
            }
            if (file.exists(fn) && !png.overwrite) stop(paste(fn, "exists"))

            ## Open PNG device
            png(filename=fn, height=png.height, width=png.width, bg="white", pointsize=png.pointsize)
            pngs.made <- c(pngs.made, list(list(fn=fn, dim=c(png.width, png.height))))
        }

        ## Save layout if needed
        if (lo.save) {
            opar <- par(c("mar", "mgp"))
            on.exit(par(opar))
        }
        
        ## Set the margins
        par(mar=mar, mgp=mgp)

        ## Compute xlim and ylim
        range.expand <- matrix(0, nrow=1, ncol=2)
        #if (!is.null(lxy$aux)) {
        #    for (i in 1:length(lxy$aux)) {
        #        for (j in 1:length(lxy$aux[[i]])) {
        #            if (lxy$aux[[i]][[j]][["type"]] == "range.expand") {
        #                range.expand <- rbind(range.expand, lxy$aux[[i]][[j]][["data"]]) 
        #            }
        #        }
        #    }
        #}
        range.expand <- c(min(c(range.expand[,1], -range.expand.tiff)), max(c(range.expand[,2], range.expand.tiff)))
        #xlim <- range(lxy$xys[,1]) + range.expand
        #ylim <- range(lxy$xys[,2]) + range.expand
        
        xlim.use <- if (is.null(xlim)) range(coordinates(lxy[["pts"]])[,1]) + range.expand else xlim
        ylim.use <- if (is.null(ylim)) range(coordinates(lxy[["pts"]])[,2]) + range.expand else ylim

        ## Create an empty plot
        plot(NULL, xlim=xlim.use, ylim=ylim.use, asp=1, axes=axes.show, xaxt=tick.show, yaxt=tick.show, 
             xlab=if (axes.titles) colnames(coordinates(lxy[["pts"]]))[1] else "", 
             ylab=if (axes.titles) colnames(coordinates(lxy[["pts"]]))[2] else "", ...)

        ## Display the tiff
        if (!is.null(tiff.fn)) {
            if (tiff.fill.plot) {
                half.plot.size <- c(-0.5, 0.5) * max(diff(xlim.use), diff(ylim.use))
                rx.tiff <- half.plot.size + mean(xlim.use)
                ry.tiff <- half.plot.size + mean(ylim.use)
            } else {
                rx.tiff <- xlim.use
                ry.tiff <- ylim.use
            }
            
            tiff.sgdf <- readpartgdal(tiff.fn, xlim=rx.tiff, ylim=ry.tiff, band=tiff.bands, silent=TRUE)
            if (tiff.pct) {
                tiff.sgdf.cols <- SGDF2PCT(tiff.sgdf, adjust.bands=FALSE)
                tiff.sgdf$idx <- tiff.sgdf.cols$idx
                image(tiff.sgdf, "idx", col=tiff.sgdf.cols$ct, add=TRUE)
            } else {
                image(tiff.sgdf, red=1, green=2, blue=3, add=TRUE)
            }
        }
        
        ## Lay down the polygons
        for (featname in names(gis.features)[sapply(gis.features, function(x) x[["type"]]=="polygon")]) {
            with(gis.features[[featname]], plot(sdf, lty=lty, pch=pch, cex=cex, col=col, border=if (is.na(border)) NULL else border, lwd=lwd, add=TRUE))
        }
                
    }

    ## Loop through all the individuals
    for (i in 1:length(id)) {
        idVal <- id[i]
        idVal.idx <- which(lxy[["pts"]][["id"]] == idVal)
        
        if (identical(col, "auto")) {
            if (is.null(lxy[["pts"]][["col"]])) {
                if (length(id) > 1 && overlay) {
                    col.use <- palette()[i]
                } else {
                    col.use <- topo.colors(length(idVal.idx))
                }
            } else {
                col.use <- as.character(lxy[["pts"]][["col"]][idVal.idx])
            }
        } else {
            col.use <- col
        }

        if (multiple.plots) {

            ## Create png device for this plot if needed
            if (!is.null(png.dir)) {
                if (first.png.made) dev.off()
                fn <- file.path(png.dir, paste(lxy[["comment"]][[idVal]], ".xys.png", sep=""))
                if (file.exists(fn) && !png.overwrite) stop(paste(fn, "exists"))
    
                ## Open PNG device
                png(filename=fn, height=png.height, width=png.width, bg="white", pointsize=png.pointsize)
                first.png.made <- TRUE
                pngs.made <- c(pngs.made, list(list(fn=fn, dim=c(png.width, png.height))))
            }
            
            ## Set the margins
            par(mar=mar, mgp=mgp)

            ## Compute xlim and ylim
            range.expand <- matrix(0, nrow=1, ncol=2)
            #if (!is.null(lxy$aux[[idVal]])) {
            #    for (j in 1:length(lxy$aux[[idVal]])) {
            #        if (lxy$aux[[idVal]][[j]][["type"]] == "range.expand") {
            #            range.expand <- lxy$aux[[idVal]][[j]][["data"]] 
            #        }
            #    }
            #} 
            range.expand <- c(min(c(range.expand[,1], -range.expand.tiff)), max(c(range.expand[,2], range.expand.tiff)))
            #xlim <- range(lxy$xys[,1]) + range.expand
            #ylim <- range(lxy$xys[,2]) + range.expand

            xlim.use <- if (is.null(xlim)) range(coordinates(lxy[["pts"]])[,1]) + range.expand else xlim
            ylim.use <- if (is.null(ylim)) range(coordinates(lxy[["pts"]])[,2]) + range.expand else ylim
            
            ## Create an empty plot
            plot(NULL, xlim=xlim.use, ylim=ylim.use, asp = 1, axes=axes.show, xaxt=tick.show, yaxt=tick.show, 
                 xlab=if (axes.titles) colnames(coordinates(lxy[["pts"]]))[1] else "", 
                 ylab=if (axes.titles) colnames(coordinates(lxy[["pts"]]))[2] else "", ...)
            
            if (!is.null(tiff.fn)) {
                #print("lets get the range"); browser()
                if (tiff.fill.plot) {
                    half.plot.size <- c(-0.5, 0.5) * max(diff(xlim.use), diff(ylim.use))
                    rx.tiff <- half.plot.size + mean(xlim.use)
                    ry.tiff <- half.plot.size + mean(ylim.use)
                } else {
                    rx.tiff <- xlim.use
                    ry.tiff <- ylim.use
                }
                
                tiff.sgdf <- readpartgdal(tiff.fn, xlim=rx.tiff, ylim=ry.tiff, band=tiff.bands, silent=TRUE)
                if (tiff.pct) {
                    tiff.sgdf.cols <- SGDF2PCT(tiff.sgdf, adjust.bands=FALSE)
                    tiff.sgdf$idx <- tiff.sgdf.cols$idx
                    image(tiff.sgdf, "idx", col=tiff.sgdf.cols$ct, add=TRUE)
                } else {
                    image(tiff.sgdf, red=1, green=2, blue=3, add=TRUE)
                }
            }        
            
        }

        #points(lxy$xys[idVal.idx,], col="gray80", type= if (connect.dots) "l" else "n", main=lxy$comment, asp=1)
        #points(lxy$xys[idVal.idx,], type="p", col=col.use, main=lxy$comment, pch=20, asp=1, cex=cex)
        
        ## Add the connecting lines if needed, and then the points themselves
        points(coordinates(lxy[["pts"]])[idVal.idx,], col="gray80", type= if (connect.dots) "l" else "n", asp=1)
        points(coordinates(lxy[["pts"]])[idVal.idx,], type="p", col=col.use, pch=20, asp=1, cex=cex)
        
        if (show.start) points(coordinates(lxy[["pts"]])[idVal.idx[1],], col="green", pch=19, cex=1.5)
        if (show.end) points(coordinates(lxy[["pts"]])[idVal.idx[length(idVal.idx)],], col="red", pch=19, cex=1.5)
        if ((nlevels(lxy[["pts"]][["id"]]) == 1 || !overlay) && title.show) title(main=if (is.null(title)) lxy[["comment"]][[idVal]] else title)

        ## Add panel.num
        if (!is.null(panel.num)) {
            if (panel.num.inside.plot) {
                text(x=par("usr")[1], y=par("usr")[4], labels=panel.num, cex=2, adj=c(-0.3,1.2), font=2)
            } else {
                mar.old <- par("mar")
                par(mar=c(0, 0.3, 0.2, 0))
                title(main=panel.num, adj=0, cex.main=2, line=-1.5)
                par(mar=mar.old)
            }
        }
        
        ## Plot the gis layers containing lines and points
        for (featname in names(gis.features)[sapply(gis.features, function(x) x[["type"]] %in% c("point", "line"))]) {
            with(gis.features[[featname]], plot(sdf, lty=lty, pch=pch, cex=cex, col=col, border=if (is.na(border)) NULL else border, lwd=lwd, add=TRUE))
        }
        
    }  #for (i in 1:nlevels(lxy[["id"]]))
    
    #if (nlevels(lxy[["pts"]][["id"]]) > 1 && overlay && title.show) title(paste(unlist(lxy[["comment"]]), collapse = "\n", sep = ""))
    if (length(id) > 1 && overlay && title.show) title(paste(unlist(lxy[["comment"]][id]), collapse = "\n", sep = ""))
    
    if (!is.null(png.dir)) {
        dev.off()
        if (status) {
            cat("png file(s) made: \n")
            print(sapply(pngs.made, function(x) x[["fn"]]))
        }
        return(invisible(pngs.made))
    }
    
}
