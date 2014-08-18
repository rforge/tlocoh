#' Create a Quicktime animation from a LoCoH-hullset object
#'
#' @param lhs A \code{\link{LoCoH-hullset}} object
#' @param id The id value(s) to be on the plot
#' @param k The k value of hullsets to export
#' @param r The r value of hullsets to export
#' @param a The a value of hullsets to export
#' @param s The s value of hullsets to export
#' @param hs.names The name(s) of saved hullsets to export
#' @param all.ids.at.once Display all the individual ids simultaneously. If False, an animation will be created for each id. T/F.
#' @param all.ids.col.unique Whether to use unique colors for each individual when plotting multiple individuals simultaneously, T/F
#' @param all.ids.col A named list of color values; the element names must match the name(s) of the ids
#' @param all.ids.legend Where to place a legend showing the color of each id (for an animation showing the movement of multiple individuals simultaneously): 'bottomright', 'bottom', 'bottomleft', 'left', 'topleft', 'top', 'topright', 'right', or 'center'. May also be \code{NULL}, in which case the legend will not be displayed. Ignored if \code{ids.legend.bln=FALSE} or \code{all.ids.col.unique = FALSE}.
#' @param all.ids.legend.cex The character expansion factor for the id legend. See parameter \code{all.ids.legend} above.
#' @param dt.start The starting date-time. An object of class POSIXt or one that can be coerced to POSIXt. If NULL, the earliest date-time in the series will be used.
#' @param dt.end The ending date-time. An object of class POSIXt or one that can be coerced to POSIXt. If NULL, the last date-time in the series will be used.
#' @param frame.method How each frame should be defined temporally, "time" - each frame represents a fixed amount of time, "location" each frame is a point in the series
#' @param frame.rtd The real-time duration of each frame (in seconds). If "auto" (default), the lowest median sampling frequency will be used
#' @param xlim A two-element numeric vector for the range of the x-axis (in map units)
#' @param ylim A two-element numeric vector for the range of the y-axis (in map units)
#' @param dt.label Add a label for the date of the frame
#' @param dt.label.col A color value/name for the date label (ignored if \code{dt.label=FALSE})
#' @param dt.label.x The x-coordinate for the date label (ignored if \code{dt.label=FALSE})
#' @param dt.label.y The y-coordinate for the date label (ignored if \code{dt.label=FALSE})
#' @param dt.label.col.bg The background color for the date label (\code{NA} for transparent background, ignored if \code{dt.label=FALSE})
#' @param title A title for the map
#' @param title.show Whether to show the title. T/F.
#' @param axes.show Whether to show the axes (ticks, labels and titles). Can be over-written by \code{axes.ticks} and \code{axes.titles}. T/F.
#' @param axes.ticks Whether to show the tick marks and labels on the axes. T/F.
#' @param axes.titles Whether to show axes titles. T/F.
#' @param mar.map Margin settings for the map, see \code{\link{par}}
#' @param mgp.map Locations of the axes elements for the map, see \code{\link{par}}
#' @param tz.local The name of the time zone of the study area
#' @param tz.local.check Check whether tz.local is a valid timezone name (not implemented) T/F.
#' @param col.by.hour.of.day Whether to color the active point by the hour of day (i.e., dark colors at night, orange for day time locations). T/F
#' @param col.hod A vector of 24 color values used to symbolize the color of the active point by the hour of day. Ignored if \code{col.by.hour.of.day = FALSE}.
#' @param rtd.center Whether to center the start and end time of the first frame around the time stamp of the first location, to help ensure that each frame has only 
#' one hull in it. T/F.
#' @param clean.multihull.frames A numeric value that determines whether and how much a hull can be time shifted to avoid having two active hulls in the same frame. Pass \code{0} to disable cleaning multi-hull frames. See details.
#' @param col.hull.active The color of the active hull
#' @param col.hull.alpha A number 0..255 for the alpha value for the hull color (semi-transparency); 0=completely transparent, 255=opaque
#' @param width The width of each frame in pixels (if \code{screen.test=FALSE}) or inches (if \code{screen.test=TRUE}).
#' @param height The height of each frame in pixels (if \code{screen.test=FALSE}) or inches (if \code{screen.test=TRUE}).
#' @param max.frames The maximum number of frames to produce.
#' @param png.pointsize The pointsize (in pixels) for the PNG image, equivalent to the height or width of a character in pixels (increase to make labels appear larger)
#' @param screen.test Create up to three sample frame(s) on the screen (instead of PNG files)
#' @param tmp.dir A directory where temporary PNG files for each frame will be created, character.
#' @param tmp.files.delete Delete the temporary PNG files when done, T/F
#' @param prompt.continue Whether to present a summary of the encoding settings and get user confirmation before continuing, T/F
#' @param fn.mov The path and filename of output Quicktime *.mov file. If NULL a filename will be automatically generated
#' @param fn.mov.dir The directory where the animation will be saved (ignored if a value for \code{fn.mov} is passed)
#' @param fn.mov.exists What to do if the animation file already exists: "auto.increment", "overwrite", "stop", or "ask". 
#' @param duration The desired duration of the animation (in seconds)
#' @param fps A numeric value for frames per second 
#' @param skip Output every nth frame. To include every frame set skip=1. Integer.
#' @param ffmpeg The name of the ffmpeg file. See notes.
#' @param create.mov Whether to actually create the mov file. Set to FALSE preview a few frames without actually encoding them.
#' @param info.only Only return info 
#' @param shp.csv The path and filename of a csv file that contains information about shapefiles, including layer names, file, and symbology.
#' @param layers The name(s) of layers in shp.csv to display in the background. Will be displayed using the symbology in shp.csv. Character vector or comma delimited string.
#' @param bg2png Save the plot background elements as a static raster image (to improve speed), ignored if \code{screen.test=TRUE}
#' @param crop.layers.to.extent Whether to crop the shapefile layers to the view extent (may speed up drawing time)
#' @param date.bar The height of the lower section of the plot to devote to the time bar, in inches. To hide the time bar completely, set \code{date.bar=0}.
#' @param date.bar.bins The target number of bins (tick marks + 1) on the time bar (integer)
#' @param col.db A single color value for the date bar axes / tick labels, character
#' @param cex.axis.db Character expansion factor for the labels on the date bar axis.
#' @param beep Beep when one, T/F
#' @param report.time Show the time taken when done, T/F
#' @param status Show progress bar and status messages 

#' @param tiff.fn The path and name of a GeoTIFF file (e.g., satellite image) that will be displayed in the background. See notes.
#' @param tiff.pct Whether or to convert the GeoTIFF to an indexed 256 color RGB image, which may speed up drawing. T/F.
#' @param tiff.bands A vector of threee integers corresponding to the bands of the GeoTIFF image that will be mapped to the red, green and blue color guns respectively.
#' @param tiff.buff A numeric buffer distance that the range of the plot will be expanded so the points are not right on the edge of the GeoTIFF.
#' @param tiff.fill.plot Whether to fill the entire plot area with the GeoTIFF. T/F.
#'
#' @note To create the animation, two and only two of the following parameters must be passed: \code{duration}, \code{fps}, and \code{skip}. 
#' The third parameter will be computed. To include every frame, pass \code{fps}, set \code{skip=1}, and leave \code{duration} out.
#'
#' Larger values for \code{fps} will result in the animation running 'faster'. Values between 10 and 20 often work well; beyond 30 fps the eye can't keep up with the motion
#' Note if you pass values for \code{fps} and \code{duration}, an appropriate value for \code{skip} will be computed but 
#' the final duration of the animation may not be exactly equal to \code{duration} because only interger values of \code{skip} are allowed.
#'
#' One will normally want to run the function a few times without actually encoding to tweak the frame layout (e.g., where the date label and legend appear).
#' To see what a frame in the output will *approximately* look like, set \code{screen.test=TRUE}. Once the screen sample looks good, next set \code{max.frames=3}, 
#' \code{tmp.dir="."} (or another folder), \code{create.mov=FALSE}, and \code{tmp.files.delete=FALSE}. This will generate a few sample frames in PNG files 
#' and not delete them so you can inspect them. Once these look good, create the full animation by setting \code{create.mov=TRUE}. 
#'
#' If frame.method is 'auto', the script will use time-based frames when multiple individuals are being animated simultaneously, and location-based frames otherwise.
#'
#' \code{duration} (the duration of the animation in seconds) should not be confused with \code{frame.rtd} which is the real-time duration 
#' of a single frame in seconds (e.g., \code{frame.rtd=3600} means each frame will represent 1 hour).
#'
#' Passing a positive value for \code{clean.multihull.frames} (default value is 4) enables time-shifting hulls if needed to prevent two hulls appearing in one frame and 
#' no hulls in the previous or next frame (which produces jerky playback when animated). The maximum amount of time a hull can be shifted is calculated by 
#' \code{frame.rtd / clean.multihull.frames}. Thus for example if \code{frame.rtd=7200} (two hours), and \code{clean.multihull.frames=4}, then if a frame has two hulls appearing in it
#' and the parent point of one of those hulls lies within 30 minutes (2 hours / 4) of the beginning or end of the frame, \emph{and} there is no hull in the adjacent frame, that
#' hull will be moved to the earlier / later frame. To disable this effect, set \code{clean.multihull.frames = 0}.
#' 
#' If \code{date.bar} is too small or too large, you might get a 'margins too large' error. Try values around 1, or 
#' hide the date bar completely by setting \code{date.bar=0}.
#'
#' The output animation is encoded in QuickTime format. The Quicktime file is encoded using the 'animation' codec, a lossless format that 'scrubs' 
#' well (i.e., you can drag the scroll bar 
#' to view frame by frame). This requires installing the open source encoding program ffmpeg. ffmpeg is a command line program that 
#' Linux and Windows users can download from http://ffmpeg.org/download.html.
#' Windows users should save the ffmpeg.exe file to the working directory or a directory on the path environment variable (e.g., c:\\windows).
#' Mac users can download ffmpegX from http://ffmpegx.com/download.html but this has not been tested (pass name to \code{ffmpeg}).
#'
#' If ffmpeg is not available, you can still use this function to generate the individual frames and then use another utility (e.g., ImageMagick, Quicktime Pro) 
#' to combine the frames into a video file. For best results use a 'lossless' compression method in the encoding program.
#' To create the individual frames only, set \code{tmp.dir="."} (the working directory) and \code{tmp.files.delete=FALSE}. 
#'
#' If \code{fn.mov.exists = "auto.increment"}, a two-digit number will be appended to the *.mov filename to avoid overwriting an existing file
#'
#' @return A list with information about each *.mov file created. Each element of the list is another list with two 
#' elements: \code{fn} (the full filename) and \code{dim} (a two-element numeric vector with the frame width and height). If no *.mov file(s) were
#' created, returns NULL. 
#'
#' @export


lhs.exp.mov <- function(lhs, id=NULL, k=NULL, r=NULL, a=NULL, s=NULL, hs.names = NULL,
                        all.ids.at.once=TRUE, all.ids.col.unique=all.ids.at.once, all.ids.col=NULL, 
                        all.ids.legend=c("bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", "center")[5],
                        all.ids.legend.cex=0.8, dt.start=NULL, dt.end=NULL, frame.method=c("auto", "time", "location")[1], frame.rtd="auto", 
                        rtd.center=TRUE, clean.multihull.frames=4, xlim=NULL, ylim=NULL,
                        dt.label=TRUE, dt.label.col="black", dt.label.col.bg=NA, dt.label.x=NULL, dt.label.y=NULL,
                        title=NULL, title.show=TRUE,
                        axes.show=TRUE, axes.ticks=axes.show, axes.titles=FALSE, 
                        mar.map=c(0.7 + (if (axes.ticks) 0.9 else 0) + (if (axes.titles) 1.3 else 0),
                                  0.5 + (if (axes.ticks) 0.9 else 0) + (if (axes.titles) 1.3 else 0), 
                                  if (title.show) 2.1 else 0.5, 
                                  0.5), 
                        mgp.map=c(0.4 + if (axes.ticks) 1.2 else 0, 0.4, 0),
                        col.hull.active="red", col.hull.alpha=255,
                        tz.local=NULL, tz.local.check=TRUE,
                        col.by.hour.of.day=FALSE, col.hod=colorRampPalette(colors()[c(24,30,553,121,26,121,553,30,24)])(24),
                        width=if (screen.test) 7 else 608, height=NULL, max.frames=NULL, png.pointsize=16+(width-480)/80, 
                        screen.test=FALSE, tmp.dir=NULL, tmp.files.delete=TRUE, prompt.continue=TRUE,
                        fn.mov=NULL, fn.mov.dir=getwd(), fn.mov.exists=c("auto.increment", "overwrite", "stop", "ask")[1], 
                        duration=NULL, fps=NULL, skip=NULL, ffmpeg="ffmpeg.exe", create.mov=TRUE, info.only=TRUE, 
                        shp.csv=NULL, layers=NULL, tiff.fn=NULL, tiff.pct=FALSE, tiff.bands=c(4,3,2), tiff.buff=500, tiff.fill.plot=TRUE,
                        bg2png=!is.null(layers), crop.layers.to.extent=TRUE,
                        date.bar=0.85, date.bar.bins=12, col.db="darkblue", cex.axis.db=0.7, 
                        beep=FALSE, report.time=TRUE, status=TRUE) {

    
    ## info.only (not implemented), will *not* make the mov, only return a list of the filename, width, and height, 

    ## col.hour.of.day is NULL or a 24 color values for each hour of the day
    ## tz.local can be NULL or a named time zone. If not null, dt will be converted to local time

    ## This will take an lxy object and export it as mov file
    if (!inherits(lhs, "locoh.lhs")) stop("lhs should be of class \"locoh.lhs\"")
    if (bg2png) if(!requireNamespace("png")) stop("Package png required for bg2png")

    ## Make sure tiff.fn exists, check tiff.bands
    if (!is.null(tiff.fn)) {
        if (!requireNamespace("rgdal")) stop("package rgdal required to display a tiff in the background")
        if (!file.exists(tiff.fn)) stop(paste(tiff.fn, "not found"))
        if (length(tiff.bands) > 3) stop("tiff.bands can not be longer than 3")
        tiff.info <- GDALinfo(tiff.fn, silent=TRUE)
        if (max(tiff.bands) > tiff.info[3]) stop(paste("Invalid value for tiff.bands. Only ", tiff.info[3], " band(s) in ", tiff.fn, "."))
        tiff.sgdf <- NULL
    }

    if (is.null(id) && is.null(r) && is.null(k) && is.null(a) && is.null(s) && is.null(hs.names)) {
        hs <- lhs
    } else {    
        for (str.param in c("k","a","r","s")) assign(str.param, vectorize.parameter(get(str.param)))
        hs <- lhs.select(lhs, id=id, r=r, k=k, a=a, s=s, hs.names=hs.names)
    }
    if (length(hs)==0) stop("No hullsets found matching those criteria")
     
    if (is.null(hs[[1]][["pts"]][["dt"]])) stop("Can't animate without date values")

    #if (is.null(id)) {
    #    id <- sapply(lhs
    #    id <- levels(lxy[["pts"]][["id"]])
    #} else {
    #    if (FALSE %in% (id %in% levels(lxy[["pts"]][["id"]]))) stop("id value(s) not found")
    #}
    
    if (length(hs)==1) all.ids.at.once <- FALSE
    if (max(table(sapply(hs, function(x) x[["id"]])))>1 && all.ids.at.once) stop("At least one ID has more than one hullset")
    
    if (!is.null(ylim) && !is.null(height)) stop("You can only pass ylim or height, not both")
    if (!is.null(duration) && !is.null(fps) && !is.null(skip)) stop(cw("You can pass at most two of the following parameters: duration, fps, and skip"), final.cr=F)
    if (!screen.test && is.null(duration) && is.null(fps) && is.null(skip)) stop("You must only pass two of the following parameters: duration, fps, and skip")    
    if (length(col.hod) != 0 && length(col.hod) != 24) stop("col.hod must be a vector with 24 color values or NULL")
    ## if (!is.null(tz.local) && tz.local.check) {if (!tz.valid(tz.local)) stop("Unknown value for tz")}
    if (!is.null(dt.end) && !is(dt.end, "POSIXt")) stop("dt.end must be of class POSIXt")
    if (!is.null(dt.start) && !is(dt.start, "POSIXt")) stop("dt.start must be of class POSIXt")
    if (frame.method=="auto") frame.method <- if (length(hs) > 1 && all.ids.at.once) "time" else "location"
    
    #if (!is.null(fn.mov) && length(fn.mov) != length(hs)) stop("The length of fn.out must equal the number of runs in lhs")

    if (is.null(width)) stop("Please supply a value for width")
    if (screen.test) {
        if (is.null(max.frames)) max.frames <- 3
        if (max.frames>10) stop("Can't create more than 10 frames for a screen test")
        if (width > 18) {
            if (status) cat("   Width too big for screen, resetting to 7 \n")
            width <- 7
        }
        round.up.to.nearest <- 1
    } else {
        if (width < 100) {
            ans <- readline(prompt = paste("Do you really want to create an animation that is only", width, "pixels wide? y/n "))
            if (ans != "y") return(invisible(NULL))
        }
        ## Round width up to nearest multiple of 16 to help encoder
        round.up.to.nearest <- 16
        if (width %% 16 != 0) {
            width <- ceiling(width/round.up.to.nearest) * round.up.to.nearest
            if (status) cat("   Increasing width to ", width, " (nearest multiple of 16) for encoding \n", sep="")
        }
        
    }
    
    if (date.bar > 0) {
        ## if (date.bar > 0.5) stop("Date bar should be 0..0.5")
        ## date.bar.cm <- paste(date.bar * 2.54, " cm", sep="")
    }
    if (!is.na(dt.label.col.bg)) date.str <- format(Sys.time(), format="%b. %d, %Y. %H:%M")
    
    ## Create a list of the indices 
    if (all.ids.at.once) {
        #idx.ids.lst <- list()
        #idx.ids.lst[[paste(id, collapse=".", sep="")]] <- list(id=id, idx=which(lxy[["pts"]][["id"]] %in% id))

        hs.idx <- 1:length(hs)
        hinfo.df <- do.call(rbind, lapply(1:length(hs), function(i) data.frame(hs.idx=i, hull.idx=1:nrow(hs[[i]][["hulls"]]), dt=hs[[i]][["pts"]][["dt"]][ hs[[i]][["hulls"]][["pts.idx"]] ])  ))
        idx.ids.lst <- list()
        idx.ids.lst[[paste(sapply(hs, function(x) x[["id"]]), collapse=".", sep="")]] <- list(hs.idx=hs.idx, hinfo.df=hinfo.df)

        ## Define a vector of colors that is in the same sequence as levels(lxy[["id"]]) (if needed)
        if (all.ids.col.unique) {
            if (is.null(all.ids.col)) {
                ids.cols <- rainbow(length(hs))
            } else {
                if (!is.list(all.ids.col) || length(all.ids.col) != length(hs)) stop("all.ids.col must be a named list with a color value for each id")
                idVals <- sapply(hs, function(x) x$id)
                if (FALSE %in% (names(all.ids.col) %in% idVals)) stop(cw("The names of the elements in all.ids.col must match the names of the individuals in lhs", final.cr=FALSE))
                ids.cols <- rep(NA, length(hs))

                for (id.name in names(all.ids.col)) ids.cols[which(idVals==id.name)] <- all.ids.col[[id.name]]
            }
            
            if (col.hull.alpha < 255) {
                col.rgb.mat <- sapply(ids.cols, function(x) as.numeric(col2rgb(x)))
                ids.cols <- apply(col.rgb.mat,2,function(x) rgb(x[1],x[2],x[3],alpha=col.hull.alpha,maxColorValue=255))
            }
            #lxy.id.int <- as.numeric(lxy[["pts"]][["id"]])
        } 
    } else {
        #idx.ids.lst <- lapply(id, function(x) list(id=x, idx=which(lxy[["pts"]][["id"]] == x)))
        #names(idx.ids.lst) <- id
        #all.ids.col.unique <- FALSE

        idx.ids.lst <- lapply(1:length(hs), function(i) list(hs.idx=i, hinfo.df=data.frame(hs.idx=i, hull.idx=1:nrow(hs[[i]][["hulls"]]), dt=hs[[i]][["pts"]][["dt"]][  hs[[i]][["hulls"]][["pts.idx"]]    ])))
        names(idx.ids.lst) <- names(hs)
        all.ids.col.unique <- FALSE
        if (length(col.hull.active) != 1) stop("Need a value for col.hull.active")
        if (col.hull.alpha < 255) {
            col.rgb <- col2rgb(col.hull.active)
            col.hull.active <- rgb(col.rgb[1], col.rgb[2], col.rgb[3], alpha=col.hull.alpha)
        }
        
    }
    
    #print("good time to check out idx.ids.lst");browser()
    
    
    ## Error check legend settings
    ids.legend.bln <- all.ids.at.once && all.ids.col.unique && !is.null(all.ids.legend)
    if (ids.legend.bln) {
        if (!all.ids.legend %in% c("bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", "center")) {
            stop("Invalid value for all.ids.legend")    
        }
        ## legend.str <- levels(lxy[["pts"]][["id"]])
        legend.str <- sapply(hs, function(x) x$id)
     }

    ## Create presets for the margins of the map and date bar
    ## col.db="darkblue"
    ## was 2.1
    ## mgp.map <- c(1.6, 0.4, 0)
    mar.db <- c(2, 5, 0, 2)
    mgp.db <- c(0, 0.4, 0)
    axes.lbl <- colnames(coordinates(hs[[1]][["pts"]]))
    tick.show <- if (axes.ticks) "s" else "n"
    
    ## Get the temp folder
    if (!screen.test) {
        if (is.null(tmp.dir)) {
            tmp.dir <- tempdir()
        } else {
            if (!file.exists(tmp.dir)) {
                dir.made <- dir.create(tmp.dir)
                if (!dir.made) stop(paste("Couldn't make output folder ", tmp.dir, sep=""))            
            }
        }
    }
    
    ## Prepare GIS features
    gis.features.full.extent <- shp.layers(layers, shp.csv=shp.csv)
    #if (length(layers) > 0) layers.plot <- NULL

    ## Convert date stamps to local time if needed
    if (!is.null(tz.local) && attr(hs[[1]][["pts"]][["dt"]], "tzone") != tz.local) {
        for (hs.idx in 1:length(hs)) {
            hs[[hs.idx]][["pts"]][["dt"]] <- as.POSIXct(format(hs[[hs.idx]][["pts"]][["dt"]], tz=tz.local), tz=tz.local)
        }
    }

    res <- list()
    
    ## Begin the main loop
    for (idx.set in 1:length(idx.ids.lst)) {
       
        #idx.this.loop <- idx.ids.lst[[idx.set]][["idx"]]
        
        ## Define the first and last dates for this group
        #dt.start.use <- if (is.null(dt.start)) min(lxy[["pts"]][["dt"]][idx.this.loop]) else dt.start
        #dt.end.use <- if (is.null(dt.end)) max(lxy[["pts"]][["dt"]][idx.this.loop]) else dt.end
        
        #print("get start and end");browser()
        
        dt.start.use <- if (is.null(dt.start)) min(idx.ids.lst[[idx.set]][["hinfo.df"]][["dt"]]) else dt.start
        dt.end.use <- if (is.null(dt.end)) max(idx.ids.lst[[idx.set]][["hinfo.df"]][["dt"]]) else dt.end

        
        #cat("dt.start.use=", dt.start.use, "\n"); cat("dt.end.use=", dt.end.use, "\n")
        

        if (frame.method=="time") {
            ## Get the frame time-window (real time duration, not animation duration)
            if (frame.rtd == "auto") {
                #frame.rtd.use <- min(lxy[["rw.params"]][lxy[["rw.params"]][["id"]] %in% idx.ids.lst[[idx.set]][["id"]], "time.step.median"])
                frame.rtd.use <- min(sapply(hs, function(x) x[["rw.params"]][["time.step.median"]]))
            } else {
                frame.rtd.use <- frame.rtd
            }
    
            ## Compute frame times offset
            dt.frame.offset <- if (rtd.center) ceiling(frame.rtd.use / 2) else 0

            ## Next define the start time of each frame. dt.frames is a vector of POSIXct objects in local time
            dt.frames <- seq(from=dt.start.use-dt.frame.offset, to=dt.end.use+dt.frame.offset, by=frame.rtd.use)
            

            ## We need to add one more date at the end of dt.frames, which we will delete later, to prevent 
            ## points beyond the range of dates from appearing on the final frame
            ## THIS WILL SCREW-UP THE TIME ZONE ATTRIBUTE, BETTER TO MANUALLY DELETE POINTS PAST DT.END.USE BELOW
            ## if (dt.end.use > dt.frames[length(dt.frames)]) dt.frames <- 
    
            ## Get the frame where each point falls, then make that into a list
            hullidx.dtframeidx <- findInterval(idx.ids.lst[[idx.set]][["hinfo.df"]][["dt"]], dt.frames)
            
            ## This next line is slow, look for a faster function, maybe split
            if (status) cat(" - Assigning hulls to frames...");flush.console()
            dt.frames.hinfoidx.lst <- lapply(1:length(dt.frames), function(x) which(hullidx.dtframeidx==x))
            
            #print("Lets look at the time method");browser()    

            ## Delete any points from the very last list element that were sampled after dt.end.use
            dt.frames.hinfoidx.lst[[length(dt.frames)]] <- dt.frames.hinfoidx.lst[[length(dt.frames)]] [ idx.ids.lst[[idx.set]][["hinfo.df"]][["dt"]][ dt.frames.hinfoidx.lst[[length(dt.frames)]] ] <= dt.end.use ]
            if (status) cat("Done.\n");flush.console()


            ## Clean up multi-hull frames by moving hulls to the previous / next frame if its within rtd/cmf of the prev/next frame
            if (!is.null(clean.multihull.frames) && clean.multihull.frames > 0) {
                if (status) cat(" - Cleaning frames with multiple hulls...");flush.console()
                hulls.moved.count <- 0
                ## Get the time threshhold to move a hull to the prev/next frame
                tdistmax <- frame.rtd.use / clean.multihull.frames
                
                ## Loop through the ids
                for (ahs.idx in idx.ids.lst[[idx.set]][["hs.idx"]]) {
                
                    ## Grab just those rows in hinfo.df that contain this indvidiual
                    if (length(idx.ids.lst[[idx.set]][["hs.idx"]]) > 1) {
                        dt.frames.hinfoidx.just.this.id.lst <- lapply(dt.frames.hinfoidx.lst, function(ridx) ridx[  idx.ids.lst[[idx.set]][["hinfo.df"]][["hs.idx"]][ridx] == ahs.idx])
                    } else {
                        dt.frames.hinfoidx.just.this.id.lst <- dt.frames.hinfoidx.lst
                    }
                    
                    ## Identify the frames that contain multiple hulls in them
                    multihull.frames.idx <- which(sapply(dt.frames.hinfoidx.just.this.id.lst, length) > 1)
                    
                    ## Loop through these
                    for (mhullframe.idx in multihull.frames.idx) {
                        ## See if the frame before it has 0 hulls
                        if (mhullframe.idx != 1 && length(dt.frames.hinfoidx.just.this.id.lst[[mhullframe.idx - 1]]) == 0) {
                            ## See if there is a hull that is close enough to the edge of the time window that it can be moved to the previous frame
                            #print("clean multi-hull frames as best we can");browser()
                            
                            ## Compute the difference in time between the hull parent point and the real-time start of the frame 
                            hulls.tdiff <- as.numeric(difftime(idx.ids.lst[[idx.set]][["hinfo.df"]][["dt"]][ dt.frames.hinfoidx.just.this.id.lst[[ mhullframe.idx ]] ], dt.frames[mhullframe.idx], units="secs"))
                            
                            ## If any, move the first one to the previous frame
                            if (sum(hulls.tdiff <= tdistmax)>0) {
                                first.hull.idx <- which.min(hulls.tdiff)
                                
                                ## Add it to the previous frame
                                hinfoidx.move <- dt.frames.hinfoidx.just.this.id.lst[[ mhullframe.idx ]] [first.hull.idx]
                                dt.frames.hinfoidx.lst[[mhullframe.idx-1]] <- c(dt.frames.hinfoidx.lst[[mhullframe.idx-1]], hinfoidx.move)

                                ## Take it out of this frame
                                dt.frames.hinfoidx.lst[[mhullframe.idx]] <- dt.frames.hinfoidx.lst[[mhullframe.idx]][ dt.frames.hinfoidx.lst[[mhullframe.idx]] != hinfoidx.move ]
                                dt.frames.hinfoidx.just.this.id.lst[[mhullframe.idx]] <- dt.frames.hinfoidx.just.this.id.lst[[mhullframe.idx]][-first.hull.idx]
                                
                                hulls.moved.count <- hulls.moved.count + 1
                            }
                        }
                    
                        ## See if there are still multiple hulls in this frame
                        if (length(dt.frames.hinfoidx.just.this.id.lst[[mhullframe.idx]]) > 1) {
                            ## See if the frame after it has 0 hulls
                            if (mhullframe.idx != nrow(idx.ids.lst[[idx.set]][["hinfo.df"]]) && length(dt.frames.hinfoidx.just.this.id.lst[[mhullframe.idx+1]]) == 0) {

                                #print("wow, found an example where I need to move a hull forward");browser()

                                ## Compute the difference in time between the hull parent point and the real-time start of the frame 
                                hulls.tdiff <- as.numeric(difftime(dt.frames[mhullframe.idx+1], idx.ids.lst[[idx.set]][["hinfo.df"]][["dt"]][dt.frames.hinfoidx.just.this.id.lst[[mhullframe.idx]] ], units="secs"))
                                
                                ## If any, move the first one to the previous frame
                                if (sum(hulls.tdiff <= tdistmax)>0) {
                                    last.hull.idx <- which.min(hulls.tdiff)
                                    
                                    ## Add it to the next frame
                                    hinfoidx.move <- dt.frames.hinfoidx.just.this.id.lst[[ mhullframe.idx ]] [last.hull.idx]
                                    dt.frames.hinfoidx.lst[[mhullframe.idx+1]] <- c(dt.frames.hinfoidx.lst[[mhullframe.idx+1]], hinfoidx.move)
                                    dt.frames.hinfoidx.just.this.id.lst[[mhullframe.idx+1]] <- c(dt.frames.hinfoidx.just.this.id.lst[[mhullframe.idx+1]], hinfoidx.move)

                                    ## Take it out of this frame
                                    dt.frames.hinfoidx.lst[[mhullframe.idx]] <- dt.frames.hinfoidx.lst[[mhullframe.idx]][ dt.frames.hinfoidx.lst[[mhullframe.idx]] != hinfoidx.move ]
                                    dt.frames.hinfoidx.just.this.id.lst[[mhullframe.idx]] <- dt.frames.hinfoidx.just.this.id.lst[[mhullframe.idx]][-last.hull.idx]

                                    hulls.moved.count <- hulls.moved.count + 1

                                }
                            }
                        }
                        
                    }
                }
                if (status) cat("Done.\n");flush.console()
                if (status) cat("   ", hulls.moved.count, " hull(s) moved to an adjacent frame \n", sep="");flush.console()
            }

        } else if (frame.method=="location") {
            #dt.frames.hinfoidx.lst <- as.list(idx.this.loop)
            #dt.frames <- lxy[["pts"]][["dt"]][idx.this.loop]

            dt.frames.hinfoidx.lst <- as.list(1:nrow(idx.ids.lst[[idx.set]][["hinfo.df"]]))
            dt.frames <- idx.ids.lst[[idx.set]][["hinfo.df"]][["dt"]]
            dt.frame.offset <- 0
            
        } else {
            stop("Unknown value for 'frame.method'")
        }
        
        
        ## Set the spatial range of the entire plot
        ## This will be used to define the spatial extent and possibly find a good location for the date label
        xys <- do.call(rbind, lapply(hs[ idx.ids.lst[[idx.set]][["hs.idx"]]  ], function(x) coordinates(x[["pts"]])))
        
        ##xys <- data.frame(coordinates(lxy[["pts"]]))[idx.this.loop, , drop=FALSE]
        
        #print("See if dt.frames.hinfoidx.lst and dt.frames makes sense");browser()
        
        
        #xys.range.lower.upper <- as.data.frame(sapply(xys, range))
        xys.range.lower.upper <- as.data.frame(apply(xys, 2, range))
        height.use <- height
        
        ## Define xlim and ylim
        if (is.null(xlim)) xlim <- xys.range.lower.upper[,1]
        if (is.null(ylim)) {
            ylim <- xys.range.lower.upper[,2]
            if (!is.null(height.use)) {
                ylim.diff.based.on.width.height.ratio <-  diff(xlim) * height.use / width 
                if (ylim.diff.based.on.width.height.ratio > diff(ylim)) {
                    ylim <- mean(ylim) + c(-ylim.diff.based.on.width.height.ratio / 2, ylim.diff.based.on.width.height.ratio / 2)
                } else {
                    ## The passed height is too small to fit all the points, set it to NULL so it will be recculated later
                    ## (Alternative is to reduce ylim)
                    height.use <- NULL
                }
            }
        }
        
        ## Prepare the background image
        if (!is.null(tiff.fn)) {
            #if (tiff.fill.plot) {
            #    half.plot.size <- c(-0.5, 0.5) * max(diff(rx), diff(ry))
            #    rx.tiff <- half.plot.size + mean(rx)
            #    ry.tiff <- half.plot.size + mean(ry)
            #} else {
            #    rx.tiff <- rx
            #    ry.tiff <- ry
            #}

            #print("ok");browser()
            
            ## Expand the range of xlim and ylim by 5%, because by default R creates axes 4% beyond the spread of the data
            tiff.xlim <- xlim + c(-1,1) * 0.05 * diff(range(xlim))
            tiff.ylim <- ylim + c(-1,1) * 0.05 * diff(range(ylim))

            tiff.sgdf <- readpartgdal(tiff.fn, xlim=tiff.xlim, ylim=tiff.ylim, band=tiff.bands, silent=TRUE, status=TRUE)
            if (tiff.pct) {
                tiff.sgdf.cols <- SGDF2PCT(tiff.sgdf, adjust.bands=FALSE)
                tiff.sgdf$idx <- tiff.sgdf.cols$idx
            }
        }

        ## If we're not going to crop gis.layers, we can define gis.features as the full extent
        if (crop.layers.to.extent && length(gis.features.full.extent) > 0 ) {
            if (requireNamespace("rgeos")) {
                gis.layers.ready <- FALSE
            } else {
                cat("rgeos package not installed, setting crop.layers.to.extent to FALSE \n")            
                gis.features <- gis.features.full.extent
                gis.layers.ready <- TRUE
            }
        } else {
            gis.features <- gis.features.full.extent
            gis.layers.ready <- TRUE
        }
        #saved.plot.background <- NULL
        bg.png <- NULL
        fn.background.png <- tempfile(fileext=".png")
        
        ## Get device resolution and height of default-sized characters in inches
        dpi <- if (screen.test)  1 else 72
        #round.up.to.nearest <- if (screen.test) 1 else 16
        no.device.open <- dev.cur() == 1 
        #if (no.device.open) dev.new()
        line.height.in <- par("csi")
        if (no.device.open) dev.off()
        
        ## Compute height of the device if needed
        if (is.null(height.use)) {
            #top.plus.bottom.margins.pixels <- dpi * sum(par("mai")[c(1,3)])

            top.plus.bottom.margins.pixels <- dpi * sum(mar.map[c(1,3)]) * line.height.in
            left.plus.right.margins.pixels <- dpi * sum(mar.map[c(2,4)]) * line.height.in
            
            needed.height.of.plot.area.pixels <- (width - left.plus.right.margins.pixels) * diff(ylim) / diff(xlim)
            
            #if (date.bar > 0) needed.height.of.plot.area.pixels <- needed.height.of.plot.area.pixels + (dpi * date.bar) + (dpi * char.size.in * 2)
            if (date.bar > 0) needed.height.of.plot.area.pixels <- needed.height.of.plot.area.pixels + (dpi * date.bar)
            
            height.use <- top.plus.bottom.margins.pixels + needed.height.of.plot.area.pixels
            
            ## If plotting to PNG, round height up to the nearest multiple of 16 (to help the video encoder)
            if (!screen.test) height.use <- ceiling(round(height.use)/round.up.to.nearest) * round.up.to.nearest
        } 
        
        #print("just found the height"); browser()
        
        ## Get the height of the date bar as a proportion of the total height
        date.bar.pth <- date.bar / (height.use / dpi)
        
        ## Find a section of the plot area where there are no xy points so we can place the date label there
        ## Basically the approach here is to divide up the plot into nxn grids, then scan through them from top to bottom and find the first 
        ## grid no points in it. If there are no grids without points, the one with with the fewest number of points in it is selected
        ## and that becomes the location of the date label
        if (dt.label) {
            if (is.null(dt.label.x) && is.null(dt.label.y)) {
                divide.axes.into.n <- 5
                label.area.num.pts <- matrix(9999, ncol=divide.axes.into.n, nrow=divide.axes.into.n)
                label.areas.xs <- seq(from=xlim[1], to=xlim[2], by=diff(xlim) / divide.axes.into.n)
                label.areas.ys <- seq(from=ylim[1], to=ylim[2], by=diff(ylim) / divide.axes.into.n)
                label.area.use.idx <- NULL
                for (j in divide.axes.into.n:1) {
                    for (i in 1:divide.axes.into.n) {
                        if (is.null(label.area.use.idx)) {
                            pts.in.this.label.area <- which(xys[,1] >= label.areas.xs[i] & xys[,1] <= label.areas.xs[i+1] & xys[,2] >= label.areas.ys[j] & xys[,2] <= label.areas.ys[j+1])
                            label.area.num.pts[i,j] <- length(pts.in.this.label.area) 
                            if (length(pts.in.this.label.area)==0) {
                                ## First one that's empty is good enough
                                label.area.use.idx <- c(i,j)
                            }
                        }
                    }
                }
                if (is.null(label.area.use.idx)) {
                    label.area.use.idx <- as.numeric(which(label.area.num.pts == min(label.area.num.pts), arr.ind = TRUE)[1,])
                }
                dtlabel.pt <- data.frame(x=mean(label.areas.xs[c(label.area.use.idx[1], label.area.use.idx[1]+1)]), 
                                         y=mean(label.areas.ys[c(label.area.use.idx[2], label.area.use.idx[2]+1)]))
            } else {
                dtlabel.pt <- data.frame(x=dt.label.x, y=dt.label.y)
            }
        }
        tb.pts <- NULL
        
        ## Compute duration, skip and fps
        duration.use <- NULL
        skip.use <- NULL
        fps.use <- NULL
        
        if (is.null(duration)) {
            if (is.null(fps)) fps.use <- 10 else fps.use <- fps
            if (is.null(skip)) skip.use <- 1 else skip.use <- skip
            frames.to.use <- seq(from=1, to=length(dt.frames.hinfoidx.lst), by=skip.use)
            if (!is.null(max.frames) && length(frames.to.use) > max.frames) frames.to.use <- frames.to.use[1:max.frames]
            duration.use <- length(frames.to.use) / fps.use
        } else {
            ## Duration is provided. Either fps or skip is NULL and needs to be computed
            duration.use <- duration
            if (is.null(fps)) {
                if (is.null(skip)) skip.use <- 1 else skip.use <- skip
                frames.to.use <- seq(from=1, to=length(dt.frames.hinfoidx.lst), by=skip.use)
                fps.use <- length(frames.to.use) / duration.use
                if (fps.use > 60) stop("Required frame rate higher than maximum value of 60. Please increase the duration or skip parameter")
                ## fps can be a decimal value, so no need to recompute duration
            } else {
                fps.use <- fps
                ## Frame rate and duration are both provided by the user, need to compute skip and then recompute duration
                skip.use <- floor(length(dt.frames.hinfoidx.lst) / (fps.use * duration.use))
                if (skip.use < 1) stop("there are not enough locations to produce enough frames for that duration and frame rate")
                frames.to.use <- seq(from=1, to=length(dt.frames.hinfoidx.lst), by=skip.use)
                duration.use <- length(frames.to.use) / fps.use
            }
        }
        
        # If needed, reduce the number of frames, but we won't compute the frame rate
        if (!is.null(max.frames) && length(frames.to.use) > max.frames) frames.to.use <- frames.to.use[1:max.frames]
        if (title.show) {
            if (is.null(title)) {
                str.title <- paste(sapply(hs, function(x) x[["id"]])[idx.ids.lst[[idx.set]][["hs.idx"]]], collapse=", ", sep="")
                ##paste(hs[[hs.name]][["id"]], ": ", format(min(hs[[hs.name]]$dt[xys.indices]), format="%Y/%m/%d"), " - ", format(max(hs[[hs.name]]$dt[xys.indices]), format="%Y/%m/%d"), sep="")
            } else {
                str.title <- title
            }
        } else {
            str.title <- NULL
        }

        #print("Check out frames.to.use");browser()

        if (screen.test) {
            if (.Platform$OS.type == "windows") {
          		  windows(width=width, height=height.use, record=T)
       	    } else {
                dev.new(width=width, height=height.use)
            }	
            opar <- par(mar=mar.map, mgp=mgp.map)
            if (date.bar > 0) layout(matrix(1:2, ncol=1), heights=c(1 - date.bar.pth, date.bar.pth))
            if (bg2png) bg2png <- FALSE
        } else {
            ## Construct a filename for the mov
            fn.mov.base.exp <- expression(paste(paste(sapply(hs, function(x) x[["id"]])[idx.ids.lst[[idx.set]][["hs.idx"]]], collapse=".", sep=""), 
                                                ".", format(dt.start.use, format="%Y-%m-%d"), 
                                                ".", format(dt.end.use, format="%Y-%m-%d"), 
                                                ".n", length(frames.to.use), ".", width, "x", height.use, sep=""))
                                          
            if (is.null(fn.mov)) {
                fn.mov.base <- eval(fn.mov.base.exp)
                fn.mov.full <- file.path(fn.mov.dir, paste(fn.mov.base, ".mov", sep=""))
            } else {
                ## User provided a value for fn.mov. Check if it is a full file name include extension
                if (substr(fn.mov, nchar(fn.mov) - 3, nchar(fn.mov)) != ".mov") {
                    fn.mov.full <- file.path(fn.mov, ".mov", sep="")
                } else { 
                    fn.mov.full <- fn.mov
                }
            }
            if (file.exists(fn.mov.full)) {
                if (fn.mov.exists == "stop") {
                    stop(paste(fn.mov.full, "exists"))
                } else if (fn.mov.exists == "overwrite") {
                    cat("  ", fn.mov.full, "exists, overwriting. \n")
                    if (!file.remove(fn.mov.full)) stop(paste("Unable to overwrite", fn.mov.full))
                } else if (fn.mov.exists == "auto.increment") {
                    i <- 0
                    while (file.exists(fn.mov.full)) {
                        i <- i + 1
                        if (i > 99) stop("Have tried 99 unique file names for mov file, but all exist. Bailing")
                        #print("lets check out this auto-increment");browser()
                        
                        ## Look for a .nn pattern in the file name, then either update the 
                        ## auto-increment digits in the file name, or append new ones
                        auto.incr.dot <- substr(fn.mov.full, nchar(fn.mov.full) - 6, nchar(fn.mov.full) - 6)
                        auto.incr.digits <- substr(fn.mov.full, nchar(fn.mov.full) - 5, nchar(fn.mov.full) - 4)
                        if (auto.incr.dot=="." && length(grep("[^0-9]", auto.incr.digits, value = TRUE)) == 0) {
                            fn.mov.full <- paste(substr(fn.mov.full, 1, nchar(fn.mov.full) - 6), sprintf("%02d", i), ".mov", sep="")    
                        } else {
                            fn.mov.full <- paste(substr(fn.mov.full, 1, nchar(fn.mov.full) - 4), ".", sprintf("%02d", i), ".mov", sep="")    
                        }
                    }
                } else if (fn.mov.exists == "ask") {
                    ans <- readline(prompt = paste(fn.mov.full, " exists. Overwrite? y/n ", sep=""))
                    if (ans == "y") {
                        if (!file.remove(fn.mov.full)) stop(paste("Unable to overwrite", fn.mov.full))
                    } else {
                        return(invisible(NULL))
                    }
                } else {
                    stop("Unknown value for fn.mov.exists")
                }
                
            }

            fn.png <- paste(tempfile(pattern="img", tmpdir=tmp.dir), "%04d.png", sep="")
            
            ## Find ffmpeg.exe and create cmd line
            if (create.mov) {
                ffmpeg.exec <- findonpath(ffmpeg)
                if (is.null(ffmpeg)) {
                    cat(cw("Cant find ffmpeg.exe. Please make sure this file is downloaded and saved either in the working directory or a directory on the PATH environment variable (e.g., c:/windows)", final.cr=T))
                    return(invisible(NULL))
                }
                cmd <- paste(ffmpeg.exec, " -r ", fps.use, " -i \"", fn.png, "\" -s ", width, "x", height.use, " -g 300 -pix_fmt rgb555be -vcodec qtrle -an \"", fn.mov.full, "\"", sep="")
            }
            
            if (prompt.continue) {
                cat("Ready to generate animation \n", sep="")
                cat("  Num frames=", length(frames.to.use), ". Duration=", round(duration.use, 1), "secs. fps=", fps.use, ". Skip=", skip.use, ". Frame size: ", 
                    width, "x", height.use, "\n", sep="")
                cat("  Temp folder: ", tmp.dir, "\n  Delete temp files: ", tmp.files.delete, "\n  Record background as PNG: ", bg2png, "\n", sep="")
                cat("  Mov file: ", if (create.mov) fn.mov.full else "<skipped>", "\n", sep="")
                ans <- readline(prompt = "Continue? y/n ")
                if (ans != "y") return(invisible(NULL))
            }
            
            start.time <- Sys.time()
            png(filename=fn.png, width=width, height=height.use, pointsize=png.pointsize)
            ##if (record.plot) dev.control(displaylist="enable")
            opar <- par(mar=mar.map , mgp=mgp.map)
            if (date.bar > 0) layout(matrix(1:2, ncol=1), heights=c(1-date.bar.pth, date.bar.pth))
            #opar <- par(mar=c(5, 3.2, 3.2, 0.5), mgp=c(2.1, 1, 0))        
        }
        on.exit(par(opar))
        
        ## Set up date bar parameters
        if (date.bar > 0) {
            ## db.axis$tick.all is a vector of dates in local time (SAST)
            db.axis <- dateticks(dt.frames, max.ticks=date.bar.bins + 1)
            db.axis.int.gmt <- as.numeric(db.axis[["tick.all"]])
            dt.frames.int.gmt <- as.numeric(dt.frames)
            db.xlim.int.gmt <- c(min(db.axis.int.gmt[1], dt.frames.int.gmt[1]), max(db.axis.int.gmt[length(db.axis.int.gmt)], dt.frames.int.gmt[length(dt.frames.int.gmt)]))
            
            ## old
            ##db.xlim <- c(min(db.axis$tick.all[1], min(dt.frames)), max(db.axis$tick.all[length(db.axis$tick.all)], max(dt.frames)))
        }
               
                    
        ## Set up the colors for the active points
        if (col.by.hour.of.day) {
            print("ok, time to set colors for hour of day - is this still needed and working?");browser()
            #print("lets pause and look at the hour of day color");browser()
            col.use <- col.hod[as.POSIXlt(dt.frames)[["hour"]] + 1]
        } else {
            col.use <- "black"
            ## take out for now col.use <- rep(col.xys.active, length(xys.indices))
        } 
        
          
        ## Make the frames
        cat(" - Creating frames\n")
        if (status) pb <- txtProgressBar(min = 0, max = length(frames.to.use), style = 3)
        make.frames.start <- Sys.time()
        
        ##for (i in 1:length(dt.frames.lxyidx.lst)) {
        for (ftu.idx in 1:length(frames.to.use)) {
            if (status) setTxtProgressBar(pb, ftu.idx)
            i <- frames.to.use[ftu.idx]

            ## If bg2png=T and this is the first pass, create a PNG file of just the plot area containing the map background, then load it to memory
            if (bg2png && is.null(bg.png)) {
            
                ## Switch to the top figure and apply map margins 
                par(mfg=c(1,1), mar=mar.map)
                
                ## Get the active figure area, width and height, in inches
                par.fin <- par("fin")
                
                ## Subtract the current (map) margins (in inches)
                par.mai <- par("mai")
                par.fin[1] <- par.fin[1] - sum(par.mai[c(2,4)])
                par.fin[2] <- par.fin[2] - sum(par.mai[c(1,3)])
                                
                ## Calculate the size of the figure area in pixels
                plot.region.pixels <- round(par.fin * dpi)
                                
                ## Switch off the current png device, will reopen it shortly
                dev.off()
                
                ## Create a new png device for just the figure area
                png(filename=fn.background.png, width=plot.region.pixels[1], height=plot.region.pixels[2], pointsize=png.pointsize)
                
                ## Create a blank plot with no margins and no axes
                par(mar=c(0,0,0,0))
                plot(NULL, xlim=xlim, ylim=ylim, axes=F, asp=1)

                ## Show the background tiff
                if (!is.null(tiff.fn)) {
                    if (tiff.pct) {
                        image(tiff.sgdf, "idx", col=tiff.sgdf.cols[["ct"]], add=TRUE)

                    } else {
                        image(tiff.sgdf, red=1, green=2, blue=3, add=TRUE)
                    }
                    box("plot")
                }

                ## Crop the GIS layers on first pass if needed
                if (!gis.layers.ready) {
                    gis.features <- shp.layers.crop(gis.features.full.extent, matrix(par("usr"), byrow=F, ncol=2))
                    gis.layers.ready <- TRUE
                }
                
                ## Put down the polygon layers the GIS layers
                for (featname in names(gis.features)[sapply(gis.features, function(x) x[["type"]]=="polygon")]) {
                    with(gis.features[[featname]], plot(sdf, lty=lty, pch=pch, cex=cex, col=col, border=if (is.na(border)) NULL else border, lwd=lwd, add=TRUE))
                }
                
                ## Put down the background points
                #if (!is.na(col.xys.background)) points(xys, col=col.xys.background, pch=20, cex=cex.xys.background)
                
                ## Put down the GIS point and line layers
                for (featname in names(gis.features)[sapply(gis.features, function(x) x[["type"]] %in% c("point", "line"))]) {
                    with(gis.features[[featname]], plot(sdf, lty=lty, pch=pch, cex=cex, col=col, border=if (is.na(border)) NULL else border, lwd=lwd, add=TRUE))
                }

                ## Plot the legend
                if (ids.legend.bln) legend(all.ids.legend, legend=legend.str, col=ids.cols, pch=20, bg="white", title="id", cex=all.ids.legend.cex) 
                
                ## We're done with all the background elements. Close the device and read the png file back into memory as a 3-dimension matrix
                dev.off()
                bg.png <- readPNG(fn.background.png)
                
                ## Reopen the original png device with the original dimensions
                png(filename=fn.png, width=width, height=height.use, pointsize=png.pointsize)
                if (date.bar > 0) layout(matrix(1:2, ncol=1), heights=c(1-date.bar.pth, date.bar.pth))
                
            }
            
            #if (!screen.test && record.plot && !is.null(saved.plot.background)) {
            #    replayPlot(saved.plot.background)
            #} else {
                
            #print('ready to create a blank plot');browser()
            
            ## Create a blank plot
            par(mar=mar.map, mgp=mgp.map)
            plot(NULL, xlim=xlim, ylim=ylim, xlab=if (axes.titles) axes.lbl[1] else "", ylab=if (axes.titles) axes.lbl[2] else "", xaxt=tick.show, yaxt=tick.show, cex.axis=0.8, main=str.title, asp=1)
            usr.map <- par("usr")
            
            if (bg2png) {
                ## Display the raster image with the static background elements
                rasterImage(bg.png, usr.map[1], usr.map[3], usr.map[2], usr.map[4], interpolate=FALSE)
                box("plot")
            } else {
            
                ## Show the background tiff
                if (!is.null(tiff.fn)) {
                    if (tiff.pct) {
                        image(tiff.sgdf, "idx", col=tiff.sgdf.cols[["ct"]], add=TRUE)
                    } else {
                        image(tiff.sgdf, red=1, green=2, blue=3, add=TRUE)
                    }
                    box("plot")
                }

                ## Crop the GIS layers on first pass if needed
                if (!gis.layers.ready) {
                    #gis.features <- shp.layers.crop(gis.features.full.extent, matrix(c(xlim,ylim), ncol=2, byrow=F))
                    gis.features <- shp.layers.crop(gis.features.full.extent, matrix(par("usr"), byrow=F, ncol=2))
                    gis.layers.ready <- TRUE
                }
                
                ## Put down the polygon layers the GIS layers
                for (featname in names(gis.features)[sapply(gis.features, function(x) x[["type"]]=="polygon")]) {
                    with(gis.features[[featname]], plot(sdf, lty=lty, pch=pch, cex=cex, col=col, border=if (is.na(border)) NULL else border, lwd=lwd, add=TRUE))
                }
                
                ## Put down the background points
                ## if (!is.na(col.xys.background)) points(xys, col=col.xys.background, pch=20, cex=cex.xys.background)
                
                ## Put down the points and lines layers
                for (featname in names(gis.features)[sapply(gis.features, function(x) x[["type"]] %in% c("point", "line"))]) {
                    with(gis.features[[featname]], plot(sdf, lty=lty, pch=pch, cex=cex, col=col, border=if (is.na(border)) NULL else border, lwd=lwd, add=TRUE))
                }

                ## Plot the legend
                if (ids.legend.bln) legend(all.ids.legend, legend=legend.str, col=ids.cols, pch=20, bg="white", title="id") 
            
                ## We'll come back and plot the active point later
            }
            
            ## Set up the date bar
            if (date.bar > 0) {
                par(mar=mar.db, mgp=mgp.db)
                plot(NULL, xlim=db.xlim.int.gmt, ylim=c(0,1), axes=F, xlab="", ylab="")
                usr.db <- par("usr")
                axis(side=1, at=db.axis.int.gmt, labels=format(db.axis[["tick.all"]], format=db.axis[["format.str"]]), cex.axis=cex.axis.db, col.axis=col.db, col=col.db, col.ticks=col.db)
            }
            
            if (!is.na(dt.label.col.bg) && is.null(tb.pts)) {
                tb.half.width <- 1.1 * strwidth(date.str) / 2
                tb.half.height <- 1.5 * strheight(date.str) / 2
                tb.pts <- data.frame(x=dtlabel.pt[,1] + tb.half.width * c(-1,1,1,-1,-1), y=dtlabel.pt[,2] + tb.half.height * c(1,1,-1,-1,1))
            }

            ## Time to plot the active point(s). Set the focus to the top frame and apply map margins
            par(mfg=c(1,1), mar=mar.map, mgp=mgp.map, usr=usr.map)
            
            #print("Plot the active hulls");browser()
            
            ## Plot the active hull
            for (j in dt.frames.hinfoidx.lst[[i]]) {
                hullpts <- hs[[   idx.ids.lst[[idx.set]][["hinfo.df"]][j, "hs.idx"]   ]][["hulls"]]@polygons[[  idx.ids.lst[[idx.set]][["hinfo.df"]][j, "hull.idx"] ]]@Polygons[[1]]@coords
                polygon(hullpts, col=if (all.ids.col.unique) ids.cols[idx.ids.lst[[idx.set]][["hinfo.df"]][j, "hs.idx"]] else col.hull.active, border=NA)
                ##Just need to get the color right
                #points(coordinates(lxy[["pts"]])[dt.frames.lxyidx.lst[[i]], ,drop=FALSE], col=if (all.ids.col.unique) ids.cols[lxy.id.int[dt.frames.lxyidx.lst[[i]]]] else col.xys.active, pch=20, cex=cex.xys.active)
            }
            
            ## Plot the date-time label
            if (dt.label) {
                if (!is.na(dt.label.col.bg)) polygon(tb.pts, col=dt.label.col.bg, border=NA)
                text(dtlabel.pt, labels=format(dt.frames[i] + dt.frame.offset, format="%b. %d, %Y. %H:%M"), font=2, col=dt.label.col)
            }
            
            ## Put a line on the date bar
            if (date.bar > 0) {
                ## Set the focus to the bottom frame
                par(mfg=c(2,1), mar=mar.db, mgp=mgp.db, usr=usr.db)
                
                #For some funny reason it won't plot the date bar line without the following:
                box(lty=0)
                
                points(x=rep(dt.frames.int.gmt[i], 2), y=c(0,0.5), type="l", col=col.hull.active, lwd=3)
            }
            
        }
        if (status) close(pb)
        
        if (screen.test) {
            return(invisible(NULL))
        } else {
            dev.off()
            if (report.time) {
                time.taken.frames = difftime(Sys.time(), make.frames.start, units="auto")
                time.taken.frames.secs = as.numeric(difftime(Sys.time(), make.frames.start, units="secs"))
                cat("   Time to make frames:", round(time.taken.frames, 1), units(time.taken.frames), " (", round(time.taken.frames.secs / length(frames.to.use), 1), " seconds/frame) \n", sep = " ") 
            }    

            if (create.mov) {
                shell.ffmpeg <- system(cmd, wait=TRUE, invisible=TRUE, intern=FALSE, minimized=FALSE)
                if (shell.ffmpeg == 0) {
                    cat("  Created ", fn.mov.full, " (", round(file.info(fn.mov.full)[["size"]] / 2^20, 3), "Mb)\n", sep="")    
                } else {
                    cat("Creating mov file appears to have failed \n")
                    return(NULL)
                }
                res[[names(idx.ids.lst)[idx.set]]] <- list(fn=fn.mov.full, dim=c(width, height.use))
            }
            if (tmp.files.delete) file.remove(sprintf(fn.png, 1:length(frames.to.use)))            
        
        }
        
    }

    if (report.time && !screen.test) {
        time.taken.total = difftime(Sys.time(), start.time, units="auto")
        cat("   Total time:", round(time.taken.total, 1), units(time.taken.total), "\n", sep = " ")    
    }    

    if (beep) {
        flush.console()
        for (i in 1:3) {
            alarm()
            Sys.sleep(0.8)
        }
    }

    return(invisible(res))
}
