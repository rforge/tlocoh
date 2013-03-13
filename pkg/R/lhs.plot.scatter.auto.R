#' Generate multiple scatterplots of hull metrics
#'
#' This is a wrapper function for \code{\link{lhr.scatter}} that creates scatterplots for pairs of hull metrics.
#' It can quickly generate several few dozen scatterplots for the purpose of visually looking for novel associations between hull metrics.

#'
#' @param hs A LoCoH-hullset object
#' @param id The names of the individual(s) to include in the plot. Character vector or comma-delimited character.
#' @param k The k value(s) of the hullset(s) to include in the plot. Numeric vector or comma-delimited character object.
#' @param r The r value(s) of the hullset(s) to include in the plot. Numeric vector or comma-delimited character object.
#' @param a The a value(s) of the hullset(s) to include in the plot. Numeric vector or comma-delimited character object.
#' @param s The s value(s) of the hullset(s) to include in the plot. Numeric vector or comma-delimited character object.
#' @param hs.names The name(s) of saved hullsets to include in the plot.
#' @param x.metrics The name(s) of hull metric(s) to plot on the x-axis. Can also be "auto" for the most common ones, or "all"
#' @param y.metrics The name(s) of hull metric(s) to plot on the y-axis. Can also be "auto" for the most common ones, or "all"
#' @param metrics.exclude The name(s) of hull metrics to exclude from both the x and y axes
#' @param exclude.same.pair Whether to exclude a hull metric being plotted against itself. T/F
#' @param exclude.reverse.pair Whether to exclude opposite pairs of metrics. T/F
#' @param png.dir The directory for a PNG file (filename will be constructed automatically). Ignored if png.fn is passed.
#' @param png.dir.make Whether to create png.dir if it doesn't exist. T/F.
#' @param png.width The width of the PNG image. Ignored if png.fn is passed.
#' @param png.height The height of the PNG image. Ignored if png.fn is passed.
#' @param png.pointsize The pointsize (in pixels) for the PNG image (increase to make labels appear larger). Equivalent to the height or width of a character in pixels.
#' @param png.fn.pre A prefix that will be used in the construction of the PNG filename
#' @param png.fn.mid A mid-fix that will be used in the construction of the PNG filename
#' @param png.fn.suf A suffix that will be used in the construction of the PNG filename
#' @param png.exists What to do if a PNG with the same filename already exists: "overwrite" or "skip"
#' @param test.sample An optional number of randomly selected pairs of hull metrics to generate for testing purposes. Or NULL.
#' @param enumerate.pairs.only Whether to only return a two-column data frame of the names of hull metrics. T/F
#' @param new.plot.window Whether to start a new plot window and turn recording on. T/F
#' 
#' @note This function generates a list of pairs of hull metrics, and then calls \code{\link{lhr.scatter}} repeatedly to generate
#' the histograms. This is a quick way to visualize the relationships between hull metrics.
#'
#' If \code{new.plot.window} is \code{TRUE} (default), a new plot window will be opened and plot recording
#' turned on so you can flip through the histograms using the PgUp and PgDn keys. You may also pass a value
#' for \code{png.dir}) which will create a bunch of PNG files which you can then view them as a slideshow.
#'
#' @return A data frame containing the pairs of hull metrics
#' @seealso \code{\link{lhs.plot.scatter}}, \code{\link{hm.expr}}
#' @export

lhs.plot.scatter.auto <- function(hs, id=NULL, k=NULL, r=NULL, a=NULL, s=NULL, hs.names = NULL, 
                             x.metrics=c("auto","all")[1], y.metrics=c("auto","all")[1], metrics.exclude=c("bearing", "sgf.nn.mean", "sgf.enc.mean"), 
                             exclude.same.pair=TRUE, exclude.reverse.pair=TRUE,
                             png.dir=NULL, png.dir.make=TRUE, png.width=800, png.height=png.width, png.pointsize=12+(png.width-480)/80,
                             png.fn.pre=NULL, png.fn.mid=NULL, png.fn.suf=NULL, png.exists=c("skip","overwrite")[1], progress.bar=TRUE, 
                             test.sample=NULL, enumerate.pairs.only=FALSE, new.plot.window=TRUE, hmap=NULL, ...) {

    if (!inherits(hs, "locoh.lhs")) stop("hs should be of class \"locoh.lhs\"")
    if (!is.null(hs[["xys"]])) stop("Old data structure detected")
    if (!require(sp)) stop("package sp required")

    if (is.null(id) && is.null(r) && is.null(k) && is.null(a) && is.null(s) && is.null(hs.names)) {
        hs.matching.idx <- 1:length(hs)
    } else {    
        hs.matching.idx <- lhs.select.which(hs, id=id, r=r, k=k, a=a, s=s, hs.names=hs.names)
    }
    if (length(hs.matching.idx)==0) stop("No sets of hulls found matching those criteria")

    ## See if the output directory exists
    if (!is.null(png.dir) && !file.exists(png.dir)) {
        if (png.dir.make) {
            dir.made <- dir.create(png.dir)
            if (!dir.made) stop(paste("Couldn't make output folder ", png.dir, sep=""))            
        } else {
            stop(paste(png.dir, " doesn't exist and png.dir.make is False, so can not continue."))
        }
    }
    
    hme <- hm.expr(names.only=FALSE)
    ddd.lst <- list(...)
    
    if (identical(x.metrics, "all")) {
        x.metrics <- names(hme)
    } else if (identical(x.metrics, "auto")) {
        x.metrics <- names(hme)[sapply(hme, function(x) x$spao.x)]
    } else {
        if (FALSE %in% (x.metrics %in% names(hme))) stop("Invalid value for x.metric")
        if (anyDuplicated(x.metrics)) stop("Duplicate values found in x.metrics")
    }

    if (identical(y.metrics, "all")) {
        y.metrics <- names(hme)
    } else if (identical(y.metrics, "auto")) {
        y.metrics <- names(hme)[sapply(hme, function(x) x$spao.y)]
    } else {
        if (FALSE %in% (y.metrics %in% names(hme))) stop("Invalid value for y.metric")
        if (anyDuplicated(y.metrics)) stop("Duplicate values found in y.metrics")
    }

    if (length (metrics.exclude) > 0) {
        x.metrics <- x.metrics[!x.metrics %in% metrics.exclude]
        y.metrics <- y.metrics[!y.metrics %in% metrics.exclude]
    }

    x.metrics.req <- lapply(hme[x.metrics], function(z) z[["req.metrics"]]) 
    y.metrics.req <- lapply(hme[y.metrics], function(z) z[["req.metrics"]]) 

    metrics.pairs <- NULL
    #bad.metrics <- NULL

    cat("Enumerating pairs of metrics..."); flush.console()
    for (hs.idx in hs.matching.idx) {
        
        hs.name <- names(hs)[hs.idx]
        
        ## Create a vector of all of the hull metrics saved
        metrics.saved <- unique(sapply(hs[[hs.idx]][["hm"]], function(x) x[["type"]]))
        
        ## Remove the non-available metrics from x.metrics.use and y.metrics.use
        x.metrics.use <- x.metrics[sapply(x.metrics, function(z) if (is.null(x.metrics.req[[z]])) TRUE else (!FALSE %in% (x.metrics.req[[z]] %in% metrics.saved)))]
        y.metrics.use <- y.metrics[sapply(y.metrics, function(z) if (is.null(y.metrics.req[[z]])) TRUE else (!FALSE %in% (y.metrics.req[[z]] %in% metrics.saved)))]
        
        ## Create a two-column data frame containing all permutations
        xm.ym <- expand.grid(x.metrics.use, y.metrics.use, stringsAsFactors=FALSE)
        names(xm.ym) <- c("xm","ym")
                    
        
        ## Get rid of duplicates and reverse pairs
        if (exclude.same.pair) xm.ym <- xm.ym[xm.ym[ ,"xm"] != xm.ym[ , "ym"], ]
        if (exclude.reverse.pair) {
            xm.ym.key <- sapply(1:nrow(xm.ym), function(i) paste(sort(c(xm.ym[i,1], xm.ym[i,2])), collapse="|", sep=""))
            xm.ym <- xm.ym[!duplicated(xm.ym.key), ]
        }
        
        ## Make sure all of the required parameters were passed to the function, *and* create a separate data frame
        ## containing all permutation(s) of the hull metric auxillary parameters (hmap) passed
        avparams.lst <- c(list(), hmap)
        for (axis.metric in unique(unlist(xm.ym))) {
            for (avparam in hme[[axis.metric]][["req.ap"]]) {
                if (!avparam %in% names(ddd.lst)) {
                    if (is.null(hme[[axis.metric]][["req.ap.def"]])) {
                        stop(paste("Required parameter missing: ", avparam, sep=""))
                    } else if (identical(hme[[axis.metric]][["req.ap.def"]][[avparam]], "all")) {
                        ## Get all of the hull parameters that have been run
                        if (avparam %in% names(hs[[hs.name]][["hm.params"]])) {
                            avparams.lst[[avparam]] <- unique(c(avparams.lst[[avparam]], hs[[hs.name]][["hm.params"]][[avparam]]))
                        }
                    } else {
                        avparams.lst[[avparam]] <- unique(c(avparams.lst[[avparam]], hme[[axis.metric]][["req.ap.def"]]))
                    }
                } else {
                    # Make sure these values have been actually computed
                    if (FALSE %in% (ddd.lst[[avparam]] %in%  hs[[hs.name]][["hm.params"]][[avparam]])) stop(paste("Value of ", avparam, " not found", sep=""))
                    avparams.lst[[avparam]] <- unique(c(avparams.lst[[avparam]], ddd.lst[[avparam]]))
                }
            }
        }
        if (length(avparams.lst) > 0) {
            ## Convert the list of auxillary variables into a data frame containing all permutations of the variables
            hmap <- expand.grid(avparams.lst, stringsAsFactors=FALSE)
        } else {
            hmap <- as.data.frame(NA)
        }
        
        ## Identify which pairs have at least one metric which requires an auxillary parameter
        xym.rap <- sapply(hme[xm.ym[,"xm"]], function(j) !is.null(j[["req.ap"]])) | sapply(hme[xm.ym[,"ym"]], function(j) !is.null(j[["req.ap"]]))
                
        ## Add the pairs that *don't* require any auxillary parameters
        if (sum(!xym.rap) > 0) {
            metrics.pairs.no.hmap <- data.frame(hs.name=hs.name, x.metric=xm.ym[!xym.rap,"xm"], y.metric=xm.ym[!xym.rap,"ym"], rap=FALSE, stringsAsFactors=FALSE)
            if (!is.na(hmap[1,1])) metrics.pairs.no.hmap <- data.frame(metrics.pairs.no.hmap, data.frame(lapply(hmap, function(x) NA))[rep(1,sum(!xym.rap)), , drop=FALSE], stringsAsFactors=FALSE)
            metrics.pairs <- rbind(metrics.pairs, metrics.pairs.no.hmap)
        }
        
        ## Add the pairs that *do* require hmap values                                                                
        if (sum(xym.rap) > 0) {
            xym.rap.hmap <- expand.grid(which(xym.rap), 1:nrow(hmap))
            metrics.pairs.hmap <- data.frame(hs.name=hs.name, x.metric=xm.ym[xym.rap.hmap[,1],"xm"], y.metric=xm.ym[xym.rap.hmap[,1],"ym"], rap=TRUE, hmap[xym.rap.hmap[,2] , , drop=FALSE], stringsAsFactors=FALSE)
            metrics.pairs <- rbind(metrics.pairs, metrics.pairs.hmap)
        }
    }
    
    cat("Done \n"); flush.console()
    
    if (!is.null(test.sample)) metrics.pairs <- metrics.pairs[sample(1:nrow(metrics.pairs), size=test.sample),]
    if (enumerate.pairs.only) return(metrics.pairs)
    fn <- NULL; fn.width <- NULL; fn.height <- NULL
    if (is.null(png.dir) && new.plot.window && .Platform$OS.type == "windows") windows(record=T)

    if (nrow(metrics.pairs) > 0) {
        cat("Producing ", nrow(metrics.pairs), " scatterplots \n", sep="")
        progress.bar <- progress.bar &&  (nrow(metrics.pairs) > 1)
        
        if (progress.bar) pb <- txtProgressBar(min = 0, max=nrow(metrics.pairs), style = 3)
        for (i in 1:nrow(metrics.pairs)) {
            if (progress.bar) setTxtProgressBar(pb, i)
            
            #cat(i, ", ", metrics.pairs[i,2], ", ", metrics.pairs[i,3], ", ", metrics.pairs[i,4], "\n", sep="")

            #print('ready to call lhs.plot.scatter');browser()
            ## Call lhs.plot.scatter
            hsp.this <- lhs.plot.scatter(hs, hs.names=metrics.pairs[i,"hs.name"], 
                x.axis=metrics.pairs[i,"x.metric"], y.axis=metrics.pairs[i,"y.metric"], 
                hmap=if(metrics.pairs[i,4]) as.list(metrics.pairs[i, 5:length(metrics.pairs), drop=FALSE]) else NULL,
                png.dir=png.dir, png.dir.make=png.dir.make, png.width=png.width, 
                png.height=png.height, png.pointsize=png.pointsize,
                png.fn.pre=png.fn.pre, png.fn.mid=png.fn.mid, png.fn.suf=png.fn.suf, status=!progress.bar, png.exists=png.exists)
    
            if (!is.null(png.dir)) {
                fn <- c(fn, hsp.this[[1]]$fn)
                fn.width <- c(fn.width, hsp.this[[1]]$dim[1])
                fn.height <- c(fn.height, hsp.this[[1]]$dim[2])
            }
        }
        if (progress.bar) close(pb)
    }
    
    if (!is.null(png.dir)) {
        metrics.pairs <- cbind(metrics.pairs, fn=fn, width=fn.width, height=fn.height) 
    }
    return(invisible(metrics.pairs))

}
