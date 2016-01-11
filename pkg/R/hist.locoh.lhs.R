#' Plot histograms of hull metrics
#'
#' @param x A \link{LoCoH-hullset} object
#' @param lhs Deprecated, use \code{x} instead
#' @param id The names of the individual(s) to include in the plot.
#' @param k The k value(s) of the hullset(s) to include in the plot. Numeric vector or comma-delimited character object.
#' @param r The r value(s) of the hullset(s) to include in the plot. Numeric vector or comma-delimited character object. 
#' @param a The a value(s) of the hullset(s) to include in the plot. Numeric vector or comma-delimited character object.
#' @param s The s value(s) of the hullset(s) to include in the plot. Numeric vector or comma-delimited character object.
#' @param hs.names The name(s) of saved hullsets to include in the plot
#' @param metric The name(s) of hull metric(s); one histogram will be created for each hull metric.
#' @param hmap A named list of hull metric auxillary parameters, the name of each list element is the name of the variable.
#' @param hmap.in.title Include the hmap value(s) in the plot subtitle. T/F
#' @param hs.name.in.title Include the name of the hullset in the plot subtitle. T/F
#' @param title A title for the plot (over-writes the construction of a title)
#' @param title.show Whether to add a title on the plot. T/F
#' @param include.missing.hulls Whether to include missing hulls on the histogram (using the default value assigned to missing hulls by the hull metrics). T/F
#' @param figs.per.page Number of plots per page
#' @param mar The plot margins. A four item numeric vector
#' @param mgp The distance away from the edge of the plot for the 1) label, 2) tick marks, and 3) axis line. A three-item numeric vector
#' @param same.axes.for.all Whether to use the same range on the axes for all histograms generated (helps to visually see differences)
#' @param ufat User-friendly axis title. T/F
#' @param breaks Number of breaks or a clustring function. See \code{\link{hist}}.
#' @param col Color value for the bars
#' @param lo.margins.set Deprecated, use \code{lo.save} instead
#' @param lo.save Whether to set the plot device options (margins, rows and columns). T/F.
#' @param png.dir The directory for a PNG file (filename will be constructed automatically).
#' @param png.dir.make Whether to create png.dir if it doesn't exist. T/F
#' @param png.width The width of the PNG image. Ignored if png.fn is passed.
#' @param png.height The height of the PNG image. Ignored if png.fn is passed.
#' @param png.overwrite Whether to overwrite an existing PNG file if it exists. T/F
#' @param title.two.id Construct a title with the names of two ids (for so.* and to.* assocation metrics). T/F
#' @param indicate.missing.hulls.in.axis.lbl Specify whether missing hulls are excluded in the axes label, T/F
#' @param panel.num A number or letter to display in the upper left hand corner of the plot when the plot will be used as part of a multi-frame graphic (as in publications). Character
#' @param panel.num.inside.plot Whether to display panel.num inside the plot area itself, as opposed to the title area. Ignored if panel.num is NULL. T/F
#' @param png.pointsize The pointsize (in pixels) for the PNG image (increase to make labels appear larger). Determines the height or width of a character in pixels.
#' @param ... Other parameters, including any auxillary parameters to specify particular hull metrics
#'
#' @note
#' To see the names of hull metrics, type \code{hm.expr()}. Only hull metrics which have been computed can be plotted.
#'
#' @return A named list of values of the hull metrics. There is one list element for each metric.
#'
#' @export
#' @method hist locoh.lhs

hist.locoh.lhs <- function(x, lhs, id=NULL, k=NULL, r=NULL, a=NULL, s=NULL, hs.names = NULL, 
                metric="area", include.missing.hulls=TRUE, hmap=NULL, 
                hmap.in.title=TRUE, hs.name.in.title=TRUE, title=NULL, title.show=TRUE,
                figs.per.page=NULL, mar=c(3, 3, if (title.show) 2.8 else 0.7, 0.5), mgp=c(1.8, 0.5, 0),
                same.axes.for.all=FALSE, ufat=TRUE, breaks="Sturges", col="gray80", lo.margins.set, lo.save=TRUE,
                png.dir=NULL, png.dir.make=TRUE, png.width=800, png.height=png.width, png.overwrite=TRUE, png.pointsize=12+(png.width-480)/80,
                title.two.id=FALSE, indicate.missing.hulls.in.axis.lbl=TRUE, panel.num=NULL, panel.num.inside.plot=!title.show, ...) {

    ## This does not yet return a list of list ($fn, $dim) when png.dir != NULL
    
    if (!missing(lhs)) {
      warning("argument lhs is deprecated; please use x instead.", call. = FALSE)
    } else {
      lhs <- x; rm(x)
    }

    if (!missing(lo.margins.set)) {
      warning("argument lo.margins.set is deprecated; please use lo.save instead.", call. = FALSE)
      lo.save <- lo.margins.set
    } 

    if (!inherits(lhs, "locoh.lhs")) stop("x should be of class \"locoh.lhs\"")

    ## Grab a list of expressions that will be used to extract the appropriate values for each hull
    hme <- hm.expr(names.only=FALSE)
    if (FALSE %in% (metric %in% names(hme))) stop(cw(paste("metric must be one of the following: ", paste(names(hme), collapse=", ", sep=""), sep="")))
    
    if (is.null(id) && is.null(r) && is.null(k) && is.null(a) && is.null(s) && is.null(hs.names)) {
        hs <- lhs
    } else {    
        hs <- lhs.select(lhs, id=id, r=r, k=k, a=a, s=s, hs.names=hs.names)
    }
    if (length(hs)==0) stop("No hullsets found matching those criteria")
    
    ## sort these by id and then s
    hs.ord <- order(sapply(hs, function(j) j[["id"]]), sapply(hs, function(j) j[["s"]]))
    res <- list()
    ddd.lst <- list(...)
    hmap.passed <- hmap
        
    ## Construct a list of hmap data frames (all permutations of hull metric auxillary parameters) for each metric
    metric.hmap.lst <- list()
    for (metric.name in metric) {
        avparams.lst <- list()
        for (avparam in hme[[metric.name]][["req.ap"]]) {
            if (avparam %in% names(ddd.lst)) {
                # Make sure these values have been actually computed
                for (hs.name in names(hs[hs.ord])) {
                    if (FALSE %in% (ddd.lst[[avparam]] %in%  hs[[hs.name]][["hm.params"]][[avparam]])) stop(paste("Value of ", avparam, " not found in hullset", sep=""))
                }
                avparams.lst[[avparam]] <- unique(c(avparams.lst[[avparam]], ddd.lst[[avparam]]))
            } else if (avparam %in% names(hmap.passed)) {                
                # Make sure these values have been actually computed
                for (hs.name in names(hs[hs.ord])) {
                    if (FALSE %in% (hmap.passed[[avparam]] %in%  hs[[hs.name]][["hm.params"]][[avparam]])) stop(paste("Value of ", avparam, " not found in hullset", sep=""))
                }
                avparams.lst[[avparam]] <- unique(c(avparams.lst[[avparam]], ddd.lst[[avparam]]))
            } else {
                # Try using the metric default value
                if (is.null(hme[[metric.name]][["req.ap.def"]])) {
                    stop(paste("Required parameter missing: ", avparam, sep=""))
                } else if (identical(hme[[metric.name]][["req.ap.def"]][[avparam]], "all")) {
                    ## Get all of the hull parameters that have been run
                    for (hs.name in names(hs[hs.ord])) {
                        avparams.lst[[avparam]] <- unique(c(avparams.lst[[avparam]], hs[[hs.name]][["hm.params"]][[avparam]]))
                    }
                } else {
                    avparams.lst[[avparam]] <- unique(c(avparams.lst[[avparam]], hme[[metric.name]][["req.ap.def"]]))
                }
    
            }
        }
        if (length(avparams.lst) == 0) {
            metric.hmap.lst[[metric.name]] <- as.data.frame(NA)
        } else {
            ## Convert the list of auxillary variables into a data frame containing all permutations of the variables
            metric.hmap.lst[[metric.name]] <- expand.grid(avparams.lst, stringsAsFactors=FALSE)
        }
    }
        
    ## If there are multiple histograms and we're going to plot them with the same set of bins for visual comparison, we need to calculate the bin break points
    if (same.axes.for.all  && (length(hs.ord) > 1 || (length(metric.hmap.lst) > 0 && nrow(metric.hmap.lst[[metric]]) > 1))) {
        if (identical(breaks, "Sturges")) stop(cw("If you plot all histograms with the same axis range, you must also pass the number of bins as parameter 'breaks'", final.cr=FALSE))
        if (length(metric) > 1) stop(cw("You can't use the same axis for different metrics, plot one metric only or set same.axes.for.all to FALSE"), final.cr=FALSE)
        
        ## Need to run through all of these to get the range of values
        xaxis.range <- NULL
        for (hs.name in names(hs[hs.ord])) {
            hmap <- metric.hmap.lst[[metric]]
            for (hmap.idx in 1:nrow(hmap)) {
                xvals <- eval(hme[[metric.name]][["expr"]])
                if (include.missing.hulls && length(xvals) < nrow(hs[[hs.name]][["pts"]])) {
                        xvals <- c(xvals, rep(hme[[metric.name]][["nhv"]], nrow(hs[[hs.name]][["pts"]]) - length(xvals)))
                }
                xvals <- xvals[!is.na(xvals)]
                if (!is.null(xvals)) {
                    xvals.range <- range(xvals)
                    xaxis.range <- c(min(xaxis.range[1], xvals.range[1]), max(xaxis.range[2], xvals.range[2]))
                } 
            }
        }
        breaks <- seq(from=xaxis.range[1], to=xaxis.range[2], length.out=breaks+1)
    }

    if (is.null(figs.per.page)) {
        if (length(hs.ord) == 1 || sum(sapply(metric.hmap.lst, nrow)) == 1) {
            ## if there is one metric, or one hs, plots-per-page is their product
            figs.per.page <- length(hs.ord) * sum(sapply(metric.hmap.lst, nrow))
        } else {
            ## We have multiple metrics and multiple hs. Set plots per page to num metrics because we loop thru those last
            figs.per.page <- sum(sapply(metric.hmap.lst, nrow))
        }
    }
    if (is.null(png.dir) && lo.save) par(mfrow = n2mfrow(figs.per.page), mar=mar, mgp=mgp, bg="white")
    
    for (hs.name in names(hs[hs.ord])) {

        for (metric.name in metric) {

            ## Make sure all of the required parameters were passed to the function, *and* set up a data frame
            ## containing all permutation(s) of the hull metric auxillary parameters (hmap) passed
            avparams.lst <- list()
            for (avparam in hme[[metric.name]][["req.ap"]]) {
                if (avparam %in% names(ddd.lst)) {
                    # Make sure these values have been actually computed
                    if (FALSE %in% (ddd.lst[[avparam]] %in%  hs[[hs.name]][["hm.params"]][[avparam]])) stop(paste("Value of ", avparam, " not found in hullset", sep=""))
                    avparams.lst[[avparam]] <- unique(c(avparams.lst[[avparam]], ddd.lst[[avparam]]))
                } else if (avparam %in% names(hmap.passed)) {                
                    # Make sure these values have been actually computed
                    if (FALSE %in% (hmap.passed[[avparam]] %in%  hs[[hs.name]][["hm.params"]][[avparam]])) stop(paste("Value of ", avparam, " not found in hullset", sep=""))
                    avparams.lst[[avparam]] <- unique(c(avparams.lst[[avparam]], hmap.passed[[avparam]]))
                } else {
                    # Try using the metric default value
                    if (is.null(hme[[metric.name]][["req.ap.def"]])) {
                        stop(paste("Required parameter missing: ", avparam, sep=""))
                    } else if (identical(hme[[metric.name]][["req.ap.def"]][[avparam]], "all")) {
                        ## Get all of the hull parameters that have been run
                        avparams.lst[[avparam]] <- unique(c(avparams.lst[[avparam]], hs[[hs.name]][["hm.params"]][[avparam]]))
                    } else {
                        avparams.lst[[avparam]] <- unique(c(avparams.lst[[avparam]], hme[[metric.name]][["req.ap.def"]]))
                    }

                }
            }
            if (length(avparams.lst) == 0) {
                hmap <- as.data.frame(NA)
            } else {
                ## Convert the list of auxillary variables into a data frame containing all permutations of the variables
                hmap <- expand.grid(avparams.lst, stringsAsFactors=FALSE)
            }
            
            for (hmap.idx in 1:nrow(hmap)) {
                xvals <- eval(hme[[metric.name]][["expr"]])
                if (is.null(xvals)) {
                    cat("  Can not find ", metric.name, " values for: ", hs.name, "\n", sep="")
                } else {
                    metric.label.sub <- NULL
                    if (include.missing.hulls && length(xvals) < nrow(hs[[hs.name]][["pts"]])) {
                        xvals <- c(xvals, rep(hme[[metric.name]][["nhv"]], nrow(hs[[hs.name]][["pts"]]) - length(xvals)))
                        if (is.na(hme[[metric.name]][["nhv"]])) {
                            metric.label.sub <- "\nmissing hulls omitted"
                        } else {
                            metric.label.sub <- paste("\nmissing hulls indicated by ", hme[[metric.name]][["nhv"]], sep="") 
                        }
                    }

                    if (is.na(hmap[hmap.idx,1])) {
                        hmap.fn <- ""
                        subtitle <- NULL                        
                    } else {
                        hmap.fn <- paste(".", sapply(1:length(hmap), function(j) paste(names(hmap)[j], ".", hmap[hmap.idx,j], sep="")), collapse=".", sep="")
                        if (hmap.in.title) {
                            subtitle <- paste(unlist(sapply(metric.name, function(myaxis) sapply(hme[[myaxis]][["req.ap.subtitle"]], function(x) eval(x)))), collapse="; ", "\n", sep="")
                        } else {
                            subtitle <- NULL
                        }
                    }

                    if (!is.null(png.dir)) {
                        fn <- file.path(png.dir, paste(hs.name, ".", metric.name, hmap.fn, ".hist.png", sep=""))
                        if (file.exists(fn) && !png.overwrite) stop(paste(fn, "exists"))
                        png(filename=fn, height=png.height, width=png.width, pointsize=png.pointsize)
                        if (lo.save) par(mar=mar, mgp=mgp, bg="white")
                    }
                    
                    metric.label <- paste(if (ufat) eval(hme[[metric.name]][["ufat"]]) else metric.name, sep="")
    
                    if (title.show) {
                        if (title.two.id) {
                            title.use <- paste(hs[[hs.name]][["id"]], " and ", substr(hmap[hmap.idx,1], 1, as.numeric(regexpr(".", hmap[hmap.idx,1], fixed=T))-1), sep="")
                        } else {
                            if (is.null(title)) {
                                title.use <- paste(if (hs.name.in.title) paste(hs.name, "\n", sep="") else "", subtitle, metric.label, sep="")
                            } else {
                                title.use <- title
                            }
                        }
                    } else {
                        title.use <- ""
                    }
    
                    if (lo.save) opar <- par(mfrow = n2mfrow(figs.per.page), mar=mar, mgp=mgp, oma=c(0,0,0,0))
                    
                    ## If xvals contains a single value, manually set the breaks and xlim
                    if ((!same.axes.for.all || figs.per.page==1) && length(unique(xvals))==1) {
                        hist(xvals, breaks=unique(xvals) + c(-0.5, 0.5), xlim=unique(xvals) + c(-2, 2), xlab=paste(metric.label, metric.label.sub, sep=""), col=col, main=title.use)
                    } else {
                        hist(xvals, breaks=breaks, xlab=paste(metric.label, metric.label.sub, sep=""), col=col, main=title.use)                                                        
                    }
                    
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
                    
                    if (!is.null(png.dir)) {
                        invisible(dev.off())                
                        cat(" - ", fn, "\n", sep="")
                    }
                    
                    res[[paste(metric.name, ".", hs.name, hmap.fn, sep="")]] <- xvals
                    
                }
            }
            
        }
    }
    
    #class(res) <- "locoh.lhshisto"
    return(invisible(res))

}
