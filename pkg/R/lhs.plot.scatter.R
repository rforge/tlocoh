#' Create scatterplot of hull metrics
#'
#' Multi-purpose scatterplot function for the hull metrics in a LoCoH-hullset
#'
#' @param lhs A LoCoH-hullset object
#' @param id The name(s) of the individual(s) to include in the plot. Character vector or comma-delimited character.
#' @param k The k value(s) of the hullset(s) to include in the plot. Numeric vector or comma-delimited character object.
#' @param r The r value(s) of the hullset(s) to include in the plot. Numeric vector or comma-delimited character object.
#' @param a The a value(s) of the hullset(s) to include in the plot. Numeric vector or comma-delimited character object.
#' @param s The s value(s) of the hullset(s) to include in the plot. Numeric vector or comma-delimited character object.
#' @param hs.names The name(s) of saved hullsets to include in the plot.
#' @param x.axis The name of a hull metric (see \code{\link{hm.expr}})
#' @param y.axis The name of a hull metric
#' @param limx The lower and upper limits of the x-axis. Two-element numeric vector.
#' @param limy The lower and upper limits of the y-axis. Two-element numeric vector.
#' @param trans.x The name of a function that will be used to transform the x-axis values. Can be any R function (e.g., "log", "sqrt"), also "square" and "cube"
#' @param trans.y The name of a function that will be used to transform the y-axis values
#' @param jiggle.x "auto" or a numeric value which will be used to add a normally distributed stochastic value (mean 0, sd=jiggle.x) to the x-axis values for better visualization of the number of points in each group (helpful when the x-values take on discrete values)
#' @param jiggle.y "auto" or a numeric value which will be used to add a normally distributed stochastic value (mean 0, sd=jiggle.y) to the y-axis values
#' @param title The title to be displayed. Character. If NULL a title will be constructed.
#' @param title.show Whether to show the title. T/F.
#' @param title.hs.name.include Whether to include the hullset name as part of the title. Ignored if title is passed. T/F.
#' @param title.axes Whether to include the names of the hull metrics on the axes as part of the subtitle. Ignored if title is passed. T/F.
#' @param title.two.id An ad-hoc way to construct a title consisting of the ids of the hullset and hs2
#' @param filter NULL or a list of filter parameters. See details.
#' @param filter.label.in.subtitle Whether to add the filter label to the plot subtitle. Ignored if title is passed or filter is NULL. T/F.
#' @param filter.sampsize.in.subtitle Whether to add the filter sample size to the plot subtitle. Ignored if title is passed or filter is NULL. T/F.
#' @param filter.col.use Whether the points will be displayed with the color(s) saved in the filter (overriding the 'col' parameter). Ignored if filter is NULL. T/F.
#' @param filter.axes.uniform Whether the upper and lower limits of the axes for each plot will be set as the limit for the entire combined dataset. Ignored if filter is NULL or values are passed for limx or limy. T/F.
#' @param col A single color value, or a vector of color values of the same length as the number of hulls, or "spiral" (see additional parameters below). Ignored if filter.col.use=T or something is passed for hsp (in which case col will be extracted from hsp).
#' @param bg Background color
#' @param lo.colors.set Whether to set the background, foreground color on the plot device. T/F.
#' @param lo.bg.as.box Whether to display the background color as a box on the plot (needed by some wrapper function that want the axes and margins to be a different color). This overrides the setting of lo.colors.set. T/F.
#' @param mar The plot margins. A four item numeric vector.
#' @param mgp The distance away from the edge of the plot for the 1) label, 2) tick marks, and 3) axis line. A three-item numeric vector.
#' @param lo.save Whether to save and reset the plot device margin settings (some wrapper functions that call this function don't want device settings reset). T/F.
#' @param lo.margins.set Whether to save and reset the plot device margin settings (some wrapper functions that call this function don't want device settings reset). T/F.
#' @param desc Which side to display automatically generated desciptive text (e.g. caption). 0=none, 1=bottom, 3=top.
#' @param cex.desc The expansion factor for the descriptive text. Numeric value.
#' @param col.desc The color of the descriptive text. Color value.
#' @param sat.base A number between 0 and 1 for the base staturation (how much of the color is present) of the center color (s=0.4 gives pastels), ramping up to 1 (full color) at edges.
#' @param val.base A number between 0 and 1 controlling how much black is in the center color (0 is total black), ramping out to no blackness at the edges
#' @param hue.offset A number between 0 and 2*pi for the color wheel rotation (in radians), which controls the color of points directly to the right of the center.
#' @param center.method Determines how the center of the color will be computed. "bbox" = the center of the bounding box of the full range of points; "mean" = the mean of the data splot
#' @param cex Expansion factor for the points on the scatterplot
#' @param type The type of plot: 'p'=points only, 'l'=line, 'b'=both
#' @param ufat Whether to substitute user-friendly axis titles. T/F.
#' @param figs.per.page The number of plots per page
#' @param add Whether to add to the current plot device. T/F.
#' @param regions Determines whether the user will be prompted to draw regions (polygons) on the scatterplot window with the mouse, that will be saved as part of the scatterplot objecr returned. Values can be 1) a number whereby the user will be prompted to create N regions with randomly assigned colors, or 2) a vector of color values in which case the regions will be assigned those colors.
#' @param prompt.labels Whether the user should be prompted to also enter labels for each of the regions drawn. Ignored if regions = NULL. T/F.
#' @param hsp Either the index(s) of a hull scatterplot(s) saved in lhs (use summary() command to which how many hsp objects have been saved), or a list of objects of class locoh.hsp. When passed, the parameters in hsp will be used to create the scatterplot
#' @param hsp.reg.col Whether to use the colors assigned to the regions in hsp. Ignored if hsp is NULL. T/F.
#' @param hsp.reg.out Whether to display the outline of the regions in hsp. Ignored if hsp is NULL. T/F.
#' @param hsp.reg.lbl Whether to display the labels of the regions in hsp. Ignored if hsp is NULL. T/F.
#' @param hsp.override Whether the parameters in hsp should override other parameters passed. Ignored if hsp is NULL. T/F.
#' @param png.fn The path and name of the PNG file to create (instead of displaying in a plot window).
#' @param png.dir The directory for a PNG file (filename will be constructed automatically). Ignored if png.fn is passed.
#' @param png.dir.make Whether to create png.dir if it doesn't exist. T/F.
#' @param png.width The width of the PNG image. Ignored if png.fn is passed.
#' @param png.height The height of the PNG image. Ignored if png.fn is passed.
#' @param png.pointsize The pointsize (in pixels) for the PNG image (increase to make labels appear larger). Equivalent to the height or width of a character in pixels.
#' @param png.fn.pre A prefix that will be used in the construction of the PNG filename. Ignored if png.fn is passed.
#' @param png.fn.mid A mid-fix that will be used in the construction of the PNG filename. Ignored if png.fn is passed.
#' @param png.fn.suf A suffix that will be used in the construction of the PNG filename. Ignored if png.fn is passed.
#' @param png.exists What to do if a PNG with the same filename already exists: "overwrite", "skip", or "abort"
#' @param status Whether to show messages. T/F.
#' @param panel.num A number or letter to display in the upper left hand corner of the plot, used when the plot will be part of a multi-frame graphic (as in publications). Character.
#' @param panel.num.inside.plot Whether to display panel.num inside the plot area itself (as opposed to the title area). T/F.
#' @param hmap A named list of hull metric auxillary parameters, the name of each list element is the name of the variable.
#' @param hmap.in.subtitle Include the hmap value(s) in the plot subtitle. T/F
#' @param check.ap.value.in.hmparams Check to make sure that hull metrics have been computed for the hull metric auxillary parameter values passed (either as separate arguments or in \code{hmap})  
#' @param ... Other parameters, including any auxillary parameters required by certain hull metrics
#'
#' @details
#' This function has two main purposes.
#' 1) To make scatterplot graphics either in a plot window or PNG file
#' 2) To create a list of objects of class "locoh.hsp", which are basically a collection of the parameters that were used to create the scatterplot including any manually drawn regions created by the user (see \code{regions}).
#'
#' Note that hull metrics must already have been computed. Several hull metrics are 'automatically' computed when the
#' hullset is defined (e.g., hull area, number of enclosed points). Other hull metrics must be created separately with functions
#' such as \code{\link{lhs.ellipses.add}} and \code{\link{lhs.visit.add}}. Auxillary parameters required for hull metrics can be passed either 
#' as individual parameters (e.g., \code{ivg=3600*12}) or as a list element (e.g., \code{hmap=list(ivg=86400)}).
#' 
#' \code{filter}, if passed, will create scatterplots for subsets of hulls. For this to work, \code{filter} must be a list
#' whose elements are named lists with the following elements: \emph{idx} = a vector of the indices of the hulls (in the hulls SpatialPolygonsDataFrame),
#' \emph{label} = a label for the subset, and \emph{col} = a color value. \code{\link{lhs.filter.hsp}} can be used to
#' create a filter list based on manually drawn regions of a hull scatterplot, and \code{\link{lhs.filter.anv}} can be used
#' to create a filter based on ranges of values of an ancillary variable. The arguments \code{filter.sampsize.in.subtitle},
#' \code{filter.col.use}, and \code{filter.axes.uniform} control how the subsets are treated in the scatterplot(s).
#' 
#' @return If \code{png.fn} or \code{png.dir} is passed, the plots are exported to PNG file(s) and the function returns a list of file names and image dimensions (in pixels).
#' Otherwise returns a named list whose element(s) are of class \emph{locoh.hsp}. This list of \emph{locoh.hsp} objects can then be 'permanently' saved in 
#' the LoCoH-hullset object using \code{\link{lhs.hsp.add}}, and/or fed into other functions such as \code{\link{plot.locoh.lhs}} (to symbolize hull points).
#'
#' @seealso \code{\link{lhs.plot.scatter.auto}}, \code{\link{hm.expr}}, \code{\link{lhs.hsp.add}}, \code{\link{lhs.filter.hsp}}, \code{\link{lhs.filter.anv}}
#' @export

lhs.plot.scatter <- function(lhs, id=NULL, k=NULL, r=NULL, a=NULL, s=NULL, hs.names = NULL, 
                x.axis=NULL, y.axis=NULL, limx=NULL, limy=NULL, trans.x=NULL, trans.y=NULL, jiggle.x="auto", jiggle.y="auto", 
                filter=NULL, filter.label.in.subtitle=TRUE, filter.sampsize.in.subtitle=TRUE, filter.col.use=TRUE, filter.axes.uniform=TRUE,
                title=NULL, title.show=TRUE, title.hs.name.include=TRUE, title.two.id=FALSE, title.axes=TRUE, 
                col=c("gray50", "spiral")[1], 
                bg=NULL, mar=c(4, 3.2, if (title.show) (if (title.hs.name.include) 3.9 else 3.2) else 0.5, 0.5), mgp=c(2.1, 0.8, 0),
                lo.save=TRUE, lo.margins.set=TRUE, lo.colors.set=TRUE, lo.bg.as.box=FALSE, 
                sat.base=NULL, val.base=NULL, hue.offset=NULL, center.method=c("bbox","mean")[2],
                cex=0.6, type=c("p","l","b")[1], ufat=NULL, 
                figs.per.page=1, add=FALSE, 
                regions=NULL, prompt.labels=TRUE, 
                hsp=NULL, hsp.reg.col=TRUE, hsp.reg.out=FALSE, hsp.reg.lbl=FALSE, hsp.override=TRUE, 
                png.fn=NULL, png.dir=NULL, png.dir.make=TRUE, png.width=800, png.height=png.width, png.pointsize=12+(png.width-480)/80,
                png.fn.pre=NULL, png.fn.mid=NULL, png.fn.suf=NULL, png.exists=c("overwrite", "skip", "abort")[1], status=TRUE,
                desc=c(NONE<-0, BOTTOM<-1, TOP<-3)[ifelse(figs.per.page==1,2,1)], cex.desc=0.8, col.desc=NULL, 
                panel.num=NULL, panel.num.inside.plot=!title.show, hmap=NULL, hmap.in.subtitle=TRUE, 
                check.ap.value.in.hmparams=TRUE, ...) {

    #CONSIDER OPTION FOR A THIRD AXIS (3D), AS WELL AS A SINGLE-SERIES SUMMARY (E.G., HISTOGRAM)
                                                                    
    ## Filter, if passed, is a list of filter parameters. The code will loop through these and generate scatterplots for each subsets of points 
    ## Each element of the filter list must be another list with the following elements:
    ##     $idx - a vector of the indices of the hulls 
    ##     $label - a label. If filter.label.in.subtitle=T, this label will be added to the plot subtitle
    ##     $col - a single color value or vector of color values. If filter.col.use=T the points will be displayed with this color(s), overriding the 'col' parameter
    ## If filter.axes.uniform=T, the upper and lower limits of the axes for each plot will be set as the limit for the entire unfiltered dataset 
    ## (ignored if values passed for limx and limy)

    ## Spiral and reg color schemes are used primarily as a legend for a map of the hull parent points
    ## hue.offset is a value in radians [0..2pi] for the rotation which controls the initial color of the color wheel (the color of points directly to the right of the centroid)

    ## Regions. Determines whether the user will be prompted to draw regions (polygons) on the scatterplot, that will be saved as part of the scatterplot. Can be
    ##  1) a number in which case the user will be prompted to create N regions that will be given randomly assigned colors
    ##  2) a vector of color values
    
    if (!inherits(lhs, "locoh.lhs")) stop("lhs should be of class \"locoh.lhs\"")

    if (add && (!is.null(png.dir) || !is.null(png.fn))) stop("You can't use the add option when creating a PNG file")
    if (identical(col,"hsp")) {
        if (is.null(hsp)) stop("To color points by hsp, you must specify the hsp")
        cat("what if hsp.override=T? \n")
    }
    
    if (!is.numeric(desc) || desc > 4)  stop("Desc must be 0 (no description) or between and 1 and 4 (side to display description")
    if (desc != 0 && figs.per.page > 1) stop("You can't display descriptive text if figs.per.page > 1. Set desc=0.")
    if (desc != 0 && add) stop("You can't display descriptive text when adding points to an existing plot. Set desc=0.")
    
    ## See if the output directory exists
    if (is.null(png.fn) && !is.null(png.dir) && !file.exists(png.dir)) {
        if (png.dir.make) {
            dir.made <- dir.create(png.dir)
            if (!dir.made) stop(paste("Couldn't make output folder ", png.dir, sep=""))            
        } else {
            stop(paste(png.dir, " doesn't exist and png.dir.make is False, so can not continue."))
        }
    }
                                             
    ## Make a copy of the non-null parameters passed. These values might be restored later on 
    ## if a saved hsp is brought up and hsp.override.passed.parameters = F
    if (!is.null(hsp) && !hsp.override) {
        params.passed <- list()
        params.names <- c("x.axis", "y.axis", "limx", "limy", "trans.x", "trans.y", "jiggle.x", "jiggle.y", "title", "ufat", 
                              "bg", "cex", "col", "hue.offset", "sat.base", "val.base", "center.method")
        for (param.name in params.names) params.passed[[param.name]] <- get(param.name)
    }
    
    ## Give default values to a few parameters
    ## (these are not given default values in the function definition so that they can override the values saved in hsp objects, see above)
    if (is.null(x.axis)) x.axis <- "area"
    if (is.null(y.axis)) y.axis <- "nep"
    if (is.null(bg)) bg <- "white"
    if (is.null(ufat)) ufat <- TRUE
    ##if (is.null(sat.base)) sat.base <- 0.5
    
    ## Do some error check on regions and interpret value passed
    if (is.null(regions)) {
        regions.make <- FALSE
        regions.num <- 0
    } else {
        if (!is.null(png.dir) || !is.null(png.fn)) stop("If drawing regions, you can't export the plot to PNG")
        #if (!is.null(regions)) stop("You can't draw new regions if you display an existing hsp")
        
        err.msg <- "'regions' can be a single number (the number of regions you want to draw), or a vector of color values"
        if (is.numeric(regions)) {
            if (length(regions) > 1) stop(err.msg)
            if (regions==0) stop(err.msg)
            regions.num <- regions
            regions.col <- NULL
        } else {
            regions.num <- length(regions)
            regions.col <- as.character(regions)
        }
        regions.make <- TRUE
        
        ## Set regions to NULL, because we're done with it and there's a possibility that a different 'regions' object will be restored from a saved hsp
        regions <- NULL
    }

    if (is.null(id) && is.null(r) && is.null(k) && is.null(a) && is.null(s) && is.null(hs.names)) {
        hs <- lhs
    } else {    
        hs <- lhs.select(lhs, id=id, r=r, k=k, a=a, s=s, hs.names=hs.names)
    }
    if (length(hs)==0) stop("No hullsets found matching those criteria")

    ## Error checking on filter
    if (!is.null(filter)) {
        if (length(hs) > 1) {
            ## Check if all hull sets have the same number of hulls. If not, then filter is not going to work
            if (length(unique(sapply(hs, function(h) length(h[["hulls"]])))) > 1) {
                stop("There are different numbers of hulls in the hull sets, so a single filter wont work")
            }
        }
        if (!is.list(filter) || !is.list(filter[[1]])) stop("filter must be a list of lists")
    }

    ## Get the list of expressions for hull metrics
    hme <- hm.expr(names.only=FALSE)

    ## Do some error checking on hsp, assign values for all saved parameters
    if (is.null(hsp)) {
        if (!x.axis %in% names(hme)) stop(cw(paste("x.axis must be one of the following: ", paste(sort(names(hme)), collapse=", ", sep=""), sep="")), final.cr=F)
        if (!y.axis %in% names(hme)) stop(cw(paste("y.axis must be one of the following: ", paste(sort(names(hme)), collapse=", ", sep=""), sep="")), final.cr=F)
        hsp <- NA
    } else {
        err.msg <- "hsp must be either the index(s) of saved scatter plot(s), or a list of objects of class 'locoh.hsp'"
        if (is.numeric(hsp)) {
            if (min(hsp) < 1 || max(hsp) > sapply(hs, function(x) length(x$hsp))) stop(err.msg)
        } else if (is.list(hsp)) {
            if (FALSE %in% (sapply(hsp, function(x) inherits(x, "locoh.hsp")))) stop(err.msg)
        } else {
             stop(err.msg)
        }
    }
    
    plots.made <- 0
    res <- list()
    col.passed <- col
    ddd.lst <- list(...)
    
    ## Start nested loop with hsp objects
    for (hsp.idx in 1:length(hsp)) {
        
        if (identical(hsp, NA) || is.numeric(hsp)) {
            ## There is no hsp object or it is saved in lhs and we have yet to retrieve it
            ## So we're going to loop through all hull sets. Sort hull sets by id and then s
            hs.ord <- order(sapply(hs, function(x) x[["id"]]), sapply(hs, function(x) x$s))
            hs.names.to.loop.thru <- names(hs[hs.ord])
        } else {
            ## Restore the parameters for this function saved in hsp
            hsp.use <- hsp[[hsp.idx]]
            for (var.name in names(hsp.use)) assign(var.name, hsp.use[[var.name]])
            if (!regions.make) regions.num <- length(regions)
            hs.names.to.loop.thru <- hs.name
         }   

        for (hs.name in hs.names.to.loop.thru) {
            
            if (is.numeric(hsp)) {
                if (is.null(hs[[hs.name]][["hsp"]][[ hsp[hsp.idx] ]])) {
                    stop(paste(hs.name, " doesn't have a saved scatter plot with index ", hsp[hsp.idx], sep=""))
                } else {
                    hsp.use <- hs[[hs.name]][["hsp"]][[hsp[hsp.idx]]] 
                    for (var.name in names(hsp.use)) assign(var.name, hsp.use[[var.name]])
                    if (!regions.make) regions.num <- length(regions)
                }
            }
            
            ## Restore non-null values of parameters that were passed, if needed
            if (!is.null(hsp) && !hsp.override) {
                if (identical(col.passed, "hsp")) {
                    ## Before restoring the parameters that were originall passed, 
                    ## we need to take note of the axes metrics and transformations from hsp, which we'll need to create the colrs
                    hsp.x.axis <- x.axis
                    hsp.y.axis <- y.axis
                    hsp.trans.x <- trans.x
                    hsp.trans.y <- trans.y
                }
                for (param.name in names(params.passed)) assign(param.name, params.passed[[param.name]])
            }
            
            ## Create the 'opposite' color from the background color to use as the color for labels and axes
            tcc <- textContrastColor(bg)
            if (is.null(col.desc)) col.desc <- if (bg =="white" || lo.bg.as.box) "darkgreen" else tcc
            
            if (is.null(filter)) {
                filter.use <- list(list(idx=1:length(hs[[hs.name]][["hulls"]]), label=NA, col=NA ))
                filter.label.in.subtitle <- FALSE
                filter.sampsize.in.subtitle <- FALSE                
                filter.col.use <- FALSE
            } else {
                filter.use <- filter
            }
            
            ## Make sure all of the required parameters were passed to the function, *and* set up a data frame
            ## containing all permutation(s) of the hull metric auxillary parameters (hmap) passed
            avparams.lst <- c(list(), hmap)
            for (axis.metric in c(x.axis, y.axis)) {
                for (avparam in hme[[axis.metric]][["req.ap"]]) {
                    if (!avparam %in% names(ddd.lst)) {
                        if (is.null(hme[[axis.metric]][["req.ap.def"]])) {
                            stop(paste("Required parameter missing: ", avparam, sep=""))
                        } else if (identical(hme[[axis.metric]][["req.ap.def"]][[avparam]], "all")) {
                            ## Get all of the hull parameters that have been run
                            avparams.lst[[avparam]] <- unique(c(avparams.lst[[avparam]], hs[[hs.name]][["hm.params"]][[avparam]]))
                        } else {
                            avparams.lst[[avparam]] <- unique(c(avparams.lst[[avparam]], hme[[axis.metric]][["req.ap.def"]]))
                        }
                    } else {
                        # Make sure these values have been actually computed
                        if (check.ap.value.in.hmparams) {
                            print("Chekc this");browser()
                            if (FALSE %in% (ddd.lst[[avparam]] %in%  hs[[hs.name]][["hm.params"]][[avparam]])) stop(paste("Value of ", avparam, " not found", sep=""))
                        }
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

            if (identical(col.passed, "hsp") && !hsp.override) stop("need to fix what happens if not hsp.override")

            for (hmap.idx in 1:nrow(hmap)) {
                
                ## Compute the the x and y values
                xvals.all <- eval(hme[[x.axis]][["expr"]])
                yvals.all <- eval(hme[[y.axis]][["expr"]])
    
                ## See if these hull metrics were found
                if (length(xvals.all)==0) {
                    cat("  ", hs.name, ": Could not find '", x.axis, "' values for x-axis", "\n", sep="")
                } else if (length(yvals.all)==0) {
                    cat("  ", hs.name, ": Could not find '", y.axis, "' values for y-axis", "\n", sep="")
                } else {

                    ## Transform the x values if needed
                    if (is.null(trans.x)) {
                        trans.x.str <- ""
                    } else {
                        trans.x.str <- paste(trans.x, " of ", sep="")
                        xvals.all <- get(trans.x)(xvals.all)
                    }
    
                    ## Transform the y values if needed
                    if (is.null(trans.y)) {
                        trans.y.str <- ""
                    } else {
                        trans.y.str <- paste(trans.y, " of ", sep="")
                        yvals.all <- get(trans.y)(yvals.all)
                    }
    
                    ## If we're going to use colors from hsp, calculate those metrics also
                    if (identical(col.passed, "hsp") && !hsp.override) {
                        ## Compute the the x and y values that will be used for symbology
                        xvals.4hspcol.all <- eval(hme[[hsp.x.axis]][["expr"]])
                        yvals.4hspcol.all <- eval(hme[[hsp.y.axis]][["expr"]])
                        if (!is.null(hsp.trans.x)) xvals.4hspcol.all <- get(hsp.trans.x)(xvals.4hspcol.all)
                        if (!is.null(hsp.trans.y)) yvals.4hspcol.all <- get(hsp.trans.y)(yvals.4hspcol.all)
                    }
                    
                    ## If we're going to loop through a filter, and filter.axes.uniform=T, save the axes limits 
                    if (length(filter) > 1 && filter.axes.uniform) {
                        if (is.null(limx)) limx <- range(xvals.all)
                        if (is.null(limy)) limy <- range(yvals.all)
                    }

                    for (filter.use.idx in 1:length(filter.use)) {
                        filt.idx <- filter.use[[filter.use.idx]][["idx"]]
                        
                        ## Filter if needed
                        if (is.null(filter)) {
                            xvals <- xvals.all
                            yvals <- yvals.all
                            if (identical(col.passed, "hsp") && !hsp.override) {
                                xvals.4hspcol <- xvals.4hspcol.all
                                yvals.4hspcol <- yvals.4hspcol.all
                            }
                        } else {
                            xvals <- xvals.all[filt.idx]
                            yvals <- yvals.all[filt.idx]
                            if (identical(col.passed, "hsp") && !hsp.override) {
                                xvals.4hspcol <- xvals.4hspcol.all[filt.idx]
                                yvals.4hspcol <- yvals.4hspcol.all[filt.idx]
                            }
                        }
        
                        ## Jiggle the x values if needed
                        if (is.numeric(jiggle.x)) {
                            jiggle.x.use <- jiggle.x
                        } else if (jiggle.x=="auto") {
                            jiggle.x.use <- hme[[x.axis]][["auto.jiggle"]]
                        } else {
                            jiggle.x.use <- 0
                        }
                        if (jiggle.x.use > 0) xvals <- xvals + rnorm(length(xvals), mean=0, sd=jiggle.x.use)
                        
                        ## Jiggle the x values if needed
                        if (is.numeric(jiggle.y)) {
                            jiggle.y.use <- jiggle.y
                        } else if (jiggle.y=="auto") {
                            jiggle.y.use <- hme[[y.axis]][["auto.jiggle"]]
                        } else {
                            jiggle.y.use <- 0
                        }
                        if (jiggle.y.use > 0) yvals <- yvals + rnorm(length(yvals), mean=0, sd=jiggle.y.use)
                        
                        ## Prepare main title
                        if (is.null(title)) {
                            if (title.two.id) {
                                main.title <- paste(hs[[hs.name]][["id"]], " and ", substr(hmap[hmap.idx,1], 1, as.numeric(regexpr(".", hmap[hmap.idx,1], fixed=T))-1), sep="")
                            } else {
                            
                                ## Prepare the subtitle string. May contain a filter label
                                subtitle <- ""
    
                                if (filter.label.in.subtitle) subtitle <- paste(filter.use[[filter.use.idx]][["label"]], sep="")
                                if (filter.sampsize.in.subtitle) subtitle <- paste(subtitle, if (nchar(subtitle)==0) NULL else " (", "n=", 
                                                                                      length(filter.use[[filter.use.idx]][["idx"]]), 
                                                                                      if (nchar(subtitle)==0) NULL else ")", sep="")
                                if (hmap.in.subtitle) {
                                    hmap.subtitles <- paste(unlist(sapply(c(x.axis, y.axis), function(myaxis) sapply(hme[[myaxis]][["req.ap.subtitle"]], function(x) eval(x)))), collapse="; ", sep="")
                                    if (nchar(hmap.subtitles) > 0 ) subtitle <- paste(subtitle, if (nchar(subtitle)==0) NULL else "\n", hmap.subtitles, sep="")
                                }
                                    
                                main.title <- ""
                                if (title.hs.name.include) main.title <- hs.name  ##paste(main.title, hs.name, "\n", sep="")
                                if (nchar(subtitle) > 0) main.title <- paste(main.title, if (nchar(main.title) > 0) "\n" else "", subtitle, sep="")
                                if (title.axes) main.title <- paste(main.title, if (nchar(main.title) > 0) "\n" else NULL, if (ufat) eval(hme[[x.axis]][["ufat"]]) else x.axis, " vs. ", if (ufat) eval(hme[[y.axis]][["ufat"]]) else y.axis, sep="")
                            }
                        } else {
                            main.title <- title
                        }
                        
                        ## Prepare the colors
                        if (filter.col.use) {
                            ## This overrides everything else
                            col.pts <- filter.use[[filter.use.idx]][["col"]]
                        } else if (identical(col,"spiral")) {
                            if (!is.null(hue.offset) && (hue.offset < 0 || hue.offset > 2*pi)) stop("hue.offset should be between 0..2*pi")
                            col.pts <- hsp.col.spiral(x=xvals, y=yvals, hue.offset=hue.offset, sat.base=sat.base, val.base=val.base, center.method=center.method)
                            ## If we extracted parameter values from a saved hsp object that contains regions (regions != null), then over-ride the colors of points in those regions
                            if (!is.null(regions) && hsp.reg.col) col.pts <- hsp.col.reg(x=xvals, y=yvals, regions=regions, col=col.pts)
                        } else if (identical(col,"none")) {
                            col.pts <- tcc
                            if (!is.null(regions) && hsp.reg.col) col.pts <- hsp.col.reg(x=xvals, y=yvals, regions=regions, col=col.pts)
                            
                        } else if (identical(col.passed, "hsp") && !hsp.override) {
                            if (identical(hsp.use$col, "spiral")) {
                                col.pts <- hsp.col.spiral(x=xvals.4hspcol, y=yvals.4hspcol, hue.offset=hsp.use$hue.offset, sat.base=hsp.use$sat.base, val.base=hsp.use$val.base, center.method=hsp.use$center.method)
                            } else {
                                col.pts <- hsp.use$col
                            }
                            if (!is.null(hsp.use$regions)) col.pts <- hsp.col.reg(x=xvals.4hspcol, y=yvals.4hspcol, regions=hsp.use$regions, col=col.pts)
                        } else {
                            col.pts <- col
                            ## If we extracted parameter values from a saved hsp object that contains regions (regions != null), then over-ride the colors of points in those regions
                            if (!is.null(regions) && hsp.reg.col) col.pts <- hsp.col.reg(x=xvals, y=yvals, regions=regions, col=col.pts)
                        }
                       
                        ## Create the descriptive text
                        x.str <- paste(trans.x.str, if (ufat) eval(hme[[x.axis]][["desc"]]) else x.axis, sep="")
                        y.str <- paste(trans.y.str, if (ufat) eval(hme[[y.axis]][["desc"]]) else y.axis, sep="")
                        hmap.str <- paste(unique(unlist(sapply(c(x.axis, y.axis), function(myaxis) sapply(hme[[myaxis]][["req.ap.desc"]], function(x) eval(x))))), collapse="", sep="")

                        
                        x.jiggle.str <- if (jiggle.x.use == 0) NULL else paste("x values have been 'jiggled' by ", jiggle.x.use, " to better see point density. ", sep="")
                        y.jiggle.str <- if (jiggle.y.use == 0) NULL else paste("y values have been 'jiggled' by ", jiggle.y.use, " to better see point density. ", sep="") 
                        desc.str <- paste("Each point represents a hull (n=", length(xvals), "). On the x-axis is ", x.str, ". On the y-axis is ", y.str, ". ", hmap.str, x.jiggle.str, y.jiggle.str, sep="")

                        oma.vals <- c(0,0,0,0)
                        if (desc != 0) {
                           desc.str.chopped <- chop2plot(desc.str, width=dev.size()[1] * 0.95, cex=cex.desc)
                           oma.vals[desc] <- as.numeric(desc.str.chopped[2])
                       }
    
                        ## Create a file name if needed and open a new PNG device
                        blnContinueWithPlot <- TRUE
                        if (!is.null(png.dir) || !is.null(png.fn)) {
                            
                            ## If each plot gets its own file, and/or this is the first time through, create a filename and open a new PNG device
                            if (figs.per.page==1 || plots.made==0) {
                                
                                if (is.null(png.fn)) {
                                    if (figs.per.page==1) {
                                        fn <- file.path(png.dir, paste(png.fn.pre, hs.name, png.fn.mid, ".", 
                                               if (is.null(trans.x)) "" else paste(trans.x, "-", sep=""), x.axis, ".vs.", 
                                               if (is.null(trans.y)) "" else paste(trans.y, "-", sep=""), y.axis, 
                                               if (is.null(filter)) "" else paste(".filt-", filter.use[[filter.use.idx]][["label"]], sep=""),
                                               if (is.na(hmap[hmap.idx,1])) "" else paste(".", sapply(1:length(hmap), function(j) paste(names(hmap)[j], "-", hmap[hmap.idx,j], sep="")), collapse="", sep=""),
                                               png.fn.suf, ".png", sep=""))

                                        ## if (is.null(filter)) "" else paste(".grp", filter.use.idx, sep=""),

                                        if (file.exists(fn)) {
                                            if (png.exists=="abort") {
                                                stop(paste(fn, "exists"))
                                            } else if (png.exists=="skip") {
                                                blnContinueWithPlot <- FALSE
                                                res <- c(res, list(list(fn=fn, dim=c(png.width, png.height))))
                                            }
                                            
                                        }
                                    } else {
                                        fn <- file.path(png.dir, paste(png.fn.pre, png.fn.mid, 
                                               if (is.null(trans.x)) "" else paste(trans.x, "-", sep=""), x.axis, ".vs.", 
                                               if (is.null(trans.y)) "" else paste(trans.y, "-", sep=""), y.axis,
                                               if (is.na(hmap[hmap.idx,1])) "" else paste(".", sapply(1:length(hmap), function(j) paste(names(hmap)[j], ".", hmap[hmap.idx,j], sep="")), collapse=".", sep=""),
                                               png.fn.suf, ".%02d.png", sep=""))

                                    }
                                
                                } else {
                                    fn <- png.fn
                                }
                                if (blnContinueWithPlot) png(filename=fn, height=png.height, width=png.width, pointsize=png.pointsize, bg=bg)
                            }
                            
                        } 

                        if (blnContinueWithPlot) {
                            if (add) {
                                points(x=xvals, y=yvals, pch=20, type=type, cex=cex, col=col.pts)
                            } else {

                                ## A PNG device has been opened if needed
                                
                                ## Save current device settings on first pass
                                if (plots.made==0 && lo.save) {
                                    opar <- par()[c("mar", "mgp", "oma", "bg", "fg", "col.axis", "col.lab", "col.main", "col.sub")]
                                    on.exit(par(opar))
                                }
                                
                                #if (figs.per.page > 1 && is.null(png.dir) && is.null(png.fn)) par(mfrow=n2mfrow(figs.per.page)
                                if (figs.per.page > 1 && plots.made==0) par(mfrow=n2mfrow(figs.per.page))
                                
                                if (lo.margins.set) {
                                    par(mar=mar, mgp=mgp)
                                    if (max(oma.vals)>0) par(oma=oma.vals)
                                }
                                
                                if (lo.colors.set && !lo.bg.as.box) {
                                    par(bg=bg, fg=tcc, col.axis=tcc, col.lab=tcc, col.main=tcc, col.sub=tcc)
                                }
                                                  
                                xlab <- paste(trans.x.str, if (ufat) eval(hme[[x.axis]][["ufat"]]) else x.axis, sep="")                                                     
                                ylab <- paste(trans.y.str, if (ufat) eval(hme[[y.axis]][["ufat"]]) else y.axis, sep="")
                                
                                if (lo.bg.as.box) {
                                    plot(NULL, xlim=if (is.null(limx)) range(xvals) else limx, ylim=if (is.null(limy)) range(yvals) else limy, 
                                         xlab=xlab, ylab=ylab, main = if (title.show) main.title else NULL)
                                    ## Manually draw a box in the figure area with a colored fill
                                    parea <- par("usr")
                                    polygon(x=c(parea[1], parea[2], parea[2], parea[1], parea[1]), y=c(parea[4], parea[4], parea[3], parea[3], parea[4]), col=bg)
                                    points(x=xvals, y=yvals, type=type, pch=20, cex=cex, col=col.pts)
                                } else {
                                    plot(x=xvals, y=yvals, xlim=limx, ylim=limy, xlab=xlab, ylab=ylab, pch=20, type=type, cex=cex, col=col.pts, 
                                         main = if (title.show) main.title else NULL)
                                }
                                plots.made <- plots.made + 1
                                #print("lets pause");browser()

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
                            }
            
                            ## If we extracted parameter values from a saved hsp object that contains regions, plot the outlines & labels if needed
                            if (!is.null(regions) && (hsp.reg.out || hsp.reg.lbl)) {
                                ##for (i in 1:length(hs[[hs.name]][["spc.reg"]][[spc.reg.idx]][["regs"]])) {
                                for (i in 1:length(regions)) {
                                    col.use <- col2rgb(regions[[i]][["col"]])
                                    col.use <- rgb(red=col.use[1], green=col.use[2], blue=col.use[3], maxColorValue = 255, alpha=0.3 * 255)
                                    pts <- regions[[i]][["poly.pts"]]
                                    if (hsp.reg.out) polygon(pts, col=col.use)
                                    if (hsp.reg.lbl) text(x=mean(range(pts[,1])), y=mean(range(pts[,2])), labels=regions[[i]][["label"]], col=tcc)
                                }
                            }
            
                            ## Add descriptive text in the outer margin
                            if (desc !=0 ) mtext(desc.str.chopped[1], side=desc, outer=TRUE, line=0 + if (desc==1) (as.numeric(desc.str.chopped[2]) - 1.75) else 0, cex=cex.desc, col=col.desc)

                            if (regions.make) {
                                regions2save <- regions.draw(n=regions.num, col=regions.col, draw.reg=TRUE, prompt.labels=prompt.labels)
                            } else {
                                regions2save <- if (regions.num==0) NULL else regions
                            } 
                        
                            ## Create either a hsp object *or* a list of fn objects to return
                            if (is.null(png.dir) && is.null(png.fn)) {
                                ## Pull out the hull metric auxilary variables that were used in this isopleth
                                if (is.na(hmap[hmap.idx,1])) {
                                    hmap.used <- NULL
                                } else {
                                    hmap.used <- as.list(hmap[hmap.idx, , drop=F])
                                    attr(hmap.used, "out.attrs") <- NULL
                                }

                                hsp.new <- list(hs.name=hs.name, x.axis=x.axis, y.axis=y.axis, limx=limx, limy=limy, trans.x=trans.x, trans.y=trans.y, jiggle.x=jiggle.x,
                                                hmap=hmap.used, title=title, ufat=ufat, bg=bg, cex=cex, col=col, hue.offset=hue.offset, sat.base=sat.base, val.base=val.base,
                                                 center.method=center.method, regions=regions2save, xvals.range=range(xvals[!is.na(xvals)]), yvals.range=range(yvals[!is.na(yvals)]))
                                class(hsp.new) <- "locoh.hsp"
                                
                                ## Construct a name for this hsp that doesn't already exist in res
                                i <- 0
                                hsp.new.name <- NULL
                                while (is.null(hsp.new.name) || !is.null(res[[hsp.new.name]])) {
                                    i <- i + 1
                                    hsp.new.name <- paste(if (is.null(trans.x)) x.axis else paste(trans.x, "(", x.axis, ")", sep=""), ".vs.", 
                                                          if (is.null(trans.y)) y.axis else paste(trans.y, "(", y.axis, ")", sep=""),
                                                          if (is.na(hmap[hmap.idx,1])) "" else paste(".", sapply(1:length(hmap), function(j) paste(names(hmap)[j], ".", hmap[hmap.idx,j], sep="")), collapse=".", sep=""),
                                                          if (regions.num == 0) "" else paste(".", regions.num, "reg", sep=""),
                                                          ".", sprintf("%02d", i), sep="")
                                }
                                res[[hsp.new.name]] <- hsp.new
                            } else {
                                ## Close the PNG device
                                if (figs.per.page==1) {
                                    invisible(dev.off())   
                                    fn <- normalizePath(path.expand(fn))
                                    res <- c(res, list(list(fn=fn, dim=c(png.width, png.height), desc=desc.str)))
                                    if (status) cat(" - ", fn, "\n", sep="")
                                    ## Add the filename to the result
                                }
                            }
                        }
                        

                        
                    }   ## for filt.idx in filters
                }
            }
            
        }   ## loop thru hs.name
    } #hsp.idx in 1:length(hsp)
    
    if (figs.per.page > 1 && (!is.null(png.dir) || !is.null(png.fn))) {
        invisible(dev.off()) 
        png.all <- sprintf(fn, 1:ceiling(plots.made/figs.per.page))
        png.all <- normalizePath(path.expand(png.all))
        res <- NULL
        for (png.file in png.all) res <- c(res, list(list(fn=png.file, dim=c(png.width, png.height))))
    }
    return(invisible(res))

}
