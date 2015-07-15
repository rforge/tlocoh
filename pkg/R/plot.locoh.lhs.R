#' Plot a LoCoH-hullset object
#'
#' Multi-purpose plotting function for a LoCoH-hullset object
#'
#' @param x A \link{LoCoH-hullset} object
#' @param lhs Deprecated, use \code{x} instead
#' @param id The names of the individual(s) to include in the plot.
#' @param k The k value(s) of the hullset(s) to include in the plot. Numeric vector or comma-delimited character object.
#' @param r The r value(s) of the hullset(s) to include in the plot. Numeric vector or comma-delimited character object.
#' @param a The a value(s) of the hullset(s) to include in the plot. Numeric vector or comma-delimited character object.
#' @param s The s value(s) of the hullset(s) to include in the plot. Numeric vector or comma-delimited character object.
#' @param hs.names The name(s) of saved hullsets to include in the plot
#' @param iso Whether to display isopleths. T/F.
#' @param rast Whether to display rasterized isopleths. T/F.
#' @param hulls Whether to display hulls. T/F.
#' @param hpp Whether to display hull parent-points. T/F.
#' @param dr Whether to display directional routes. T/F.
#' @param nn Whether to display nearest neighbors (requires that a value for ptid is passed and nearest neighbors were saved when creating the hullset object). T/F.
#' @param ellipses Whether to display bounding ellipses (requires that ellipses were saved when computing ellipses for the hullset). T/F.
#' @param allpts Whether to display all points. T/F.
#' @param ptid One or more ptid (point id) values. A separate plot will be drawn 'zoomed in' to each point indicated. Can also be "auto", in which case ptid will be selected at random. Used primarily to inspect the hull, nearest neighbors, and/or ellipse for specific hulls.
#' @param ptid.highlight Whether to highlight the point specified by ptid. T/F.
#' @param add Whether to add to the existing plot. T/F.
#' @param aoi An area-of-interest object (e.g., box), used to 'zoom in' to specific parts of the plot. aoi objects may be created by the function \code{aoi()}.
#' @param iso.idx The index(s) of the isopleths to plot. Use the summary() function to see the indices of the isopleths.
#' @param iso.sort.metric The sort metric(s) of the isopleths that will be displayed. Character.
#' @param iso.legend Whether to include a legend for the isopleths.
#' @param legend.space An expansion factor for the x-axis that will be used to make room for the legend.
#' @param dr.metric The name of the metric for the directional routes to be displayed (acts to filter on which directional routes are displayed). See \code{\link{lhs.dr.add}}
#' @param dr.thresh.val The threshhold value for the directional routes to be displayed (acts to filter on which directional routes are displayed). See \code{\link{lhs.dr.add}}
#' @param dr.thresh.type The threshhold type for the directional routes to be displayed (acts to filter on which directional routes are displayed). See \code{\link{lhs.dr.add}}
#' @param dr.smooth The smoothing factor for the directional routes to be displayed (acts to filter on which directional routes are displayed). See \code{\link{lhs.dr.add}}
#' @param lwd.dr The line width of directional routes.
#' @param pch.allpts The plot character for all points
#' @param cex.nn The expansion factor for nearest neighbor points.
#' @param cex.hpp The expansion factor for hull parent-points.
#' @param cex.allpts The expansion factor for all points.
#' @param cex.pp The expansion factor for the parent point.
#' @param cex.axis The expansion factor for axis labels.
#' @param cex.legend The expansion factor for the legend.
#' @param col.hpp The color of hull parent points. Either a single color value or a vector of color values of the same length as the number of hulls.
#' @param col.hpp.na The color of hull parent points that have no value of the metric specified by \code{hpp.classify}. Single color value. 
#' @param col.hulls.border The outline color of hulls. Either a single color value or a vector of color values of the same length as the number of hulls.
#' @param col.hulls.fill The fill color of hulls. 
#' @param col.ellipses The outline color of bounding ellipses. Either a single color value or a vector of color values of the same length as the number of hulls.
#' @param col.allpts The color of all points. Single color value.
#' @param col.nn The color of nearest neighbors. Single color value.
#' @param col.nn.pp The color of parent-points. Used only when ptid is passed. Single color value.
#' @param col.iso.fill A number corresponding to a preset color ramp that will be used to display the isopleths. 1=red to blue, 2=yellow to red, 3=blue to red, 4=red to yellow. Alternately, a character vector of color values of the same length as the number of isopleth levels.
#' @param col.iso.opacity A number 0..1 for the opacity of isopleth fill color, where 1 is opaque and 0 is transparent
#' @param col.iso.border Color value of the isopleth border. Color value (use NA for no border).
#' @param col.iso.scale When plotting a subset of isopleths (by ecc or par), whether to scale the isopleth colors to the values of the subset. Not being used.
#' @param col.dr The color of directional routes.
#' @param hpp.classify How to classify hull parent points. May be "none", the name of a hull metric, or "hsp" (hull scatterplot)
#' @param hpp.classify.common.scale.discrete Whether to classify hull parent points using a common scale when there are multiple maps produced. Ignored if hpp.classify is 'none' or 'hsp'. T/F.
#' @param hpp.classify.bins The number of equal-interval bins of the hull metric value to create when classifying hull parent-points by a hull metric.
#' @param hpp.classify.chop The proportion of hull parent-points at the tails of the distribution to remove when calculating the bins.
#' @param hpp.classify.legend Whether to include a legend for the classification of hull parent-points. T/F.
#' @param col.ramp The first and last values of the color ramp to use when classifying hull parent-points by a hull metric. Two-item vector with color values.
#' @param col.ramp.bins The number of bins in the color ramp when classifying hull parent-points by a hull-metric
#' @param hsp The hull scatterplot object to use when displaying hull parent points by a hull scatterplot. May be a hull scatterplot object or the index of a hull scatterplot saved in the hullset.
#' @param record Whether to open a new plot window and turn on recording. T/F.
#' @param figs.per.page The number of plots per page.
#' @param same.axes.4all Whether to use the same axes ranges for all plots. T/F.
#' @param ufat Whether to use user-friendly-axis-titles. T/F.
#' @param ufipt Whether to use user-friendly-isopleth-plot-titles. T/F.
#' @param axes.show Whether to show the axes. T/F.
#' @param axes.ticks Whether to show the tick marks and labels on the axes. T/F.
#' @param axes.titles Whether to show axes titles. T/F.
#' @param title The title to be displayed. Character. If NULL a title will be constructed.
#' @param title.show Whether to show the title. T/F.
#' @param title.inc The element(s) to include in the title (ignored if \code{title} is passed)
#' @param subtitle.inc The element(s) to include in the subtitle (ignored if \code{title} is passed)
#' @param mar The plot margins. A four item numeric vector.
#' @param mgp The distance away from the edge of the plot for the 1) label, 2) tick marks, and 3) axis line. A three-item numeric vector.
#' @param lo.save Whether to save and reset the plot device margin settings (some wrapper functions that call this function don't want device settings reset). T/F.
#' @param lo.margins.set Whether to save and reset the plot device margin settings (some wrapper functions that call this function don't want device settings reset). T/F.
#' @param desc Which side to display automatically generated desciptive text (e.g. caption). 0=none, 1=bottom, 3=top.
#' @param cex.desc The expansion factor for the descriptive text. Numeric value.
#' @param col.desc The color of the descriptive text. Color value.
#' @param gmap The name of a background image that will be downloaded from Google: \code{"none"}, 
#' \code{"roadmap"}, \code{"satellite"}, \code{"hybrid"}, or \code{"terrain"}. May also be a object of type \code{locoh.gmap}, see Notes.
#' @param gmap.one4all Whether to download a single background image for all ids. T/F
#' @param tiff.fn The path and name of a GeoTIFF file (e.g., satellite image) that will be displayed in the background. See notes.
#' @param tiff.pct Whether or to convert the GeoTIFF to an indexed 256 color RGB image, which may speed up drawing. T/F.
#' @param tiff.bands A vector of exactly one (for a single band image) or exactly three integers corresponding to the bands of the GeoTIFF image that will be mapped to the red, 
#' green and blue color guns respectively,. Ignored if \code{tiff.fn} only contains one band.
#' @param tiff.col A vector of color values for plotting single-band images in the background. Ignored if using three bands.
#' @param tiff.buff A numeric buffer distance that the range of the plot will be expanded so the points are not right on the edge of the GeoTIFF.
#' @param tiff.fill.plot Whether to fill the entire plot area with the GeoTIFF. T/F.
#' @param shp.csv The path and filename of a csv file that contains information about shapefiles, including layer names, file, and symbology.
#' @param layers The name(s) of layers in shp.csv to display in the background. Will be displayed using the symbology in shp.csv. Character vector or comma delimited string
#' @param png.fn The path and name of the PNG file to create (instead of displaying in a plot window)
#' @param png.dir The directory for a PNG file (filename will be constructed automatically). Ignored if png.fn is passed
#' @param png.dir.make Whether to create png.dir if it doesn't exist. T/F
#' @param png.fn.pre A prefix that will be used in the construction of the PNG filename. Ignored if png.fn is passed
#' @param png.fn.mid A mid-fix that will be used in the construction of the PNG filename. Ignored if png.fn is passed
#' @param png.fn.suf A suffix that will be used in the construction of the PNG filename. Ignored if png.fn is passed
#' @param png.fn.incld.hs.name Whether to include the hullset name as part of the PNG filename. (T/F). Ignored if png.fn is passed
#' @param png.each.plot.separate Whether to make each plot in a separate plot / PNG. T/F. Ignored if png.fn is passed
#' @param png.width The width of the PNG image. Ignored if png.fn is passed
#' @param png.height The height of the PNG image. Ignored if png.fn is passed
#' @param png.pointsize The pointsize (in pixels) for the PNG image (increase to make labels appear larger). Equivalent 
#' to the height or width of a character in pixels.
#' @param png.overwrite Whether to overwrite an existing PNG file if it exists. T/F
#' @param status Whether to show messages. T/F
#' @param panel.num A number or letter to display in the upper left hand corner of the plot when the plot will be used as part 
#' of a multi-frame graphic (as in publications). Character
#' @param panel.num.inside.plot Whether to display panel.num inside the plot area itself, as opposed to the title area
#' @param hmap A named list of hull metric auxillary parameters, the name of each list element is the name of the variable
#' @param iso.level The isopleth levels to plot, numeric vector
#' @param xlim The lower and upper limit of the x-axis, two-element numeric vector
#' @param ylim The lower and upper limit of the y-axis, two-element numeric vector
#' @param check.ap.value.in.hmparams Whether to check if hull metrics exist for the auxillary parameters passed. T/F
#' @param ... Other parameters, including any auxillary parameters required by certain hull metrics
#'
#' @note
#' This is a multi-purpose plotting function for \link{LoCoH-hullset} objects. You specify which objects in the hullset to include on the plot by passing parameters. 
#'
#' All hullsets have hulls and hull parent points that can be plotted (e.g., code{hulls=TRUE, hpp=TRUE}. All hullsets also have the original 
#' locations saved and these can be added to the plot by setting \code{allpts=TRUE}. Depending how the hullset was 
#' created (see \code{\link{lxy.lhs}}), some hullsets may also have the nearest neighbors for each parent point saved, 
#' which can also be plotted (\code{nn=TRUE}).
#'
#' Hullsets for which have bounding ellipses (\code{\link{lhs.ellipses.add}}) or directional 
#' routes (\code{\link{lhs.dr.add}}) have been computed can have these elements plotted by setting \code{ellipses=TRUE} 
#' and \code{dr=TRUE} respectively. If isopleths have been constructed (\code{\link{lhs.iso.add}}), these 
#' can be plotted by setting \code{iso=TRUE}. If you only want to plot some of the isopleths, you can specify which one(s) to plot 
#' with the parameters \code{iso.idx} (the index(s) of the isopleths to plot, see \code{\link{summary.locoh.lhs}}) or 
#' \code{iso.sort.metric}.
#' 
#' The plot title can be manually set with the \code{title} parameter. If no value for \code{title} is passed, a title will be constructed. You can define
#' what elements will go in the title and subtitle (i.e., the second line of the title) with the \code{title.inc} and \code{subtitle.inc} parameters.
#' To omit the plot title completely, set \code{title.show=FALSE}.
#'
#' To display an image from Google in the background, set gmap to \code{"roadmap"}, \code{"satellite"}, \code{"hybrid"}, or \code{"terrain"}. 
#' This requires an internet connection. When creating plots of hullsets from multiple individuals, 
#' \code{gmap.one4all} determines whether a single background image is used for all individuals (faster to download but could result in blurry 
#' backgrounds if the individuals occupy different parts of the landscape), or a separate image is used for each individual. In the later case,
#' You may also set gmap to an object of type \code{locoh.gmap}, so the image(s) don't have to be 
#' downloaded each time. See \code{lhs.gmap} (in the tlocoh.dev package). 

#' \code{tiff.fn}, \code{tiff.pct}, \code{tiff.buff}, \code{tiff.bands}, \code{tiff.col},
#' and \code{tiff.fill.plot} control the display of a GeoTIFF image in
#' the plot background. The GeoTIFF image must be georeferenced in the
#' same coordinate system as the locoh-hullset object, and the pixel
#' values must be 'prestretched' for display. \code{tiff.bands}
#' controls which bands in the TIFF file will be displayed using the
#' red, green, and blue color guns if using three bands. In a Landsat TM image, for example, the
#' first four bands are blue, green, red, and infrared. To display a TM
#' GeoTIFF image as 'natural colors', you would set \code{tiff.bands=c(3,2,1)}.
#' If \code{tiff.pct=T}, the script will create a indexed 256-color
#' version of the image, which may result in quicker drawing time
#' particularly if several plots are being drawn. If using a single-band, for example a 
#' DEM or classified image, the colors are set by \code{tiff.col}. \code{tiff.buff} can be
#' used to expand the range of values on the x and y axis so that you see
#' a bit of the background image beyond the extent of the points.
#'
#' @return If png.fn or png.dir is passed, a list object where each element is a three-element list of the 
#' properties of the PNG file(s) created: $fn, $dim, $desc. Otherwise, returns a vector of desc
#'
#' @export
#' @import sp
#' @method plot locoh.lhs

plot.locoh.lhs <- function (x, lhs, id=NULL, k=NULL, r=NULL, a=NULL, s=NULL, hs.names = NULL, 
    iso=FALSE, rast=FALSE, hulls=FALSE, hpp=FALSE, dr=FALSE, nn=FALSE, ellipses=FALSE, allpts=FALSE, ptid=NULL, ptid.highlight=TRUE, add=FALSE, 
    aoi=NULL, iso.idx=NULL, iso.sort.metric=NULL, iso.legend=TRUE, legend.space=0.25, 
    dr.metric=NULL, dr.thresh.val=NULL, dr.thresh.type=NULL, dr.smooth=NULL, lwd.dr=2,
    pch.allpts=16, cex.nn=2, cex.hpp=0.6, cex.allpts=0.5, cex.pp=2, cex.axis=0.8, cex.legend=0.8,
    col.hpp="gray50", col.hpp.na="yellow", col.hulls.border="gray50", col.hulls.fill=NA, col.ellipses="red", col.allpts=c("auto","gray")[1], 
    col.nn="black", col.nn.pp="blue", col.iso.fill=1, col.iso.opacity=1, col.iso.border=NA, col.iso.scale = TRUE, col.dr="red", 
    hpp.classify=c("none", "hsp", hm.expr(names.only=TRUE, print=FALSE, desc=FALSE))[1],
    hpp.classify.bins=10, hpp.classify.chop=0.01, hpp.classify.legend=TRUE, hpp.classify.common.scale.discrete=TRUE,
    col.ramp=c("gray10,gray90", "rainbow")[1], col.ramp.bins=10, hsp=NULL, 
    record=FALSE, figs.per.page=1, same.axes.4all=NULL, ufat=TRUE, ufipt=TRUE, 
    axes.show=TRUE, axes.ticks=axes.show, axes.titles=axes.show, 
    title=NULL, title.show=TRUE,
    title.inc=c("title", "hs.name", "id", "kar", "features", "hpp.classify", "hmap", "ptid")[if (is.null(title)) 3:4 else 1],
    subtitle.inc=c("title", "hs.name", "id", "kar", "features", "hpp.classify", "hmap", "ptid")[if (is.null(title)) 5:8 else 0],
    mar=c(if (axes.titles || axes.ticks) 3.3 else 0.5, if (axes.titles || axes.ticks) 3.2 else 0.5, if (title.show) 3.2 else 0.5, 0.5),
    mgp=c(2, 0.7, 0), lo.save=TRUE, lo.margins.set=TRUE,
    desc=c(NONE<-0, BOTTOM<-1, TOP<-3)[ifelse(figs.per.page==1,2,1)], cex.desc=0.8, col.desc="darkgreen", 
    gmap=c("none", "roadmap", "satellite", "hybrid", "terrain")[1], gmap.one4all=TRUE, 
    tiff.fn=NULL, tiff.pct=FALSE, tiff.bands=c(4,3,2), tiff.col=gray(0:255/255), tiff.buff=0, tiff.fill.plot=TRUE,
    shp.csv=NULL, layers=NULL, 
    png.fn=NULL, png.dir=NULL, png.dir.make=TRUE, png.fn.pre=NULL, png.fn.mid=NULL, png.fn.suf=NULL, png.fn.incld.hs.name=TRUE, 
    png.each.plot.separate=TRUE, png.width=800, png.height=png.width, png.pointsize=12+(png.width-480)/80, png.overwrite=TRUE, 
    status=TRUE, panel.num=NULL, panel.num.inside.plot=!title.show, hmap=NULL,
    iso.level=NULL, xlim=NULL, ylim=NULL, check.ap.value.in.hmparams=TRUE, ...) {

    ## removed: sp=0, spc.reg.det.xaxis=NULL, spc.reg.det.yaxis=NULL, spc.reg.det.layout="auto", spc.reg.det.layout.map.nrow=2, spc.reg.det.layout.det.ncol=NULL,
    ##          sat.base=NULL, val.base=NULL, hue.offset=NULL, center.method=c("bbox","mean")[1],  
    ##          spc.reg.idx=1, spc.reg.col.default="gray80", spc.reg.col.view=NULL, 
    ##          col.hpp=c("cyan-red", "rainbow", "spiral", "reg")[1], 
    ##          hulls.pp.sort="area", hulls.pp.sort2=NULL, 
    ##          @param ivg Value(s) for inter-visit gap, required if points are classified by a time-use metric or a hullscatter plot 
    #                       that requires a time-use metric. Inter-visit gap is the period of time (in second) which must pass before another 
    #                       occurrence of the individual in the hull is considered a separate visit.
    #           iso.par=FALSE, iso.ecc=FALSE (used to be used for filtering which isopleths to plot)
    
    
    ## If png.fn or png.dir is passed, will return a list of list objects with info about the PNG file(s) created: $fn, $dim, $desc
    ## Otherwise, returns a vector of desc

    ## axes.show, axes.titles, and axes.ticks control which part(s) of the axes appear
    ## To hide the axes completely (along with tick markes and labels), set axes.show=F
    ## To show the axes lines but no tick marks or labels, set axes.titles=F, axes.ticks=F      
    
    ## ufat = user-friendly axis titles
    ## If record = TRUE, it will create a new device (plot window) and record the plots there
             
    if (!missing(lhs)) {
      warning("argument lhs is deprecated; please use 'x' instead.", call. = FALSE)
    }

    lhs <- x; rm(x)
    if (!inherits(lhs, "locoh.lhs")) stop("lhs should be of class \"locoh.lhs\"")
    if (rast && !requireNamespace("raster", quietly=TRUE)) stop("package raster required, please install")
    if (is.null(gmap)) gmap <- "none"
    if (!identical(gmap,"none")) {
        if (!requireNamespace("dismo", quietly=TRUE)) stop("package dismo required to display a background image, please install")
        if (!requireNamespace("rgdal", quietly=TRUE)) stop("package rgdal required to display a background image, please install")
        if (!requireNamespace("raster", quietly=TRUE)) stop("package raster required to display a background image, please install")    
        if (inherits(gmap, "locoh.gmap") && gmap.one4all) gmap.one4all <- FALSE
    }
    
    ## Make sure tiff.fn exists, check tiff.bands
    if (!is.null(tiff.fn)) {
        if (!requireNamespace("rgdal", quietly=TRUE)) stop("package rgdal required to display a tiff in the background")
        if (!file.exists(tiff.fn)) stop(paste(tiff.fn, "not found"))
        if (!identical(gmap,"none")) stop("you can't display a gmap and tiff at the same time")
        range.expand.tiff <- tiff.buff
        tiff.sgdf <- NULL
        if (length(tiff.bands) > 3) stop("tiff.bands can not be longer than 3")
    }

    if (!is.numeric(desc) || desc > 4)  stop("Desc must be 0 (no description) or between and 1 and 4 (side to display description")
    if (desc != 0 && figs.per.page > 1) stop("You can't display description if figs.per.page > 1. Set desc=0.")
    if (is.null(same.axes.4all)) same.axes.4all <- (length(ptid) <= 1)
    
    if (!iso && !rast && !nn && !hulls && !ellipses && !allpts && !hpp && !dr) stop(cw("Don't know what to plot. Set at least one of the following parameters to TRUE: iso, rast, hulls, hpp, nn, allpts, dr, or ellipses", exdent=2))
    if (nn && is.null(ptid)) stop("To plot nearest neighbors you must specify a ptid")
    if ((iso || hpp) && nn) stop("You can't plot isopleths or hpp with nearest neighbors")
    if (iso && ellipses) stop("You can not plot isopleths and ellipses together")
    if (iso && hulls) stop("You can not plot isopleths and hulls together")

    if (!is.null(ptid) && iso) stop("You can not specify a point id(s) when plotting isopleths")
    if (!is.null(ptid) && hpp) stop("You can not specify a point id(s) when plotting hpp")
    if (length(ptid) > 1 &&  same.axes.4all) stop("same.axes.4all can not be TRUE with multiple ptid")    
    if (add && dev.cur()==1) stop("Can't add points to an existing plot, no plot window open")
    
    hme <- hm.expr(names.only=FALSE)   ## will use this list of expressions later
    ddd.lst <- list(...)
    
    if (!hpp.classify %in% c("none", "hsp", names(hme))) stop("Unknown value for hpp.classify")
    #if (!is.null(iso.sort.hulls.by)) {if (!iso.sort.hulls.by %in% names(hme)) stop("Illegal value for iso.sort.hulls.by")}
        
    if (is.null(id) && is.null(r) && is.null(k) && is.null(a) && is.null(s) && is.null(hs.names)) {
        hs <- lhs
    } else {    
        for (str.param in c("k","a","r","s")) assign(str.param, vectorize.parameter(get(str.param)))
        hs <- lhs.select(lhs, id=id, r=r, k=k, a=a, s=s, hs.names=hs.names)
    }
    if (length(hs)==0) stop("No hullsets found matching those criteria")
    
    iso.num.plotted <- 0
    plots.made <- 0             ## used to keep track when we need to create a new PNG file
    tick.show <- if (axes.ticks) "s" else "n"

    ## Do some error checking on hsp, if its ok then assign values for all saved parameters
    if (is.null(hsp)) {
        if (hpp.classify == "hsp") stop("To classify by hull scatterplot, you must also pass a value for hsp")
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
    
    ## Prepare the expressions that will be used to piece together the title and subtitle based on what's passed
    ## in title.inc and subtitle.inc
    title.pieces.exp.lst <- list(title=expression(title), hs.name=expression(hs.name), id=expression(hs[[hs.name]][["id"]]), kar=expression(paste(hs[[hs.name]][["mode"]], "=", hs[[hs.name]][[hs[[hs.name]][["mode"]]]], sep="")), features=expression(title.feats.str), hpp.classify=expression(title.hpp.classify), hmap=expression(title.hmap), ptid=expression(if (ptidVal==0) NULL else paste("ptid=", ptidVal, sep="")))
    title.inc <- vectorize.parameter(title.inc, type="character", sort.res=FALSE)
    subtitle.inc <- vectorize.parameter(subtitle.inc, type="character", sort.res=FALSE)
    if (FALSE %in% (title.inc %in% names(title.pieces.exp.lst))) stop("Unknown value in title.inc")
    if (FALSE %in% (subtitle.inc %in% names(title.pieces.exp.lst))) stop("Unknown value in subtitle.inc")

    ## Error checking if hpp=T
    hmap.passed <- hmap
    hmap <- as.data.frame(NA)
    if (hpp) {
        if (length(hpp.classify) != 1) stop("Only one value for hpp.classify allowed")
        
        ## Identify all of the hull metrics that will be used
        if (hpp.classify != "none") {
            if (hpp.classify == "hsp") {
                ## Grab the hull metrics needed from the hsp(s)
                if (is.list(hsp)) {
                    hull.metrics.needed <- unique(as.character(sapply(hsp, function(x) c(x$x.axis, x$y.axis))))
                } else {
                    hull.metrics.needed <- unique(as.character(sapply(hs, function(x) sapply(x$hsp[hsp], function(h) c(h$x.axis, h$y.axis )))))
                }
            } else {
                hull.metrics.needed <- hpp.classify
            }

            ## Make sure all of the required parameters were passed to the function, *and* set up a data frame
            ## containing all permutation(s) of the hull metric auxillary parameters (hmap) passed
            avparams.lst <- c(list(), hmap.passed)
            for (axis.metric in hull.metrics.needed) {
                for (avparam in hme[[axis.metric]][["req.ap"]]) {
                    if (avparam %in% names(ddd.lst)) {
                        # Make sure these values have been actually computed
                        if (check.ap.value.in.hmparams) {
                            for (hs.name in names(lhs)) {
                                if (FALSE %in% (ddd.lst[[avparam]] %in%  hs[[hs.name]][["hm.params"]][[avparam]])) stop(paste("Value of ", avparam, " not found", sep=""))
                            }
                        }
                        
                        avparams.lst[[avparam]] <- unique(c(avparams.lst[[avparam]], ddd.lst[[avparam]]))
                     } else {
                        if (is.null(hme[[axis.metric]][["req.ap.def"]])) {
                            stop(paste("Required parameter missing: ", avparam, sep=""))
                        } else if (identical(hme[[axis.metric]][["req.ap.def"]][[avparam]], "all")) {
                            ## Get all of the hull parameters that have been run
                            for (hs.name in names(lhs)) {
                                avparams.lst[[avparam]] <- unique(c(avparams.lst[[avparam]], hs[[hs.name]][["hm.params"]][[avparam]]))
                            }
                        } else {
                            avparams.lst[[avparam]] <- unique(c(avparams.lst[[avparam]], hme[[axis.metric]][["req.ap.def"]]))
                        }
                    } 
                }
            }
            if (length(avparams.lst) > 0) {
                ## Convert the list of auxillary variables into a data frame containing all permutations of the variables
                ## Oct 2012 - I decided to not use expand.grid, because rarely will you get hull metrics with multiple independent aux parameters each
                ## If that is really needed, add a parameter to the function ap.mix.all=FALSE
                hmap <- as.data.frame(avparams.lst)
                ##hmap <- expand.grid(avparams.lst, stringsAsFactors=FALSE)
            } else {
                hmap <- as.data.frame(NA)
            }
            
        }
    }

    if (ellipses && (TRUE %in% sapply(hs, function(x) is.null(x[["ellipses"]])))) stop("Ellipses haven't been created. Try running lhs.ellipses.add")
    if ((iso || rast) && length(unlist(lapply(hs, function(x) names(x$isos)))) == 0) stop("No isopleths found in lhs")

    ## If hpp, create color values for the color ramp
    
    if (hpp && hpp.classify!="none" && hpp.classify!="hsp" && !hme[[hpp.classify]][["discrete"]]) {
        if (identical(col.ramp, "rainbow")) {
            col.hpp.use <- rainbow(hpp.classify.bins, end=5/6)
        } else {
            col.hpp.use <- colorRampPalette(vectorize.parameter(col.ramp, type="character", sort.res=FALSE))(hpp.classify.bins)
        }
    }
    
    ## If plotting hull/nn/enc for a single parent point, make sure the ptid is found
    if (!is.null(ptid)) {
        ## If ptid = 'auto', select one at random
        #if (identical(ptid, "auto")) ptid <- hs[[1]][["pts"]][["ptid"]][sample(1:length(hs[[1]][["pts"]][["ptid"]]), 1)]
        if (identical(ptid, "auto")) ptid <- sample(hs[[1]][["pts"]][["ptid"]], 1)

        ## Make sure all ptid exist in all runs
        for (ptidVal in ptid) {
            for (hs.name in names(hs)) {
                if (!ptidVal %in% hs[[hs.name]][["pts"]][["ptid"]]) {
                    stop(paste("Didn't find ptid ", ptidVal, " in ", hs.name, ".\n", sep=""))
                }
            }
        }
        if (same.axes.4all && length(ptid)==1) pp.idx <- which(hs[[1]][["pts"]][["ptid"]] == ptid)
    }
    
    ## Set the number of plots per page and check for the output folder
    if (is.null(png.dir) && is.null(png.fn)) {
        if (record && .Platform$OS.type == "windows") windows(record=TRUE)
        res <- NULL
    } else {
        if (add) stop("Can not add to an existing plot, PNG device not open.")
        if (is.null(png.fn) && !file.exists(png.dir)) {
            if (png.dir.make) {
                dir.made <- dir.create(png.dir)
                if (!dir.made) stop(paste("Couldn't make output folder ", png.dir, sep=""))            
            } else {
                stop(paste(png.dir, " doesn't exist and png.dir.make is False, so can not continue."))
            }
        }
        res <- list()
    }
    
    ## first.png.made <- FALSE
    desc.all <- NULL
    png.counter <- 0
    
    ## Prepare GIS features
    gis.features <- if (is.null(layers)) list() else shp.layers(layers, shp.csv=shp.csv)
    
    ## Compute the range of all runs combined to set the axis scales
    if (same.axes.4all  || (!identical(gmap,"none") && gmap.one4all)) {
        if (is.null(aoi)) {
            if (is.null(ptid)) {
                bbox.all <- do.call("rbind", lapply(1:length(hs), function(hs.idx) t(hs[[hs.idx]][["pts"]]@bbox)))
                rx <- range(bbox.all[, 1])
                ry <- range(bbox.all[, 2])
            } else {
                ## There's a ptid, so we have a single parent point that exists in potentially multiple runs
                ## Want to get all the ellipses, hulls, or nn for this point in these runs, then take the union of them to 
                ## get the common ranges of axes
                
                ## Get the index value of pts that matches ptid for each selected hullset
                ptid.pp.idx.vec <- sapply(1:length(hs), function(hs.idx) which(hs[[hs.idx]][["pts"]][["ptid"]]==ptid)) 
                
                if (ellipses) {
                    theta <- seq(0, 2 * pi, length = 72)
                    ellps.all.params <- do.call("rbind", lapply(1:length(hs), function(hs.idx) hs[[hs.idx]][["ellipses"]][hs[[hs.idx]][["ellipses"]][["pts.idx"]]==ptid.pp.idx.vec[hs.idx], ]))                    
                    ellps.all.pts <- do.call(rbind, lapply(1:nrow(ellps.all.params), function(i) {
                                          p <- ellps.all.params[i,]
                                          data.frame(x = p$cx + p$a * cos(theta) * cos(p$alpha) - p$b * sin(theta) * sin(p$alpha),
                                                    y = p$cy + p$a * cos(theta) * sin(p$alpha) + p$b * sin(theta) * cos(p$alpha))}))
                    rx <- range(ellps.all.pts[, 1])
                    ry <- range(ellps.all.pts[, 2])
                } else {

                    ## Create a list of the bounding box coordinates of all of the hulls constructed from this parent point
                    bb.hulls.this.ptid.lst <- lapply(1:length(hs), function(hs.idx) t(bbox(hs[[hs.idx]][["hulls"]][hs[[hs.idx]][["hulls"]]@data[["ptid"]]==ptid, ])))
                    
                    #all.runs.hull.this.ptid <- lapply(hs, function(hs) hs[["hulls"]][[as.character(ptid)]])
                    
                    ## Create a two-column data frme containing all of the points of all of the hulls
                    xy.all <- do.call(rbind, bb.hulls.this.ptid.lst)
                    
                    rx <- range(xy.all[, 1])
                    ry <- range(xy.all[, 2])
                    
                }                
            }
            
            ## Expand the range of the axes if there is a background tiff
            if (!is.null(tiff.fn)) {
                rx <- rx + c(-range.expand.tiff, range.expand.tiff)
                ry <- ry + c(-range.expand.tiff, range.expand.tiff)
            }
            
            ## Make space for a legend if needed
            if ((hpp && (!hpp.classify %in% c("none", "hsp")) && hpp.classify.legend) || (iso && iso.legend)) {
                rx[1] <- rx[1] - (legend.space * diff(rx))
            }
        } else {
            rx <- range(aoi[, 1])
            ry <- range(aoi[, 2])
        }

        ## Prepare the background image
        if (!identical(gmap,"none") && !inherits(gmap,"locoh.gmap")) {
        
            #if (is.list(gmap)) stop("If you pass a gmap list, don't set same.axes.4all=T")
            
            if (status) cat("Downloading common background image...")
        
            bbprj.sp <- SpatialPoints(cbind(rx, ry), proj4string=hs[[1]]$pts@proj4string)
            bbll.sp <- spTransform(bbprj.sp, CRS("+proj=longlat +datum=WGS84"))
            
            ## Project the extent of the locoh-hullset to lat long
            #extLatLong <- projectExtent(hs[[1]]$pts, CRS("+proj=longlat +datum=WGS84"))
            
            ## Download a basemap from Google
            base.map.merc <- dismo::gmap(bbll.sp, type=gmap)
            base.map.col <- base.map.merc@legend@colortable
            
            ## Project the downloaded basemap using nearest neighbor resampling
            base.map.rast <- raster::projectRaster(base.map.merc, crs=hs[[1]]$pts@proj4string, method="ngb")
            
            if (status) cat("Done\n")
            
            #image(base.map.rast, col=col.merc, add=T)
            #image(base.map.rast, col=base.map.col, add=T)
            
        } else if (!is.null(tiff.fn) && is.null(tiff.sgdf)) {
            if (tiff.fill.plot) {
                half.plot.size <- c(-0.5, 0.5) * max(diff(rx), diff(ry))
                rx.tiff <- half.plot.size + mean(rx)
                ry.tiff <- half.plot.size + mean(ry)
            } else {
                rx.tiff <- rx
                ry.tiff <- ry
            }
            tiff.sgdf <- readpartgdal(tiff.fn, xlim=rx.tiff, ylim=ry.tiff, band=tiff.bands, silent=TRUE, status=TRUE)
            if (is.null(tiff.sgdf)) {
                tiff.fn <- NULL
            } else {
                if (tiff.pct) {
                    if (length(tiff.sgdf@data)==3) {
                        tiff.sgdf.cols <- rgdal::SGDF2PCT(tiff.sgdf, adjust.bands=FALSE)
                        tiff.sgdf$idx <- tiff.sgdf.cols$idx
                    } else {
                        cat("   Incorrect number of bands, can't convert image to indexed RGB\n")
                        tiff.pct <- FALSE
                    }
                }
            }
        }
    }

    # Save current device settings if plotting to screen 
    if (is.null(png.dir) && is.null(png.fn)) {
        if (lo.save) {
            opar <- par()[c("mar", "mgp", "oma")]
            ## Next line remmed because we actually *don't* want to reset mar and oma on exit, because presuming the plot window stays open we need to 
            ## keep these settings in order for the locator() function (used by aoi() to work correctly. If the plot has an outer margin for the
            ## description, then resetting the incoming values on exit will also reset the plot area. If the user wants to 
            ## make other plots, they can open a new device
            ## on.exit(par(opar))
        }
        if (lo.margins.set) {
            par(mar=mar, mgp=mgp)
            if (figs.per.page > 1) par(mfrow = n2mfrow(figs.per.page))
        }
    }
    
    ## Start nested loop with hsp objects
    for (hsp.idx in 1:length(hsp)) {
        hsp.use <- NULL

        if (identical(hsp, NA) || is.numeric(hsp)) {
            ## sort hull sets by id and then s
            hs.ord <- order(sapply(hs, function(x) x[["id"]]), sapply(hs, function(x) x[["s"]]))
            hs.names.to.loop.thru <- names(hs[hs.ord])
        } else {
            ## Restore the parameters for this function saved in hsp
            ## for (var.name in names(hsp[[hsp.idx]])) assign(var.name, hsp[[hsp.idx]][[var.name]])
            hsp.use <- hsp[[hsp.idx]]
            hs.names.to.loop.thru <- intersect(hsp.use[["hs.name"]], names(hs))
         }   
        
        for (hs.name in hs.names.to.loop.thru) {
            
            if (is.numeric(hsp)) {
                if (is.null(hs[[hs.name]][["hsp"]][[ hsp[hsp.idx] ]])) {
                    stop(paste(hs.name, " doesn't have a saved scatter plot with index ", hsp[hsp.idx], sep=""))
                } else {
                    hsp.use <- hs[[hs.name]][["hsp"]][[hsp[hsp.idx]]] 
                }
            }
            
            #plot.title <- hs.name   ## Default plot title

            ## Set up dr.idx.use
            if (dr) {
                if (is.null(hs[[hs.name]][["dr"]])) {
                    dr.idx.use <- numeric()
                } else {
                    ## See which of the directional routes meet the users specifications
                    
                    #dr.all.idx <- 1:length(hs[[hs.name]][["dr"]])
                    dr.all.idx <- order(sapply(hs[[hs.name]][["dr"]], function(x) x$thresh.type), sapply(hs[[hs.name]][["dr"]], function(x) x$thresh.val))
                    
                    dr.idx.use <- dr.all.idx
                    
                    if (!is.null(dr.metric)) dr.idx.use <- intersect(dr.idx.use, dr.all.idx[sapply(hs[[hs.name]][["dr"]][dr.all.idx], function(x) x$metric %in% dr.metric)])
                    if (!is.null(dr.thresh.val)) dr.idx.use <- intersect(dr.idx.use, dr.all.idx[sapply(hs[[hs.name]][["dr"]][dr.all.idx], function(x) x$thresh.val %in% dr.thresh.val)])
                    if (!is.null(dr.thresh.type)) dr.idx.use <- intersect(dr.idx.use, dr.all.idx[sapply(hs[[hs.name]][["dr"]][dr.all.idx], function(x) x$thresh.type %in% dr.thresh.type)])
                    if (!is.null(dr.smooth)) dr.idx.use <- intersect(dr.idx.use, dr.all.idx[sapply(hs[[hs.name]][["dr"]][dr.all.idx], function(x) x$smooth %in% dr.smooth)])
                }
                if (length(dr.idx.use)==0) {
                    cat("  No directional routes found \n")
                    dr <- FALSE
                    dr.idx.use <- NA
                } 
                
            } else {
                dr.idx.use <- NA
            }
            
            ## Set up the iso.df
            has.iso <- TRUE
            has.rast <- TRUE
            if (iso || rast) {
                num.iso.lst <- length(hs[[hs.name]][["isos"]])
                if (num.iso.lst == 0) {
                    num.iso.lst <- 1
                    has.iso <- FALSE
                    has.rast <- FALSE
                } 
                
                if (rast) {
                    if (TRUE %in% sapply(hs[[hs.name]][["isos"]], function(x) is.null(x[["rast"]]))) has.rast <- FALSE
                }
            } else {
                num.iso.lst <- 1
                has.iso <- FALSE
                has.rast <- FALSE
            }
            
            if (rast && !has.rast) stop("Rasterized isopleths not found. Use lhs.iso.rast in the tlocoh.dev package.")
            
            ## If the user passed a value for iso.idx, limit the isopleths plotted to just those
            if (is.null(iso.idx)) {
                isos.idx.use <- 1:num.iso.lst
            } else {
                isos.idx.use <- intersect(1:num.iso.lst, iso.idx)
                if (length(isos.idx.use)==0) {
                    has.iso <- FALSE
                    isos.idx.use <- 1
                }
            }
            
            ## Filter isos.idx.use 
            #if (iso.par) isos.idx.use <- intersect(isos.idx.use, (1:length(hs[[hs.name]]$isos))[as.vector(sapply(hs[[hs.name]]$isos, function(x) !is.null(x$par)))])
            #if (iso.ecc) isos.idx.use <- intersect(isos.idx.use, (1:length(hs[[hs.name]]$isos))[as.vector(sapply(hs[[hs.name]]$isos, function(x) !is.null(x$ecc)))])
            
            if (!is.null(iso.sort.metric)) isos.idx.use <- intersect(isos.idx.use, (1:length(hs[[hs.name]][["isos"]]))[as.vector(sapply(hs[[hs.name]][["isos"]], function(x) x[["sort.metric"]] %in% iso.sort.metric ))])
            if (length(isos.idx.use)==0 && is.null(iso.sort.metric)) cat("  ", hs.name, ": no matching isopleths found \n", sep="")

            ## If hpp, create color values for the color ramp
            #if (hpp && hpp.classify!="none" && hpp.classify!="hsp" && !hme[[hpp.classify]][["discrete"]]) {
            #    if (identical(col.ramp, "rainbow")) {
            #        col.hpp.use <- rainbow(hpp.classify.bins, end=5/6)
            #    } else {
            #        col.hpp.use <- colorRampPalette(vectorize.parameter(col.ramp, type="character", sort.res=FALSE))(hpp.classify.bins)
            #    }
            #}


            ## If classifying hpp by a metric which is discrete, make loop through all of the auxillary parameters
            ## and get a master list of possible values
            ## Loop through hmap.idx, get vals, make a list of the unique ones, and creaet a common color ramp

            if (hpp) {
                if (hpp.classify=="none") {
                    col.hpp.use <- col.hpp                
                } else if (hpp.classify!="hsp" && hme[[hpp.classify]][["discrete"]] && hpp.classify.common.scale.discrete) {
    
                    ## We might want to put this before the hs.name loop starts
    
                    ## Get all unique values
                    hpp.unique.vals <- NULL
                    for (hmap.idx in 1:nrow(hmap)) {
                        hpp.unique.vals <- unique(c(hpp.unique.vals, unique(eval(hme[[hpp.classify]][["expr"]]))))
                    }
                    if (is.null(hpp.unique.vals)) stop(paste("Can not evaluate: ", hme[[hpp.classify]][["expr"]], sep=""))
                    
                    ## Create a blank color ramp for the full range of values
                    hpp.possible.vals <- seq(from=min(hpp.unique.vals), to=max(hpp.unique.vals))
                    col.hpp.use <- rep(NA, length(hpp.possible.vals))
    
                    ## Identify the non-zero values
                    chpv.idx <- seq_along(hpp.possible.vals)
                    if (hme[[hpp.classify]][["zero2na"]]) chpv.idx <- chpv.idx[hpp.possible.vals != 0]
    
                    ## Fill in color ramp
                    if (identical(col.ramp, "rainbow")) {
                        col.hpp.use[chpv.idx] <- rainbow(length(chpv.idx), end=5/6)
                    } else {
                        col.hpp.use[chpv.idx] <- colorRampPalette(vectorize.parameter(col.ramp, type="character", sort.res=FALSE))(length(chpv.idx))
                    }
    
                    ## Prepare labels for the legend
                    legend.labels <- as.character(hpp.possible.vals)
                    legend.col <- col.hpp.use
    
                }
            } 
            
            for (hmap.idx in 1:nrow(hmap)) {
            for (iso.idx in isos.idx.use ) {
                iso.num.plotted <- iso.num.plotted + 1
            for (ptidVal in n2z(ptid)) {
            for (dr.idx in dr.idx.use) {
            
                ## Hereth begins the code to actually plot stuff
                
                title.ptid.str <- NULL
                title.feats.str <- NULL
                title.hpp.classify <- NULL
                title.hmap <- NULL

                if (!is.null(ptid)) title.ptid.str <- paste("ptid=", ptidVal, sep="")
                if (dr) title.feats.str <- c(title.feats.str, names(hs[[hs.name]][["dr"]])[dr.idx])
                
                ## If there's a ptid, the let go ahead and prepare the shape(s) that will go on the plot, which we may need to set the global axes ranges
                if (!is.null(ptid)) {
                    pts.idx <- which(hs[[hs.name]][["pts"]][["ptid"]] == ptidVal)
                    hull.idx <- which(hs[[hs.name]][["hulls"]]@data[["ptid"]] == ptidVal)
                                        
                    nn.pts <- coordinates(hs[[hs.name]][["pts"]])[hs[[hs.name]][["enc.pts"]][["idx"]][[hull.idx]] [hs[[hs.name]][["enc.pts"]][["nn"]][[hull.idx]]], , drop=FALSE]
                    pp.pts <- coordinates(hs[[hs.name]][["pts"]])[pts.idx, , drop=FALSE]

                    if (hulls) {
                        hull.pts <- hs[[hs.name]][["hulls"]]@polygons[[hull.idx]]@Polygons[[1]]@coords
                    }
                    if (ellipses) {
                        ellps.params <- hs[[hs.name]][["ellipses"]][hs[[hs.name]][["ellipses"]][["pts.idx"]]==pts.idx, ]
                        theta <- seq(0, 2 * pi, length = 72)
                        ellps.pts <- with(ellps.params,
                                          data.frame(x = cx + a * cos(theta) * cos(alpha) - b * sin(theta) * sin(alpha),
                                                     y = cy + a * cos(theta) * sin(alpha) + b * sin(theta) * cos(alpha)))
                    }
                }
                
                ## Compute the range of the x and y axes if needed
                if (!same.axes.4all || inherits(gmap, "locoh.gmap")) {
                    if (is.null(aoi)) {
                        if (is.null(ptid)) {
                            #rx <- range(coordinates(hs[[hs.name]][["pts"]])[,1])
                            rx <- bbox(hs[[hs.name]][["pts"]])[1,]
                            ry <- bbox(hs[[hs.name]][["pts"]])[2,]
                        } else {
                            if (ellipses) {
                                rx <- range(ellps.pts[,1])
                                ry <- range(ellps.pts[,2])
                            } else if (nn) {
                                rx <- range(nn.pts[,1])
                                ry <- range(nn.pts[,2])
                            } else if (hulls) {
                                rx <- range(hull.pts[,1])
                                ry <- range(hull.pts[,2])
                            } else {
                                stop("Don't know how to set the axes, what is going to be plotted?")
                            }
                        }
    
                        ## Expand the range of the axes if there is a background tiff
                        if (!is.null(tiff.fn)) {
                            rx <- rx + c(-range.expand.tiff, range.expand.tiff)
                            ry <- ry + c(-range.expand.tiff, range.expand.tiff)
                        }
        
                        ## Make space for a legend if needed
                        if ((hpp && (!hpp.classify %in% c("none", "hsp")) && hpp.classify.legend) || (iso && iso.legend)) {
                            rx[1] <- rx[1] - (legend.space * diff(rx))
                        }
                    
                    } else {
                        rx <- range(aoi[, 1])
                        ry <- range(aoi[, 2])
                    }
    
                    ## Prepare the background image

                    if (!identical(gmap,"none")) {
                        
                        if (inherits(gmap, "locoh.gmap")) {
                            idVal <- hs[[hs.name]]$id
                            if (idVal %in% names(gmap)) {  
                                base.map.rast <- gmap[[idVal]]$bg.rast
                                base.map.col <- gmap[[idVal]]$bg.col
                            
                            } else {
                                warning(paste("gmap image not found for ", idVal, sep="")) 
                                base.map.rast <- NULL
                                base.map.col <- NULL
                            }
                            
                        } else if (!gmap.one4all) {
                            if (status) cat("Getting background image...")
                        
                            bbprj.sp <- SpatialPoints(cbind(rx, ry), proj4string=hs[[1]]$pts@proj4string)
                            bbll.sp <- spTransform(bbprj.sp, CRS("+proj=longlat +datum=WGS84"))
                            
                            ## Download a basemap from Google
                            base.map.merc <- dismo::gmap(bbll.sp, type=gmap)
                            base.map.col <- base.map.merc@legend@colortable
                            
                            ## Project the downloaded basemap using nearest neighbor resampling
                            base.map.rast <- raster::projectRaster(base.map.merc, crs=hs[[1]]$pts@proj4string, method="ngb")
                            
                            if (status) cat("Done\n")
                        }
                        
                    } else if (!is.null(tiff.fn)) {
                        if (tiff.fill.plot) {
                            half.plot.size <- c(-0.5, 0.5) * max(diff(rx), diff(ry))
                            rx.tiff <- half.plot.size + mean(rx)
                            ry.tiff <- half.plot.size + mean(ry)
                        } else {
                            rx.tiff <- rx
                            ry.tiff <- ry
                        }
                        tiff.sgdf <- readpartgdal(tiff.fn, xlim=rx.tiff, ylim=ry.tiff, band=tiff.bands, silent=TRUE, status=TRUE)
                        if (is.null(tiff.sgdf)) {
                            tiff.fn <- NULL
                        } else {
                            if (tiff.pct) {
                                if (length(tiff.sgdf@data)==3) {
                                    tiff.sgdf.cols <- rgdal::SGDF2PCT(tiff.sgdf, adjust.bands=FALSE)
                                    tiff.sgdf$idx <- tiff.sgdf.cols$idx
                                } else {
                                    cat("   Incorrect number of bands, can't convert image to indexed RGB\n")
                                    tiff.pct <- FALSE
                                }
                            }
                        }
                    }
                    
                }
                xlim.use <- if (is.null(xlim)) rx else xlim
                ylim.use <- if (is.null(ylim)) ry else ylim
                
                ## Create the descriptive text string
                if (iso || rast) {
                    desc.str <- paste(hs[[hs.name]][["isos"]][[iso.idx]][["desc"]], " ", hs[[hs.name]][["desc"]], sep="")
                } else {
                    ## Create a string for the elements that will be on the plot
                    elements.in.plot <- c(if (hulls) "hulls" else NULL, if (hpp) "parent points" else NULL, if (ellipses) "ellipses" else NULL, if (nn) "nearest neighbors" else NULL)
                    elements.in.plot[1] <- paste(toupper(substr(elements.in.plot[1], 1, 1)), substr(elements.in.plot[1], 2, nchar(elements.in.plot[1])), sep="")
                    if (length(elements.in.plot) > 1) elements.in.plot[length(elements.in.plot)] <- paste("and ", elements.in.plot[length(elements.in.plot)], sep="")
                    collapse.char <- if (length(elements.in.plot) > 2) ", " else " "
                    elements.in.plot.str <- paste(paste(elements.in.plot, collapse=collapse.char, sep=""), " for ", hs[[hs.name]][["id"]], sep="")
                    ptid.str <- if (is.null(ptid)) NULL else paste(" for ptid=", ptidVal, sep="")
                    ptid.highlight.str <- if (is.null(ptid) || !ptid.highlight) NULL else "The parent point is shown by a triangle. "
                    nn.str <- if (nn) "Nearest neighbors used for hull construction are circled. " else NULL
                    hmap.str <- NULL
                    if (hpp) {
                        if (hpp.classify == "hsp") {
                            classify.str <- paste("Points colored by their location in ", hsp.use[["x.axis"]], "-", hsp.use[["y.axis"]], " space", ". ", sep="")
                            if (!is.na(hmap[hmap.idx,1])) hmap.str <- paste(unique(unlist(sapply(c(hsp.use[["x.axis"]], hsp.use[["y.axis"]]), function(myaxis) sapply(hme[[myaxis]][["req.ap.desc"]], function(x) eval(x))))), collapse="", sep="")
                        } else if (hpp.classify == "none"){
                            classify.str <- ""
                        } else {
                            classify.str <- paste("Points colored by ", eval(hme[[hpp.classify]][["ufat"]]), ". ", sep="") 
                            if (!is.na(hmap[hmap.idx,1])) hmap.str <- paste(unlist(sapply(c(hpp.classify), function(myaxis) sapply(hme[[myaxis]][["req.ap.desc"]], function(x) eval(x)))), collapse="", sep="")
                        }                                                      
                    } else {
                        classify.str <- ""
                    }
                    
                    desc.str <- paste(elements.in.plot.str, ptid.str, ". ", ptid.highlight.str, nn.str, classify.str, hmap.str, hs[[hs.name]]$desc, sep="")
                }
                

                ## Outer margin area
                oma.vals <- c(0,0,0,0)
                if (desc !=0 ) {
                    desc.str.chopped <- chop2plot(desc.str, width=dev.size()[1] * 0.95, cex=cex.desc)
                    oma.vals[desc] <- as.numeric(desc.str.chopped[2])
                }

                ## Start a new plot or PNG if needed
                ## We shouldn't have to check for set.lo, because if the calling function doesn't want a new PNG file started, they should simply not pass png.dir
                if ((!is.null(png.dir) || !is.null(png.fn))) {
                    
                    ## See if there's going to be something to plot this loop
                    if (!iso || has.iso) {
                    
                        ## If plots per page = 1, a custom file name will be constructed for each one png
                        ## If plots per page > 1, a base file name with an auto-increment token will be created the first pass only
                        
                        ## If its time to start a new PNG file
                        if (figs.per.page == 1 || plots.made == 0) {
                            
                            ## First turn off the device if there's already one running
                            if (plots.made > 0) dev.off()
                            
                            ## if (first.png.made && png.each.plot.separate) dev.off()
                            
                            
                            
                            if (is.null(png.fn)) {
                                
                                ## Construct the pieces or tokens of the filename
                                hs.name.str <- if (png.fn.incld.hs.name) paste(hs.name, "", sep="") else NULL
                                hmap.fn.str <- if (is.na(hmap[hmap.idx,1])) "" else paste(".", sapply(1:length(hmap), function(j) paste(names(hmap)[j], ".", hmap[hmap.idx,j], sep="")), collapse="", sep="")
                                iso.on.plot <- ifelse(iso, ".iso", "")
                                hulls.on.plot <- ifelse(hulls, ".hulls", "")
                                hpp.on.plot <- ifelse(hpp, ".hpp", "")
                                hpp.classify.str <- if (hpp && hpp.classify != "none") paste("-", hpp.classify, sep="") else ""
                                #hpp.sort2 <- ifelse(hpp && !is.null(hulls.pp.sort2), paste("-", hulls.pp.sort2, sep=""), "") 
                                
                                ellipses.on.plot <- ifelse(ellipses, ".elp", "")
                                dr.on.plot <- ifelse(dr, ".dr", "")
                                dr.name <- ifelse(dr, paste(".", names(hs[[hs.name]][["dr"]])[dr.idx], sep=""), "")
                                allpts.on.plot <- ifelse(allpts, ".allpts", "")
                                nn.on.plot <- ifelse(nn, ".nn", "")
                                ptid.token <- ifelse(is.null(ptid), "", paste("ptid", ptidVal, ".", sep=""))
                                ptid.token.manual.num <- if (length(ptid)==1) paste(sprintf("%02d", png.counter), ".", sep="") else ""
                                auto.num <- ".%02d"
                                ## manual.num <- paste(".", sprintf("%02d", png.counter), sep="")
                                
                                if (figs.per.page==1) {
                                    
                                    if (iso && has.iso) {
                                        ## Construct a filename based on the name of this isopleth, example: iso.id.iso-name
                                        ## after we change the standard of iso.name, we'll need to add in the id
                                        fn <- file.path(png.dir, paste(png.fn.pre, hs.name.str, ".", names(hs[[hs.name]][["isos"]])[iso.idx], png.fn.mid, png.fn.suf, ".png", sep=""))
                                    } else {
                                        ## Construct a filename, customized for this plot, in the form
                                        ## ptid, hs.name, hpp, hpp-sort, elpse|dr|allpts|nn, hmap.vals, .png
                                        fn <- file.path(png.dir, paste(png.fn.pre, 
                                                    ptid.token, ptid.token.manual.num,
                                                    hs.name.str, png.fn.mid, hmap.fn.str,
                                                    hulls.on.plot, hpp.on.plot, hpp.classify.str,  
                                                    dr.name,
                                                    ellipses.on.plot, dr.on.plot, allpts.on.plot, nn.on.plot,                    
                                                    png.fn.suf, ".png", sep=""))
                                    }
    
                                    ## If the file exists, overwrite or abort
                                    if (file.exists(fn)) {
                                        cat(" - ", fn, " exists. ", sep="")
                                        if (png.overwrite) {
                                            cat("Overwriting. \n")
                                            if (!file.remove(fn)) stop("Can't overwrite existing png file")
                                        } else {
                                            cat("Aborting. \n"); return(invisible(NULL))
                                        }
                                    }
                                
                                } else {
                                    ## figs.per.page > 1, construct a file name that will be the base for all plots
                                    ## in the form of hs.name + elements on plotted + auto-increment
                                    fn <- file.path(png.dir, paste(png.fn.pre, 
                                                    if (length(ptid)==1) ptid.token else "",
                                                    hs.name, png.fn.mid,
                                                    iso.on.plot, hulls.on.plot, hpp.on.plot, ellipses.on.plot, dr.on.plot, allpts.on.plot, nn.on.plot,
                                                    png.fn.suf, auto.num, ".png", sep=""))
                                    ##if these are isopleth plots, then the base name should be the hs then isos01-02, isos03-04, (bring in plots made)
                                    
                                }
                                ## fn <- normalizePath(path.expand(fn), mustWork=FALSE)  ## this won't work on older versions of R, so will normalize path at end when PNG file(s) exist
                            } else {
                                fn <- png.fn
                            }
                                                    
                            ## Open PNG device
                            ## par(bg="white") probably not needed, but see
                            png(filename=fn, height=png.height, width=png.width, bg="white", pointsize=png.pointsize)
                            
                            # Save / set current device settings on first pass only
                            # Don't have to set multi-frame row because those aren't allowed for PNG files (??)
                            # OMA vals will be set later
                            # CHANGING THIS TO SETTING ON ALL PASSES. DON'T KNOW WHY I THOUGHT MARGINS SHOULD ONLY BE
                            # SET OF FIRST PASS
                            #if (lo.margins.set && plots.made == 0) {
                            if (lo.margins.set) opar <- par(mar=mar, mgp=mgp)
                            
                            res <- c(res, list(list(fn=fn, dim=c(png.width, png.height))))
                            if (status) {cat(" - creating ", fn, "\n", sep=""); flush.console()}
                            
                            png.counter <- png.counter + 1     ## used for ptid.token.manual.num
                        
                        }  ## if figs.per.page == 1 || plots.made==0
                            
                    }
                }   ## Started a new PNG() device if needed
    
                ## previous the chunk of code to compile desc was located here
                
                if (!add) {
                    if (lo.margins.set) if (max(oma.vals) > 0) par(oma=oma.vals)    
                    
                    ## Create a new plot
                    plot(NULL, xlab=if (axes.titles) "x" else "", ylab=if (axes.titles) "y" else "", asp=1, type="n", xlim=xlim.use, ylim=ylim.use,
                         axes=axes.show, xaxt=tick.show, yaxt=tick.show, cex.axis=cex.axis)
                    plots.made <- plots.made + 1
                
                    ## Show the background tiff / gmap
                    if (!is.null(tiff.fn)) {
                        if (tiff.pct) {
                            image(tiff.sgdf, "idx", col=tiff.sgdf.cols[["ct"]], add=TRUE)
                        } else {
                            if (length(tiff.sgdf@data)==3) {
                                image(tiff.sgdf, red=1, green=2, blue=3, add=TRUE)
                            } else {
                                if (length(tiff.col)==0) stop("Something is wrong. tiff.col should not be an empty vector")
                                image(tiff.sgdf, col=tiff.col, add=TRUE)
                            }
                        }
                    } else if (!identical(gmap,"none")) {
                        raster::image(base.map.rast, col=base.map.col, add=T)
                    }

                    ## Plot polygon layers 
                    for (featname in names(gis.features)[sapply(gis.features, function(x) x[["type"]]=="polygon")]) {
                        with(gis.features[[featname]], plot(sdf, col=col, border=border, lty=lty, lwd=lwd, add=TRUE))
                    }
                    
                }
                
    
                ## Get the upper and lower limits of the x and y axes, which we will use later if allpts=T to determine which points to plot
                axes.range <- as.list(par("usr"))
                names(axes.range) <- c("xmin", "xmax", "ymin", "ymax")
                if (allpts || nn) {
                    if (col.allpts=="auto") {
                        if (hpp) {
                            col.allpts.use <- "gray90"
                        } else {
                            col.allpts.use <- topo.colors(nrow(hs[[hs.name]][["pts"]]))
                        }
                    } else if (length(col.allpts) == 1) {
                        col.allpts.use <- rep(col.allpts, nrow(hs[[hs.name]][["pts"]]))
                    } else {
                        col.allpts.use <- col.allpts
                    }
                }
                
                if (is.null(ptid)) {
                    if ((iso || rast) && has.iso) {
                        ## For shorthand, create a copy of the isopleth list element
                        iso.lst <- hs[[hs.name]][["isos"]][[iso.idx]]
                        
                        #num.iso <- length(iso.lst$isopleths)
                        num.iso <- length(hs[[hs.name]][["isos"]][[iso.idx]][["polys"]])
                        
                        ## Create a variable to keep track of any null isopleths to exclude them in the legend
                        ##iso.non.null <- rep(TRUE, num.iso)
    
                        ## If there's been filtering                        
                        ## THIS WHOLE SECTION NEEDS UPDATING
                        if (FALSE) {
                            num.cols.for.subseting <- 200
                            if ((is.null(iso.lst$ecc) && is.null(iso.lst$par)) || !col.iso.scale) {
                                num.cols <- num.iso
                                col.sub <- NULL
                            } else {
                                num.cols <- num.cols.for.subseting + 1
                                if (!is.null(iso.lst$par)) {
                                    col.sub <- c((iso.lst$par[1] - iso.lst$par.fr[1]) / diff(range(iso.lst$par.fr)), 
                                                 (iso.lst$par[2] - iso.lst$par.fr[1]) / diff(range(iso.lst$par.fr)))
                                } else if (!is.null(iso.lst$ecc)) {
                                    col.sub <- c((iso.lst$ecc[1] - iso.lst$ecc.fr[1]) / diff(range(iso.lst$ecc.fr)), 
                                                 (iso.lst$ecc[2] - iso.lst$ecc.fr[1]) / diff(range(iso.lst$ecc.fr)))
                                }
                            }
                        }
                        num.cols <- num.iso
                        col.sub <- NULL
    
                        ## Define the colors from most dense to least dense
                        col.iso.fill.err.msg <- cw("col.iso should be a preset (1-4), or a character vector of color names equal in length to the number of isopleth levels", exdent=2, final.cr=FALSE)
                        if (identical(col.iso.fill, 1)) {
                            ## red to blue
                            col.iso.fill.use <- colorRampPalette(c("#FF0000", "#0000FF", "#BFBFFF"))(num.cols)
                        } else if (identical(col.iso.fill, 2)) {
                            ## yellow to red
                            col.iso.fill.use <- heat.colors(num.cols)
                        } else if (identical(col.iso.fill, 3)) {
                            ## blue to red
                            col.iso.fill.use <- rev(colorRampPalette(c("#FF0000", "#0000FF", "#BFBFFF"))(num.cols))
                        } else if (identical(col.iso.fill, 4)) {
                            ## red to yellow
                            col.iso.fill.use <- rev(heat.colors(num.cols))
                        } else if (is.character(col.iso.fill)) {
                            if (length(col.iso.fill) != num.cols) stop(col.iso.fill.err.msg)
                            col.iso.fill.use <- col.iso.fill                        
                        } else {
                            stop(col.iso.fill.err.msg)
                        }
                        
                        ## Reduce opacity if needed
                        if (col.iso.opacity < 1) {
                            col.iso.fill.use <- paste(col.iso.fill.use, toupper(as.hexmode(floor(255 * col.iso.opacity))), sep="")
                        }
                        
                        ## The big area polygons will draw first, so reverse the colors -- NOT NEEDED 
                        #col.iso.fill.use <- rev(col.iso.fill.use)
                        
                        ## Take a subset of the colors if needed
                        if (!is.null(col.sub)) {
                            print("Taking a subset of colors - needs to be updated")
                            #if (!is.null(iso.lst$par) || !is.null(iso.lst$ecc)) col.iso.fill.use <- rev(col.iso.fill.use)
                            #col.sub.start.end.idx <- 1 + round(col.sub * num.cols.for.subseting)
                            #col.iso.fill.use <- col.iso.fill.use[round(seq(from=col.sub.start.end.idx[1], to=col.sub.start.end.idx[2], length.out=num.iso))]
                        }
                    
                        if (rast) {
                            plot(iso.lst[["rast"]], add=T)
                            if (allpts) plot(hs[[hs.name]][["pts"]], add=TRUE, pch=pch.allpts, col=col.allpts.use, cex=cex.allpts)
                        }

                        if (iso) {
                            if (is.null(iso.level)) {
                                plot(iso.lst[["polys"]], add=TRUE, col=col.iso.fill.use, border=col.iso.border)
                                if (allpts) plot(hs[[hs.name]][["pts"]], add=TRUE, pch=pch.allpts, col=col.allpts.use, cex=cex.allpts)
                            } else {
                                
                                # Commented out in version 1.31
                                #if (allpts) plot(hs[[hs.name]][["pts"]], add=TRUE, pch=pch.allpts, col=col.allpts.use, cex=cex.allpts)
                                
                                iso.levels.thishs <- iso.lst[["polys"]]@data[["iso.level"]]
                                iso.level.rowidx <- which(sapply(iso.levels.thishs, function(x) TRUE %in% sapply(iso.level, function(y) isTRUE(all.equal(x,y)))))
                                if (length(iso.level.rowidx) != length(iso.level)) {
                                  bad.levels <- paste(iso.level[!sapply(iso.level, function(x) TRUE %in% sapply(iso.levels.thishs, function(y) isTRUE(all.equal(x,y))))], sep=", ")
                                  warning(paste("Isopleth level(s) not found: ", bad.levels, sep=""))
                                }
                                
                                # In version 1.31, I decided to display specific isopleth levels not with an outline,
                                # but filled as usual (because holes are not easily distinguished with outlines)
                                # if (is.na(col.iso.border)) col.iso.border <- col.iso.fill.use[iso.level.rowidx]
                                
                                if (length(iso.level.rowidx) > 0) {
                                    #plot(iso.lst[["polys"]][iso.level.rowidx,], add=TRUE, col=NA, border=col.iso.border, lwd=2)
                                    plot(iso.lst[["polys"]][iso.level.rowidx,], add=TRUE, col=col.iso.fill.use[iso.level.rowidx], border=col.iso.border, lwd=2)
                                }
                                if (allpts) plot(hs[[hs.name]][["pts"]], add=TRUE, pch=pch.allpts, col=col.allpts.use, cex=cex.allpts)
                            }
                        }


                        ## Set up the plot title and subtitle
                        if (ufipt) {
                            ## Use the user-friendtly isopleth plot title saved with the iso list element
                            title.feats.str <- c(title.feats.str, iso.lst[["ufipt"]])
                        
                        } else {
                            ## For appearance sake, split the plot tile at a period if too long
                            title.feats.str <- c(title.feats.str, paste(strSplitAtChar(names(hs[[hs.name]][["isos"]])[iso.idx], char=".", size=round(65 /par("mfrow")[2])), sep=""))

                        }

                        ## Plot allpts on top of isos
                        #if (allpts) points(hs[[hs.name]]$xys, pch=ifelse(hpp,3,16), col=col.allpts.use, cex=cex.allpts)

                        ## Plot points and lines gis layers on top of isos
                        for (featname in names(gis.features)[sapply(gis.features, function(x) x$type %in% c("point", "line"))]) {
                            with(gis.features[[featname]], plot(sdf, pch=pch, cex=cex, col=col, lwd=lwd, lty=lty, add=TRUE))
                        }
                        if (length(gis.features)>0) box("plot")
                        
                        if (iso.legend && !add) {
                            if (is.null(iso.level)) {
                                iso.levels.ord <- order(iso.lst[["polys"]]@data[["iso.level"]])
                            } else {
                                iso.levels.ord <- sort(iso.level.rowidx)
                            }
                            legend("topleft", legend=as.character(iso.lst[["polys"]]@data[["iso.level"]][iso.levels.ord]), fill=col.iso.fill.use[iso.levels.ord], bg="white", title="Iso Level", cex=cex.legend)
                            box()
                        }
                        
                    } else {
                        ## Not isos, must be hulls, hpp, allpts, nn, and/or ellipses
                        
                        ## Plot points and lines gis layers first
                        for (featname in names(gis.features)[sapply(gis.features, function(x) x[["type"]] %in% c("point", "line"))]) {
                            with(gis.features[[featname]], plot(sdf, pch=pch, cex=cex, col=col, lwd=lwd, lty=lty, add=TRUE))
                        }
    
                        if (hulls) {
                            plot(hs[[hs.name]][["hulls"]], add=TRUE, border=col.hulls.border, col=col.hulls.fill)
                        }
        
                        if (allpts) plot(hs[[hs.name]][["pts"]], add=TRUE, pch=pch.allpts, col=col.allpts.use, cex=cex.allpts)
    
                        if (dr) {
                            for (i in 1:length(hs[[hs.name]][["dr"]][[dr.idx]][["lines"]])) {
                                points(coordinates(hs[[hs.name]][["pts"]])[hs[[hs.name]][["dr"]][[dr.idx]][["lines"]][[i]], ], type="l", col=col.dr, lwd=lwd.dr)
                            }
                            
                            ## Laydown points (again) in case they got covered up by the lines
                            for (featname in names(gis.features)[sapply(gis.features, function(x) x$type %in% c("point"))]) {
                                with(gis.features[[featname]], plot(sdf, pch=pch, cex=cex, col=col, add=TRUE))
                            }
                            
                            
                        }
                        
                        if (hpp) {

                            hpp.xys <- coordinates(hs[[hs.name]][["pts"]])[hs[[hs.name]][["hulls"]]@data[["pts.idx"]], , drop=FALSE]
                            title.feats.str <- c(title.feats.str, "hull parent points")
                            
                            ## If there is an aoi, filter out those points which fall outside the aoi 
                            if (!is.null(aoi)) {
                                xys.in.aoi <- (hpp.xys[,1] >= aoi[1,1] & hpp.xys[,1] <= aoi[2,1] & hpp.xys[,2] >= aoi[2,2] & hpp.xys[,2] <= aoi[1,2])
                            } else {
                                xys.in.aoi <- rep(TRUE, nrow(hpp.xys))
                            }
                            
                            ## This object sets up a loop that has since been removed from the package
                            ## To put it back in, see "plot.locoh.lhs.still-has-code-for-hmap.grp.R"
                            # hmap.grp.iter <- NA
                            
                            ## Take note of hmap.idx so we can restore it later
                            hmap.idx.orig <- hmap.idx
                            
                            if (hpp.classify != "hsp") col.hpp.use.orig <- col.hpp.use

                                if (hmap.idx >= 1 && hmap.idx <= nrow(hmap)) {
                            
                                    if (hpp.classify == "none") {
                                        ## Plot points with a single-color
                                        points(hpp.xys[xys.in.aoi, , drop=FALSE], type="p", pch=16, col=col.hpp, cex=cex.hpp) 
                                    
                                    } else if (hpp.classify == "hsp") {
                                        ## Plot with a hsp object
                                        
                                        if (hmap.idx.orig == hmap.idx) title.hpp.classify <- c(title.hpp.classify, paste(if (ufat) hme[[hsp.use[["x.axis"]]]][["ufat"]] else hsp.use[["x.axis"]], "-", if (ufat) hme[[hsp.use[["y.axis"]]]][["ufat"]] else hsp.use[["y.axis"]], sep=""))
        
                                        ## Pull out the values that will be used to color the points
                                        hsp.xvals <- eval(hme[[hsp.use[["x.axis"]]]][["expr"]])
                                        hsp.yvals <- eval(hme[[hsp.use[["y.axis"]]]][["expr"]])
                                        if (length(hsp.xvals)==0 || length(hsp.yvals)==0) stop("Can't find values for saved hull scatter plot")
                                        
                                        if (hmap.idx.orig == hmap.idx) title.hmap <- paste(unique(unlist(sapply(c(hsp.use[["x.axis"]],hsp.use[["y.axis"]]), function(myaxis) sapply(hme[[myaxis]][["req.ap.subtitle"]], function(x) eval(x))))), collapse=", ", sep="")
        
                                        ## Transform the x values if needed
                                        if (is.null(hsp.use[["trans.x"]])) {
                                            trans.x.str <- ""
                                        } else {
                                            trans.x.str <- paste(hsp.use[["trans.x"]], " of ", sep="")
                                            hsp.xvals <- get(hsp.use[["trans.x"]])(hsp.xvals)
                                        }
                        
                                        ## Transform the y values if needed
                                        if (is.null(hsp.use[["trans.y"]])) {
                                            trans.y.str <- ""
                                        } else {
                                            trans.y.str <- paste(hsp.use[["trans.y"]], " of ", sep="")
                                            hsp.yvals <- get(hsp.use[["trans.y"]])(hsp.yvals)
                                        }
                                        
                                        pt.ord.by.col <- 1:length(hsp.xvals)
                                        if (hsp.use[["col"]]=="spiral") {
                                            hulls.pp.cols <- hsp.col.spiral(hsp.xvals, hsp.yvals, sat.base=hsp.use[["sat.base"]], val.base=hsp.use[["val.base"]], hue.offset=hsp.use[["hue.offset"]], center.method=hsp.use[["center.method"]]) 
                                        } else {
                                            hulls.pp.cols <- hsp.use[["col"]]
                                        }
                                        
                                        ## If there are regions, apply those colors
                                        if (!is.null(hsp.use[["regions"]])) {
        
                                            ## Get a character vector containing a color string for each hull parent point
                                            hulls.pp.cols <- hsp.col.reg(hsp.xvals, hsp.yvals, regions=hsp.use[["regions"]], col=hulls.pp.cols)
                                            
                                            ## Next, we want to come up with an alternative order of the points, so the points that are different than the 
                                            ## default color, which we presume to be the points of interest, are drawn last (and hence are not hidden) 
                                            
                                            ## Add a row number to hulls.pp.cols because later we will need to re-sort after the merge operation
                                            hulls.pp.cols.orn <- data.frame(orn=1:length(hulls.pp.cols), col=hulls.pp.cols)
                                            
                                            ## Make a character vector of all the colors used
                                            cols.used <- unique(c(sapply(hsp.use$regions, function(x) x[["col"]]), hulls.pp.cols))
                                            
                                            ## Add a sort color by (scb) column, so that the non-default colors are plotted *last*
                                            cols.used.df <- data.frame(col=cols.used, scb=length(cols.used):1)
                                            
                                            ## Merge the two tables on the common column 'col'. 
                                            hulls.pp.cols.rank.merged <- merge(x=hulls.pp.cols.orn, y=cols.used.df)
                                            
                                            ## Resort on 'orn' to recover the original sort order that coincides with the original point order
                                            hulls.pp.cols.rank.merged <- hulls.pp.cols.rank.merged[order(hulls.pp.cols.rank.merged[["orn"]]),]
                                            
                                            ## Get the order by rank
                                            pt.ord.by.col <- order(hulls.pp.cols.rank.merged[["scb"]])
                                            
                                        }
                                        
            
                                        ## If there is an aoi, filter out those points which fall outside the aoi 
                                        pt.ord.by.col <- pt.ord.by.col[xys.in.aoi[pt.ord.by.col]]
            
                                        ## Add the hull parent points to the plot
                                        points(hpp.xys[pt.ord.by.col, , drop=FALSE], type="p", pch=16, col=hulls.pp.cols[pt.ord.by.col], cex=cex.hpp)
            
                                        ## No legend option with two-series coloring (yet)
                                        ##if (hpp.classify.legend && col.hpp != 3) legend("topleft", legend=legend.labels, fill=legend.col, bg="white", cex=0.8, title=hulls.pp.sort)
                                        
                                    
                                    } else {
                                    
                                        ## hpp is not none and not hsp
                                            
                                        ## Classify with a single hull metric, color using a color ramp

                                        if (hmap.idx.orig == hmap.idx) title.hpp.classify <- c(title.hpp.classify, paste(if (ufat) eval(hme[[hpp.classify]][["ufat"]]) else hpp.classify, sep=""))
                                        if (hmap.idx.orig == hmap.idx) title.hmap <- paste(unlist(sapply(hme[[hpp.classify]][["req.ap.subtitle"]], function(x) eval(x))), collapse=", ", sep="")
                                        

                                        ## Pull out the values that will be used to color the points
                                        hulls.pp.classify.val <- eval(hme[[hpp.classify]][["expr"]])
                                        if (is.null(hulls.pp.classify.val)) stop(paste("Can not evaluate: ", hme[[hpp.classify]][["expr"]], sep=""))


                                        if (TRUE %in% is.infinite(unlist(hulls.pp.classify.val))) {
                                            stop("Found infinite values in the hull metric. Investigate.")
                                        }
                                        
                                        if (is.vector(hulls.pp.classify.val)) {
                                            ## Create a default vector of gray colors for hpp
                                            hulls.pp.cols <- rep(col.hpp.na, nrow(hpp.xys))

                                        ## Pick out the points that have finite values (i.e., not NA)
                                            f.idx <- which(is.finite(hulls.pp.classify.val))

                                            if (hme[[hpp.classify]][["discrete"]]) {

                                                if (hpp.classify.common.scale.discrete) {
                                                    ## Look up the matching row in hpp.possible.vals, already created, and use that value of col.hpp.use
                                                    hulls.pp.cols[f.idx] <- col.hpp.use[ match(hulls.pp.classify.val[f.idx], hpp.possible.vals)]

                                                } else {

                                                    hpp.possible.vals <- sort(unique(hulls.pp.classify.val[f.idx]))

                                                    if (identical(col.ramp, "rainbow")) {
                                                        col.hpp.use <- rainbow(length(hpp.possible.vals), end=5/6)
                                                    } else {
                                                        col.hpp.use <- colorRampPalette(vectorize.parameter(col.ramp, type="character", sort.res=FALSE))(length(hpp.possible.vals))
                                                    }

                                                    ## Prepare labels for the legend
                                                    legend.labels <- as.character(hpp.possible.vals)
                                                    legend.col <- col.hpp.use

                                                    zero.idx <- which(hpp.possible.vals == 0)
                                                    if (length(zero.idx) > 0 && hme[[hpp.classify]][["zero2na"]]) {
                                                        col.hpp.use[zero.idx] <- NA
                                                        legend.labels <- legend.labels[-zero.idx]
                                                        legend.col <- legend.col[-zero.idx]
                                                    }

                                                    hulls.pp.cols[f.idx] <- col.hpp.use[ match(hulls.pp.classify.val[f.idx], hpp.possible.vals)   ]
                                                }

                                            } else {

                                                if (diff(range(hulls.pp.classify.val[f.idx])) == 0) {
                                                    ## There's only one value, can't apply a color ramp
                                                    col.allsame <- "#0000FF"
                                                    hulls.pp.cols[f.idx] <- col.allsame
                                                    col.hpp.use <- col.allsame

                                                    ## Define hulls.pp.classify.val.cut and hulls.pp.cols.ramp for the legend
                                                    hulls.pp.classify.val.cut <- as.factor(hulls.pp.classify.val[f.idx])

                                                } else {

                                                    ## Scale the classify values 0-1, chopping off a proportion of the tails given by hpp.classify.chop

                                                    ## Chopping off a proportion of the tails given by hpp.classify.chop
                                                    hulls.pp.classify.val.new.range <- sort(hulls.pp.classify.val[f.idx])[c(max(1, length(f.idx) * hpp.classify.chop), length(f.idx) * (1 - hpp.classify.chop))]
                                                    hulls.pp.classify.val[hulls.pp.classify.val[f.idx] < hulls.pp.classify.val.new.range[1]] <- hulls.pp.classify.val.new.range[1]
                                                    hulls.pp.classify.val[hulls.pp.classify.val[f.idx] > hulls.pp.classify.val.new.range[2]] <- hulls.pp.classify.val.new.range[2]

                                                    ## Next put the values in evenly-spaced bins
                                                    hulls.pp.classify.val.cut <- cut(hulls.pp.classify.val[f.idx], breaks=hpp.classify.bins, dig.lab = 2)

                                                    ## Reverse the color ramp if this is one of those metrics where we want high values to be blue and low values to be red
                                                    ## if (!hme[[hpp.classify]][["iso.dec"]]) col.hpp.use <- rev(col.hpp.use)

                                                    #hulls.pp.classify.val.reverse = list("area"=TRUE, "nnn"=FALSE, "nep"=FALSE, "ecc"=FALSE, "par"=FALSE, "ptbo"=FALSE, "mnlotb"=FALSE, "nmnlotb"=FALSE, "ennn"=FALSE)
                                                    #if (hulls.pp.classify.val.reverse[[hulls.pp.sort]])

                                                    ## Create a vector of color codes for the points
                                                    hulls.pp.cols[f.idx] <- col.hpp.use[as.numeric(hulls.pp.classify.val.cut)]

                                                    ## Prepare labels for the legend
                                                    legend.labels <- levels(hulls.pp.classify.val.cut)
                                                    legend.col <- col.hpp.use
                                                }
                                            }


                                            ## If there are any hulls.pp points that have 'NA' as the metric value, given them a special color
                                            napp.idx <- which(is.na(hulls.pp.classify.val))
                                            if (length(napp.idx) > 0) {
                                                if (!"no hull value" %in% legend.labels) {
                                                    legend.labels <- c(legend.labels, "no hull value")
                                                    legend.col <- c(legend.col, col.hpp.na)
                                                    hulls.pp.cols[napp.idx] <- col.hpp.na
                                                }
                                            }



                                            ## If showing all points and there are fewer hulls than all points, add 'no-hull' to the legend
                                            if (allpts && (length(hs[[hs.name]][["hulls"]]) < length(hs[[hs.name]][["pts"]]))) {
                                                if (!"no hull" %in% legend.labels) {
                                                    legend.labels <- c(legend.labels, "no hull")
                                                    legend.col <- c(legend.col, col.allpts.use)
                                                }
                                            }

                                        } else {
                                            #print("Lets work on an expression for hulls.pp.cols")
                                            # This is completely ad-hoc, need to either remove or make it part of the hm.expression
                                            # Also need to figure out where to plot the legend (same problem as hsp legend)
                                            
                                            xtab.df <- data.frame(as.list(table(hs[[hs.name]][["hulls"]]@data[[hs[[hs.name]][["hm"]][which(sapply(hs[[hs.name]][["hm"]], function(x) x$type == "pep"))[1]][[1]][["aux"]][["pep.var"]]]])))
                                            oid.vals <- as.character(sapply(hs[[hs.name]][["hm"]][sapply(hs[[hs.name]][["hm"]], function(x) x$type == "pep")], function(x) x$aux$pep.val))
                                            prop.all <- as.numeric(xtab.df[oid.vals  ] / sum(xtab.df[oid.vals ]))
                                            hulls.pp.cols <- multi.proportion.col(hulls.pp.classify.val, prop.all=prop.all)[["col"]]

                                        }
        
                                        points(hpp.xys[xys.in.aoi, , drop=FALSE], type="p", pch=16, col=hulls.pp.cols[xys.in.aoi], cex=cex.hpp)
                                        if (hpp.classify.legend && hmap.idx == hmap.idx.orig) {
                                            legend("topleft", legend=legend.labels, fill=legend.col, bg="white", cex=0.8, title = if (ufat) hme[[hpp.classify]][["ufat"]] else hpp.classify)
                                            box()
                                        }
                                    
                                    }    ## plot with a single hull metric and a color ramp
                                
                                }
                                
                            
                            
                            
                            ## end loop for (hg.idx in hmap.grp.iter) 
                            ## }
                            
                            ## Restore hmap.idx
                            hmap.idx <- hmap.idx.orig
                            
                            
    
                        }
    
        
                        if (ellipses) {
                            title.feats.str <- c(title.feats.str, "ellipses")
                            theta <- seq(0, 2 * pi, length = 72)
                            for (i in 1:nrow(hs[[hs.name]][["ellipses"]])) {
                                ellps.pts <- with(hs[[hs.name]][["ellipses"]][i, ],
                                              data.frame(x = cx + a * cos(theta) * cos(alpha) - b * sin(theta) * sin(alpha),
                                                         y = cy + a * cos(theta) * sin(alpha) + b * sin(theta) * cos(alpha)))
                                points(ellps.pts, type="l", col=col.ellipses)
                            }
                        }
                        
                        if (length(hs) * length(n2z(ptid)) > 1) box()
        
                    }
                    #if (length(gis.features)>0) box("plot")
        
                } else {
                
                    ## There's a ptid
                    
                    ## Plot background points first
                    if (allpts) {
                        #pts.in.box.idx <- with(hs[[hs.name]], which(xys[,1] >= axes.range$xmin & xys[,1] <= axes.range$xmax & xys[,2] >= axes.range$ymin & xys[,2] <= axes.range$ymax))
                        
                        ## Create a SpatialPolygons object for the plot area then identify which of allpts fall in it
                        filter.to.plot.area <- FALSE
                        if (filter.to.plot.area) {
                            pa <- par("usr")
                            pa.box.x <- pa[c(1,2,2,1,1)]
                            pa.box.y <- pa[c(4,4,3,3,4)]
                            pa.box.sp <- SpatialPolygons(list(Polygons(list(Polygon(cbind(pa.box.x, pa.box.y))), ID="1")), proj4string=hs[[hs.name]][["pts"]]@proj4string)
                            pts.in.pa <- over(pa.box.sp, as(hs[[hs.name]][["pts"]], "SpatialPoints"), returnList=T)[[1]]
                        } else {
                            pts.in.pa <- rep(TRUE, length(hs[[hs.name]][["pts"]]))
                        }
                        
                        plot(hs[[hs.name]][["pts"]][pts.in.pa,], add=TRUE, pch=16, col=col.allpts.use[pts.in.pa], cex=cex.allpts)
                        
                    }
                    
                    ## Highlight the parent point
                    if (ptid.highlight) {
                        plot(hs[[hs.name]][["pts"]][pp.idx, ], add=TRUE, pch=16, cex=cex.allpts)
                        plot(hs[[hs.name]][["pts"]][pp.idx, ], add=TRUE, pch=24, cex=2)
                        
                    }

                    ## Next, highlight the nearest neighbors
                    if (nn) {
                        points(pp.pts, cex=cex.pp, col=col.nn.pp, bg=col.allpts.use[pts.idx], pch=24)
                        points(pp.pts, cex=1, col="black", pch=20)
                        points(nn.pts, cex=cex.nn, col=col.nn, pch=1)
                    } 
                    
                    ## Next, add the hull
                    #if (hulls) points(hull.pts, type="l", col=col.hulls.border)
                    if (hulls) polygon(hull.pts, border=col.hulls.border, col=col.hulls.fill)
                    
                    ## Finally, add the bounding ellipse
                    if (ellipses) points(ellps.pts, type="l", col=col.ellipses)

                }
                if (length(gis.features)>0) box("plot")
                
                #hmap.subtitles <- paste(unlist(sapply(c(x.axis, y.axis), function(myaxis) sapply(hme[[myaxis]][["req.ap.subtitle"]], function(x) eval(x)))), collapse="; ", sep="")

                ## Add the plot title
                if (title.show) {
                    #if (is.null(title)) {

                    
                        title.pieces <- unlist(sapply(title.inc, function(x) eval(title.pieces.exp.lst[[x]])))
                        subtitle.pieces <- unlist(sapply(subtitle.inc, function(x) eval(title.pieces.exp.lst[[x]])))
                        if (is.null(subtitle.pieces)) {
                            subtitle.pieces.str <- NULL
                        } else {
                            subtitle.pieces.str <- paste("\n", paste(subtitle.pieces, collapse=", ", sep=""), sep="")
                        } 
                        title.use <- paste(paste(title.pieces, collapse=", ", sep=""), subtitle.pieces.str, sep="")
                    
                    #} else {
                    #    title.use <- title
                    #}
                    title(title.use)
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
                # if (col.hpp != "reg" || is.null(spc.reg.det.xaxis)) title(paste(plot.title, subtitle, sep=""))
                
                ## Add descriptive text in the outer margin
                if (desc !=0 ) mtext(desc.str.chopped[1], side=desc, outer=TRUE, line=0 + if (desc==1) (as.numeric(desc.str.chopped[2]) - 1.25) else 0, cex=cex.desc, col=col.desc)
                desc.all <- c(desc.all, desc.str)
                
            }
            }    
            }
            }
            ## Done making the individual plots

        }
    }
    if (iso && iso.num.plotted==0) cat("No isopleths found that meet these criteria. \n")
    if (!is.null(png.dir) || !is.null(png.fn)) {
        invisible(dev.off())
        
        ## Recreate result if figs.per.page > 1 (because only element in fn but possibly multiple files created)
        if (figs.per.page > 1) {
            width.height <- res[[1]][["dim"]]
            png.all <- sprintf(res[[1]][["fn"]], 1:ceiling(plots.made/figs.per.page) )
            res <- NULL
            for (png.file in png.all) res <- c(res, list(list(fn=png.file, dim=width.height)))
        }
        for (i in 1:length(res)) {
            res[[i]][["fn"]] <- normalizePath(path.expand(res[[i]][["fn"]]))            
            res[[i]][["desc"]] <- desc.all[i]
        }
                
    } else {
        res <- desc.all
    }
    return(invisible(res))
    
}
