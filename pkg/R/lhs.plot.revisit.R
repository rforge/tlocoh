#' Plot distribution of revisit times
#'
#' Plots a histogram of the revisit times a LoCoH-hullset area
#'
#' @param lhs A LoCoH-hullset object
#' @param id The id(s) of the individual(s) to include in the plot
#' @param k A k-value for the number of nearest neighbors around each point to include in the plot
#' @param r A r-value for the number of nearest neighbors around each point to include in the plot
#' @param a A a-value for the number of nearest neighbors around each point to include in the plot
#' @param s The s value(s) of nearest neighbor sets to include in the plot. If NULL, all values will be used
#' @param hs.names The name(s) of saved hullsets to plot
#' @param ta.min The minimum time away (in seconds) to include on the histogram , can also be 'auto'
#' @param ta.max The maximum time away (in seconds) to include. If NULL then no upper limit will be imposed
#' @param ta.min.auto.tau The minimum time away to include in 'auto' expressed as the number of median sampling intervals. Ignored if \code{ta.min} is not \code{'auto'}.
#' @param xaxis.vals A numeric vector of time-away values (in seconds) that will be labeled on the x-axis.
#' @param breaks A value for 'breaks' that will be passed to the \code{hist} function (see help function for breaks)
#' @param col A color value for the histogram bars
#' @param bg  A color value for the plot background
#' @param figs.per.page Number of plots per page
#' @param legend Whether to include a legend. T/F.
#' @param title The title to be displayed. Character. If NULL a title will be constructed.
#' @param title.show Whether to show the title. T/F.
#' @param subtitle Whether to add a subtitle to the automatically constructed title (when \code{title=NULL}, otherwise ignored)
#' @param mar The plot margins. A four item numeric vector
#' @param mgp The distance away from the edge of the plot for the 1) label, 2) tick marks, and 3) axis line. A three-item numeric vector
#' @param png.fn A filename for a PNG file
#' @param png.dir The directory for a PNG file (filename will be constructed automatically). Ignored if png.fn is passed
#' @param png.dir.make Whether to create png.dir if it doesn't exist. T/F
#' @param png.width The width of the PNG image
#' @param png.height The height of the PNG image
#' @param png.pointsize The pointsize (in pixels) for the PNG image, equivalent to the height or width of a character in pixels (increase to make labels appear larger)
#' @param png.fn.pre A prefix that will be used in the construction of the PNG filename. Ignored if png.fn is passed.
#' @param png.fn.suf A suffix that will be used in the construction of the PNG filename. Ignored if png.fn is passed.
#' @param png.overwrite Whether to overwrite an existing PNG file if it exists. T/F.
#' @param panel.num A number or letter to display in the upper left hand corner of the plot when the plot will be used as part of a multi-frame graphic (as in publications). Character
#' @param panel.num.inside.plot Whether to display panel.num inside the plot area itself, as opposed to the title area. Ignored if panel.num is NULL. T/F
#' @param legend.space The amount of additional space on the lower end of the x-axis to make room for the legend. Expressed as a proportion of the range of the x-axis values
#' @param ... Additional parameters that will be passed to the \code{\link{hist}} function
#'
#' @return A list of objects of class histogram (one for each hullset plotted)
#'
#' @details This function will plot a histogram of the revisitation times for a hullset. Revisitation time is simply the time between points enclosed in a hull. Basically, all points enclosed by each hull are examined and their time intervals computed. This may help reveal where there are natural temporal cycles in revisitation, for example if you see a spike in revisitation around 24 hours there may be daily revisitation pattern in some of the hulls. You can specify the minimum amount of time-away to show in the histogram (e.g., if enclosed points that are separated by the median sampling interval are of little interest), as well as the maximum time-away period. Note that revisition metrics do *not* have to be computed for the hullset for the histogram to be computed.
#'
#' @seealso \code{\link{lhs.visit.add}}
#'
#' @export
#' @import pbapply

lhs.plot.revisit <- function(lhs, id=NULL, k=NULL, r=NULL, a=NULL, s=NULL, hs.names=NULL, 
                             ta.min="auto", ta.max=NULL, breaks=40, xaxis.vals=NULL, ta.min.auto.tau=4, col="gray80", 
                             figs.per.page=1, title=NULL, title.show=TRUE, subtitle=TRUE, 
                             mar=c(2.8, 3.2, if (title.show) (if (subtitle) 3.2 else 2.3) else 0.5, 0.5), mgp=c(1, 0.7, 0), 
                             png.fn=NULL, png.dir=NULL, png.dir.make=TRUE, png.width=800, png.height=png.width, png.pointsize=12+(png.width-480)/80,
                             png.fn.pre=NULL, png.fn.suf=NULL, png.overwrite=TRUE,
                             panel.num=NULL, panel.num.inside.plot=!title.show, bg="white", legend.space=if (legend) 0.05 else 0, ...) {

  ## If overlay=T, all series are combined on one axis
  ## Taken out: series=c("hullsets","iso.levels")[ifelse(length(lhs)==1,1,2)], 

    if (!inherits(lhs, "locoh.lhs")) stop("lhs should be of class \"locoh.lhs\"")
    if (TRUE %in% sapply(lhs, function(hs) is.null(hs[["pts"]][["dt"]]))) stop("Date stamps not found, can't plot revisitation times")

    if (is.null(id) && is.null(r) && is.null(k) && is.null(a) && is.null(s) && is.null(hs.names)) {
        hs <- lhs
    } else {    
        for (str.param in c("k","a","r","s")) assign(str.param, vectorize.parameter(get(str.param)))
        hs <- lhs.select(lhs, id=id, r=r, k=k, a=a, s=s, hs.names=hs.names)
    }
    if (length(hs)==0) stop("No hullsets found matching those criteria")

    ## See if the output directory exists
    if (is.null(png.fn) && !is.null(png.dir) && !file.exists(png.dir)) {
        if (png.dir.make) {
            dir.made <- dir.create(png.dir)
            if (!dir.made) stop(paste("Couldn't make output folder ", png.dir, sep=""))            
        } else {
            stop(paste(png.dir, " doesn't exist and png.dir.make is False, so can not continue."))
        }
    }
    
    if (is.null(png.dir) && is.null(png.fn)) {
        opar <- par(mfrow = n2mfrow(figs.per.page), mar=mar, mgp=mgp, bg=bg)
        on.exit(par(opar))
    }
    
    res <- list()
    
    for (hs.idx in 1:length(hs)) {
        ## Open a PNG device if needed
        if (!is.null(png.dir) || !is.null(png.fn)) {
            png.fn.use <- NULL
            if (is.null(png.fn)) {
                png.fn.use <- file.path(png.dir, paste(png.fn.pre, names(hs)[hs.idx], png.fn.suf, ".png", sep=""))
            } else {
                png.fn.use <- png.fn
            }
            if (file.exists(png.fn.use) && !png.overwrite) stop(paste(png.fn.use, "exists"))
            png(filename=png.fn.use, height=png.height, width=png.width, pointsize=png.pointsize, bg=bg)
            #res <- c(res, png.fn.use)
        }
        if (is.null(opar)) opar <- par(mfrow = n2mfrow(figs.per.page), mar=mar, mgp=mgp)

        dt.int <- as.numeric(hs[[hs.idx]][["pts"]][["dt"]])
        if (identical(ta.min,"auto")) {
            ta.min.use <- hs[[hs.idx]][["rw.params"]][["time.step.median"]] * ta.min.auto.tau
        } else {
            ta.min.use <- ta.min
        }

        cat("  Computing revisit times for ", names(hs)[hs.idx], "\n", sep="")
        all.revisit.times <- unlist(pblapply(hs[[hs.idx]][["enc.pts"]][["idx"]], function(x) diff(dt.int[x])))        
        
        ## Keep only those that are larger than ta.min.tau
        if (!is.null(ta.min.use)) all.revisit.times <- all.revisit.times[all.revisit.times >= ta.min.use]            
        if (!is.null(ta.max)) all.revisit.times <- all.revisit.times[all.revisit.times <= ta.max]

        ## Construct the plot title
        if (title.show) {
            if (is.null(title)) {
                title.use <- paste("Revisitation Times\n", names(hs)[hs.idx], sep="")
            } else {
                title.use <- title
            }
        } else {
            title.use <- NULL
        }
        
        ## Compute the histogram. We won't actually print just yet because we are adding labels manually and 
        ## may need to enlarge the xlim to include '0' 
        hist.obj <- hist(all.revisit.times, breaks=breaks, plot=FALSE)
        
        # Compute the xaxis label points
        if (is.null(xaxis.vals)) {
            xaxis.at <- pretty(range(hist.obj[["breaks"]]))
        } else {
            xaxis.at <- xaxis.vals
        }
        xaxis.labels <- sapply(xaxis.at, secs.fmt)
        
        ## Now plot the histogram specifying xlim but without axes
        plot(hist.obj, axes=FALSE, xlab="time away", ylab="frequency", main=title.use, col=col, xlim=range(c(xaxis.at, hist.obj$breaks)), ...)
        
        ## Add the axes manually
        axis(side=1, cex.axis=0.9, pos=0, at=xaxis.at, labels=xaxis.labels)
        axis(side=2, cex.axis=0.9, pos=xaxis.at[1])

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

        ## Close PNG device
        if (!is.null(png.dir) || !is.null(png.fn)) invisible(dev.off())
        
        res[[names(hs)[hs.idx]]] <- hist.obj
    
    }
    
    return(invisible(res))

    #if (!is.null(png.dir) || !is.null(png.fn)) {
    #    cat(paste(" - ", res, collapse="\n", sep=""), "\n", sep="")
    #} 
    
}
