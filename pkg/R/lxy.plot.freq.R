#' Plots of sampling frequency
#'
#' Creates plots of the temporal extent and sampling frequency of a LoCoH-xy object
#'
#' @param lxy A LoCoH-xy object
#' @param id The id(s) of the individual(s) to include in the plot
#' @param deltat.by.date Produce a plot of the sampling interval by date. T/F
#' @param samp.by.date Produce a scatterplot showing the number of samples by date. T/F
#' @param cp Produce a plot of the cummulative proportion of sampling frequencies. T/F
#' @param cp.min The minimum sampling frequency (expressed as a proportion of tau) shown on the plot
#' @param cp.max The maximum sampling frequency (expressed as a proportion of tau) shown on the plot
#' @param time.unit A unit of time that will be used on the y-axis when plotting frequency by date
#' @param xlim The range for the x-axis. Can be a two-element vector (min, max), NULL (range will be
#' computed from the data), or "same" (same range will be used for all individuals).
#' @param ylim The range for the y-axis.
#' @param ylim.deltat.by.date A two-element numeric vector with values 0..1. Will interpret these values as the first and last percentiles of delta.t to plot
#' @param sbd.y.jiggle An amount to 'jiggle' the y values to better see point density
#' @param sbd.vline An interval of time (in seconds) for which vertical lines will be included on the sampling interval by date plot, to help see
#' the dates
#' @param sbd.vline.trunc.units.to A character "secs", "mins", "hours" or "days" specifying where to draw the first vertical lines. 
#' quantiles of delta.t when setting the range of ylim when \code{deltat.by.date=TRUE} 
#' @param cex.pts The expansion factor for points on the plot
#' @param desc Which side to display automatically generated desciptive text (e.g. caption). 0=none, 1=bottom, 3=top.
#' @param cex.desc The expansion factor for the descriptive text. Numeric value.
#' @param col.desc The color of the descriptive text. Color value.
#' @param mar The plot margins. A four item numeric vector
#' @param mgp The distance away from the edge of the plot for the 1) label, 2) tick marks, and 3) axis line. A three-item numeric vector
#' @param figs.per.page The number of plots per page.
#' @param status Show status messages. T/F
#' @param title The title to be displayed. Character. If NULL a title will be constructed.
#' @param title.show Whether to show the title. T/F
#' @param subtitle The plot subtitle. If 'auto' a default subtitle will be used, or pass an empty string \code{''} to omit. Character
#' @param panel.num A number or letter to display in the upper left hand corner of the plot when the plot will be used as part of a multi-frame graphic (as in publications). Character
#' @param panel.num.inside.plot Whether to display panel.num inside the plot area itself, as opposed to the title area. T/F
#' @param png.dir The directory for a PNG file (filename will be constructed automatically). Ignored if png.fn is passed
#' @param png.dir.make Whether to create png.dir if it doesn't exist. T/F
#' @param png.width The width of the PNG image
#' @param png.height The height of the PNG image
#' @param png.overwrite Whether to overwrite an existing PNG file if it exists. T/F
#' @param png.pointsize The pointsize (in pixels) for the PNG image, equivalent to the height or width of a character in pixels (increase to make labels appear larger)
#' @param ... Additional parameters that will be passed to the \code{\link{plot}} function
#'
#' @note Plotting the cummulative percentage of sampling frequencies (code{cp=TRUE}) can help determine a threshhold below which
#' pairs of points should be considered to be part of the same 'burst' (see \code{\link{lxy.thin.bursts}}). For example, if most points 
#' are sampled 1 hour apart, but there are a few bursts of points that were only 2 minutes apart, the cumuulative percentage 
#' of sampling frequencies will reveal a cluster of sampling intervals around 2 minutes and the majority around 1 hour.
#'
#' Plotting the sampling frequency over time can help identify gaps in the data as well as frequencies for harmonising sampling frequency
#'
#' @return A list of lists, one for each plot containing the filename (NULL if no png made), the image dimensions (NULL), descriptive text, and id
#'
#' @export
#' @seealso \code{\link{lxy.thin.bursts}}, \code{\link{lxy.thin.byfreq}}

lxy.plot.freq <- function(lxy, id=NULL, deltat.by.date=F, samp.by.date=F, cp=F, cp.min=0, cp.max=1,
                            time.unit=c("auto", "sec", "min", "hour", "day", "week", "month", "year")[1],
                            xlim=NULL, ylim=NULL, ylim.deltat.by.date=c(0, 0.999), sbd.y.jiggle=0.1,
                            sbd.vline=NULL, sbd.vline.trunc.units.to = c("secs", "mins", "hours", "days")[4],
                            cex.pts=0.3, desc=c(0,1,3)[2], cex.desc=0.8, col.desc="darkgreen", 
                            title=NULL, title.show=TRUE, subtitle="auto",
                            mar=c(2.8, 2.8, if (title.show) 2.8 else 0.5, 0.5), mgp=c(1.8, 0.5, 0), figs.per.page=NULL, status=TRUE,
                            panel.num=NULL, panel.num.inside.plot=!title.show, 
                            png.dir=NULL, png.dir.make=TRUE, png.width=800, png.height=png.width, png.overwrite=TRUE, png.pointsize=12+(png.width-480)/80, ...) {

    if (!inherits(lxy, "locoh.lxy")) stop("lxy should be of class \"locoh.lxy\"")
    if (!require(sp)) stop("package sp required")
    if (is.null(lxy[["pts"]][["dt"]])) stop("Date-time values not found")
    if (!cp && !deltat.by.date && !samp.by.date) stop("Don't know what to plot. Set cp, deltat.by.date, or samp.by.date to TRUE")
    if ((cp || deltat.by.date) && samp.by.date) stop("You can only plot samples by date by itself")
    if (!is.numeric(desc) || desc > 4)  stop("Desc must be 0 (no description) or between and 1 and 4 (side to display description")
    if (!time.unit %in% c("auto", "sec", "min", "hour", "day", "week", "month", "year")) stop("Unknown value for time.unit")
    if (!identical(xlim, "same") && !is.null(xlim) && ((cp + deltat.by.date) > 1 )) stop("If you provide a value for xlim, you can only cp *or* deltat.by.date, not both")

    if (is.null(id)) {
        id <- rev(levels(lxy[["pts"]][["id"]]))
    } else {
        if (FALSE %in% (id %in% levels(lxy[["pts"]][["id"]]))) stop("id value(s) not found")
    }
    
    if (is.null(figs.per.page)) {
        if (samp.by.date) {
            figs.per.page <- 1        
        } else {
            figs.per.page <- length(id)
        }
    } 
    if (figs.per.page > 1 && desc != 0) desc <- 0
    num.plots.made <- 0
    
    if (is.null(png.dir)) {
        img.dim <- NULL
        par(mar=mar, mgp=mgp, mfrow = n2mfrow(figs.per.page))
    } else {
        img.dim <- c(png.width, png.height)
        ## Create png folder if needed
        if (!file.exists(png.dir)) {
            if (png.dir.make) {
                dir.made <- dir.create(png.dir)
                if (!dir.made) stop(paste("Couldn't make output folder ", png.dir, sep=""))
            } else {
                stop(paste(png.dir, " doesn't exist and png.dir.make is False, so can not continue."))
            }
        }
        pngs.made <- NULL

        ## If multiple plots per page, open PNG device using a filename that will automatically increment when new PNGs are needed
        if (figs.per.page > 1) {
            fn.cp <- if (cp) ".freq-cp" else ""
            fn.bd <- if (deltat.by.date) ".deltat-by-date" else ""
            fn.base <- paste(with(as.data.frame(table(lxy[["pts"]][["id"]])), paste(Var1, ".n", Freq, sep="")), collapse=".", sep="")
            fn <- file.path(png.dir, paste(fn.base, fn.cp, fn.bd, ".%02d.png", sep=""))
            if (file.exists(fn) && !png.overwrite) stop(paste(fn, "exists"))
            png(filename=fn, height=png.height, width=png.width, bg="white", pointsize=png.pointsize)
            opar <- par(mfrow = n2mfrow(figs.per.page), mar=mar, mgp=mgp)
        }
        
        
    }

    ## Compute the common range of the x-axis
    if (identical(xlim, "same")) {
        idx.comb <- which(lxy[["pts"]][["id"]] %in% id)
        xlim.same.bydate <- c(min(lxy[["pts"]][["dt"]][idx.comb]), max(lxy[["pts"]][["dt"]][idx.comb]))
        xlim.same.cp <- c(cp.min, cp.max)
    }

    #on.exit(par(opar))

    res <- list()
    #desc.lst <- list()

    for (idVal in id) {
        idx <- which(lxy[["pts"]][["id"]] == idVal)
        tau <- lxy[["rw.params"]][lxy[["rw.params"]][["id"]] == idVal, "time.step.median"]
        
        ##dt.int is the difference in seconds between time stamps, starting from dt[2] - dt1[1]
        dt.int <- diff(as.numeric(lxy[["pts"]][["dt"]][idx]))
        
        if (cp) {
            dt.int.prop.tau <- sort(dt.int) / tau
            dt.int.prop.tau <- dt.int.prop.tau[dt.int.prop.tau >= cp.min & dt.int.prop.tau <= cp.max]

            ## Create png device for combined set of plots if needed
            if (!is.null(png.dir) && figs.per.page==1) {
                fn <- file.path(png.dir, paste(lxy[["comment"]][[idVal]], ".sfreq-cum-prnct.png", sep=""))
                if (file.exists(fn) && !png.overwrite) stop(paste(fn, "exists"))
                png(filename=fn, height=png.height, width=png.width, bg="white", pointsize=png.pointsize)
                par(mar=mar, mgp=mgp)
                pngs.made <- c(pngs.made, fn)
            } else if (is.null(png.dir)) {
                fn <- NULL
            }

            ## Prepare desc and outer margin area
            desc.str <- paste("This plot shows the cummulative percentage of sampling frequencies of ", idVal, " between ", format(min(lxy[["pts"]][["dt"]][idx]), format="%Y-%m-%d"),
              " and ", format(max(lxy[["pts"]][["dt"]][idx]), format="%Y-%m-%d"), " (n=", length(idx), ") sorted smallest to largest. Sampling frequencies are expressed as a fraction of the median sampling frequency tau.", sep="")
            if (length(dt.int) != length(dt.int.prop.tau)) desc.str <- paste(desc.str, " Only sampling frequencies between ", cp.min, " and ", cp.max, " of tau are displayed (n=", length(dt.int.prop.tau), ").", sep="")
            #desc.lst[[paste(idVal, ".cp", sep="")]] <- desc.str
            res[[paste(idVal, ".cp", sep="")]] <- list(fn=fn, dim=img.dim, desc=desc.str, dt.int.prop.tau=dt.int.prop.tau, id=idVal)
            if (desc !=0 ) {
                desc.str.chopped <- chop2plot(desc.str, width=dev.size()[1], cex=cex.desc)
                oma.vals <- c(0,0,0,0)
                oma.vals[desc] <- as.numeric(desc.str.chopped[2])
                par(oma=oma.vals)
            }
            
            xlab <- expression(Delta * t / tau)
            ylab <- "cummulative proportion"
            
            if (identical(xlim, "same")) {
                xlim.use <- xlim.same.cp
            } else {
                xlim.use <- xlim
            }
            ylim.use <- ylim

            if (title.show) {
                if (is.null(title)) {
                    subtitle.use <- if (identical(subtitle,"auto")) " sampling frequency" else subtitle
                    title.str <- paste(idVal, subtitle.use, sep="")
                } else {
                    title.str <- title
                }
            } else {
                title.str <- NULL
            }
            
            plot(dt.int.prop.tau, seq(from=0, to=1, length.out=length(dt.int.prop.tau)), type="p", pch=20, cex=cex.pts, xlab=xlab, ylab=ylab, xlim=xlim.use, ylim=ylim.use,
                 main=title.str, ...)
                 
            ## Add descriptive text in the outer margin
            if (desc !=0 ) mtext(desc.str.chopped[1], side=desc, outer=TRUE, line=0 + if (desc==1) (as.numeric(desc.str.chopped[2]) - 1.25) else 0, cex=cex.desc, col=col.desc)

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
            num.plots.made <- num.plots.made + 1
            if (!is.null(png.dir) && figs.per.page==1) dev.off()

        } 
        
        if (deltat.by.date) {
            ## Create png device for combined set of plots if needed
            if (!is.null(png.dir) && figs.per.page==1) {
                fn <- file.path(png.dir, paste(lxy[["comment"]][[idVal]], ".deltat-by-date.png", sep=""))
                if (file.exists(fn) && !png.overwrite) stop(paste(fn, "exists"))
                png(filename=fn, height=png.height, width=png.width, bg="white", pointsize=png.pointsize)
                par(mar=mar, mgp=mgp)
                pngs.made <- c(pngs.made, fn)
            } else if (is.null(png.dir)) {
                fn <- NULL
            }

            #opar <- par(mfrow = n2mfrow(figs.per.page), mar=mar, mgp=mgp)

            ## Prepare desc and outer margin area
            desc.str <- paste("This plot shows the sampling interval for ", idVal, " between ", format(min(lxy[["pts"]][["dt"]][idx]), format="%Y-%m-%d"),
                              " and ", format(max(lxy[["pts"]][["dt"]][idx]), format="%Y-%m-%d"), " (n=", length(idx), ").", sep="")
            res[[paste(idVal, ".deltat.by.date", sep="")]] <- list(fn=fn, dim=img.dim, desc=desc.str, id=idVal)
            ##desc.lst[[paste(idVal, ".deltat.by.date", sep="")]] <- desc.str
            if (desc !=0 ) {
                desc.str.chopped <- chop2plot(desc.str, width=dev.size()[1])
                oma.vals <- c(0,0,0,0)
                oma.vals[desc] <- as.numeric(desc.str.chopped[2])
                par(oma=oma.vals)
            }

            if (time.unit=="auto") {
                time.unit.use <- time.unit.from.val(tau)
            } else {
                time.unit.use <- time.unit
            }

            ylab <- parse(text=paste("Delta * t ~ \" (", time.unit.use, "s)\"", sep=""))
            if (identical(xlim, "same")) {
                xlim.use <- xlim.same.bydate
            } else {
                xlim.use <- xlim
            }

            if (is.null(ylim)) {
                if (is.null(ylim.deltat.by.date)) {
                    ylim.use <- NULL
                } else {
                    if (length(ylim.deltat.by.date) != 2 || max(ylim.deltat.by.date) > 1 || min(ylim.deltat.by.date) < 0) stop("Invalid value for ylim.deltat.by.date")
                    yVals <- sort(dt.int / time.factor(time.unit.use))
                    yVals.len <- length(yVals)
                    ylim.use <- c(yVals[max(1, yVals.len * ylim.deltat.by.date[1])], yVals[yVals.len * ylim.deltat.by.date[2]])
                }
            } else {
                ylim.use <- ylim
            }

            if (title.show) {
                if (is.null(title)) {
                    subtitle.use <- if (identical(subtitle,"auto")) " sampling frequency by date" else subtitle
                    title.str <- paste(idVal, subtitle.use, sep="")
                } else {
                    title.str <- title
                }
            } else {
                title.str <- NULL
            }
            
            plot(x=lxy[["pts"]][["dt"]][idx[-1]], y=dt.int / time.factor(time.unit.use), type="p", pch=20, cex=cex.pts, xlab="date", ylab=ylab, xlim=xlim.use, ylim=ylim.use, 
                 main=title.str, ...)

            ## Add descriptive text in the outer margin
            if (desc !=0 ) mtext(desc.str.chopped[1], side=desc, outer=TRUE, line=0 + if (desc==1) (as.numeric(desc.str.chopped[2]) - 1.25) else 0, cex=cex.desc, col=col.desc)

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
            num.plots.made <- num.plots.made + 1
            if (!is.null(png.dir) && figs.per.page==1) dev.off()

        }
    }

    if (samp.by.date) {
        idx <- which(lxy[["pts"]][["id"]] %in% id)
        
        if (!is.null(xlim)) {
            if (!is(xlim, "POSIXt")) stop("xlim must be vector of POSIXt objects")
            idx <- idx[lxy[["pts"]][["dt"]][idx]>=xlim[1] & lxy[["pts"]][["dt"]][idx] <= xlim[2]]
        }
        
        tau <- min(lxy[["rw.params"]][lxy[["rw.params"]][["id"]] %in% id, "time.step.median"])
        
        ## Create png device for combined set of plots if needed
        if (!is.null(png.dir) && figs.per.page==1) {
            fn <- file.path(png.dir, paste(paste(id, collapse = ".", sep = ""), ".n", length(idx), ".samp-by-date.png", sep=""))
            if (file.exists(fn) && !png.overwrite) stop(paste(fn, "exists"))
            png(filename=fn, height=png.height, width=png.width, bg="white", pointsize=png.pointsize)
            par(mar=mar, mgp=mgp)
            pngs.made <- c(pngs.made, fn)
        } else if (is.null(png.dir)) {
            fn <- NULL
        }

        ## Prepare desc and outer margin area
        desc.str <- paste("This plot shows sampling for ", paste(id, collapse=", ", sep=""), " between ", format(min(lxy[["pts"]][["dt"]][idx]), format="%Y-%m-%d"),
                          " and ", format(max(lxy[["pts"]][["dt"]][idx]), format="%Y-%m-%d"), " (n=", length(idx), ").", sep="")
        if (sbd.y.jiggle > 0) desc.str <- paste(desc.str, " Y-values have been 'jiggled' to see point density better.", sep="")
        if (!is.null(sbd.vline)) desc.str <- paste(desc.str, " Vertical lines are placed every ", secs.fmt(sbd.vline), ".", sep="")

        #desc.lst[[paste(paste(id, collapse=".", sep=""), ".samp.by.date", sep="")]] <- desc.str
        res[[paste(paste(id, collapse=".", sep=""), ".samp.by.date", sep="")]] <- list(fn=fn, dim=img.dim, desc=desc.str, id=idVal)
        
        if (desc !=0 ) {
            desc.str.chopped <- chop2plot(desc.str, width=dev.size()[1])
            oma.vals <- c(0,0,0,0)
            oma.vals[desc] <- as.numeric(desc.str.chopped[2])
            par(oma=oma.vals)
        }

        if (time.unit=="auto") {
            time.unit.use <- time.unit.from.val(tau)
        } else {
            time.unit.use <- time.unit
        }

        xs <- lxy[["pts"]][["dt"]][idx]
        
        ys.factor <- lxy[["pts"]][["id"]][idx, drop=TRUE]
        ys.int <- as.numeric(ys.factor) + runif(length(idx), min=-sbd.y.jiggle, max=sbd.y.jiggle)

        if (title.show) {
            if (is.null(title)) {
                subtitle.use <- if (identical(subtitle,"auto")) " locations by date" else subtitle
                title.str <- paste(paste(id, collapse=" ", sep=""), subtitle.use, sep="")
            } else {
                title.str <- title
            }
        } else {
            title.str <- NULL
        }
        
        plot(x=xs, y=ys.int, type="p", pch=20, cex=cex.pts, xlab="date", ylab="id", yaxt="n", xaxt="n", main=title.str, ...)
        
        xs.ticks <- dateticks(range(xs), max.ticks=8)
        
        axis(side=1, at=xs.ticks[["tick.all"]], labels=format(xs.ticks[["tick.all"]], format=xs.ticks[["format.str"]]))
        axis(side=2, at=1:nlevels(ys.factor), labels=levels(ys.factor), tck=0)

        if (!is.null(sbd.vline)) {
            dt.int.start <- as.POSIXct(trunc(min(xs), sbd.vline.trunc.units.to))
            abline(v=seq(from=dt.int.start, to=max(xs), by=sbd.vline), lty=3, col="gray70", lwd=0.1)
        }

        ## Add descriptive text in the outer margin
        if (desc !=0 ) mtext(desc.str.chopped[1], side=desc, outer=TRUE, line=0 + if (desc==1) (as.numeric(desc.str.chopped[2]) - 1.25) else 0, cex=cex.desc, col=col.desc)

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
        num.plots.made <- num.plots.made + 1
        if (!is.null(png.dir) && figs.per.page==1) dev.off()

    }
    
    if (!is.null(png.dir)) {
        if (figs.per.page > 1) {
            dev.off()
            pngs.made <- sprintf(fn, 1:ceiling(num.plots.made / figs.per.page) )
        }            
        if (status) {
            cat("png file(s) made: \n")
            print(pngs.made)
        }
    }

    return(invisible(res))

}
