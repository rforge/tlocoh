#' @title Plot distributions of the time span of nearest neighbors
#'
#' @description Plot distributions of the time span of nearest neighbors as a proportion of the median sampling interval
#'
#' @param lxy A \link{LoCoH-xy} object
#' @param s The s value(s) of nearest neighbor sets to include in the plot. If NULL, all values will be used
#' @param k A k-value for the number of nearest neighbors around each point to include in the plot
#' @param r A r-value for the number of nearest neighbors around each point to include in the plot
#' @param a A a-value for the number of nearest neighbors around each point to include in the plot
#' @param id The id(s) of the individual(s) to include in the plot
#' @param type The type of plot for the time span: \code{'hist'}, \code{'boxplot'}, or \code{'mean'}
#' @param outline Show outliers in the box plots T/F
#' @param breaks The breaks parameter for a histogram, see \code{\link{hist}}
#' @param desc Which side to display automatically generated desciptive text (e.g. caption). 0=none, 1=bottom, 3=top.
#' @param cex.desc The expansion factor for the descriptive text. Numeric value.
#' @param col.desc The color of the descriptive text. Color value.
#' @param col.hist The color of the histogram bars. Color value.
#' @param mar The plot margins. A four item numeric vector
#' @param mgp The distance away from the edge of the plot for the 1) label, 2) tick marks, and 3) axis line. A three-item numeric vector
#' @param figs.per.page The number of plots per page.
#' @param title The title to be displayed. Character. If NULL a title will be constructed.
#' @param title.show Whether to show the title. T/F
#' @param title.obj.name Whether to add the name of the lxy object to the plot title (ignored if \code{title} is passed). T/F
#' @param panel.num A number or letter to display in the upper left hand corner of the plot when the plot will be used as part of a multi-frame graphic (as in publications). Character
#' @param panel.num.inside.plot Whether to display panel.num inside the plot area itself, as opposed to the title area. Ignored if panel.num is NULL. T/F
#' @param panel.num.cex The expansion factor for the panel number. Ignored if panel.num is NULL. 
#' @param no.sci.notation Whether to avoid the use of scientific notation on labels on the x-axis. T/F
#' @param png.dir The directory for a PNG file (filename will be constructed automatically). Ignored if png.fn is passed
#' @param png.dir.make Whether to create png.dir if it doesn't exist. T/F
#' @param png.width The width of the PNG image
#' @param png.height The height of the PNG image
#' @param png.overwrite Whether to overwrite an existing PNG file if it exists. T/F
#' @param png.pointsize The pointsize (in pixels) for the PNG image, equivalent to the height or width of a character in pixels (increase to make labels appear larger)
#' @param status Show progress bar and status messages. T/F
#' @param ... Additional parameters that will be passed to the \code{\link{plot}} function
#'
#' @return A list of plots created, with one element per id, and each element consisting of another 
#' list with elements for the filename (or NULL), filename dimensions in pixels (or NULL), the timespan values 
#' (as a list with one element for each value of s), descriptive text, and the k/a/r value
#'
#' @export

lxy.plot.tspan <- function(lxy, s=NULL, k=NULL, a=NULL, r=NULL, id=NULL, type=c("hist", "boxplot", "mean")[1], outline=FALSE, breaks=20,
                             desc=c(0,1,3)[2], cex.desc=0.8, col.desc="darkgreen", col.hist="gray80", 
                             mar=c(if (type=="hist") 1.5 else 3, 3, if (type=="hist") 2 else if (title.show) 3 else 0.5, 0.5), mgp=c(1.9, 0.5, 0), figs.per.page=NULL, 
                             title=NULL, title.show=TRUE, title.obj.name=FALSE,
                             panel.num=NULL, panel.num.inside.plot=!title.show, panel.num.cex=2, no.sci.notation=FALSE,
                             png.dir=NULL, png.dir.make=TRUE, png.width=800, png.height=png.width, png.overwrite=TRUE, png.pointsize=12+(png.width-480)/80, status=TRUE, ...) {


    if (!inherits(lxy, "locoh.lxy")) stop("lxy should be of class \"locoh.lxy\"")
    if (is.null(lxy[["pts"]][["dt"]])) stop("Can't plot tspan, no time stamps found")
    if (is.null(lxy[["nn"]])) stop("Please first identify nearest neighbors with lxy.nn.add(), then try again.")
    if (length(k) + length(a) + length(r) != 1) stop("Must provide one and only one of the following: k, r, or a")
    
    if (is.null(id)) {
        id <- levels(lxy[["pts"]][["id"]])
    } else {
        if (FALSE %in% (id %in% levels(lxy[["pts"]][["id"]]))) stop("id value(s) not found")
    }
    
    res <- list()
    cols.keep <- c("pp.idx", "nn.idx")
    not.enuf.nn.exp <- expression(stop(cw(paste("Insufficient number of nearest neighbors available for s=", s.all.df[s.idx, "s"], " and ", str.subtitle, ". Find more nearest neighbors with lxy.nn.add()", sep=""), final.cr=F, exdent=2)))

    ## Create png folder if needed
    if (is.null(png.dir)) {
        img.dim <- NULL
        png.fn <- NULL
    } else {
        if (!file.exists(png.dir)) {
            if (png.dir.make) {
                dir.made <- dir.create(png.dir)
                if (!dir.made) stop(paste("Couldn't make output folder ", png.dir, sep=""))
            } else {
                stop(paste(png.dir, " doesn't exist and png.dir.make is False, so can not continue."))
            }
        }
        img.dim <- c(png.width, png.height)
    }
    
    ## Convert date stamps to integers (num secs since 1970 in UTC)
    lxy.dt.int <- as.numeric(lxy[["pts"]][["dt"]])

    for (idVal in id) {
        cat("Computing time span for ", idVal, "\n", sep=""); flush.console()
        res[[idVal]] <- list()
        
        ## Get the random walk parameters for this id
        tau <- lxy[["rw.params"]][lxy[["rw.params"]][["id"]] == idVal , "time.step.median"]
        
        ## Enumerate all nearest neighbor sets for this ID, order by 's'
        nn.idx.this.id <- which(sapply(1:length(lxy[["nn"]]), function (i) lxy[["nn"]][[i]][["id"]]==idVal))
        s.all.df <- do.call(rbind, lapply(nn.idx.this.id, function(i) data.frame(nn.set.idx=i, s=lxy[["nn"]][[i]][["s"]])))
        s.all.df <- s.all.df[order(s.all.df[["s"]]), ]
          
        ## Select those nearest neighbor sets that have a matching value of s
        if (is.null(s)) {
            sdf.idx <- 1:nrow(s.all.df)
        } else {
            sdf.idx <- match(s, s.all.df[["s"]])
            if (TRUE %in% is.na(sdf.idx)) stop("s value(s) not found") 
            #s.use <- intersection(s, s.all)
        }

        ## Set plots per page
        if (is.null(figs.per.page)) {
            if (type=="boxplot" || type=="mean") {
                figs.per.page.use <- 1
            } else if (type=="hist") {
                figs.per.page.use <- length(sdf.idx)
            } else {
                stop("unknown value for 'type'")
            }
        } else {
            figs.per.page.use <- figs.per.page
        }

        ## Create png device for combined set of plots if needed
        if (!is.null(png.dir)) {
            png.fn <- file.path(png.dir, paste(paste(unlist(lxy[["comment"]]), collapse = ".", sep = ""), ".tspan.", type, ".png", sep=""))
            if (file.exists(png.fn) && !png.overwrite) stop(paste(png.fn, "exists"))
            par(bg="white")
            png(filename=png.fn, height=png.height, width=png.width, bg="white", pointsize=png.pointsize)
        }
        opar <- par(mfrow = n2mfrow(figs.per.page.use), mar=mar, mgp=mgp, oma=c(0,0,0,0))
        
        tspan.lst <- list()
        nnn.lst <- list()
        
        ## Loop thru the s values of the nearest neighbor sets for this idVal
        if (status) pb <- txtProgressBar(min=0, max=length(sdf.idx), style=3)
        for (j in 1:length(sdf.idx)) {
            s.idx <- sdf.idx[j]

            ## Check if there is enuf nearest neighbors, and get the nn for the passed value of k/r/a
            if (!is.null(k)) {
                str.subtitle <- paste("k=", k, sep="")
                if (lxy[["nn"]][[s.all.df[s.idx, "nn.set.idx"]]][["kmax"]] < k) eval(not.enuf.nn.exp)
                nn.df <- with(lxy[["nn"]][[s.all.df[s.idx, "nn.set.idx"]]], nn.df[nn.df[["nn.rank"]] <= k, cols.keep])
            } else if (!is.null(r)) {
                str.subtitle <- paste("r=", r, sep="")
                if (lxy[["nn"]][[s.all.df[s.idx, "nn.set.idx"]]][["rmax"]] < r) eval(not.enuf.nn.exp)
                nn.df <- with(lxy[["nn"]][[s.all.df[s.idx, "nn.set.idx"]]], nn.df[nn.df[["tsd"]] <= r, cols.keep])
            } else if (!is.null(a)) {
                str.subtitle <- paste("a=", a, sep="")
                if (lxy[["nn"]][[s.all.df[s.idx, "nn.set.idx"]]][["amax"]] < a) eval(not.enuf.nn.exp)
                nn.df <- with(lxy[["nn"]][[s.all.df[s.idx, "nn.set.idx"]]], nn.df[nn.df[["tsd.cumsum"]] <= a, cols.keep])
            }
        
            if (status) setTxtProgressBar(pb, j)

            tspan.lbl <- if (no.sci.notation) format(s.all.df[s.idx, "s"], scientific=FALSE, trim=T, drop0trailing=T) else as.character(s.all.df[s.idx, "s"])
            
            ## Compute the time span of the nearest neighbors for each parent point as a proportion of tau
            nn.dts.lst <- split(lxy.dt.int[nn.df[["nn.idx"]]], nn.df[["pp.idx"]])
            tspan.lst[[tspan.lbl]] <- as.numeric(sapply(lapply(nn.dts.lst, range), diff) / tau)
            
            ## Find the number of nearest neighbors, which is the number of points minus one for the parent point
            nnn.lst[[tspan.lbl]] <- as.numeric(sapply(nn.dts.lst, length)) - 1
            

        } ### for s.idx 
               
        if (status) close(pb)
        
        if (title.show) {
            if (is.null(title)) {
                title.str <- paste("Time Span", if (type == "mean") " / nnn" else "", "\n", if (title.obj.name) paste(deparse(substitute(lxy)), ":", sep="") else "", idVal, ", ", str.subtitle, sep="")
            } else {
                title.str <- title
            }
        } else {
            title.str <- NULL
        }

        ## Prepare desc and outer margin area
        if (type=="mean") {
            desc.str <- paste("This plot shows the mean time span of hulls (as a proportion of the median sampling interval) over the number of neareset neighbors used in the hull construction when ", str.subtitle, " for id=", idVal, ".", sep="")
        } else {
            desc.str <- paste("This plot shows the distribution of the time span of the nearest neighbors as a proportion of the median sampling interval when ", str.subtitle, " for id=", idVal, ".", sep="")
        }
        
        oma.vals <- c(0, 0, if (type=="hist" && title.show) 3 else 0, 0)
        if (desc !=0 ) {
            ## Save the current mfrow values, then set it to one by one
            mfrow.vals <- par("mfrow")
            par(mfrow=c(1,1))
            desc.str.chopped <- chop2plot(desc.str, width=dev.size()[1] * 0.95, cex=cex.desc)
            oma.vals[desc] <- oma.vals[desc] + as.numeric(desc.str.chopped[2])
            ## Reset mfrow vals
            par(mfrow=mfrow.vals)
        }
        par(oma=oma.vals)
        #print("about to make a plot"); browser()
        
        ## Create the plot 
        if (type == "hist") {
            for (i in 1:length(tspan.lst)) {
                hist(tspan.lst[[i]], breaks=breaks, main=paste("s=", names(tspan.lst)[i], sep=""), col=col.hist, xaxt="n", xlab="", ylab="freq", ...)
                axis(side=1, pos=0)
            }
            mtext(title.str, side=3, outer=TRUE, font=2, cex=1.2)
        } else if (type == "boxplot") {
            boxplot(tspan.lst, outline=outline, main=title.str, xlab="s", ylab="time span / tau", ...)

        } else if (type == "mean") {
            xs <- s.all.df[sdf.idx, "s"]
            ys <- sapply(1:length(sdf.idx), function(i) mean(tspan.lst[[i]] / nnn.lst[[i]]))
            plot(xs, ys, type="b", col="red", main=title.str, xlab="s", ylab="time span / tau / nnn", ...)

        }

        ## Add descriptive text in the outer margin
        if (desc !=0 ) mtext(desc.str.chopped[1], side=desc, outer=TRUE, line=0 + if (desc==1) (as.numeric(desc.str.chopped[2]) - 1.25) else 0, cex=cex.desc, col=col.desc)

        ## Add panel.num
        if (!is.null(panel.num)) {
            if (figs.per.page.use > 1) par(mfg=c(1,1))
            if (panel.num.inside.plot) {
                text(x=par("usr")[1], y=par("usr")[4], labels=panel.num, cex=panel.num.cex, adj=c(-0.3,1.2), font=2)
            } else {
                mar.old <- par("mar")
                par(mar=c(0, 0.3, 0.2, 0))
                title(main=panel.num, adj=0, cex.main=panel.num.cex, line=-1.5)
                par(mar=mar.old)
            }
        }


        ## Turn off the png device
        if (!is.null(png.dir)) dev.off()
        
        ## Save result
        names(tspan.lst) <- paste("s", names(tspan.lst), sep="")
        res[[idVal]] <- list(fn=png.fn, dim=img.dim, tspan=tspan.lst, desc=desc.str, method=str.subtitle)

    }  ## for idVal

    if (!is.null(png.dir) && status) {
        cat("png file(s) made: \n")
        print(as.character(sapply(res, function(x) x[["fn"]])))
    }
    
    return(invisible(res))

}
