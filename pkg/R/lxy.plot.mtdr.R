#' @title Plot distribution of the ratio the maximum theoretical distance ratio for nearest neighbors
#'
#' @description Plot distributions of the ratio of maximum theoretical distance times 's' over TSD for all unique pairs of nearest neighbors
#'
#' @param lxy A \link{LoCoH-xy} object
#' @param s The s value(s) of nearest neighbor sets to include in the plot. If NULL, all values will be used
#' @param k A k-value for the number of nearest neighbors around each point to include in the plot
#' @param r A r-value for the number of nearest neighbors around each point to include in the plot
#' @param a A a-value for the number of nearest neighbors around each point to include in the plot
#' @param type Which ratio to plot: maximum theoretical distance over TSD, or maximum theoretical distance over Euclidean distance
#' @param offset.dups An amount in map units for which pairs of points with the same location but different times will be offset so that a TSD or Euclidean distance can be calculated (see also \code{\link{lxy.lhs}})
#' @param id The id(s) of the individual(s) to include in the plot
#' @param show.samp.size Whether to display the sample size of the number of unique pairs of points for each value of s, T/F
#' @param outline Show outliers in the box plots T/F
#' @param desc Which side to display automatically generated desciptive text (e.g. caption). 0=none, 1=bottom, 3=top.
#' @param cex.desc The expansion factor for the descriptive text. Numeric value.
#' @param col.desc The color of the descriptive text. Color value.
#' @param mar The plot margins. A four item numeric vector
#' @param mgp The distance away from the edge of the plot for the 1) label, 2) tick marks, and 3) axis line. A three-item numeric vector
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
#' @details This function plots the ratio of the maximum theoretical distance the individual could have traveled (multiplied by 's') over the 
#' total TSD distance metric for all nearests neighbors for the specified value of k/a/r for different values of s. 
#' This ratio will of course never be greater than one because the maximum theoretical distance
#' is one of the terms in TSD. The purpose of this function is to see the effect of s on the relative contribution 
#' of the time term in the TSD metric.
#'
#' This function requires that nearest neighbors already be computed. To identify nearest neighbors for a range of 's' values,
#' use \code{\link{lxy.ptsh.add}} (see examples).
#' 
#' @return A list of plots created, with one element per id, and each element consisting of another 
#' list with elements for the filename (or NULL), filename dimensions in pixels (or NULL), the MTD ratios 
#' (as a list with one element for each value of s), descriptive text, and the k/a/r value
#'
#' @examples
#' ## Identify a range of 's' values from space-selection to time-selection, and find 10 nearest neighbors for each value of 's'
#' lxy <- lxy.ptsh(lxy, nn=TRUE)
#' ## Plot the ratio of maximum theoretical distance travel to TSD 
#' lxy.plot.mtdr(lxy)
#'
#' @export

lxy.plot.mtdr <- function(lxy, s=NULL, k=NULL, a=NULL, r=NULL, type=c("mtd.tsd", "mtd.ed")[1], offset.dups=1, 
                             id=NULL, show.samp.size=TRUE, outline=FALSE,
                             desc=c(0,1,3)[2], cex.desc=0.8, col.desc="darkgreen",
                             mar=c(3, 3, if (title.show) 3 else 0.7, 0.5), mgp=c(1.9, 0.5, 0), figs.per.page=NULL, title=NULL, title.show=TRUE, title.obj.name=FALSE,
                             panel.num=NULL, panel.num.inside.plot=!title.show, xaxis.no.sci.notation=FALSE,
                             png.dir=NULL, png.dir.make=TRUE, png.width=800, png.height=png.width, png.overwrite=TRUE, png.pointsize=12+(png.width-480)/80, status=TRUE, ...) {

    if (!inherits(lxy, "locoh.lxy")) stop("lxy should be of class \"locoh.lxy\"")
    if (!require(sp)) stop("package sp required")
    if (!require(pbapply)) stop("package pbapply required")
    if (is.null(lxy[["pts"]][["dt"]])) stop("Can't plot maximum theoretical distance ratio, no time stamps found")
    if (is.null(lxy[["nn"]])) stop("Please first identify nearest neighbors with lxy.nn.add(), then try again.")
    if (length(k) + length(a) + length(r) != 1) stop("Must provide one and only one of the following: k, r, or a")
    
    if (is.null(id)) {
        id <- levels(lxy[["pts"]][["id"]])
    } else {
        if (FALSE %in% (id %in% levels(lxy[["pts"]][["id"]]))) stop("id value(s) not found")
    }
    
    res <- list()
    cols.keep <- c("pp.idx", "nn.idx", "tsd")
    str.title.lst <- list(mtd.tsd="TSD", mtd.ed="Euclidean Distance")
    not.enuf.nn.exp <- expression(stop(cw(paste("Insufficient number of nearest neighbors available for s=", s.all.df[s.idx, "s"], " and ", str.subtitle, ". Find more nearest neighbors with lxy.nn.add()", sep=""), final.cr=F, exdent=2)))

    ylim <- if (show.samp.size && type=="mtd.tsd") c(0, 1.03) else NULL
    #print("forcing ylim to NULL"); ylim <- NULL
    at.least.one.dup.loc <- FALSE
    
    ## Create png folder if needed, set plots per page
    if (is.null(png.dir)) {
        if (is.null(figs.per.page)) figs.per.page <- length(id)
        opar <- par(mfrow = n2mfrow(figs.per.page), mar=mar, mgp=mgp, oma=c(0,0,0,0))
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
        if (is.null(figs.per.page)) {
            figs.per.page <- 1
        } else {
            if (figs.per.page > 1) stop("When saving dedr plots as PNG files, you can only have one plot per page")
        }
        img.dim <- c(png.width, png.height)
    }

    
    ## Convert date stamps to integers (num secs since 1970 in UTC)
    lxy.dt.int <- as.numeric(lxy[["pts"]][["dt"]])

    xaxt <- if (xaxis.no.sci.notation) "n" else NULL

    for (idVal in id) {
        cat("Computing maximum theoretical distance over ", str.title.lst[[type]], " for ", idVal, "\n", sep=""); flush.console()
        res[[idVal]] <- list()
        
        ## Create png device for combined set of plots if needed
        if (!is.null(png.dir)) {
            png.fn <- file.path(png.dir, paste(paste(unlist(lxy[["comment"]]), collapse = ".", sep = ""), ".", type, ".png", sep=""))
            if (file.exists(png.fn) && !png.overwrite) stop(paste(png.fn, "exists"))
            par(bg="white")
            png(filename=png.fn, height=png.height, width=png.width, bg="white", pointsize=png.pointsize)
            opar <- par(mfrow = n2mfrow(figs.per.page), mar=mar, mgp=mgp, oma=c(0,0,0,0))
        }

        ## Get the random walk parameters for this id
        tau <- lxy[["rw.params"]][lxy[["rw.params"]][["id"]] == idVal , "time.step.median"]
        d.bar <- lxy[["rw.params"]][lxy[["rw.params"]][["id"]] == idVal , "d.bar"]
        vmax <- lxy[["rw.params"]][lxy[["rw.params"]][["id"]] == idVal , "vmax"]
        
        ## Enumerate all nearest neighbor sets for this ID
        nn.idx.this.id <- which(sapply(1:length(lxy[["nn"]]), function (i) lxy[["nn"]][[i]][["id"]]==idVal))
        s.all.df <- do.call(rbind, lapply(nn.idx.this.id, function(i) data.frame(nn.set.idx=i, s=lxy[["nn"]][[i]][["s"]] )))
        s.all.df <- s.all.df[order(s.all.df[["s"]]), ]
          
        ## Select those nearest neighbor sets that have a matching value of s
        if (is.null(s)) {
            sdf.idx <- 1:nrow(s.all.df)
        } else {
            sdf.idx <- match(s, s.all.df[["s"]])
            if (TRUE %in% is.na(sdf.idx)) stop("s value(s) not found") 
            #s.use <- intersection(s, s.all)
        }
        #if (length(s.use)==0) stop("s not found in nearset neighbor table")
        
        mtd.ratio <- list()
        
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

            ## To remove duplicate pairs of indices without regard to order, first identify which rows are in descending order,
            ## then swap the first two columns in those rows
            nn.rev.ord <- nn.df[,"pp.idx"] > nn.df[,"nn.idx"]
            nn.df[nn.rev.ord,] <- cbind(nn.df[nn.rev.ord,"nn.idx"], nn.df[nn.rev.ord,"pp.idx"], nn.df[nn.rev.ord,"tsd"])
            
            ## remove duplicates
            nn.df <- unique(nn.df)
            
            ## remove rows with same parent point & nn.idx
            nn.df <- nn.df[nn.df[,1] != nn.df[,2], ]
            
            ## Compute the max theoretical distance
            if (lxy[["nn"]][[ s.all.df[s.idx,"nn.set.idx"] ]][["time.term"]] == "dif") {
                dif.dist <- sqrt(s.all.df[s.idx, "s"] * (lxy.dt.int[nn.df[,2]] - lxy.dt.int[nn.df[,1]]) / tau) * d.bar
            } else if (lxy[["nn"]][[ s.all.df[s.idx,"nn.set.idx"] ]][["time.term"]] == "vmax") {
                dif.dist <- s.all.df[s.idx, "s"] * vmax * (lxy.dt.int[nn.df[,2]] - lxy.dt.int[nn.df[,1]])
            }

            if (type=="mtd.tsd") {
                ## If there are pairs of points with the same location, recalculate TSD for those pairs using a random offset.
                if (offset.dups > 0) {
                    tsd.zero <- nn.df[,"tsd"] == 0
                    if (any(tsd.zero)) {
                        at.least.one.dup.loc <- TRUE

                        if (lxy[["nn"]][[ s.all.df[s.idx,"nn.set.idx"] ]][["time.term"]] == "dif") {                        
                            nn.df[tsd.zero, "tsd"] <- sqrt(offset.dups ^ 2 + s.all.df[s.idx, "s"] * d.bar^2 * abs(lxy.dt.int[nn.df[tsd.zero,2]] - lxy.dt.int[nn.df[tsd.zero,1]]) / tau)

                        } else if (lxy[["nn"]][[ s.all.df[s.idx,"nn.set.idx"] ]][["time.term"]] == "vmax") {
                            nn.df[tsd.zero, "tsd"] <- sqrt(offset.dups ^ 2 + (s.all.df[s.idx, "s"] * vmax * abs(lxy.dt.int[nn.df[tsd.zero,2]] - lxy.dt.int[nn.df[tsd.zero,1]]))^2)

                        } else {
                            stop("unknown value for time.term")
                        }
                    }
                }

                mtd.ratio[[as.character(s.all.df[s.idx, "s"])]] <- dif.dist / nn.df[,"tsd"]
                
            } else if (type=="mtd.ed") {
                ## Calculate the euclidean dist
                euc.dist <- sqrt((coordinates(lxy[["pts"]])[nn.df[,2], 1] - coordinates(lxy[["pts"]])[nn.df[,1], 1])^2 + (coordinates(lxy[["pts"]])[nn.df[,2], 2] - coordinates(lxy[["pts"]])[nn.df[,1], 2])^2)
                
                ## If there are pairs of points with the same location, use the offset.dups value
                if (offset.dups > 0) euc.dist[euc.dist==0] <- offset.dups
                
                mtd.ratio[[as.character(s.all.df[s.idx, "s"])]] <- dif.dist / euc.dist

            } else {
                stop("Unknown value for 'type'")
            }
        }        
        if (status) close(pb)
        
        if (title.show) {
            if (is.null(title)) {
                title.str <- paste("Max Theoretical Distance : ", str.title.lst[[type]], "\n", if (title.obj.name) paste(deparse(substitute(lxy)), ":", sep="") else "", idVal, ", ", str.subtitle, sep="")
                
                
            } else {
                title.str <- title
            }
        } else {
            title.str <- NULL
        }

        ## Prepare desc and outer margin area
        desc.str <- paste("This plot shows the distribution of the ratio of the maximum theoretical distance times 's' to ", str.title.lst[[type]],
                          " for all unique pairs of nearest neighbors when ", str.subtitle, " for id=", idVal, ".",
                          if (at.least.one.dup.loc) paste(" Pairs of duplicate locations were offset by ", offset.dups, " map unit(s).", sep="") else "",
                          sep="")
        if (desc !=0 ) {
            desc.str.chopped <- chop2plot(desc.str, width=dev.size()[1] * 0.95, cex=cex.desc)
            oma.vals <- c(0,0,0,0)
            oma.vals[desc] <- as.numeric(desc.str.chopped[2])
            par(oma=oma.vals)
        }
        
        ## Create the plot        
        bp.info <- boxplot(mtd.ratio, outline=outline, ylim=ylim, main=title.str, xlab="s", ylab=paste("MTD / ", str.title.lst[[type]], sep=""), xaxt=xaxt, ...)
        if (xaxis.no.sci.notation) axis(side=1, at=1:length(mtd.ratio), labels=format(as.numeric(names(mtd.ratio)), scientific=FALSE, trim=T, drop0trailing=T), ...)
        if (show.samp.size) text(x=1:length(sdf.idx), y=bp.info[["stats"]][5,] , labels=paste("n=", sapply(mtd.ratio, length), sep=""), pos=3, cex=0.8)


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


        ## Turn off the png device
        if (!is.null(png.dir)) dev.off()
        
        ## Save result
        names(mtd.ratio) <- paste(type, ".s", names(mtd.ratio), sep="")
        res[[idVal]] <- list(fn=png.fn, dim=img.dim, mtd.ratio=mtd.ratio, desc=desc.str, type=type, method=str.subtitle)

    }  ## for idVal

    if (!is.null(png.dir) && status) {
        cat("png file(s) made: \n")
        print(as.character(sapply(res, function(x) x[["fn"]])))
    }
    
    return(invisible(res))
}
