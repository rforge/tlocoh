#' Plots the proportion of time-selected hulls over 's'
#'
#' @param lxy A LoCoH-xy object
#' @param id The name(s) of individuals to plot
#' @param ptsh.idx The index number of the saved ptsh table to use (ignored if \code{use.nn=TRUE})
#' @param use.nn Whether to create the plot based on nearest neighbor sets (as opposed to saved ptsh tables, see Details). T/F.
#' @param k Value for the k method if creating the plot based on nearest neighbor tables (ignored if \code{use.nn=FALSE})
#' @param r Value for the r method if creating the plot based on nearest neighbor tables (ignored if \code{use.nn=FALSE})
#' @param a Value for the a method if creating the plot based on nearest neighbor tables (ignored if \code{use.nn=FALSE})
#' @param slim The lower and upper bounds for s, two-element numeric vector
#' @param desc Which side to display automatically generated desciptive text (e.g. caption). 0=none, 1=bottom, 3=top.
#' @param cex.desc The expansion factor for the descriptive text. Numeric value.
#' @param col.desc The color of the descriptive text. Color value.
#' @param title The title to be displayed. Character. If NULL a title will be constructed.
#' @param title.show Whether to show the title. T/F.
#' @param legend A character object specifying where to put the legend (ignored when \code{use.nn=TRUE})
#' @param mar The plot margins. A four item numeric vector
#' @param mgp The distance away from the edge of the plot for the 1) label, 2) tick marks, and 3) axis line. A three-item numeric vector
#' @param figs.per.page The number of plots per page.
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
#' @details This function will plot the proportion of total hulls that are 'time-selected'. Time-selected means all of the
#' nearest neighbors are were sampled sequentially in time. This is one end of the spectrum as far as nearest neighbor
#' identification goes, the other being space-selected (i.e., time has no bearing). The \code{s} parameter in the TSD equation
#' determines the degree to which time plays a role in the point-to-point 'distance'. When \code{s=0}, time plays no role
#' in TSD and TSD is equivalent to Euclidean distance. As \code{s} increases, time plays a bigger and bigger role until
#' eventually nearest neighbor selection is equivalent to selection points based only on their separation in time.
#'
#' In order to plot the proportion of time-selected hulls (ptsh), nearest neighbors must have  already been identified. This 
#' can be done in one of two ways. The \code{\link{lxy.ptsh.add}} function will compute ptsh for different values of s using
#' a random sample of points (to save time), automatically picking values of s such that ptsh is close to target values provided by the user. Alternately, one can identify nearest neighbors for different values of \code{s}
#' using \code{\link{lxy.nn.add}} and then run this function with \code{use.nn=TRUE}. The main difference between \code{\link{lxy.ptsh.add}}
#' and \code{\link{lxy.nn.add}} is that \code{\link{lxy.ptsh.add}} finds nearest neighbors for a random sample of points, and doesn't
#' actually save the nearest neighbor information for individual points, whereas \code{\link{lxy.nn.add}} identifies and saves
#' nearest neighbor information for each and every point.
#'
#' @return A list of lists, one for each plot containing the filename (NULL if no png made), the image dimensions (or NULL), the descriptive text, the id,
#' and a matrix of the values
#'
#' @seealso \code{\link{lxy.ptsh.add}}, \code{\link{lxy.nn.add}}
#'
#' @examples
#' data(toni.lxy)
#' toni.lxy <- lxy.ptsh.add(toni.lxy)
#' lxy.plot.ptsh(toni.lxy)
#'
#' @export

lxy.plot.ptsh <- function(lxy, id=NULL, ptsh.idx=NULL, use.nn=FALSE, k=NULL, r=NULL, a=NULL, slim=NULL,
                          desc=c(0,1,3)[2], cex.desc=0.8, col.desc="darkgreen", title=NULL, title.show=TRUE,
                          legend=c("none", "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", "center")[2],
                          mar=c(3, 3, if (title.show) 2.8 else 0.7, 0.5), mgp=c(1.8, 0.5, 0), figs.per.page=NULL,
                          panel.num=NULL, panel.num.inside.plot=!title.show, png.dir=NULL, png.dir.make=TRUE, 
                          png.width=800, png.height=png.width, png.overwrite=TRUE, png.pointsize=12+(png.width-480)/80, ...) {

    if (!inherits(lxy, "locoh.lxy")) stop("lxy should be of class \"locoh.lxy\"")
    
    if (use.nn) {
        if (is.null(lxy[["nn"]])) stop("nn not calculated. Run lxy.nn.add() and try again")
        if (length(k) + length(r) + length(a) != 1) stop("To plot ptsh from saved nn sets, please specify k, a, or r")
        idVal.all <- unique(sapply(lxy[["nn"]], function(x) x[["id"]]))
    } else {
        if (is.null(lxy[["ptsh"]])) stop("ptsh not calculated. Run lxy.ptsh.add() and try again")
        idVal.all <- names(lxy[["ptsh"]])
    }
    
    if (is.null(id)) {
        id.use <- idVal.all
    } else {
        id.use <- intersect(id, idVal.all)
        if (length(id.use)==0) stop("No ptsh objects found for those id value(s)")
    }
    res <- list()
    
    if (is.null(figs.per.page)) {
        if (is.null(png.dir)) {
            figs.per.page <- length(id.use)
        } else {
            figs.per.page <- 1
        }
     } else {
        if (figs.per.page > 1) stop("When saving plots as PNG files, you can only have one plot per page")
     }
    if (figs.per.page > 1) desc <- 0

    ## Create png folder if needed
    if (is.null(png.dir)) {
        img.dim <- NULL
        opar <- par(mfrow = n2mfrow(figs.per.page), mar=mar, mgp=mgp, oma=c(0,0,0,0))
    } else {
        img.dim <- c(png.width, png.height)
        if (!file.exists(png.dir)) {
            if (png.dir.make) {
                dir.made <- dir.create(png.dir)
                if (!dir.made) stop(paste("Couldn't make output folder ", png.dir, sep=""))
            } else {
                stop(paste(png.dir, " doesn't exist and png.dir.make is False, so can not continue."))
            }
        }
        first.png.made <- FALSE
        pngs.made <- NULL
    }

    ## Get the correct mode character and value if extracting values from saved nn
    if (use.nn) {
        for (mode.param in c("k","a","r")) {
            if (!is.null(get(mode.param))) {
                mode.str <- mode.param
                mode.val <- get(mode.param)
            }
        }
    }

    for (idVal in id.use) {
        idVal.idx <- which(lxy[["pts"]][["id"]] == idVal)

        #res[[idVal]] <- list()
        
        ## Create png device for combined set of plots if needed
        if (is.null(png.dir)) {
            png.fn <- NULL
        } else {
            png.fn <- file.path(png.dir, paste(paste(unlist(lxy[["comment"]][idVal]), collapse = ".", sep = ""), ".sptsh.png", sep=""))
            if (file.exists(png.fn) && !png.overwrite) stop(paste(png.fn, "exists"))
            par(bg="white")
            png(filename=png.fn, height=png.height, width=png.width, bg="white", pointsize=png.pointsize)
            pngs.made <- c(pngs.made, png.fn)
            opar <- par(mfrow = n2mfrow(figs.per.page), mar=mar, mgp=mgp, oma=c(0,0,0,0))
        }
        
        ## Prepare desc
        desc.str <- paste("This plot shows the proportion of time-selected hulls (i.e., hulls constructed from time sequential locations) for ", idVal, " for a range of s values. ", sep="")
        if (use.nn) {
            data.computed.from <- paste("Data computed from all saved nearest neighbors for ", mode.str, "=", mode.val, ".", sep="")
        } else {
            ## Add another sentence about the number of samples
            if (is.null(ptsh.idx)) {
                ptsh.idx.use <- 1:length(lxy[["ptsh"]][[idVal]])
            } else {
                ptsh.idx.use <- intersect(ptsh.idx, 1:length(lxy[["ptsh"]][[idVal]]))
            }
            data.computed.from <- paste("Data computed from n=", paste(sapply(lxy[["ptsh"]][[idVal]][ptsh.idx.use], function(x) x[["n"]]), collapse=",", sep=""), " randomly selected hulls for k=", paste(sapply(lxy[["ptsh"]][[idVal]][ptsh.idx.use], function(x) x[["k"]]), collapse=",", sep=""), " nearest neighbors.", sep="")
        }
        desc.str <- paste(desc.str, data.computed.from, sep="")
        
        ## Prepare outer margin area
        if (desc !=0 ) {
            desc.str.chopped <- chop2plot(desc.str, width=dev.size()[1] * 0.9, cex=cex.desc)
            oma.vals <- c(0,0,0,0)
            oma.vals[desc] <- as.numeric(desc.str.chopped[2])
            par(oma=oma.vals)
        }

        ## Initialize title.str
        if (title.show) {
            if (!is.null(title)) title.str <- title
        } else {
            title.str <- NULL
        }
        
        if (use.nn) {
            if (title.show && is.null(title)) title.str <- paste("s vs. ptsh\n", idVal, ", nn sets, ", mode.str, "=", mode.val, sep="")
            
            ## Get the indices of the nearest neighbor sets affiliated with this idVal
            nn.idVal.idx <- which(sapply(lxy[["nn"]], function(x) x[["id"]]==idVal))

            ## Loop through these nn sets and construct a matrix of s and ptsh
            s.ptsh <- NULL
            cat("Computing ptsh for each saved nearest neighbor set \n")
            pb <- txtProgressBar(min=0, max=length(nn.idVal.idx), style = 3)
            for (i in 1:length(nn.idVal.idx)) {
                setTxtProgressBar(pb, i)
            
                idx <- nn.idVal.idx[[i]]
                if (lxy[["nn"]][[idx]][[paste(mode.str, "max", sep="")]] < mode.val) stop("Insufficient number of nearest neighbors")
                if (mode.str == "k") {
                    good.rows.idx <- lxy[["nn"]][[idx]][["nn.df"]][["nn.rank"]] <= k
                } else if (mode.str == "a") {
                    good.rows.idx <- lxy[["nn"]][[idx]][["nn.df"]][["tsd.cumsum"]] <= a
                } else if (mode.str == "r") {
                    good.rows.idx <- lxy[["nn"]][[idx]][["nn.df"]][["tsd"]] <= r
                }
                
                nn.lst <- with(lxy[["nn"]][[idx]][["nn.df"]][good.rows.idx, c("pp.idx","nn.idx")], split(nn.idx, pp.idx))
                ptsh.cur <- sum(sapply(nn.lst, function(x) max(diff(sort(match(x, idVal.idx))))) == 1)  / length(nn.lst)
                s.ptsh <- rbind(s.ptsh, c(lxy[["nn"]][[idx]][["s"]], ptsh.cur))
            }
            close(pb)
            
            s.ptsh <- s.ptsh[order(s.ptsh[,1]), , drop=FALSE]
            
            plot(s.ptsh, type="l", xlab="s", ylab="proportion time-selected hulls", main=title.str, ...)
            points(s.ptsh, pch=20)
            abline(v=pretty(range(s.ptsh[,1]), n=15), lty=3, col="gray", lwd=0.1)
            #res[[idVal]][[1]] <- s.ptsh
            
            xmat <- s.ptsh[,1,drop=FALSE]; ymat <- s.ptsh[,2,drop=FALSE]
        
        } else {

            if (is.null(ptsh.idx)) {
                ptsh.idx.use <- 1:length(lxy[["ptsh"]][[idVal]])
            } else {
                ptsh.idx.use <- intersect(ptsh.idx, 1:length(lxy[["ptsh"]][[idVal]]))
            }

            if (!is.null(slim)) {
                if (length(slim) != 2) stop("slim should be a two-element numeric vector")
                for (i in ptsh.idx.use) {
                    svals <- lxy[["ptsh"]][[idVal]][[i]][["s.ptsh"]][,1]
                    lxy[["ptsh"]][[idVal]][[i]][["s.ptsh"]] <- lxy[["ptsh"]][[idVal]][[i]][["s.ptsh"]][ svals >= slim[1] & svals <= slim[2], ]
                }
            }
        
            xlim <- range(sapply(lxy[["ptsh"]][[idVal]][ptsh.idx.use], function(x) x[["s.ptsh"]][,1]))
            ylim <- range(sapply(lxy[["ptsh"]][[idVal]][ptsh.idx.use], function(x) x[["s.ptsh"]][,2]))

            if (title.show && is.null(title)) title.str <- paste("s vs. ptsh\n", idVal, ", sample data", sep="")
            plot(NULL, xlim=xlim, ylim=ylim, xlab="s", ylab="proportion time-selected hulls", main=title.str, ...)

            ## Create a matrix of 'NA' values to store the values plotted
            max.num.vals <- max(sapply(lxy[["ptsh"]][[idVal]][ptsh.idx.use], function(x) nrow(x[["s.ptsh"]])))
            xmat <- ymat <- matrix(NA, ncol=length(ptsh.idx.use), nrow=max.num.vals)

            ## Put down the individual series
            for (j in 1:length(ptsh.idx.use)) {
                i <- ptsh.idx.use[j]
                s.ptsh <- lxy[["ptsh"]][[idVal]][[i]][["s.ptsh"]]
                #res[[idVal]][[i]] <- s.ptsh
                points(s.ptsh, type="l", col=palette()[i])
                points(s.ptsh, pch=20, cex=1, col=palette()[i])
                #print("pause");browser()
                xmat[1:nrow(s.ptsh),j] <- s.ptsh[,1,drop=FALSE]
                ymat[1:nrow(s.ptsh),j] <- s.ptsh[,2,drop=FALSE]
            }

            ## Create vertical lines
            abline(v=pretty(xlim, n=15), lty=3, col="gray", lwd=0.1)
            
            if (!identical(legend,"none")) {
                legend.labels <- paste("n=", sapply(lxy[["ptsh"]][[idVal]][ptsh.idx.use], function(x) x[["n"]]), ", k=", sapply(lxy[["ptsh"]][[idVal]][ptsh.idx.use], function(x) x[["k"]]), sep="")
                legend(legend, legend=legend.labels, col=palette()[1:length(lxy[["ptsh"]][[idVal]])], lty=1, bg="white")
            }
            
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

        if (!is.null(png.dir)) dev.off()

        res[[paste(idVal, ".sptsh", sep="")]] <- list(fn=png.fn, dim=img.dim, desc=desc.str, id=idVal, x=xmat, y=ymat)

    }

    if (!is.null(png.dir)) {
        cat("png file(s) made: \n")
        print(pngs.made)
    }

    return(invisible(res))

}


