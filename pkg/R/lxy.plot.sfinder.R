#' Plot distributions of 's' such that the vmax/diffusion term in TSD are equivalent to the actual displacement
#'
#' This will find the value of s that will result in the time term of TSD being approximately equal to the distance term
#'
#' @param lxy A LoCoH-xy object
#' @param id The id(s) of the individual(s) to include in the plot
#' @param delta.t numeric vector of delta.t values (in seconds) for which a s-values will be computed. Can also be 'auto' in which case \code{delta.t.auto.n} delta.t values will be computed ranging from 3 * tau to the 80th percent point-to-point interval for the entire dataset
#' @param delta.t.auto.n The number of delta-ts to use when delta.t="auto"
#' @param delta.t.err The proportion of delta.t within which the interval between two points must fall to be included in the plot
#' @param outline Show outliers in the box plots T/F
#' @param desc Which side to display automatically generated desciptive text (e.g. caption). 0=none, 1=bottom, 3=top.
#' @param cex.desc The expansion factor for the descriptive text. Numeric value.
#' @param time.term The type of time term to use ('vmax' is computed as deltaT * maximum possible velocity, while 'dif' computes Gauusian diffusion distance)
#' @param col.desc The color of the descriptive text. Color value.
#' @param mar The plot margins. A four item numeric vector
#' @param mgp The distance away from the edge of the plot for the 1) label, 2) tick marks, and 3) axis line. A three-item numeric vector
#' @param figs.per.page The number of plots per page.
#' @param title The title to be displayed. Character. If NULL a title will be constructed.
#' @param title.show Whether to show the title. T/F.
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
#' @return A list of lists, one for each plot containing the filename (NULL if no png made), the image dimensions (or NULL), the descriptive text, the id, 
#' and a list of the svals
#'
#' @export
  
lxy.plot.sfinder <- function(lxy, id=NULL, delta.t="auto", delta.t.auto.n=8, delta.t.err=0.01, outline=FALSE, 
                             desc=c(0,1,3)[2], cex.desc=0.8, col.desc="darkgreen", time.term=c("vmax", "dif")[1],
                             mar=c(3, 3, if (title.show) 2.8 else 0.7, 0.5), mgp=c(1.8, 0.5, 0), figs.per.page=NULL, title=NULL, title.show=TRUE,
                             panel.num=NULL, panel.num.inside.plot=!title.show, 
                             png.dir=NULL, png.dir.make=TRUE, png.width=800, png.height=png.width, png.overwrite=TRUE, png.pointsize=12+(png.width-480)/80, ...) {

    ## Currently it could just report it, but maybe later save it in the data structure
    ## Can also get this from the rw.params (I think)
    ## delta.t is a numeric vector of delta.t values (in seconds) for which a s-values will be computed such that the time
    ##         term of TSD is equivalent to the Euclidean term. If 'auto,n' it will n intervals of tau starting from 3tau to the 80% 

    if (!inherits(lxy, "locoh.lxy")) stop("lxy should be of class \"locoh.lxy\"")
    
    if (is.null(lxy[["pts"]][["dt"]])) stop("Can't overlay the diffusion distance, no time stamps found")
    if (is.null(lxy[["rw.params"]])) stop("Can't overlay the diffusion distance, rw.params not saved")
    if (is.null(delta.t)) stop("Delta.t can't be null")
    if (!time.term %in% c("vmax", "dif")) stop("Unknown value for 'time.term'")

    if (is.null(id)) {
        id <- levels(lxy[["pts"]][["id"]])
    } else {
        if (FALSE %in% (id %in% levels(lxy[["pts"]][["id"]]))) stop("id value(s) not found")
    }

    if (is.null(figs.per.page)) {
        if (is.null(png.dir)) {
            figs.per.page <- length(id)
        } else {
            figs.per.page <- 1
        }
     } else {
        if (figs.per.page > 1) stop("When saving plots as PNG files, you can only have one plot per page")
     }
    
    lxy.dt.int <- as.numeric(lxy[["pts"]][["dt"]])
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
    
    res <- list()
    for (idVal in id) {
        ## Grab the random walk parameters for this idVal
        tau <- lxy[["rw.params"]][lxy[["rw.params"]][["id"]] == idVal , "time.step.median"]
        d.bar <- lxy[["rw.params"]][lxy[["rw.params"]][["id"]] == idVal , "d.bar"]
        vmax <- lxy[["rw.params"]][lxy[["rw.params"]][["id"]] == idVal , "vmax"]

        ids.idx <- which(lxy[["pts"]][["id"]] == idVal)
        svals.lst <- list()

        if (identical(delta.t, "auto")) {
            ## Calculate the delta.t, making sure the values are multiples of tau
            max.delta.t <- 0.8
            #tau.num <- round(as.numeric(difftime(lxy[["pts"]][["dt"]] [length(lxy[["pts"]][["dt"]]) * max.delta.t], lxy[["pts"]][["dt"]][1], units="secs")) / tau)
            tau.num <- round(as.numeric(difftime(lxy[["pts"]][["dt"]] [ids.idx[length(ids.idx) * max.delta.t]], lxy[["pts"]][["dt"]][1], units="secs")) / tau)
            tau.int <-  max(1, ceiling(tau.num / delta.t.auto.n))
            tau.first <- 6
            delta.t.use <- (tau.first * tau) + (0:(delta.t.auto.n - 1) * tau.int * tau)
        } else {
            delta.t.use <- delta.t
        }

        
        for (i in 1:length(delta.t.use)) {
    
            e <- delta.t.use[i]
            if (e < tau) stop("Delta-t can not be less than the median sampling frequency")
            
            delta.t.lbound <- e * (1 - delta.t.err)
            delta.t.ubound <- e * (1 + delta.t.err)
    
            ## Get all elligible pairs of points. First, we identify the starting points that occur earlier enough in the dataset
            ## as to have a possible match
            idx.start.pt <- ids.idx[1:findInterval(lxy.dt.int[ids.idx[length(ids.idx)]] - delta.t.lbound, lxy.dt.int[ids.idx])]
            
            ## Next we find the best choice for an end point. Because lxy.dt.int is in ascending order, so we can use 
            ## the lightening-fast findInterval fucntion to find the index the point whose time stamp is less than or equal 
            ## to the starting point plus the upper bound of the allowable delta-t
            idx.end.pt <- ids.idx[findInterval(lxy.dt.int[idx.start.pt] + delta.t.ubound, lxy.dt.int[ids.idx])]
            
            ## We still have to determine if the time interval is within the allowed range, some of them could be too short,
            ## so let's compute the actual time difference
            end.start.diff <- lxy.dt.int[idx.end.pt] - lxy.dt.int[idx.start.pt]
            
            ## Identify which pairs are within the allowed range
            end.start.diff.good <- (end.start.diff >= delta.t.lbound) & (end.start.diff <= delta.t.ubound)

            ## if (debug) cat("  Number of starting points evaluated: ", length(idx.start.pt), ". Number found to have elligible end partners: ", sum(end.start.diff.good), "\n", sep="")
            
            ## Keep just the pairs whose delta-t is within the allowed range
            idx.start.pt <- idx.start.pt[end.start.diff.good]
            idx.end.pt <- idx.end.pt[end.start.diff.good]
            
            ## Calculate s-eq for these pairs of points and save in svals.lst
            if (time.term == "dif") {
                #svals.lst[[as.character(e)]] <- sqrt((coordinates(lxy[["pts"]])[idx.end.pt,1] - coordinates(lxy[["pts"]])[idx.start.pt,1])^2 + (coordinates(lxy[["pts"]])[idx.end.pt,2] - coordinates(lxy[["pts"]])[idx.start.pt,2]  )^2) * tau / ((lxy.dt.int[idx.end.pt] - lxy.dt.int[idx.start.pt]) * d.bar^2)
                svals.lst[[as.character(e)]] <- sqrt((coordinates(lxy[["pts"]])[idx.end.pt,1] - coordinates(lxy[["pts"]])[idx.start.pt,1])^2 + (coordinates(lxy[["pts"]])[idx.end.pt,2] - coordinates(lxy[["pts"]])[idx.start.pt,2]  )^2) / (d.bar * sqrt((lxy.dt.int[idx.end.pt] - lxy.dt.int[idx.start.pt]) / tau ))
            } else if (time.term == "vmax") {
                svals.lst[[as.character(e)]] <- sqrt((coordinates(lxy[["pts"]])[idx.end.pt,1] - coordinates(lxy[["pts"]])[idx.start.pt,1])^2 + (coordinates(lxy[["pts"]])[idx.end.pt,2] - coordinates(lxy[["pts"]])[idx.start.pt,2])^2)  / ((lxy.dt.int[idx.end.pt] - lxy.dt.int[idx.start.pt]) * vmax)
            
            }
    
        }
        
        ## Create png device for combined set of plots if needed
        if (is.null(png.dir)) {
            png.fn <- NULL
        } else {
            png.fn <- file.path(png.dir, paste(paste(unlist(lxy[["comment"]]), collapse = ".", sep = ""), ".sfinder.png", sep=""))
            if (file.exists(png.fn) && !png.overwrite) stop(paste(png.fn, "exists"))
            par(bg="white")
            png(filename=png.fn, height=png.height, width=png.width, bg="white", pointsize=png.pointsize)
            pngs.made <- c(pngs.made, png.fn)
            opar <- par(mfrow = n2mfrow(figs.per.page), mar=mar, mgp=mgp, oma=c(0,0,0,0))
        }

        ## Prepare desc and outer margin area
        desc.str <- paste("This plot shows the distribution of the s term in TSD for ", idVal, " for a range of delta-t such that the ", if (time.term=="vmax") "maximum distance possible" else "Gaussian diffusion",  " distance term is equal to the actual displacement.", sep="")
        res[[paste(idVal, ".sfinder", sep="")]] <- list(fn=png.fn, dim=img.dim, desc=desc.str, svals=svals.lst, id=idVal)
        if (desc !=0 ) {
            desc.str.chopped <- chop2plot(desc.str, width=dev.size()[1] * 0.9, cex=cex.desc)
            oma.vals <- c(0,0,0,0)
            oma.vals[desc] <- as.numeric(desc.str.chopped[2])
            par(oma=oma.vals)
        }

        if (title.show) {
            if (is.null(title)) {
                title.str <- parse(text=paste("\"", idVal, ": distribution of \" * s[eq]", sep=""))
            } else {
                title.str <- title
            }
        } else {
            title.str <- NULL
        }
        
        names(svals.lst) <- sapply(as.numeric(names(svals.lst)), secs.fmt)
        boxplot(svals.lst, main=title.str, xlab=expression(Delta * t), ylab=expression(s[eq]) , outline=outline, ...)

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

    }
    if (!is.null(png.dir)) {
        cat("png file(s) made: \n")
        print(pngs.made)
    }

    return(invisible(res))

}
