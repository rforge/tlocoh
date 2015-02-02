#' Show a summary of a LoCoH-xy object
#'
#' Prints a summary of a locoh xy object (set of locations)
#'
#' @param object A \link{LoCoH-xy} object
#' @param lxy Deprecated, use \code{object} instead
#' @param file A file name where the results will be saved
#' @param dt.int Whether to show a summary of the sampling interval
#' @param round.coords The number of digits to display for the coordinates of the spatial extent
#' @param ptsh Show table of 's' and proportion of time selected hulls values (if available)
#' @param ... Other arguments
#'
#' @seealso \code{\link{lxy.plot.freq}}, \code{\link{lxy.ptsh.add}}
#'
#' @method summary locoh.lxy
#' @export
#' @import sp

summary.locoh.lxy <- function(object, lxy, file="", dt.int=FALSE, round.coords=1, ptsh=FALSE, ...) {

    if (!missing(lxy)) warning("argument lxy is deprecated; please use 'object' instead.", call. = FALSE)
    lxy <- object

    if (!inherits(lxy, "locoh.lxy")) stop("object should be of class \"locoh.lxy\"")
    if (!is.null(lxy[["xys"]])) stop("Old data structure detected. Fix with lxy.repair()")
    if (file!="") sink(file=file)

    cat("Summary of LoCoH-xy object:", deparse(substitute(object)), "\n")
    
    cat("***Locations\n")
    ids.df <- do.call(rbind, lapply(levels(lxy[["pts"]][["id"]]), function(id) data.frame(id=id, num.pts=sum(lxy[["pts"]][["id"]]==id), dups=length(which(duplicated(coordinates(lxy[["pts"]][lxy[["pts"]][["id"]]==id, ]))))   )))
    print(formatdf4print(ids.df, indent=3), row.names=FALSE)
    
    cat("***Time span\n")
    if (is.null(lxy[["pts"]][["dt"]])) {
        cat("   no times recorded \n")
    } else {
        time.span.df <- NULL
        for (idVal in levels(lxy[["pts"]][["id"]])) {
            time.span <- range(lxy[["pts"]][["dt"]][lxy[["pts"]][["id"]]==idVal])
            time.span.difftime <- diff(time.span)
            time.span.df <- rbind(time.span.df, c(idVal, format(time.span, format = "%Y-%m-%d"), 
                                  paste(round(as.numeric(time.span.difftime),digits=1), " ", attr(time.span.difftime, "units"), sep="")))
        }
        time.span.df <- as.data.frame(time.span.df)
        names(time.span.df) <- c("id", "begin", "end", "period")
        print(formatdf4print(time.span.df, indent=3), row.names=FALSE)
    
        if (dt.int) {
            if (!is.null(lxy[["dt.int"]])) {
                cat("***Sampling intervals", 
                    if (lxy[["dt.int"]][lxy[["dt.int"]][["id"]]==idVal,"rounded.to.nearest"][1] > 1) paste(" (rounded to nearest ", lxy[["dt.int"]][lxy[["dt.int"]][["id"]]==idVal,"rounded.to.nearest"][1], " sec)", sep="") else "", 
                    "\n", sep="")
                for (idVal in levels(lxy[["pts"]][["id"]])) {
                    if (nlevels(lxy[["pts"]][["id"]])>1) cat("   ", idVal, "\n", sep="")
                    dt.int.df <- lxy[["dt.int"]][lxy[["dt.int"]][["id"]]==idVal,c("interval", "count")]
                    dt.int.df[,"interval"] <- paste(dt.int.df[,"interval"], "s (", sapply(dt.int.df[,"interval"], secs.fmt), ")", sep="")
                    print(formatdf4print(dt.int.df, indent=3), row.names=FALSE)
                }
            }    
        }
    }

    cat("***Spatial extent \n")
    for (idVal in levels(lxy[["pts"]][["id"]])) {    
        if (nlevels(lxy[["pts"]][["id"]])>1) cat("   ", idVal, "\n", sep="")
        cat("     x:", paste(round(range(coordinates(lxy[["pts"]][lxy[["pts"]][["id"]]==idVal, ])[, 1]), round.coords), collapse=" - "), "\n")
        cat("     y:", paste(round(range(coordinates(lxy[["pts"]][lxy[["pts"]][["id"]]==idVal, ])[, 2]), round.coords), collapse=" - "), "\n")
    }
    cat("  proj: ", lxy[["pts"]]@proj4string@projargs, "\n", sep="")
    
    if (!is.null(lxy[["rw.params"]])) {
        cat("***Movement properties \n")
        rw.params.df <- lxy[["rw.params"]]
        rownames(rw.params.df) <- paste("   ", rownames(rw.params.df), sep="")
        rw.params.df <- transform(rw.params.df, time.step.median=paste(rw.params.df$time.step.median, " (", sapply(rw.params.df$time.step.median, secs.fmt), ")",sep=""))
        print(formatdf4print(rw.params.df, indent=3), row.names=FALSE)
    }

    cat("***Ancilliary Variables: \n")
    if (is.null(lxy[["anv"]])) {
        cat("   -none- \n")
    } else {
        anv.names <- as.character(lxy[["anv"]][,"anv"])
        desc.idx <- !sapply(lxy[["anv"]][,"desc"], is.na)
        anv.names[desc.idx] <- paste(anv.names[desc.idx], " (", as.character(lxy[["anv"]][desc.idx,"desc"]), ")", sep="")
        cat(cw(paste(anv.names, collapse="; ", sep=""), final.cr=TRUE, indent=3, exdent=3))
    }

    if (!is.null(lxy[["ptsh"]])) {
        cat("***ptsh s-values computed\n")
        if (ptsh) {
            for (idVal in names(lxy[["ptsh"]])) {
                for (ptsh.lst in lxy[["ptsh"]][[idVal]]) {
                    cat("   id=", ptsh.lst[["id"]], ", k=", ptsh.lst[["k"]], ", sample size =", ptsh.lst[["n"]], "\n", sep="")
                    row.idx <- match(ptsh.lst[["target.s"]], ptsh.lst[["s.ptsh"]][,1])
                    print(formatdf4print(data.frame(ptsh.lst[["s.ptsh"]])[row.idx,2:1], indent=0), row.names=FALSE)
                }
            }
        
        
        } else {
            ptsh.info <- NULL        
            for (idVal in names(lxy[["ptsh"]])) {
                ptsh.info <- rbind(ptsh.info, do.call(rbind, lapply(lxy[["ptsh"]][[idVal]], function(x) data.frame(id=idVal, k=x$k, n=x$n, ptsh=paste(x$target.ptsh, collapse=", ", sep=""), stringsAsFactors=FALSE))))
            }
            ptsh.info.f4c <- formatdf4cat(ptsh.info, indent=3, wrap.last.col=TRUE, just.left=rep(FALSE,4), print=TRUE)
            
        }

        
    }
    


    cat("***Nearest-neighbor set(s): \n")
    if (is.null(lxy[["nn"]])) {
        cat("   none saved \n")
    } else {
        names.ord <- order(sapply(lxy[["nn"]], function(x) x[["id"]]), sapply(lxy[["nn"]], function(x) x[["s"]]))
        for (i in names.ord) {
            cat("   ", i, " ", names(lxy[["nn"]])[i], "\n", sep="")
            if (!is.null(lxy[["nn"]][[i]][["auto.a.df"]])) {
                auto.a.df.tmp <- lxy[["nn"]][[i]][["auto.a.df"]]
                auto.a.df.tmp <- auto.a.df.tmp[order(auto.a.df.tmp[["ptp"]], auto.a.df.tmp[["nnn"]]),]
                rownames(auto.a.df.tmp) <- paste("       auto.a #", 1:nrow(auto.a.df.tmp), sep="")
                print(auto.a.df.tmp)
            }
        }
    }


    if (!is.null(lxy[["amin"]])) {
        cat("***Minimum-a for point inclusion \n")
        print(formatdf4print(lxy[["amin"]]), row.names=FALSE)
    }


    if (file!='') sink()


}
