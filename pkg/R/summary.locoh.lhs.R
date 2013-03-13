#' summary.locoh.lhs
#'
#' Prints a summary of a LoCoH-hullset object
#'
#' @param lhs A LoCoH-hullset object
#' @param file A file name
#' @param id The name(s) of individuals to summarize
#' @param k The k value of hullsets to summarize
#' @param r The r value of hullsets to summarize
#' @param a The a value of hullsets to summarize
#' @param s The s value of hullsets to summarize

#'
#' @export

summary.locoh.lhs <- function(lhs, file='', id=NULL, k=NULL, r=NULL, a=NULL, s=NULL, hs.names=NULL, iso.details=FALSE, desc=FALSE, compact=FALSE) {

    if (!inherits(lhs, "locoh.lhs")) stop("lhs should be of class \"locoh.lhs\"")
    if (!is.null(lhs[[1]][["xys"]])) stop("Old data structure detected")
    if (!require(sp)) stop("package sp required")
    if (file!="") sink(file=file)

    cat("Summary of LoCoH-hullset object:", deparse(substitute(lhs)), "\n")
    cat("T-LoCoH version: ", as.character(attr(lhs, "tlocoh_ver")), "\n", sep="")

    if (is.null(id) && is.null(r) && is.null(k) && is.null(a) && is.null(s) && is.null(hs.names)) {
        hs.indices <- 1:length(lhs)
    } else {    
        hs.indices <- lhs.select.which(lhs, id=id, r=r, k=k, a=a, s=s, hs.names=hs.names)
    }

    for (hs.idx in hs.indices) {
        cat("[", hs.idx, "] ", names(lhs)[hs.idx], "\n", sep="")
        if (desc) {
            cat(cw(paste("desc: ", lhs[[hs.idx]][["desc"]], sep=""), final.cr=TRUE, indent=4, exdent=10))        
        } else {
            if (!compact) {
                cat("      id: ", lhs[[hs.idx]][["id"]], "\n", sep="")
                cat("     pts: ", length(lhs[[hs.idx]][["pts"]]), "\n", sep="")
                if (!is.null(lhs[[hs.idx]][["pts"]][["dt"]])) { 
                    drange <- range(lhs[[hs.idx]][["pts"]][["dt"]])
                    cat("   dates: ", format(drange[1], usetz=TRUE), " to ", format(drange[2], usetz=TRUE), "\n", sep="")
                } 
                cat("movement: tau=", lhs[[hs.idx]][["rw.params"]][["time.step.median"]], " (", secs.fmt(lhs[[hs.idx]][["rw.params"]][["time.step.median"]]), "), vmax=", lhs[[hs.idx]][["rw.params"]][["vmax"]], ", d.bar=", lhs[[hs.idx]][["rw.params"]][["d.bar"]], "\n", sep="")
                cat("   hulls: ", nrow(lhs[[hs.idx]][["hulls"]]), if (is(lhs[[hs.idx]][["hulls"]], "SpatialPolygonsDataFrame")) "" else " (attribute data only)", "\n", sep="")
                cat("    dups: ", length(lhs[[hs.idx]][["dups"]][["dups.idx"]]), sep="")
                if (length(lhs[[hs.idx]][["dups"]][["dups.idx"]]) > 0) {
                    cat(" (offset by ", lhs[[hs.idx]][["dups"]][["offset"]], " map unit)", sep="")
                }
                cat("\n")
                
                cat("    mode: ", lhs[[hs.idx]][["mode"]], "=", lhs[[hs.idx]][[lhs[[hs.idx]][["mode"]]]], ", kmin=", lhs[[hs.idx]][["kmin"]], ", s=", lhs[[hs.idx]][["s"]], "\n", sep="")
            }
        }
        
        if (!compact) {
            cat(cw(paste("metrics: ", paste(sort(names(lhs[[hs.idx]][["hm"]])), collapse=", ", sep=""), sep=""), final.cr=TRUE, indent=1, exdent=10))
            if (!is.null(lhs[[hs.idx]][["anv"]])) { 
                cat("     anv: ", paste(sort(as.character(lhs[[hs.idx]][["anv"]][["anv"]])), collapse=", ", sep=""), "\n", sep="")
            }        
            
            if (!is.null(lhs[[hs.idx]][["hm.params"]])) { 
                cat(cw(paste("hmap: ", paste(sapply(1:length(lhs[[hs.idx]][["hm.params"]]), function(i) paste(names(lhs[[hs.idx]][["hm.params"]])[i], " (", paste(lhs[[hs.idx]]$hm.params[[i]], collapse=", ", sep=""), ")", sep=""  )), collapse="; ", sep=""), sep=""), final.cr=TRUE, indent=4, exdent=10))
            }
        }

        
        if (is.null(lhs[[hs.idx]][["isos"]])) {         
            if (!compact) {
                cat("    isos: ") 
                cat("-none- \n")
            }
        } else {
            cat("    isos: ") 
            for (i in 1:length(lhs[[hs.idx]][["isos"]])) {
                if (i > 1) cat("          ")
                cat("[", i, "] ", names(lhs[[hs.idx]][["isos"]])[i], "\n", sep="")
                if (iso.details) print(formatdf4print(lhs[[hs.idx]][["isos"]][[i]][["polys"]]@data, indent=14), row.names=FALSE)
            }
        }        
        
        if (!compact) {

            if (!is.null(lhs[[hs.idx]][["hsp"]])) {         
                cat(cw(paste("hsp: ", paste(sort(names(lhs[[hs.idx]][["hsp"]])), collapse=", ", sep=""), sep=""), final.cr=TRUE, indent=5, exdent=10))
            }        
    
            if (!is.null(lhs[[hs.idx]][["dr"]])) {         
                cat(cw(paste("dr: ", paste(sort(names(lhs[[hs.idx]][["dr"]])), collapse=", ", sep=""), sep=""), final.cr=TRUE, indent=6, exdent=10))
            }        
    
            other.elements <- character(0)
            if (!is.null(lhs[[hs.idx]][["ellipses"]])) other.elements <- c(other.elements, "ellipses")
            if (!is.null(lhs[[hs.idx]][["hso"]])) other.elements <- c(other.elements, "spatial overlap indices")
            if (!is.null(lhs[[hs.idx]][["hto"]])) other.elements <- c(other.elements, "temporal overlap indices")
            if (length(other.elements)==0) other.elements <- "-none-"
            cat(cw(paste("other: ", paste(other.elements, collapse=", ", sep=""), sep=""), final.cr=TRUE, indent=3, exdent=10))
            cat("\n")
        }

    }

    if (file!='') sink()
    return(invisible(NULL))

}
