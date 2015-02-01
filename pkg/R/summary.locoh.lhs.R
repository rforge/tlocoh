#' Show summary of a LoCoH-hullset
#'
#' Displays a summary of a LoCoH-hullset object
#'
#' @param object A \link{LoCoH-hullset} object
#' @param file A file name
#' @param id The name(s) of individuals to summarize
#' @param k The k value of hullsets to summarize
#' @param r The r value of hullsets to summarize
#' @param a The a value of hullsets to summarize
#' @param s The s value of hullsets to summarize
#' @param hs.names The name(s) of hullsets to summarize
#' @param iso.details Display details of the isopleths. T/F
#' @param hsp.details Display details about saved hull scatterplots. T/F
#' @param desc Display the hullset description. T/F
#' @param compact Use a compact format. T/F
#' @param ... Other arguments
#'
#' @export
#' @method summary locoh.lhs

summary.locoh.lhs <- function(object, file='', id=NULL, k=NULL, r=NULL, a=NULL, s=NULL, hs.names=NULL, iso.details=FALSE, hsp.details=FALSE, desc=FALSE, compact=FALSE, ...) {

    if (!missing(lhs)) {
      warning("argument lhs is deprecated; please use object instead.", call. = FALSE)
    } 

    lhs <- object
    if (!inherits(lhs, "locoh.lhs")) stop("object should be of class \"locoh.lhs\"")
    if (!is.null(lhs[[1]][["xys"]])) stop("Old data structure detected")
    if (file!="") sink(file=file)

    cat("Summary of LoCoH-hullset object:", deparse(substitute(object)), "\n")
    if (!compact) cat("Created by: T-LoCoH ", as.character(attr(lhs, "tlocoh_ver")), "\n", sep="")

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
                cat("[", i, "] ", names(lhs[[hs.idx]][["isos"]])[i], if (is.null( lhs[[hs.idx]][["isos"]][[i]][["rast"]])) "" else " (+rast)", "\n", sep="")
                if (iso.details) print(formatdf4print(lhs[[hs.idx]][["isos"]][[i]][["polys"]]@data, indent=14), row.names=FALSE)
            }
        }        
        
        if (!compact) {

            if (!is.null(lhs[[hs.idx]][["hsp"]])) {         
                cat("     hsp: ") 
                for (i in 1:length(lhs[[hs.idx]][["hsp"]])) {
                    
                    if (i > 1) cat("          ")
                    #cat(cw(paste("hsp: ", paste(sort(names(lhs[[hs.idx]][["hsp"]])), collapse=", ", sep=""), sep=""), final.cr=TRUE, indent=5, exdent=10))
                    cat("[", i, "] ", names(lhs[[hs.idx]][["hsp"]])[i], "\n", sep="")
                    #cat(cw(paste("hsp: ", paste(sort(names(lhs[[hs.idx]][["hsp"]])), collapse=", ", sep=""), sep=""), final.cr=TRUE, indent=5, exdent=10))
                    if (hsp.details) {
                        if (!is.null(lhs[[hs.idx]][["hsp"]][[i]][["regions"]])) {
                            hsp.filt <- lhs.filter.hsp(lhs, hsp=i)
                            regions.str <- paste(sapply(hsp.filt, function(x) paste(x[["label"]], " (", x[["col"]], ",n=", length(x[["idx"]]), ")", sep="")), collapse="; ")
                            regions.str <- paste("reg: ", regions.str, sep="")
                            cat(cw(regions.str, final.cr=TRUE, indent=14, exdent=19))
                        }
                    }
                }
            
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
            
            cat(cw(paste("created: ", lhs[[hs.idx]][["gen.date"]]), final.cr=TRUE, indent=1, exdent=10))
            cat("\n")
        }

    }

    if (file!='') sink()
    return(invisible(NULL))

}
