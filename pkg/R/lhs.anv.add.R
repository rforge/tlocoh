#' Adds an ancillary value to a hullset
#'
#' Add ancillary values to the points of a hullset, which can then be used as hull metrics
#'
#' @param lhs A LoCoH-hullset object
#' @param id The name(s) of individuals to export
#' @param k The k value of hullsets to export
#' @param r The r value of hullsets to export
#' @param a The a value of hullsets to export
#' @param s The s value of hullsets to export
#' @param hs.names The name(s) of saved hullsets to export
#' @param anv A vector, named list, or data frame with the same number of values as points
#' @param anv.desc A character vector of descriptions
#' @param overwrite Whether to overwrite existing variables with the same names (T/F)
#'
#' @return A \link{LoCoH-hullset} object
#'
#' @details This will add one or more ancillary values (columns) to the point locations
#' in a \link{LoCoH-hullset} object.
#'
#' @export


lhs.anv.add <- function(lhs, id=NULL, k=NULL, r=NULL, a=NULL, s=NULL, hs.names=NULL,
                        anv, anv.desc=NULL, overwrite=FALSE) {

    if (!inherits(lhs, "locoh.lhs")) stop("lhs should be of class \"locoh.lhs\"")

    if (is.null(id) && is.null(r) && is.null(k) && is.null(a) && is.null(s) && is.null(hs.names)) {
        hs.matching.idx <- 1:length(lhs)
    } else {
        hs.matching.idx <- lhs.select.which(lhs, id=id, r=r, k=k, a=a, s=s, hs.names=hs.names)
    }
    if (length(hs.matching.idx)==0) stop("No sets of hulls found matching those criteria")

    ## if anv is a vector, convert to a named list
    if (is.vector(anv)) {
        lst <- list()
        lst[[deparse(substitute(anv))]] <- anv
        anv <- lst
    }

    err.msg <- cw("anv must be a dataframe, vector, or named list with the same number of values as locations", exdent=3, final.cr=FALSE)
    if (!is.list(anv)) stop(err.msg)
    if (is.null(names(anv)))  stop(err.msg)
    #if (FALSE %in% (sapply(anv, length)==length(lxy[["pts"]]))) stop(err.msg)
    if (is.null(anv.desc)) {
        anv.desc <- rep(NA, length(anv))
    } else {
        if (length(anv.desc) != length(anv)) stop("anv.desc must be the same length as the number of variables in anv")
    }

    anv.skipped <- NULL
    anv.wrongnum <- NULL
    
    for (hs.idx in hs.matching.idx) {

        ## Initialize the catalog if not already there
        if (is.null(lhs[[hs.idx]][["anv"]])) lhs[[hs.idx]][["anv"]] <- data.frame(anv="", desc="", stringsAsFactors=FALSE)[0,]
    
        for (i in 1:length(anv)) {
            anv.name <- names(anv)[i]
            if (anv.name %in% lhs[[hs.idx]][["anv"]][["anv"]]) {
                if (overwrite) {
                    if (length(anv[[i]]) == nrow(lhs[[hs.idx]][["pts"]])) {
                        lhs[[hs.idx]][["pts"]]@data[[anv.name]] <- anv[[i]]
                        lhs[[hs.idx]][["anv"]][ lhs[[hs.idx]][["anv"]][["anv"]]==anv.name , ] <- c(anv.name, anv.desc[i])
                    } else {
                        anv.wrongnum <- c(anv.wrongnum, paste(names(lhs)[hs.idx], ": ", anv.name, sep=""))
                    }
                } else {
                    anv.skipped <- c(anv.skipped, paste(names(lhs)[hs.idx], ": ", anv.name, sep=""))
                }
            } else {
                if (length(anv[[i]]) == nrow(lhs[[hs.idx]][["pts"]])) {
                    print("closer");browser()
                    lhs[[hs.idx]][["anv"]] <- rbind(lhs[[hs.idx]][["anv"]], data.frame(anv=anv.name, desc=anv.desc[i], stringsAsFactors=FALSE))
                    lhs[[hs.idx]][["pts"]]@data[[anv.name]] <- anv[[i]]
                } else {
                    anv.wrongnum <- c(anv.wrongnum, paste(names(lhs)[hs.idx], ": ", anv.name, sep=""))
                }
            }
        }

    }

    if (!is.null(anv.skipped)) cat("The following variable(s) were skipped because they already exist:\n", paste("  ", anv.skipped, sep="", collapse="\n"), sep="")
    if (!is.null(anv.wrongnum)) cat("The following variable(s) were skipped because the number of elements doesn't match the number of points:\n", paste("  ", anv.skipped, sep="", collapse="\n"), sep="")

    return(lhs)

}
