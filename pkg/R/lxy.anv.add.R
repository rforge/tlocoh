#' Add ancillary variable(s) to a LoCoH-xy object
#'
#' @param lxy A \link{LoCoH-xy} object
#' @param anv A vector, named list or data frame with the same number of values as points
#' @param anv.desc A character vector of descriptions 
#' @param overwrite Whether to overwrite existing variables with the same names (T/F)
#'
#' @return A \link{LoCoH-xy} object 
#'
#' @export

lxy.anv.add <- function(lxy, anv, anv.desc=NULL, overwrite=FALSE) {

    if (!inherits(lxy, "locoh.lxy")) stop("lxy should be of class \"locoh.lxy\"")
    if (is.null(lxy[["pts"]])) stop("Old data structure detected, please update with lxy.repair()")
    if (!require(sp)) stop("package sp required")

    ## if anv is a vector, convert to a named list
    if (is.vector(anv)) {
        lst <- list()
        lst[[deparse(substitute(anv))]] <- anv
        anv <- lst
    }
    
    err.msg <- cw("anv must be a dataframe, vector, or named list with the same number of values as locations", exdent=3, final.cr=FALSE)
    if (!is.list(anv)) stop(err.msg) 
    if (is.null(names(anv)))  stop(err.msg)
    if (FALSE %in% (sapply(anv, length)==length(lxy[["pts"]]))) stop(err.msg)
    if (is.null(anv.desc)) {
        anv.desc <- rep(NA, length(anv))
    } else {
        if (length(anv.desc) != length(anv)) stop("anv.desc must be the same length as the number of variables in anv")
    }

    ## Initialize the catalog if not already there
    if (is.null(lxy[["anv"]])) lxy[["anv"]] <- data.frame(anv="", desc="", stringsAsFactors=FALSE)[0,]
    
    anv.skipped <- NULL
    for (i in 1:length(anv)) {
        anv.name <- names(anv)[i]
        if (anv.name %in% lxy[["anv"]][["anv"]]) {
            if (overwrite) {
                lxy[["anv"]][ lxy[["anv"]][["anv"]]==anv.name , ] <- c(anv.name, anv.desc[i])
                lxy[["pts"]]@data[[anv.name]] <- anv[[i]]
            } else {
                anv.skipped <- c(anv.skipped, anv.name)
            }
        } else {
            lxy[["anv"]] <- rbind(lxy[["anv"]], data.frame(anv=anv.name, desc=anv.desc[i], stringsAsFactors=FALSE))
            lxy[["pts"]]@data[[anv.name]] <- anv[[i]]
        }
    }
    
    if (!is.null(anv.skipped)) cat("The following variable(s) were skipped because they already exist: ", paste(anv.skipped, collapse=", ", sep=""), "\n", sep="")
    
    return(lxy)

}
