#' Merge LoCoH-hullsets
#'
#' @param ... Two or more LoCoH-hullset objects
#' @param check.class Whether to enforce that all objects merged belong to class locoh.lhs (T/F)
#'
#' @return A LoCoH-hullset object
#'
#' @note Hullsets in different objects can be merged together, but the hullsets themselves remain separate. If
#' duplicate hullsets are detected (e.g., same id, same mode, same parameter value), an error message will appear.
#'
#' @export

lhs.merge <- function(..., check.class=TRUE) {

    lhs.lst <- list(...)
    if (length(lhs.lst)==1) stop("Can't merge one object")
    if (length(lhs.lst)==0) return(NULL)
    
    if (check.class) {if (FALSE %in% sapply(lhs.lst, function(lhs) is(lhs, "locoh.lhs"))) stop("all merged objects should be class \"locoh.lhs\"")}
    
    lhs.names.all <- unlist(lapply(lhs.lst, names))
    if (anyDuplicated(lhs.names.all)) stop("Duplicate hullsets detected")
    
    lhs1 <- lhs.lst[[1]]
    if (!is.null(lhs1[["xys"]])) stop("Old data structure detected")
    
    for (lhs2 in lhs.lst[-1]) {
        lhs1 <- c(lhs1, lhs2)
    }
    
    class(lhs1) <- "locoh.lhs"
    attr(lhs1, "tlocoh_ver") <- packageVersion("tlocoh")
    
    return(lhs1)
    
}
