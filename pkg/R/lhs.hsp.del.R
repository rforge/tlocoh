#' Delete a hull scatterplot from a LoCoH-hullset object
#'
#' Deletes a saved hull scatterplot from a LoCoH-hullset object
#'
#' @param lhs A LoCoH-hullset object
#' @param hsp.idx The index(s) of the hullsets to delete. A numeric vector or 'all'
#' @param status Show messages, T/F
#'
#' @note
#' Use the summary() command to see the indices of saved hull scatterplots
#'
#' @return A LoCoH-hullset object
#'
#' @export

lhs.hsp.del <- function(lhs, hsp.idx="all", status=TRUE) {

    ## Deletes hull scatterplots saved with lhs
    ## In the future we could add some filters for lhs so hsp only deleted from certain hull sets
    
    if (!inherits(lhs, "locoh.lhs")) stop("lhs should be of class \"locoh.lhs\"")
  
    deleted.hsp <- character()
    for (hs.name in names(lhs)) {
        hsp.names <- names(lhs[[hs.name]][["hsp"]])
        if (hsp.idx=="all") {
            lhs[[hs.name]][["hsp"]] <- NULL
        } else {
            lhs[[hs.name]][["hsp"]][hsp.idx] <- NULL
        }
        deleted.hsp <- c(deleted.hsp, hsp.names[!hsp.names %in% names(lhs[[hs.name]][["hsp"]])])
    }
    
    if (status) {
        if (length(deleted.hsp)==0 ) {
            cat("No hull scatterplots deleted \n")
        } else {
            cat("Hull scatterplot(s) deleted: \n")
            print(deleted.hsp, row.names=FALSE)
        }
    }
    return(lhs)
}
