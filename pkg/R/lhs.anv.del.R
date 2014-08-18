#' Delete ancillary variables from a LoCoH-hullset
#'
#' @param lhs A LoCoH-hullset object
#' @param anv The name(s) of ancillary variables to remove
#' @param id The name(s) of individuals to process
#' @param k The k value of hullsets to process
#' @param r The r value of hullsets to process
#' @param a The a value of hullsets to process
#' @param s The s value of hullsets to process
#' @param hs.names The name(s) of hullsets to process
#' @param status Show status messages (T/F)
#'
#' @note
#' To see which ancillary variables a hullset contains, use the \code{summary()} function. 
#'
#' Copying ancillary variables is optional when creating a hullset with \code{\link{lxy.lhs}}.
#'
#' @return A LoCoH-hullset object
#'
#' @seealso \code{\link{summary.locoh.lhs}}
#' @export

lhs.anv.del <- function(lhs, anv=NULL, id=NULL, k=NULL, r=NULL, a=NULL, s=NULL, hs.names = NULL, status=TRUE) {

    if (!inherits(lhs, "locoh.lhs")) stop("lhs should be of class \"locoh.lhs\"")
    if (length(anv)==0) stop("anv is a required parameter")
    anv <- vectorize.parameter(anv, type="character")

    if (is.null(id) && is.null(r) && is.null(k) && is.null(a) && is.null(s) && is.null(hs.names)) {
        hs.matching.idx <- 1:length(lhs)
    } else {    
        hs.matching.idx <- lhs.select.which(lhs, id=id, r=r, k=k, a=a, s=s, hs.names=hs.names)
    }
    if (length(hs.matching.idx)==0) stop("No sets of hulls found matching those criteria")
    
    deleted.anv <- NULL
    for (hs.idx in hs.matching.idx) {
        
        for (anvVal in as.character(anv)) {
            if (!is.null(lhs[[hs.idx]][["pts"]][[anvVal]])) {
                lhs[[hs.idx]][["pts"]][[anvVal]] <- NULL
                deleted.anv <- rbind(deleted.anv, data.frame(hs=names(lhs)[hs.idx], anv.del=anvVal))
                lhs[[hs.idx]][["anv"]] <- lhs[[hs.idx]][["anv"]][lhs[[hs.idx]][["anv"]][["anv"]] != anvVal , ] 
            }
        }
    }
    if (!is.null(deleted.anv)) {
        cat("Ancillary variable(s) deleted: \n")
        print(deleted.anv)
    } else {
        cat("Ancillary variable(s) not found \n")
    }
    
    return(lhs)
    
}
