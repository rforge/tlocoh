#' Delete an isopleth
#'
#' Delete isopleth(s) from a LoCoH-hullset object
#'
#' @param lhs A LoCoH-hullset object
#' @param iso.idx The index(s) of isopleths to delete (numeric vector or 'all')
#'
#' @note 
#' To see the indices of isopleths, run \code{\link{summary.locoh.lhs}}
#'
#' @return A LoCoH-hullset object
#'
#' @export

lhs.iso.del <- function(lhs, iso.idx="all") {

    if (!inherits(lhs, "locoh.lhs")) stop("lhs should be of class \"locoh.lhs\"")
    if (!identical(iso.idx,"all") && !is.numeric(iso.idx)) stop("iso.idx must be either an integer vector or 'all'")
     
    for (hs.idx in 1:length(lhs)) {
        if (identical(iso.idx, "all")) {
            lhs[[hs.idx]][["isos"]] <- NULL
        } else {
            lhs[[hs.idx]][["isos"]][iso.idx] <- NULL
        }
    }
    return(lhs)
}
