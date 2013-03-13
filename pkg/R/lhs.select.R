#' Select hullsets
#'
#' @param lhs A LoCoH-hullset object
#' @param id The name(s) of individuals to extract
#' @param k The k value of hullsets to extract
#' @param r The r value of hullsets to extract
#' @param a The a value of hullsets to extract
#' @param s The s value of hullsets to extract
#' @param hs.names The name(s) of saved hullsets to extract
#' @param hs.idx The indices of saved hullsets to extract
#' @param status Show status messages (T/F)
#'
#' @return A LoCoH-hullset object
#'
#' @export

lhs.select <- function (lhs, id=NULL, k=NULL, r=NULL, a=NULL, s=NULL, hs.names=NULL, hs.idx=NULL, status=TRUE) {
  
    ## returns a locoh-hullset object with just the hullsets specified by the parameters
    ## if any of the parameters are omitted (and thus null), doesn't use that parameter to filter
    
    if (!inherits(lhs, "locoh.lhs")) stop("lhs should be of class \"locoh.lhs\"")
    
    k <- vectorize.parameter(k)
    a <- vectorize.parameter(a)
    r <- vectorize.parameter(r)
    s <- vectorize.parameter(s)
    
    lhs.new = list()
    class(lhs.new) <- "locoh.lhs"
    
    if (is.null(hs.names)) hs.names <- names(lhs)
    if (is.null(hs.idx)) hs.idx <- 1:length(lhs)
    hs.names.use <- intersect(names(lhs)[hs.idx], hs.names)
    
    for (hs.name in hs.names.use) {
        if (is.null(k) || n2z(lhs[[hs.name]][["k"]]) %in% k) {
            if (is.null(id) || lhs[[hs.name]][["id"]] %in% id) {
                if (is.null(a) || n2z(lhs[[hs.name]][["a"]]) %in% a) {
                  if (is.null(r) || n2z(lhs[[hs.name]][["r"]]) %in% r) {
                      if (is.null(s) || TRUE %in% sapply(s, function(sVal) isTRUE(all.equal(lhs[[hs.name]][["s"]], sVal)))) {
                          lhs.new[[hs.name]] <- lhs[[hs.name]]
                      }
                  }
                }
            }
        }
    }

    if (length(lhs.new)==0) {
        stop("No hullsets match those criteria")    
    } else {
        attr(lhs.new, "tlocoh_ver") <- attr(lhs, "tlocoh_ver")
        return(invisible(lhs.new))
    }
}
