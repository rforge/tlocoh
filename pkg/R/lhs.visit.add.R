#' Add time use hull metrics to a LoCoH-hullset object
#'
#' Computes visitation and duration hull metrics based on inter-visit gap value(s)
#'
#' @param lhs A LoCoH-hullset object
#' @param ivg Value(s) for inter-visit gap (in seconds) (numeric vector)
#' @param check_samp_int Whether or not to check whether \code{ivg} is greater than the median sampling interval. T/F.
#' @param status Show status messages. T/F.
#'
#' @note
#' The inter-visit gap is the period of time (in seconds) which must pass before another occurrence in the hull can be considered a separate visit.
#' Occurences in the hull are considered a separate visit only if the animal was absent from the hull for a period of time >= ivg.
#'
#' For each ivg value, the function examines each hull and computes the number of separate visits to the hull (visitation, hull metric name = "nsv")
#' and the mean number of locations per visit (visit duration, hull metric name = "mnlv")
#'
#' @return A LoCoH-hullset object
#'
#' @export
#' @import pbapply

lhs.visit.add <- function(lhs, ivg=NULL, check_samp_int=TRUE, status=TRUE) {

    if (!inherits(lhs, "locoh.lhs")) stop("lhs should be of class \"locoh.lhs\"")
    if (TRUE %in% sapply(lhs, function(hs) is.null(hs[["pts"]][["dt"]]))) stop("Date stamps not found, can't add time use metrics")
    if (is.null(ivg)) stop("Need a value for the inter-visit gap (ivg)")

    for (hs.idx in 1:length(lhs)) {
        if (status) cat(names(lhs)[hs.idx], "\n")
        
        tau <- lhs[[hs.idx]][["rw.params"]][1, "time.step.median"]
        dt.range.int <- as.numeric(range(lhs[[hs.idx]][["pts"]][["dt"]]))
        
        for (ivg.idx in 1:length(ivg)) {
            ivgVal <- ivg[ivg.idx]
            
            if (check_samp_int && ivgVal < tau) {
                cat("ivg should not be smaller than median sampling frequency, skipping... \n")
            } else {
                if (status) cat(ivg.idx, " of ", length(ivg), ". Computing the number of visits in each hull for ivg=", ivgVal, " (", secs.fmt(ivgVal), ")\n", sep="")
                
                ivg.tab.lst <- pblapply(lhs[[hs.idx]][["enc.pts"]][["idx"]], function(x) as.numeric(table(cumsum(c(1, diff(as.numeric(lhs[[hs.idx]][["pts"]][["dt"]][x])) >= ivgVal)))))
                
                nsv <- sapply(ivg.tab.lst, length)
                mnlv <- sapply(ivg.tab.lst, mean)
                
                lhs[[hs.idx]][["hulls"]][[paste("nsv.", ivgVal, sep="")]] <- nsv
                lhs[[hs.idx]][["hulls"]][[paste("mnlv.", ivgVal, sep="")]] <- mnlv
                
                lhs[[hs.idx]][["hm"]][[  paste("nsv.", ivgVal, sep="") ]] <- list(type="nsv", aux=list(ivg=ivgVal))
                lhs[[hs.idx]][["hm"]][[  paste("mnlv.", ivgVal, sep="") ]] <- list(type="mnlv", aux=list(ivg=ivgVal))
                                
                if (is.null(lhs[[hs.idx]][["hm.params"]])) lhs[[hs.idx]][["hm.params"]] <- list()
                lhs[[hs.idx]][["hm.params"]][["ivg"]] <- unique(c(lhs[[hs.idx]][["hm.params"]][["ivg"]], ivgVal))
                
            }
        }
        
    }    
    return(lhs)

}
