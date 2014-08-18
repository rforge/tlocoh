#' Add hull metrics for proportion of enclosed points by each hull for an ancillary variable
#'
#' Computes proportion of enclosed points of a second set of points
#'
#' @param lhs A \link{LoCoH-hullset} object
#' @param pep.var Name(s) of ancillary variable(s) saved with the LoCoH-hullset.
#' @param pep.val Vector of value(s) for which the percentage of enclosed points will be calculated. If NULL, all unique values of pep.var will be used.
#' @param npep Whether to compute the normalized proportion of enclosed points hull metric (normalized by the proportion of points in the entire dataset), T/F
#' @param status Show messages, T/F
#'
#' @note
#'
#' The proportion of enclosed points is a hull metric that measures relative association among N individuals when you
#' have movement data for multiple individuals simultaneous over the same tim period. The general idea is to create hulls
#' for all of the locations combined (ignoring which location was for each individual), then for each hull look at the
#' proportion of enclosed points for each individual. If the individuals ignored each other, one would expect the
#' proportion of enclosed points for any given hull to be approximately equal to the proportions of each individual in the
#' entire dataset. Deviations from this random mixing null model reflect places where the individuals did not mix evenly
#' (e.g., one dominated). 
#' 
#' Do compute pep, all points must have the same id (beause you create hulls with the combined dataset) with the original
#' id values saved as an ancillary variable (see \code{\link{lxy.id.new}}). Create hulls as you normally would using the 'a' or 'k' method,
#' although it would be recommneded to omit time (let s=0) because the time difference between locations means different things for different pairs of points
#' depending on whether they are from the same or different individuals. Pass the name of the ancillary variable that contains the original ids as
#' \code{pep.var} and the original id value(s) as \code{pep.val} (if \code{pep.val} is omitted the pep metric will be computed for all id values
#' found in \code{pep.var}. \emph{npep} (normalized proportion of enclosed points) normalizes pep by the proportion of that
#' individual in the entire dataset, such that \emph{npep=1} means the individual was in the hull in the same proportion
#' as it was in the entire dataset. 
#'                                                                           '
#' @return A LoCoH-hullset object
#' 
#' @seealso \code{\link{lxy.id.new}}
#' @export

lhs.pep.add <- function(lhs, pep.var, pep.val=NULL, npep=TRUE, status=TRUE) {

    if (!inherits(lhs, "locoh.lhs")) stop("lhs should be of class \"locoh.lhs\"")
    if (is.null(pep.var)) stop("pep.var is a required value")
    if (is.null(lhs[[1]][["anv"]])) stop("Ancillary variables data frame not found")
    hm.created <- NULL
    
    for (hs.idx in 1:length(lhs)) {
        if (status) cat(names(lhs)[hs.idx], "\n")
        if (is.null(lhs[[hs.idx]][["hm.params"]])) lhs[[hs.idx]][["hm.params"]] <- list()
        
        if (FALSE %in% (pep.var %in% lhs[[hs.idx]][["anv"]][["anv"]] )) stop("Ancillary variable not found")
        
        for (pepVar in pep.var) {
            ## Create pep.val.use (a vector of the values to check)
            
            if (is.null(pep.val)) {
                pep.val.use <- unique(lhs[[hs.idx]][["hulls"]][[pepVar]])
                if (length(pep.val.use)>10) stop("There are more than 10 values for this ancillary variable. Please specify which one(s) to compute")
            } else {
                pep.val.use <- pep.val
            }

            ## If we are computing the proportion of points in the entire dataset (i.e., npep=T), get the 
            ## proportion of total points for each value of pep.val.use
            if (npep) prp.tot.pts <- table(lhs[[hs.idx]][["hulls"]][[pepVar]]) / nrow(lhs[[hs.idx]][["pts"]])
            
            ## Add the name of this ancillary variable to the 'hm.params' element
            lhs[[hs.idx]][["hm.params"]][["pep.var"]] <- c(lhs[[hs.idx]][["hm.params"]][["pep.var"]], pepVar)
            
            for (pepVal in pep.val.use) {
            
                ## Createa a numeric vector of ones and zeros whether each point belongs to pepVal
                ptval.int <- as.integer(lhs[[hs.idx]][["pts"]][[pepVar]] == pepVal)
                
                ## Sum up the number of points enclosed by each hull that are equal to pepVal
                if (status) cat(" - computing the proportion of ", pepVal, " in each hull \n", sep=""); flush.console()
                pep.vals <- sapply(lhs[[hs.idx]][["enc.pts"]][["idx"]], function(x) sum(ptval.int[x]) / length(x))
                
                ## Create pep and npep hull metrics
                hm.name <- paste("pep.", pepVar, ".", pepVal, sep="")
                lhs[[hs.idx]][["hm"]][[hm.name]] <- list(type="pep", aux=list(pep.var=pepVar, pep.val=pepVal))
                lhs[[hs.idx]][["hulls"]][[hm.name]] <- pep.vals
                
                ## Add this value of pepVal to the list of values saved in hm.params
                lhs[[hs.idx]][["hm.params"]][["pep.val"]] <- c(lhs[[hs.idx]][["hm.params"]][["pep.val"]], pepVal)
                
                hm.created <- c(hm.created, hm.name)
                
                if (npep) {
                    hm.name <- paste("npep.", pepVar, ".", pepVal, sep="")
                    lhs[[hs.idx]][["hm"]][[hm.name]] <- list(type="npep", aux=list(pep.var=pepVar, pep.val=pepVal))
                    lhs[[hs.idx]][["hulls"]][[hm.name]] <- pep.vals / prp.tot.pts[[pepVal]]
                    hm.created <- c(hm.created, hm.name)
                }
                
            }
        }

    }    
    if (status & length(hm.name)>0) {
        cat("Hull metric(s) created: \n")
        cat(paste(" - ", hm.created, collapse="\n", sep=""), "\n")
    }
    return(lhs)
    
}
