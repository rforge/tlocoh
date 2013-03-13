#' Add hull metrics for proportion of enclosed points by each hull for an ancillary variable
#'
#' Computes proportion of enclosed points of a second set of points
#'
#' @param lhs A LoCoH-hullset object
#' @param pep.var Name(s) of ancillary variable(s) saved with the LoCoH-hullset.
#' @param pep.val Vector of value(s) for which the percentage of enclosed points will be calculated. If NULL, all unique values of pep.var will be used.
#' @param npep Whether to compute the normalized proportion of enclosed points hull metric (normalized by the proportion of points in the entire dataset), T/F
#' @param status Show messages, T/F
#'
#' @note
#' Normally \code{pep.var} should be a factor or character vector. To see which ancillary variables are saved in a LoCoH-hullset 
#' use \code{\link{summary.locoh.lhs}}
#'
#' @return A LoCoH-hullset object
#' 
#' @export

lhs.pep.add <- function(lhs, pep.var, pep.val=NULL, npep=TRUE, status=TRUE) {

    if (!require(pbapply)) stop("package pbapply required")
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
            
                #print("right now, we have a var and a val");browser()
                
                ## Createa a numeric vector of ones and zeros whether each point belongs to pepVal
                ptval.int <- as.integer(lhs[[hs.idx]][["pts"]][[pepVar]] == pepVal)
                
                ## Sum up the number of points enclosed by each hull that are equal to pepVal
                if (status) cat(" - computing the proportion of ", pepVal, " in each hull \n", sep=""); flush.console()
                #pep.vals <- pbsapply(lhs[[hs.idx]][["hulls.enc"]][["idx"]], function(idx) sum(part.of.this.val[idx]) / length(idx))
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
