#' Returns a R expression that will recreate a saved scatterplot of hull metrics
#'
#' @param hsp A list of objects of class locoh.hsp
#' @param reg_nodes_prec The number of decimal places that will be preserved when printing the coordinates of the nodes of manually defined polygon regions in scatterplot space
#' @param var_name The name(s) of the variable that will be initialized in the output code 
#'
#' @details
#' Hull metric scatterplot settings can be saved as an object for reuse as a map legend or subsetting
#' data (see \emph{T-LoCoH Tips} on website). Saved scatterplot settings can include the boundaries of manually
#' defined (i.e., with the mouse) regions of interest, that can be used as a map legend or for data subsetting.
#' The purpose of this function is to generate R code so that 
#' polygons that were manually defined with the mouse in a hull metric scatterplot can be recreated
#' (e.g., in a markdown document) for the purposes of reproducible research. \code{hsp2expr} will
#' return a valid chunk of R code that recreates the saved scatterplot settings. This code can be
#' copy-pasted into a RMarkdown document or other report format so the work may be recreated.
#' 
#' @return A chunk of R code that will reproduce the \code{hsp} object 
#'
#' @export
   
hsp2expr <- function(hsp, reg_nodes_prec=3, var_name="hsp") {
    
    ## Check hsp is the correct data type
    err.msg <- "hsp must be a saved scatter plot(s), or a list of objects of class 'locoh.hsp'"
    if (!is.list(hsp)) stop(err.msg)
    if (FALSE %in% (sapply(hsp, function(x) inherits(x, "locoh.hsp")))) stop(err.msg) 

    all_hsp <- "list("
    
    for (i in 1:length(hsp)) {
        this_hsp <- paste("'", names(hsp)[i], "'=list(", sep="")
                      
        elements_numeric <- c("limx", "limy", "val.base", "sat.base", "hue.offset", "cex", "ufat", "xvals.range", "yvals.range")
        for (strElement in elements_numeric) {
            if (!is.null(hsp[[i]][[strElement]])) {
                if (length(hsp[[i]][[strElement]]) == 1) {
                    this_hsp <- paste(this_hsp, strElement, "=", hsp[[i]][[strElement]], ", ", sep="")
                } else {
                    this_hsp <- paste(this_hsp, strElement, "=c(", paste(hsp[[i]][[strElement]], collapse=", "), "), ", sep="")
                }
            }
        }
        
        elements_text <- c("hs.name", "x.axis", "y.axis", "title", "col", "bg", "center.method", "jiggle.x", "jiggle.y", "trans.x", "trans.y")
        for (strElement in elements_text) {
            if (!is.null(hsp[[i]][[strElement]])) {
                this_hsp <- paste(this_hsp, strElement, "=\"", hsp[[i]][[strElement]], "\", ", sep="")
            }
        }
        
        
        ## Do regions
        if (!is.null(hsp[[i]][["regions"]])) {
            all_reg <- "list("
            for (j in 1:length(hsp[[i]][["regions"]])) {
                this_reg <- ""
                this_reg <- paste(this_reg, "list(label=\"", hsp[[i]]$regions[[j]]$label, "\", ", sep="")
                this_reg <- paste(this_reg, "col=\"", hsp[[i]]$regions[[j]]$col, "\", ", sep="")
                this_reg <- paste(this_reg, "poly.pts=data.frame(x=c(", paste(round(hsp[[i]]$regions[[j]]$poly.pts$x, reg_nodes_prec), collapse=","), "), y=c(", paste(round(hsp[[i]]$regions[[j]]$poly.pts$y, reg_nodes_prec), collapse=","), "))), ", sep="")
                all_reg <- paste(all_reg, this_reg, sep="")
            }

            ## Chop off final two characters and add final parenthesis
            all_reg <- paste(substr(all_reg, 0, nchar(all_reg) - 2), ")", sep="")

            # Add to this HSP
            this_hsp <- paste(this_hsp, "regions=", all_reg, ", ", sep="")
        }
                
        ## Do hmap
        if (!is.null(hsp[[i]][["hmap"]])) {
            all_hmap <- "list("
            for (j in 1:length(hsp[[i]][["hmap"]])) {
                this_hmap <- ""
                ## This presumes the hull metric auxillary parameter is numeric
                ## If it is character, we need to put in quotes
                this_hmap <- paste(names(hsp[[i]][["hmap"]])[j], "=", hsp[[i]][["hmap"]][[j]], ", ", sep="")
                all_hmap <- paste(all_hmap, this_hmap, sep="")
            }
            
            ## Chop off final two characters and add final parenthesis
            all_hmap <- paste(substr(all_hmap, 0, nchar(all_hmap) - 2), ")", sep="")
            
            # Add to this HSP
            this_hsp <- paste(this_hsp, "hmap=", all_hmap, ", ", sep="")
        }
        
        
        ## Chop off final two characters and add final parenthesis
        this_hsp <- paste(substr(this_hsp, 0, nchar(this_hsp) - 2), "), ", sep="")
        
        all_hsp <- paste(all_hsp, this_hsp, sep="")
        
    }
    
    ## Chop off final two characters and add final parenthesis
    all_hsp <- paste(substr(all_hsp, 0, nchar(all_hsp) - 2), ")", sep="")
    
    hsp_expr <- paste(var_name, " <- ", all_hsp, "\n", "for (i in 1:length(", var_name, ")) class(", var_name, "[[i]]) <- c(\"locoh.hsp\", \"list\")", sep="")
    
    
    cat(hsp_expr, "\n")
    
    return(invisible(hsp_expr))
    
}
