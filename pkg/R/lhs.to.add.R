#' Add hull metrics for association analysis
#'
#' @param lhs A LoCoH-hullset object
#' @param save.hto Whether to save the list of hull indices that temporally overlap, T/F
#' @param maxdt The maximum difference in time (in seconds) for two hulls to be considered 'overlapping' in time. Can also be \code{auto},
#' in which case half ot the smallest median sampling interval will be used.
#'
#' @return A LoCoH-hullset object
#'
#' @export


lhs.to.add <- function(lhs, id="all", hs2.id="all", maxdt="auto", save.hto=TRUE, status=TRUE ) {

    if (!inherits(lhs, "locoh.lhs")) stop("lhs should be of class \"locoh.lhs\"")
    if (!require(sp)) stop("package sp required")
    if (!require(pbapply)) stop("package pbapply required")
    if (!is.null(lhs[["xys"]])) stop("Old lhs data structure detected")
    
    start.time <- Sys.time()
    if (status) cat(" - start time: ", as.character(start.time), "\n", sep = "")
    
    ## Make a data frame of all of the hullsets and their properties
    lhs.info <- do.call(rbind, lapply(lhs, function(x) data.frame(id=x$id, s=x$s, k=n2z(x$k), a=n2z(x$a), r=n2z(x$r), stringsAsFactors=FALSE)))
    id.all <- unique(lhs.info[["id"]])
    if (length(id.all)==1) stop("This hull metric requires a hullset collection with more than one ID")

    if (identical(id, "all")) id <- id.all
    if (identical(hs2.id, "all")) hs2.id <- id.all
        
    for (idVal in id) {
        ## Save the indices of the hullset(s) which have this id
        hsi.idVal1 <- which(lhs.info[["id"]] == idVal)
        
        for (h1 in hsi.idVal1) {
            if (status) cat(names(lhs)[h1], "\n");flush.console()
            
            if (save.hto) {if (is.null(lhs[[h1]][["hto"]])) lhs[[h1]][["hto"]] <- list()}
            h1.tau <- lhs[[h1]][["rw.params"]][["time.step.median"]][1]
            
            ## Get the time stamp of each hull and convert to the num of seconds since 1970-01-01
            h1.pp.dt.int <- as.integer(lhs[[h1]][["pts"]][["dt"]][lhs[[h1]][["hulls"]][["pts.idx"]]])

            ## Get centroid of each hull saved as a xy matrix
            h1.ctr <- t(sapply(lhs[[h1]][["hulls"]]@polygons, function(x) apply(x@Polygons[[1]]@coords,2,mean)))

            for (idVal2 in hs2.id) {
                if (idVal2 != idVal) {
                    hsi.idVal2 <- which(lhs.info[["id"]] == idVal2)
                    
                    for (h2 in hsi.idVal2) {                    
                        if (status) cat(" - finding temporal overlaps with ", names(lhs)[h2], "\n", sep=""); flush.console()
                        
                        ## Finalize maxdt.use
                        if (identical(maxdt, "auto")) {
                            h2.tau <- lhs[[h2]][["rw.params"]][["time.step.median"]][1]
                            maxdt.use <- min(h1.tau, h2.tau) / 2
                        } else {
                            maxdt.use <- maxdt
                        }
                        
                        ## Get the time stamp of each point and convert to seconds
                        h2.pp.dt.int <- as.integer(lhs[[h2]][["pts"]][["dt"]][lhs[[h2]][["hulls"]][["pts.idx"]]])

                        ## Create a xy matrix of the centroids
                        h2.ctr <- t(sapply(lhs[[h2]][["hulls"]]@polygons, function(x) apply(x@Polygons[[1]]@coords,2,mean)))
                        
                        ## Identify the hulls that overlap temporally within maxdt.use
                        ## This is not the most efficient way to identify temporal overlap but it works
                        ## might be a way to use FindInterval 
                        to.lst <- lapply(h1.pp.dt.int, function(x) which(abs(h2.pp.dt.int - x) <= maxdt.use)) 
                        
                        ## Compute the centroid distances
                        to.c2c.dist.lst <- lapply(1:length(to.lst), function(i) sqrt((h1.ctr[i,1]- h2.ctr[to.lst[[i]],1])^2 + (h1.ctr[i,2]- h2.ctr[to.lst[[i]],2])^2))
                        
                        ## Compute the mean centroid distance which will be the metric
                        to.mcd <- sapply(to.c2c.dist.lst, mean)
                        
                        ## Change NaN to NA
                        to.mcd[is.nan(to.mcd)] <- NA

                        ## Construct a name for hullset 2 that will be used as part of the name for hull metric
                        hs2.name <- paste(idVal2, ".s", lhs[[h2]][["s"]], ".", lhs[[h2]][["mode"]], lhs[[h2]][[  lhs[[h2]][["mode"]]  ]], sep="")

                        ## Add an item to hm containing the meta data for to.mcd
                        to.mcd.name <- paste("to.mcd.", hs2.name, sep="")
                        lhs[[h1]][["hm"]][[to.mcd.name]] <- list(type="to.mcd", aux=list(hs2.id=idVal2, hs2.s=lhs[[h2]][["s"]], hs2.k=lhs[[h2]][["k"]], hs2.a=lhs[[h2]][["a"]], hs2.r=lhs[[h2]][["r"]], maxdt=maxdt.use))

                        ## Add the hull metric to the SpatialPolygonsDataFrame
                        lhs[[h1]][["hulls"]]@data[[to.mcd.name]] <- to.mcd

                        ## Add hs2 to the hm.params list
                        if (is.null(lhs[[h1]][["hm.params"]])) lhs[[h1]][["hm.params"]] <- list()
                        lhs[[h1]][["hm.params"]][["hs2"]] <- unique(c(lhs[[h1]][["hm.params"]][["hs2"]], hs2.name))

                        ## Save the temporal overlap list
                        if (save.hto) {
                            lhs[[h1]][["hto"]][[names(lhs)[h2]]] <- list(id=idVal2, s=lhs[[h2]][["s"]], mode=lhs[[h2]][["mode"]], k=n2z(lhs[[h2]][["k"]]), a=n2z(lhs[[h2]][["a"]]), r=n2z(lhs[[h2]][["r"]]), to.lst=to.lst, maxdt=maxdt.use)
                        }

                    }
                    
                    
                }
            }
        }
    }
    
    print("Still need to save hs2 to hm.params - maybe hs.name?")
    
    if (status) {
        time.taken = difftime(Sys.time(), start.time, units="auto")
        cat("Total time:", round(time.taken,1), units(time.taken), "\n", sep = " ")
    }

    return(lhs)

}
