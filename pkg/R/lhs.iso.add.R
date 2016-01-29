#' Compute isopleths
#'
#' Adds isopleth(s) to a LoCoH-hullset object 
#'
#' @param lhs A LoCoH-hullset object
#' @param id The id(s) of the hullsets to create isopleths for 
#' @param k The k value of hullsets to create isopleths for
#' @param r The r value of hullsets to create isopleths for
#' @param a The a value of hullsets to create isopleths for
#' @param s The s value of hullsets to create isopleths for
#' @param hs.names The name(s) of saved hullsets to create isopleths for
#' @param sort.metric The name of a hull metric that will be used to sort the hulls prior to merging into isopleths
#' @param iso.method The method used to define isopleths. Default is "pt.quantiles" which defines isopleths as containing a quantile of points (e.g., 
#' the 0.1th isopleth contains 10\% of the points). Can also be "hm.vals", in which case the isopleth level represents not a proportion of points enclosed 
#' but a value of the hull metric (e.g., the 0.1 isopleth contains hulls whose hull metric is <= 0.1)
#' @param iso.levels A numeric vector of the levels of the isopleths. See details.
#' @param iso.cap.method A character object specifying how isopleths defined by proportions of enclosed points 
#' will be 'capped'. Ignored when \code{iso.method="hm.vals"}. See details.
#' @param scale.iso.levels.to.hm.vals Whether to linearly scale iso.levels (presumed to be 0..1) to the minimum 
#' and maximum of the sort.metric value. This is used in conjunction iso.method='hm.vals', to produce 
#' isopleths as the aggregation of hulls whose sort.metric value is >= i\% of the maximum, where i is the isopleth level. T/F
#' @param subset.metric The name of a hull metric that will be used to create subsets of isopleths
#' @param subset.vals A two-column data frame (or matrix) containing the lower and upper bounds of the hull metric in subset.metric, 
#' from which subsets of hulls will be extract for separate isopleth construction. Ignored if subset.metric is not provided. 
#' Can also be a character string in the form of "equal intervals, n", or "quantitles, n" where n is the number of strata desired
#' (shorthand versions of these commands are "ei,n" and "q,n" where n is a number). The script will compute the break points either 
#' spread equally between the minimum and maximum subset metric value (equal interval) or so an equal number of hulls is in each strata
#' @param allow.gpc Allow functions from the gpclib package if the functions from rgeos fail. See details. T/F
#' @param sliver_check Whether to check for and delete slivers in isopleths. See details. T/F
#' @param beep Beep when done. T/F
#' @param status Show status messages. T/F
#' @param ... Additional auxillary parameters for the hull sort metric 
#'
#' @details
#' This function creates isopleth(s) for a LoCoH-hullset object. This involves sorting hulls by one of the hull metrics, 
#' and then cummulatively unioning them together until a certain level is reached at which point the union is saved as an isopleth. By default, 
#' hulls will be sorted according to the area (for k-method) or number of enclosed points (r- and a-methods), producing density isopleths
#' (i.e., utilization distributions).
#'
#' When \code{iso.method=="pt.quantiles"} (the default), isopleths will be defined by the proportion of points they enclose (given in
#' \code{iso.levels}). Thus for example
#' the 50\% isopleth contains 50\% of the total points. The default isopleth levels are 0.15, 0.25, ..., 0.95. To get an isopleth that encloses all 
#' points in the dataset, \code{iso.levels} must include '1'. 
#'
#' When \code{iso.method=="hm.vals"}, isopleths will be defined by the value of the hull metrics (provided in \code{iso.levels}). So if for example \code{sort.metric="scg.nn.mean"} 
#' (average speed of all points identified as nearest neighbors), and \code{scg.nn.mean} varied from 0.01 to 3.2, the 0.6 isopleth would be the 
#' union of all hulls whose average nearest neighbor speed was 0.6 or less. If you would like hulls to be identified by hull metric values, 
#' but don't know the range of hull metric values, you can set isopleth levels between 0..1 and set \code{scale.iso.levels.to.hm.vals=TRUE}.
#'
#' By default, isopleths defined by quantiles of enclosed points are 'capped' as the smallest number of progressively unioned hulls that enclose a number of points equal 
#' to or greater than the isopleth level. For example, if there are 1000 points in the dataset, the 50\% isopleth would be the smallest union
#' of hulls that encloses 500 or more points. Alternately, when \code{iso.cap.method = "<-"}, the isopleth will be the largest number of 
#' hulls which enclose up to but not more than the isopleth level. The actual number of points enclosed by each isopleth is included in the
#' data table for the isopleth (which you can view by setting \code{iso.details=TRUE} when calling \code{\link{summary.locoh.lhs}}).
#' 
#' The hull metric used for sorting must already have been computed. Several hull metrics (e.g., hull area, number of enclosed points) are 
#' 'automatically' computed when a hullset is initially generated (see \code{\link{lxy.lhs}}). Other hull metrics must 
#' be created separately with functions such as \code{\link{lhs.ellipses.add}} and \code{\link{lhs.visit.add}}.
#' You can use the \code{\link{summary.locoh.lhs}} function to see which isopleths have been saved in a LoCoH-hullset object. 
#' 
#' Hulls are unioned using the gUnion and gUnaryUnion functions from the rgeos package, which in general is very fast. These functions occasionally fail when lines are too close together or there are other topological conditions.
#' This is not a problem with the data, but an inherent limitation of the algorithms / processing. If \code{allow.gpc=TRUE}, functions from the gpclib package will 
#' be used as backup. If \code{sliver_check=TRUE}, isopleths will checked for 'slivers'. Slivers (as used here) refer to polygons or holes with <3 unique nodes. 
#' Slivers can be caused by reductions in numeric precision or rounding errors, and are not uncommon.
#' 
#' @return A \link{LoCoH-hullset} object
#'
#' @seealso \code{\link{isopleths}}
#'
#' @examples
#' # Create 0.5 and 0.95 isopleths. By not specifying the sort.metric, 
#' # density isopleths will be created by default, with hulls sorted 
#' # by area (k-method) or number of enclosed points (r and a method)
#'
#' # lhs <- lhs.iso.add(lhs, iso.levels=c(0.5, 0.95))
#'
#' # Compute hull metrics for a 24 hour inter-visit gap, then produce isopleths with hulls sorted by
#' # the number of separate visits (e.g., visitation)
#' # lhs <- lhs.visit.add(lhs, ivg=3600.24)
#' # lhs <- lhs.iso.add(lhs, sort.metric="nsv", ivg=3600*24)
#'
#' @export

lhs.iso.add <- function(lhs, id=NULL, k=NULL, r=NULL, a=NULL, s=NULL, hs.names = NULL, 
                        sort.metric=c("auto", hm.expr(names.only=T, desc=F, print=F))[1], 
                        iso.levels=c(0.1,0.25,0.5,0.75,0.95), iso.method=c("pt.quantiles", "hm.vals")[1], 
                        iso.cap.method=c(">=", "<=")[1], 
                        scale.iso.levels.to.hm.vals=(iso.method=="hm.vals" && max(iso.levels <= 1)), 
                        subset.metric=NULL, subset.vals=NULL, allow.gpc=TRUE, sliver_check=TRUE, beep=FALSE, status=TRUE, ...) {

    # taken out @param ivg Value(s) for inter-visit gap, required if the sort metric is a time-use metric (e.g., visitation). 
    # An inter-visit gap is the period of time (in second) which must pass before another occurrence 
    # of the individual in the hull is considered a separate visit.
     
    if (!inherits(lhs, "locoh.lhs")) stop("lhs should be of class \"locoh.lhs\"")
    if (anyDuplicated(iso.levels)) stop("Duplicate iso.levels detected")

    start.time <- Sys.time()
    if (is.null(id) && is.null(r) && is.null(k) && is.null(a) && is.null(s) && is.null(hs.names)) {
        hs <- lhs
    } else {    
        hs <- lhs.select(lhs, id=id, r=r, k=k, a=a, s=s, hs.names=hs.names)
    }
    if (length(hs)==0) stop("No hullsets found matching those criteria")
    rm(lhs)
    
    ## Create a collection of the name(s) of run(s) to include
    if (is.null(hs.names)) hs.names <- names(hs)

    if (is.null(iso.levels)) stop("iso.levels is a required parameter")
    if (length(sort.metric) != 1) stop("Please provide a single value for sort.metric")

    ## Get the list of hull metric expressions
    hme <- hm.expr(names.only=FALSE, desc=FALSE, print=FALSE)

    ## Preliminary error checks for subset.metric
    if (!is.null(subset.metric)) {
        if (!subset.metric %in% names(hme)) stop(cw(paste("subset.metric must be one of the following: ", paste(names(hme), collapse=", ", sep=""), sep="")))
        if (is.null(subset.vals)) stop(cw(paste("You need to provide a value for subset.vals in order to subset by ", subset.metric, ". subset.vals can either be a two-column matrix containing the lower and upper bounds for each strata of ", subset.metric, ", or a string of the form 'equal interval,n' or 'quantiles,n', where n is the desired number of strata.", sep="")))
        
        #the check for the provision of hmap parameters for subset.metric has been moved below

        ## Parse and error-check subset.vals
        err.msg <- cw("Unknown format for subset.vals. Try 'equal intervals, n' or 'quantiles, n', where n=number of desired strata", final.cr=F)
        if (is.character(subset.vals)) {
            if (length(subset.vals) == 1) {
                subset.vals.tokens <- strTrim(strsplit(subset.vals, ",", fixed = TRUE)[[1]])
                if (length(subset.vals.tokens) != 2) stop(err.msg)
                if (substr(subset.vals.tokens[1], 1, 14) %in% c("equal interval","ei")) {
                    method <- "ei"
                } else if (substr(subset.vals.tokens[1], 1, 8) %in% c("quantile","q")) {
                    method <- "q"
                } else {
                    stop(err.msg)                           
                }
                if (length(grep("[^0-9]", subset.vals.tokens[2], value=TRUE)) == 0) {
                    num.strata <- as.integer(subset.vals.tokens[2])
                } else {
                    stop(err.msg)                           
                }
                subset.vals <- list(method=method, num.strata=num.strata)
            } else {
                stop(err.msg)
            }
             
        } else {
            ## Consider checking upper and lower bound of setset.vals
            if (FALSE %in% (subset.vals[,1] < subset.vals[,2])) stop(paste("The lower bound(s) for subset.vals must be less than the upper bound(s)", sep=""))
            ## names(subset.vals) <- c("lbound", "ubound")
        } 
    }

    ## Error check iso.method
    if (!iso.method %in% c("pt.quantiles", "hm.vals")) stop("iso.method must be 'pt.quantiles' or 'hm.vals'")
    if (iso.method == "pt.quantiles" && (min(iso.levels) < 0 || max(iso.levels) > 1)) stop("iso.levels must be between 0 and 1")
    if (scale.iso.levels.to.hm.vals && (min(iso.levels) < 0 || max(iso.levels) > 1)) stop("if scale.iso.levels.to.hm.vals=TRUE, then iso.levels must be between 0 and 1")
    
    ## Create a list object that will be used below for constructing the iso list element name
    iso.method.fn.token.lst <- list(pt.quantiles=".iso-q", hm.vals=".iso-hmv")

    ddd.lst <- list(...)

    if (status) {
        cat("Merging hulls into isopleths\n")
        #if (.Platform$OS.type == "windows") cat(cw("If R crashes, try running this under the 64-bit version of R (or an earlier version, see help for details)", final.cr=T))
        flush.console()
    }

    ## Start big nested loop
    ##for (hs.name in names(hs)[hs.matching.idx]) {
    
    for (hs.name in hs.names) {

        #hs.mode.var <- hs[[hs.name]][["mode"]]
        if (sort.metric=="auto") {
            if (hs[[hs.name]][["mode"]]=="k") {
                sort.metric.use <- "area"
            } else {
                sort.metric.use <- "nep"        
            }
        } else {
            sort.metric.use <- sort.metric
        }
        
        if (!sort.metric.use %in% names(hme)) stop(cw(paste("sort.metric must be one of the following: ", paste(names(hme), collapse=", ", sep=""), sep="")))
    
        ## Make sure all required hull metric auxillary parameters (hmap) were passed to the function 
        ## and set up a data frame containing all permutation(s) of the hull metric auxillary parameters (hmap) passed 
        avparams.lst <- list()
        for (hull.metric in c(sort.metric.use, subset.metric)) {
            for (avparam in hme[[hull.metric]][["req.ap"]]) {
                if (!avparam %in% names(ddd.lst)) {
                    if (is.null(hme[[hull.metric]][["req.ap.def"]])) {
                        stop(paste("Required parameter missing: ", avparam, sep=""))                        
                    } else if (hme[[hull.metric]][["req.ap.def"]]=="all") {
                        param.vals.use <- hs[[hs.name]][["hm.params"]][[avparam]]
                    } else {
                        param.vals.use <- hme[[hull.metric]][["req.ap.def"]]
                    }
                } else {
                    param.vals.use <- ddd.lst[[avparam]]
                }
                
                # Make sure these values have been actually computed
                if (FALSE %in% (param.vals.use %in%  hs[[hs.name]][["hm.params"]][[avparam]])) {
                    fun.str <- if (is.null(hme[[hull.metric]][["fun"]])) "" else paste(" This metric may be created using the function ", hme[[hull.metric]][["fun"]], ".", sep="")                        
                    stop(cw(paste("Value of ", avparam, " not found.", fun.str, sep=""), final.cr=FALSE))
                }

                ## Add to the list
                avparams.lst[[avparam]] <- unique(c(avparams.lst[[avparam]], param.vals.use))  
            }
        }
        if (length(avparams.lst) > 0) {
            ## Convert the list of auxillary variables into a data frame containing all permutations of the variables
            hmap <- expand.grid(avparams.lst, stringsAsFactors=FALSE)
        } else {
            hmap <- as.data.frame(NA)        
        }

        for (hmap.idx in 1:nrow(hmap)) {
        
            ## Define subset values dataframe (ssv.df)
            if (is.null(subset.metric)) {
                ssv.df <- as.data.frame(NA)
            } else {
                ## Grab the values for subsetting
                hm.vals.subset <- eval(hme[[subset.metric]][["expr"]])
    
                ## If subset.vals is one of the 'auto' formats, construct ss.vals.df
                if (is.list(subset.vals)) {
                    n <- subset.vals$num.strata
                    if (subset.vals$method == "ei") {
                        subset.vals.seq <- seq(from=min(hm.vals.subset), to=max(hm.vals.subset), length.out=n+1)
                    } else {
                        subset.vals.seq <- quantile(hm.vals.subset, prob=seq(from=0, to=1, length.out=n+1))
                    }
                    ssv.df <- data.frame(lbound=subset.vals.seq[1:n], ubound=subset.vals.seq[2:(n+1)])
    
                    ## Add a little bit to the very last ubound so we don't miss any of those
                    ssv.df[nrow(ssv.df),2] <- ssv.df[nrow(ssv.df),2] + 0.000001
                    
                } else {
                    ## Presume the user passed a data frame for subset.vals
                    ssv.df <- subset.vals
                }
            }
            
            ## Pull out subset of hulls
            for (ssv.df.rnum in 1:nrow(ssv.df)) {
                if (is.null(subset.metric)) {
                    hulls2merge.idx <- 1:length(hs[[hs.name]][["hulls"]])        
                } else {
                    hulls2merge.idx <- which(hm.vals.subset >= ssv.df[ssv.df.rnum, 1] & hm.vals.subset < ssv.df[ssv.df.rnum, 2])
                    if (length(hulls2merge.idx)==0) cat(cw(paste(" - no hulls found with ", subset.metric, " between ",  ssv.df[ssv.df.rnum, 1],
                                                                 " and ",  ssv.df[ssv.df.rnum, 2], ". Skipping.", sep=""), indent=1, exdent=2, final.cr=T))
                }
                if (length(hulls2merge.idx) > 0) {

                    ## Get the hull metric values for sort.metric.use
                    hm.vals <- eval(hme[[sort.metric.use]][["expr"]])[hulls2merge.idx]
                    
                    ## Prepare a helpful error message
                    if (is.null(hm.vals)) {
                        if (is.null(hme[[sort.metric.use]][["fun"]])) {
                            hm_fun <- ""
                        } else {
                            hm_fun <- paste(" See also ", hme[[sort.metric.use]][["fun"]], ".", sep="")
                        }
                        stop(cw(paste("No values found for hull metric '", sort.metric.use, "'. Make sure values for this hull metric have been computed.", hm_fun, sep=""), exdent=2))
                    }

                    #hm.vals.ord.orig <- order(hm.vals)

                    ## If there is a second hull metric defined as a 'tie-breaker', sort on both fields
                    if (is.null(hme[[sort.metric.use]][["iso.hm2"]])) {
                        hm.vals.ord <- order(hm.vals)
                        str.ties <- NULL
                    } else {
                        str.ties <- paste(" with ties broken by ", hme[[sort.metric.use]][["iso.hm2"]], if (hme[[sort.metric.use]][["iso.hm2.dec"]]) " descending" else NULL, sep="")
                        hm2 <- eval(hme[[   hme[[sort.metric.use]][["iso.hm2"]]   ]][["expr"]])[hulls2merge.idx]
                        if (hme[[sort.metric.use]][["iso.dec"]] != hme[[sort.metric.use]][["iso.hm2.dec"]] ) hm2 <- -hm2
                        hm.vals.ord <- order(hm.vals, hm2)
                    }

                    ## Show message about hull sorting done
                    if (status) cat(hs.name, "\n  Sorting hulls by ", sort.metric.use, if (is.na(hmap[hmap.idx,1])) "" else paste(".", sapply(1:length(hmap), function(j) paste(names(hmap)[j], ".", hmap[hmap.idx,j], sep="")), collapse=".", sep=""), if (hme[[sort.metric.use]][["iso.dec"]]) " descending" else NULL, str.ties, "...", sep="")
                    hulls2merge.idx.srt <- hulls2merge.idx[hm.vals.ord]
                    if (status) cat("Done. \n", sep="")

                    ## Assign iso.levels.use to either iso.levels (default) or possibly scaled to min(hm.vals)..max(hm.vals)
                    iso.levels <- sort(iso.levels)
                    if (iso.method == "pt.quantiles") {
                        iso.levels.use <- iso.levels
                    } else if (iso.method == "hm.vals") {
                        if (scale.iso.levels.to.hm.vals) {
                            iso.levels.use <- min(hm.vals) + iso.levels * (max(hm.vals) - min(hm.vals))
                        } else {
                            iso.levels.use <- iso.levels
                        }
                        
                        ## Sort iso.levels.use increasing or decreasing
                        ## iso.levels.use <- sort(iso.levels.use, decreasing=hme[[sort.metric.use]][["iso.dec"]])
                    } 

                    ## Create a descriptive comment that will be used as caption or footnote text for plots of the isopleths 
                    iso.desc <- paste(if (length(iso.levels)==1) "This isopleth was " else "These isopleths were ", "constructed from ",
                                         if (is.null(subset.metric)) "" else "a subset of ", length(hulls2merge.idx), " ",
                                         if (is.null(subset.metric)) "" else paste("out of ", length(hs[[hs.name]][["hulls"]]), " ", sep=""), "hulls",
                                         if (is.null(subset.metric)) "" else paste(" (", if (is.list(subset.vals)) paste(subset.vals$method, " ", sprintf("%02d", ssv.df.rnum), " of ", nrow(ssv.df), ", ", sep="") else "",
                                                                                   signif(as.numeric(ssv.df[ssv.df.rnum, 1]), digits=digits), 
                                                                                   " < ", subset.metric, " < ",  
                                                                                   signif(as.numeric(ssv.df[ssv.df.rnum, 2]), digits=digits), ")", sep=""),
                                                                                   " sorted", if (hme[[sort.metric.use]][["iso.dec"]]) " descending" else NULL,
                                                                                   " by ", hme[[sort.metric.use]]$ufat, ". ",
                                                                                   if ("ivg" %in% names(hmap)) paste("Separate visits defined by an inter-visit gap period >= ", hmap[hmap.idx, "ivg"], " seconds (", secs.fmt(hmap[hmap.idx, "ivg"]), "). ", sep="") else "",
                                                                                   "Isopleth levels ", 
                                                                                   if (iso.method == "pt.quantiles") "indicate the proportion of total points enclosed" else 
                                                                                   paste("specify the range of hull values for ", sort.metric.use, sep=""), 
                                                                                   ".", sep="")
                                                                                   

                    ## Create a user-friendly plot title
                    iso.ufipt <- hme[[sort.metric.use]][["ufipt"]]
                      
                    if (status) {
                        cat("  Unioning hulls \n")
                        flush.console()
                    }
                    
                    #decreasing <- hme[[sort.metric.use]][["iso.dec"]] 

                    ##Pass to hulls2iso 
                    ## - hm.vals sorted (increasing)
                    ## - hulls sorted by hm.vals (increasing)
                    ## - enc.pts also sorted by hm.vals (increasing)
                    ## - iso.levels (also sorted increasing)
                    ## - total number of points (for computing isopleth quantile levels)
                    ## - a flag 'decreasing' for whether larger values mean more density
                    
                    polys.spdf <- hulls2iso.rgeos(hulls=hs[[hs.name]][["hulls"]][hulls2merge.idx.srt,], 
                                               points.lst=hs[[hs.name]][["enc.pts"]][["idx"]][hulls2merge.idx.srt], iso.levels=iso.levels.use, 
                                               iso.method=iso.method, hm.vals=hm.vals[hm.vals.ord], decreasing=hme[[sort.metric.use]][["iso.dec"]], 
                                               iso.cap.method=iso.cap.method, total.num.points=length(hs[[hs.name]][["pts"]]), hs.name=hs.name, 
                                               sliver_check=sliver_check, status=status)

#                    polys.spdf <- get("hulls2iso.rgeos", 1)(hulls=hs[[hs.name]][["hulls"]][hulls2merge.idx.srt,], 
#                                               points.lst=hs[[hs.name]][["enc.pts"]][["idx"]][hulls2merge.idx.srt], iso.levels=iso.levels.use, 
#                                               iso.method=iso.method, hm.vals=hm.vals[hm.vals.ord], decreasing=hme[[sort.metric.use]][["iso.dec"]], 
#                                               iso.cap.method=iso.cap.method, total.num.points=length(hs[[hs.name]][["pts"]]), hs.name=hs.name, 
#                                               sliver_check=sliver_check, status=status)
#                    
                    if (is.null(polys.spdf)) {
                        if (status) cat("  Not enough hulls to make isopleths!!! \n")
                    } else {
                        if (identical(polys.spdf, "error")) {
                            if (allow.gpc) {
                                if (!requireNamespace("gpclib")) stop(cw("rgeos can not handle this polygon union. We need to load the 'gpclib' package but it isn't installed. Please install gpclib and try again. Windows users must install gpclib from source (which requires installing RTools first, see http://tlocoh.r-forge.r-project.org/manual_install.html for details).", final.cr=F))
                                if (status) cat("  Challenging geometry...switching to gpclib  \n")
                                polys.spdf <- hulls2iso.gpc(hulls=hs[[hs.name]][["hulls"]][hulls2merge.idx.srt,], 
                                                   points.lst=hs[[hs.name]][["enc.pts"]][["idx"]][hulls2merge.idx.srt], iso.levels=iso.levels.use, 
                                                   iso.method=iso.method, hm.vals=hm.vals[hm.vals.ord], decreasing=hme[[sort.metric.use]][["iso.dec"]], 
                                                   iso.cap.method=iso.cap.method, total.num.points=length(hs[[hs.name]][["pts"]]), hs.name=hs.name, 
                                                   sliver_check=sliver_check, status=status)
                            } else {
                                if (status) cat("  Union error.  \n")
                            }
                        }
                        rm(hm.vals)
                        
                        if (is(polys.spdf, "SpatialPolygonsDataFrame")) {
                        
                            ## Pull out the hull metric auxilary variables that were used in this isopleth
                            if (is.na(hmap[hmap.idx,1])) {
                                hmap.used <- NULL
                            } else {
                                hmap.used <- as.list(hmap[hmap.idx, , drop=F])
                                attr(hmap.used, "out.attrs") <- NULL
                            }
                            
                            ## Create an empty list if the $isos element doesn't exist yet
                            if (is.null(hs[[hs.name]][["isos"]])) hs[[hs.name]][["isos"]] <- list()
                            
                            ## Put together a name for this group of isopleth
                            digits <- 3                                                                                                         
                            iso.name <- paste("iso.srt-", sort.metric.use, 
                                      if (is.na(hmap[hmap.idx,1])) "" else paste(".", sapply(1:length(hmap), function(j) paste(names(hmap)[j], ".", hmap[hmap.idx,j], sep="")), collapse=".", sep=""),
                                      iso.method.fn.token.lst[[iso.method]],
                                      if (is.null(subset.metric)) "" else paste(".ss-", subset.metric, if (is.list(subset.vals)) paste("-", subset.vals$method, sprintf("%02d", ssv.df.rnum), "of", nrow(ssv.df), sep="") else "", "-", paste(signif(as.numeric(ssv.df[ssv.df.rnum, ]), digits=digits), collapse="-", sep=""), sep=""), 
                                      ".h", length(hulls2merge.idx), ".i", length(iso.levels), sep="")
                    
                            
                            ## Save this isopleth
                            hs[[hs.name]]$isos[[iso.name]] <- list(ufipt=iso.ufipt, desc=iso.desc,
                                            iso.method=iso.method, sort.metric=sort.metric.use, 
                                            hmap=hmap.used, polys=polys.spdf, 
                                            subset.metric=subset.metric,
                                            subset.vals=if (is.null(subset.metric)) NULL else as.numeric(ssv.df[ssv.df.rnum, ]),
                                            subset.vals.fr=if (is.null(subset.metric)) NULL else range(ssv.df))
                        }
                    }
                    
                                                                                                    
                }
            }
    
        }
            
    }
    
    if (status) {
        time.taken = difftime(Sys.time(), start.time, units="auto")
        cat("Total time:", round(time.taken,1), units(time.taken), "\n", sep = " ")    
    }
    
    if (beep) {
        flush.console()
        for (i in 1:3) {
            alarm()
            Sys.sleep(0.8)
        }
    }
    return(hs)  

}
