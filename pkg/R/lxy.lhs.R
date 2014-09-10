#' Create a LoCoH-hullset object from a LoCoH-xy object
#'
#' @description Creates a LoCoH-hullset object (class "locoh.lhs") containing one or more sets of hulls.
#'
#' @param lxy A \link{LoCoH-xy} object
#' @param id The id value(s) of \code{lxy} for which hullsets will be created. If \code{NULL} all ids will be used.
#' @param s Value(s) for the s term in the time-scaled-distance equation for point-to-point distance. When \code{s=0} time is excluded and TSD is equivalent to Euclidean distance. Numeric vector or comma-separated string. Can also be 'all'
#' @param a Value(s) for the adaptive method. Numeric vector or comma-separated string. \code{a} also be the results of the \code{\link{auto.a}} function, see notes.
#' @param r Value(s) for the r method. Numeric vector or comma separate list of values.
#' @param k Value(s) for the k method. Numeric vector or comma separate list of values.
#' @param kmin A minimum number of nearest neighbors for each parent point regardless of the method used. Set \code{k=2} to ensure that every point is part of a hull. Integer.
#' @param decimal.places The number of decimal places for the k/r/a parameter value used when constructing the name of a hullset. Note this has no effect on hull creation or the precision of values used, only the number of digits that appear in the hullset name.
#' @param offset.dups A number of map units to randomly offset duplicate points. Set \code{offset.dups=0} to ignore duplicate points
#' @param anv.copy Copy the ancillary variables data frame (if exists), T/F
#' @param velocity.metrics Compute the velocity hull metrics
#' @param ud Deprecated (no longer used). Use \code{iso.add} instead
#' @param iso.add Whether to also create density isopleths, T/F
#' @param iso.levels Isopleth levels (see also \code{\link{lhs.iso.add}}), numeric vector. Ignored if |code{iso.add=FALSE}.
#' @param pbo.style Progress bar style (see pbapply package) 
#' @param beep Beep when done. T/F
#' @param status Show messages. T/F
#' @param save.hulls Whether to save the hulls. T/F
#' @param save.enc.pts Whether to save the enclosed points. T/F
#'
#' @note
#' This function creates a \link{LoCoH-hullset} object from a \link{LoCoH-xy} object. Other functions allow you to do things with 
#' hullsets, including computing additional hull metrics, constructing isopleths, directional routes, generating scatterplots, exporting, etc.
#' Note that before you use this function, nearest neighbors must have already been identified and saved in the 
#' input \link{LoCoH-xy} object (see \code{\link{lxy.nn.add}}).
#'
#' If \code{iso.add=TRUE}, after the hulls are created the function will create density isopleths using the default 
#' sort order (area for \emph{k-method}, number of enclosed points for the \emph{a} and \emph{r-methods}). You can 
#' control which isopleth levels are created using the \code{iso.levels} argument.
#'
#' When working with a large dataset where memory limts may affect performance, you can choose to not save the hulls or enclosed points by 
#' setting \code{save.hulls=FALSE} and \code{save.enc.pts=FALSE}. See the vignette on working with large datasets.
#'
#' @return An object of class \link{locoh.lhs}
#'
#' @seealso \code{\link{xyt.lxy}} for creating LoCoH-xy objects
#' \code{\link{lhs.iso.add}} for adding isopleths
#' \code{\link{lhs.ellipses.add}} for computing bounding ellipses
#' \code{\link{lhs.visit.add}} for computing time-use metrics
#' \code{\link{lhs.plot.scatter}} for creating scatterplots of hull metrics
#' \code{\link{plot.locoh.lhs}} for plotting a LoCoH-hullset
#' \code{\link{hulls}} for extracting hulls as a SpatialPolygonsDataFrame
#' \code{\link{lhs.exp.shp}} to export hulls as shapefile
#'
#' @export
#' @import pbapply sp

lxy.lhs <- function (lxy, id=NULL, s=0, a=NULL, r=NULL, k=NULL, kmin=0, anv.copy=TRUE, 
                    decimal.places=1, offset.dups=1, velocity.metrics=TRUE,
                    ud=NULL, iso.add=FALSE, iso.levels=c(0.1,0.25,0.5,0.75,0.95), pbo.style=3, beep=FALSE, status=TRUE, 
                    save.hulls=TRUE, save.enc.pts=TRUE) {
    
    if (!inherits(lxy, "locoh.lxy")) stop("lxy should be of class \"locoh.lxy\"")
    if (is.null(lxy[["pts"]])) stop("Old data structure")
    if (!is.null(ud)) stop("The 'ud' argument is no longer used. Use 'iso.add' instead")

    ###########################################################################
    ## MAKE SURE A VALID COMBO OF PARAMETERS WAS PASSED
    ###########################################################################

    ## Identify which method to use depending on which parameters were passed
    
    if (is.null(a) && is.null(r) && !is.null(k)) {
        mode <- "Fixed-k"
        str.param <- "k"
    } else if (is.null(a) && !is.null(r) && is.null(k)) {
        mode <- "Fixed-r"
        str.param <- "r"
    } else if (!is.null(a) && is.null(r) && is.null(k)) {
        str.param <- "a"
        if (is.data.frame(a)) {
            mode <- "Auto-a"
        } else if (is.numeric(a)) {
            mode <- "Fixed-a"
        } else {
            stop("a must be a numeric vector or data frame")
        }
    } else {
        stop(cw(exdent=2, "Could not determine what algorithm to use. Make sure to pass in only one of the following parameters: k (fixed-k), r (fixed-r), or a (fixed-a, auto-a)"))
    }
    
    if (is.null(id)) {
        id <- levels(lxy[["pts"]][["id"]])
    } else {
        if (FALSE %in% (id %in% levels(lxy[["pts"]][["id"]]))) stop(strwrap(paste("Can't find the following ID(s) in lxy: ", paste(id[!(id %in% levels(lxy[["pts"]][["id"]]))], collapse=", ", sep=""), sep="")))
    }
   
   if (identical(s, "all")) s <- sort(unique(sapply(lxy[["nn"]], function(x) x[["s"]])))
   
    ## Convert string objects to vectors if needed
    k <- vectorize.parameter(k, n2z=TRUE)
    r <- vectorize.parameter(r, n2z=TRUE)
    if (mode != "Auto-a") a <- vectorize.parameter(a, n2z=TRUE)
    s <- vectorize.parameter(s)
    
    if (max(s) > 0 && is.null(lxy[["pts"]][["dt"]])) stop("Can't set s > 0 because no date stamps were found in lxy")
    
    ###########################################################################
    ## RANGE CHECKS
    ###########################################################################

    ## Check to make sure k, r, or a is in a valid range
    if (mode == "Fixed-k") {
        min.points <- min(sapply(levels(factor(lxy[["pts"]][["id"]])), function(x) length(which(lxy[["pts"]][["id"]] == x))))
        if (TRUE %in% (k > min.points)) stop(paste("too large a number for k, you can't have more k's than you have points (", min.points, ")", sep = ""))               
        if (TRUE %in% (k < 2)) stop("too small a number for k (minimum value is 2 because you need 3 points to make a triangle)")
    } else if (mode == "Fixed-r") { 
        if (TRUE %in% (r <= 0)) stop("r must be a positive number")
    } else if (mode == "Fixed-a") { 
        if (TRUE %in% (a <= 0)) stop("a must be a positive number")
    } else if (mode == "Auto-a") { 
        auto.a.names <- c("meth", "ptp", "nnn", "tct")
        if (!identical(sort(auto.a.names), sort(names(a)))) stop(paste("a must contain these columns:", paste(auto.a.names, collapse=", ")))
        if (TRUE %in% (a[,"ptp"] <= 0) || TRUE %in% (a[,"ptp"] > 1)) stop("ptp must be between 0 and 1")
        if (TRUE %in% (a[,"nnn"] <= 0)) stop("nnn must be greater than 0")
    }
    
    ## Make sure there are at least five points
    if (nrow(lxy[["pts"]]) < 5) stop("at least 5 locations are required to create hulls")

    ## create an empty list to hold the results of this function
    res <- list()
    runs.not.added <- NULL
    
    ## Get a data frame with the parsed values of all the names of the lxy[["nn"]] list elements, will use this later to see if 
    ## nearest neighbors for this value of s have already been identified
    #if (!is.null(lxy[["nn"]])) nn.names <- nn.name.df(names(lxy[["nn"]]))
    
    if (!is.null(lxy[["nn"]])) nn.names <- do.call(rbind, lapply(lxy[["nn"]], function(x) data.frame(id=x$id, tt=x$time.term, s=x$s, n=length(x$ptid), kmax=x$kmax, rmax=x$rmax, amax=x$amax)))
    
    ## Create an empty vector we will use to convext lxy.pts.idx to lhs.pts.idx
    lxy2lhs <- rep(0, length(lxy[["pts"]]))
    
    ## In preparation for tspan and velocity hull metric computation, convert time stamps to seconds 
    if (!is.null(lxy[["pts"]][["dt"]])) lxy.dt.int <- as.numeric(lxy[["pts"]][["dt"]])
    
    ## In preparation for velocity hull metric computation, initialize vector to record speed 
    ## We will fill this vector with the actual speed at each point once we're in the idVal loop
    #if (velocity.metrics && !is.null(lxy[["pts"]][["dt"]])) {
    #    speed.com.gf.all <- rep(NA, length(lxy[["pts"]]))
    #}    
    
    pbo.orig <- pboptions(type="txt", style=pbo.style)
    on.exit(pboptions(pbo.orig))
                        
    start.time <- Sys.time()
    if (status) cat("Using nearest-neighbor selection mode: ", mode, "\n", sep = "")
    if (status) cat("Constructing hulls and hull metrics...\n")
    
    
    
    ## Start big nested loop
    for (idVal in id) {
        
        ## Identify the indices of points and date-times for this individual only
        idVal.idx <- which(lxy[["pts"]][["id"]] == idVal)

        ## Fill in lxy2lhs so we can map original pts.idx values to their index in idVal.idx
        lxy2lhs[idVal.idx] <- 1:length(idVal.idx)

        # If a value for ptid was passed, this means the user only wants to find hulls for those parent points
        # So create a subset of the parent points that match this id as well as one of those ptid
        #if (is.null(ptid)) {
        #    idVal.idx.sub <- idVal.idx
        #} else {
        #    idVal.idx.sub <- idVal.idx[lxy[["pts"]][["ptid"]][idVal.idx] %in% ptid]
        #    if (length(idVal.idx.sub) == 0) cat("No points found for those ptid. \n")
        #}

        ## Grab the random walk parameters for this individual
        rw.params <- lxy[["rw.params"]][lxy[["rw.params"]][["id"]] == idVal,]

        ## Record the indices of the duplicate locations
        dups.idx <- idVal.idx[duplicated(coordinates(lxy[["pts"]])[idVal.idx,])]
        
        ## Jiggle duplicate points if needed
        if (offset.dups > 0 && length(dups.idx) > 0) {
            ## Create random angles
            theta <- runif(length(dups.idx)) * 2 * pi
            
            ## Convert the coordinates to an editable data frame, and adjust the coordinates of the duplicate points
            coords.df <- coordinates(lxy[["pts"]])
            coords.df[dups.idx,1] <- coords.df[dups.idx,1] + cos(theta) * offset.dups
            coords.df[dups.idx,2] <- coords.df[dups.idx,2] + sin(theta) * offset.dups
            
            ## Recreate the spatial points data frame
            lxy[["pts"]] <- SpatialPointsDataFrame(coords=coords.df, data=lxy[["pts"]]@data, proj4string=lxy[["pts"]]@proj4string, match.ID=FALSE)
            if (status) cat(idVal, ": ", length(dups.idx), " duplicate points were randomly displaced by ", offset.dups, " map unit(s) \n", sep="")
            rm(coords.df)
            
        } 

        if (length(dups.idx) > 0) {
            dups.lst <- list(dups.idx=lxy2lhs[dups.idx], offset=offset.dups)
        } else {
            dups.lst <- list(dups.idx=NULL, offset=NULL)
        }
        
        ## In preparation for computing the average speed of enclosed points (hull metric),
        ## make a vector of the "going forward" and "coming and going" speed of each point for this individual
        if (velocity.metrics && !is.null(lxy[["pts"]][["dt"]])) {
              
            ## Define all.but.last.i, all.but.last.next, all.but.first.i, all.but.first,prev
            abl.i <- abf.p <- idVal.idx[-length(idVal.idx)]
            abl.n <- abf.i <- idVal.idx[-1]
            
            ## Delta-t and distance going forward (i to i+1)
            dt.gf.idVal <- c(lxy.dt.int[abl.n] - lxy.dt.int[abl.i], NA)
            dist.gf.idVal <- c(with(data.frame(coordinates(lxy[["pts"]])), sqrt((x[abl.n] - x[abl.i])^2 + (y[abl.n] - y[abl.i])^2 )), NA)
            
            ## Delta-t and distance coming (i-1 to i)
            dt.com.idVal <- c(NA, lxy.dt.int[abf.i] - lxy.dt.int[abf.p])      
            dist.com.idVal <- c(with(data.frame(coordinates(lxy[["pts"]])), sqrt((x[abf.i] - x[abf.p])^2 + (y[abf.i] - y[abf.p])^2 )), NA)

            speed.com.gf.idVal <- (dist.com.idVal + dist.gf.idVal) / (dt.com.idVal + dt.gf.idVal)
            rm(dist.com.idVal, dist.gf.idVal, dt.com.idVal, dt.gf.idVal, abf.i, abf.p, abl.i, abl.n)
            
            ## STILL NEED TO FILTER OUT BASED ON TIME, DIST, OR SPEED > 2 STD.DEV
        }

        
        one.loop <- (length(n2z(s)) * length(n2z(k)) * nrow(as.data.frame(n2z(a))) * length(n2z(r))) == 1
        
        for (kVal in n2z(k)) {
        for (rVal in n2z(r)) {
        for (a.idx in 1:nrow(as.data.frame(a))) {
            if (is.numeric(a)) {
                aVal <- a[a.idx]
                auto.a <- NULL
            } else {
                aVal <- 0
                auto.a <- as.list(a[a.idx,])
            } 
        for (sVal in n2z(s)) {
            
            ## Look for a saved set of nearest neighbors
            blnCont <- FALSE

            if (!is.null(lxy[["nn"]])) {
                ## Find those nn sets that have the same value of id, n (number of parent points), and
                
                ## Because values of s are not necessarily integers and thus may not be represented perfectly by in the 
                ## floating double format, we must use the all.equal() function to compare
                
                nn.idxs <- which(nn.names[["id"]] == idVal & nn.names[["n"]] == length(idVal.idx) & sapply(nn.names[["s"]], function(x) isTRUE(all.equal(x, sVal))))
                for (nn.idx in nn.idxs) {
                    ## See if the ptid are the same
                    if (identical(sort(lxy[["nn"]][[nn.idx]][["ptid"]]), sort(lxy[["pts"]][["ptid"]][idVal.idx]))) {
                        ## Found a match, now lets see if there are enough neighbors saved
                        
                        ## If mode==auto.a, see if there is a saved value of auto.a 
                        if (mode=="Auto-a") {                                
                            blnAutoAOK <- FALSE
                            ## See if there is a matching row in the data frame of saved auto.a
                            if (!is.null(lxy[["nn"]][[nn.idx]][["auto.a.df"]])) {
                                matching.saved.auto.a <- with(lxy[["nn"]][[nn.idx]][["auto.a.df"]], which(a.meth==auto.a[["a.meth"]] & a.pp==auto.a[["a.pp"]] & a.nn==auto.a[["a.nn"]] & a.h==auto.a[["a.h"]] & a.tct==auto.a[["a.tct"]]))
                                if (length(matching.saved.auto.a) > 0) {
                                    blnAutoAOK <- TRUE
                                    aVal <- lxy[["nn"]][[nn.idx]][["auto.a.df"]][matching.saved.auto.a, "aVal"]
                                }
                            }
                        } else {
                            blnAutoAOK <- TRUE
                        }
                            
                        ## See if this set of nearest neighbors has enough neighbors in it
                        if (max(kmin, kVal) <= lxy[["nn"]][[nn.idx]][["kmax"]] && rVal <= lxy[["nn"]][[nn.idx]][["rmax"]] && aVal <= lxy[["nn"]][[nn.idx]][["amax"]] && blnAutoAOK) {
                            blnCont <- TRUE
                            break
                        } 
                    }
                }
            }
            
            ## Construct a name for this hullset in the form: ag208.k15.s0.srt-area
            lhs.name <- paste(idVal, ".pts", length(idVal.idx), ".", str.param, round(get(paste(str.param,"Val",sep="")),decimal.places), ".s", sVal, ".kmin", kmin, sep="")
            cat("\n", lhs.name, "\n", sep="")
            
            if (!blnCont) {
                cat(cw(indent=1, exdent=3, x=paste(" - Unfortunately there isn't a set of nearest neighbors in lxy for this value of s :-( \n Run lxy.nn.add() and try again\n")))
                runs.not.added <- c(runs.not.added, lhs.name)
            } else {
                if (status) {
                    cat("  Found a suitable set of nearest neighbors \n")
                    if (mode=="Auto-a" && blnAutoAOK) cat("  Found a saved value for auto.a: ", aVal, "\n")
                }
                
                ## Grab the right rows from the nn table
                if (mode == "Fixed-k") {
                    pp.nn.df.idx <- which(lxy[["nn"]][[nn.idx]][["nn.df"]][["nn.rank"]] <= max(kmin, kVal))
                } else if (mode == "Fixed-r") { 
                    pp.nn.df.idx <- which(lxy[["nn"]][[nn.idx]][["nn.df"]][["tsd"]] <= rVal | lxy[["nn"]][[nn.idx]][["nn.df"]][["nn.rank"]] <= kmin)
                } else if (mode == "Fixed-a" || mode == "Auto-a") { 
                    pp.nn.df.idx <- which(lxy[["nn"]][[nn.idx]][["nn.df"]][["tsd.cumsum"]] <= aVal | lxy[["nn"]][[nn.idx]][["nn.df"]][["nn.rank"]] <= kmin)
                }
                
                ## Create hulls 
                if (status) cat("  Identifying the boundary points for each parent point \n")

                ## Convert the nn data frame to a list. Each list element is a vector of the indices of the nearest neighbors of a parent point
                nn.idx.lst <- split(lxy[["nn"]][[nn.idx]][["nn.df"]][pp.nn.df.idx, "nn.idx"], lxy[["nn"]][[nn.idx]][["nn.df"]][pp.nn.df.idx, "pp.idx"])
                rm(pp.nn.df.idx)
                if (one.loop) lxy[["nn"]] <- NULL

                ## Filter out those that don't have at least three points
                nn.idx.lst <- nn.idx.lst[sapply(nn.idx.lst, length) >= 3]

                if (length(nn.idx.lst) == 0) {
                    cat("No hulls were created. If you are using Fixed-r or Fixed-a locoh, try entering a bigger value for r or a, or set kmin to a value greater than or equal to 2 \n")
                    runs.not.added <- c(runs.not.added, lhs.name)
                } else {
                
                    ## Create a version of nn.idx.lst in which the indices have been converted from lxy$pts to lhs$pts (i.e., this ID only)
                    nn.idx.lst.lhs <- lapply(nn.idx.lst, function(x) lxy2lhs[x])
                    
                    ## For each of these list elements, find the points that lie along the boundary of the MCP. chull returns indices
                    nn.idx.lst.boundary.idx <- pblapply(nn.idx.lst, function(j) chull(coordinates(lxy[["pts"]])[j,]))    
                    if (length(nn.idx.lst.boundary.idx) != length(nn.idx.lst)) stop("For some reason not all nn sets got boundary points identified, collinear? investigate")
                    
                    if (status) cat("  Converting boundary points into polygons\n")
                    
                    ## Create a list of Polygons objects
                    Sr1 <- pblapply(1:length(nn.idx.lst.boundary.idx), function(i) Polygons(list(Polygon(coords=coordinates(lxy[["pts"]])[nn.idx.lst[[i]][c(nn.idx.lst.boundary.idx[[i]], nn.idx.lst.boundary.idx[[i]][1])],  ]  , hole=F)), ID=as.character(i)))

                    ## Create SpatialPolygons object
                    hulls.sp <- SpatialPolygons(Sr1, proj4string=lxy[["pts"]]@proj4string)
                    if (length(hulls.sp) != length(nn.idx.lst)) stop("Some sets of boundary points didn't get converted to hulls, could be collinear, investigate and filter out")
                    rm(Sr1, nn.idx.lst.boundary.idx)

                    ## Initialize lists for hull metrics and meta data
                    hulls.data <- list()
                    hulls.meta <- list()
                    
                    ## Create columns for ptid and pts.idx
                    nn.idx.lst.pp.pts.idx <- as.numeric(names(nn.idx.lst))
                    hulls.data[["pts.idx"]] <- lxy2lhs[nn.idx.lst.pp.pts.idx]
                    hulls.data[["ptid"]] <- lxy[["pts"]][["ptid"]][nn.idx.lst.pp.pts.idx]
                    if (one.loop) rm(lxy2lhs)
                    
                    #hulls.data[["pts.idx"]] <- match(hulls.data[["ptid"]], lxy[["pts"]][["ptid"]][idVal.idx])
                    
                    ## Compute number of nearest neighbors (used in nearest neighbor construction)
                    hulls.data[["nnn"]] <- as.numeric(sapply(nn.idx.lst, length))
                    hulls.meta[["nnn"]] <- list(type="nnn", aux=NULL)
                    
                    ## Add area to hulls metrics list
                    if (status) cat("  Calculating area and perimeter..."); flush.console()

                    ## Grab the area from the @polygons slot
                    hulls.data[["area"]] <- sapply(hulls.sp@polygons, function(x) x@area)
                    hulls.meta[["area"]] <- list(type="area", aux=NULL)

                    ## Extract a list of data frames containing the coordinates (closed) of each hull 
                    hulls.coords.lst <- lapply(hulls.sp@polygons, function(x) x@Polygons[[1]]@coords)
                    
                    ## For each hull, sum up the lengths of the segments
                    hulls.data[["perim"]] <- sapply(hulls.coords.lst, function(pts.df) sum(sqrt(((pts.df[-nrow(pts.df),1] - pts.df[-1,1])^2) + ((pts.df[-nrow(pts.df),2] - pts.df[-1,2])^2))))
                    hulls.meta[["perim"]] <- list(type="perim", aux=NULL)
                    if (status) cat("Done.\n")  
                    rm(hulls.coords.lst)
                    
                    ## Calculate the time window width of the nearest neighbors
                    if (!is.null(lxy[["pts"]][["dt"]])) {
                        tau <- rw.params[1, "time.step.median"]
                        if (status) cat("  Calculating the time span of each hull..."); flush.console()
                        hulls.data[["tspan"]] <- as.numeric(sapply(nn.idx.lst, function(x) (max(lxy.dt.int[x]) - min(lxy.dt.int[x])) / tau))
                        hulls.meta[["tspan"]] <- list(type="tspan", aux=NULL)
                        if (status) cat("Done.\n"); flush.console()
                        if (one.loop) rm(lxy.dt.int)
                    }

                    ## Find enclosed points for 1) nep hull metric and 2) enc.pts (list of enclosed points indices)
                    if (sVal == 0) {
                        ## When time is not included, enc.pts.idx is simply the points used for hull construction
                        ## and enc.pts.nn is a matching list of TRUE values
                        enc.pts.idx <- nn.idx.lst.lhs
                        enc.pts.nn <- lapply(nn.idx.lst.lhs, function(x) rep(TRUE, times=length(x)))
                        
                    } else {
                        if (status) cat("  Identifying enclosed points...", sep=""); flush.console()
                        
                        enc.pts.idx <- over(hulls.sp, as(lxy[["pts"]][idVal.idx,], "SpatialPoints"), returnList=TRUE)
                        if (status) cat("Done.\n"); flush.console()

                        ## Create a list of boolean vectors that indicate which of the enclosed points are also a nearest neighbor
                        enc.pts.nn <- lapply(1:length(enc.pts.idx), function(j) enc.pts.idx[[j]] %in% nn.idx.lst.lhs[[j]])

                    }
                    
                    #cat("\n")

                    ## Save number of enclosed points to the list of hull metrics
                    hulls.data[["nep"]] <- sapply(enc.pts.idx, length)
                    hulls.meta[["nep"]] <- list(type="nep", aux=NULL)

                    ## Create a hull metric for the average speed of points enclosed in the hull
                    if (velocity.metrics && !is.null(lxy[["pts"]][["dt"]])) {                    
                        
                        hulls.data[["scg.enc.mean"]] <- sapply(enc.pts.idx, function(x) signif(mean(speed.com.gf.idVal[x], na.rm=TRUE), digits=4))
                        hulls.meta[["scg.enc.mean"]] <- list(type="scg.enc.mean", aux=NULL)

                        hulls.data[["scg.enc.sd"]] <- sapply(enc.pts.idx, function(x) signif(sd(speed.com.gf.idVal[x], na.rm=TRUE), digits=4))
                        hulls.meta[["scg.enc.sd"]] <- list(type="scg.enc.sd", aux=NULL)

                        hulls.data[["scg.nn.mean"]] <- sapply(nn.idx.lst.lhs, function(x) signif(mean(speed.com.gf.idVal[x], na.rm=TRUE), digits=4))
                        hulls.meta[["scg.nn.mean"]] <- list(type="scg.nn.mean", aux=NULL)

                        hulls.data[["scg.nn.sd"]] <- sapply(nn.idx.lst.lhs, function(x) signif(sd(speed.com.gf.idVal[x], na.rm=TRUE), digits=4))
                        hulls.meta[["scg.nn.sd"]] <- list(type="scg.nn.sd", aux=NULL)
                    }
                    rm(nn.idx.lst.lhs, nn.idx.lst)
                    
                    ## Convert the hull metrics list to a data frame
                    hulls.data.df <- data.frame(hulls.data)
                    rm(hulls.data)
                      
                    ## Initialize hm.params
                    hm.params <- NULL

                    ## If there are ancillary variables, record the names of the numeric ones in hm.params, so they can be called as hull metrics
                    if (anv.copy && !is.null(lxy[["anv"]])) {
                        if (sum(sapply(lxy[["pts"]]@data[as.character(lxy[["anv"]][["anv"]])], is.numeric)) > 0) {
                            hm.params <- list(anv=lxy[["anv"]][["anv"]][sapply(lxy[["pts"]]@data[as.character(lxy[["anv"]][["anv"]])], is.numeric)])
                        }
                        #hulls.data.df <- data.frame(hulls.data.df, lxy[["pts"]]@data[idVal.idx, as.character(lxy[["anv"]][["anv"]]), drop=FALSE])
                        hulls.data.df <- data.frame(hulls.data.df, lxy[["pts"]]@data[nn.idx.lst.pp.pts.idx, as.character(lxy[["anv"]][["anv"]]), drop=FALSE])
                        anv.meta <- lxy[["anv"]]
                    } else {
                        anv.meta <- NULL
                    }
                    rm(nn.idx.lst.pp.pts.idx)
                
                    ## Create the hulls spdf
                    hulls.spdf <- SpatialPolygonsDataFrame(hulls.sp, data=hulls.data.df, match.ID=FALSE)
                    rm(hulls.sp)
                    
                    hs.desc <- paste("Hulls created from ", length(idVal.idx), " locations of ", idVal, " using the ", 
                                 mode, " method (", str.param, "=", round(get(paste(str.param,"Val",sep="")),decimal.places), ", s=", sVal, ", kmin=", kmin,
                                 if (length(dups.lst[["dups.idx"]])==0) "" else paste(", ", length(dups.lst[["dups.idx"]]), " duplicate points offset by ", dups.lst[["offset"]], " map units", sep=""), 
                                 if (nrow(hulls.spdf) < length(idVal.idx)) paste(", insufficient neighbors found to make hulls for ", length(idVal.idx) - nrow(hulls.spdf), " points.", sep="") else "", ").", sep="")

                    ## Subset the SpatialPoints object to just the points that are of this id
                    pts.idVal <- lxy[["pts"]][idVal.idx, ]
                    if (one.loop) rm(idVal.idx)
                    
                    ## Drop unused factors
                    for (factor.col.name in names(pts.idVal@data)[sapply(pts.idVal@data, is.factor)]) {
                        pts.idVal@data[[factor.col.name]] <- pts.idVal@data[[factor.col.name]][,drop=TRUE]
                    }
                    
                    ## Add objects to 'res'        
                    hullset <- list(a1=list(id=idVal, pts=pts.idVal, anv=anv.meta, 
                                       rw.params=rw.params, mode=str.param,
                                       k=if (kVal==0) NULL else kVal, r=if (rVal==0) NULL else rVal, a=if(aVal==0) NULL else aVal, 
                                       auto.a=auto.a, s=sVal, kmin=kmin, dups=dups.lst, desc=hs.desc,
                                       hulls=hulls.spdf, hm=hulls.meta, 
                                       enc.pts=list(idx=enc.pts.idx, nn=enc.pts.nn), 
                                       hm.params=hm.params, gen.date=date()))
                    
                    ## Clean out some potentially big objects from memory
                    rm(hulls.spdf, hulls.meta, enc.pts.idx, enc.pts.nn, pts.idVal)
                    
                    ## Create default utilization distributions
                    if (iso.add) {
                        class(hullset) <- c("locoh.lhs")
                        if (status) cat("  Computing density isopleths \n"); flush.console()
                        hullset <- lhs.iso.add(hullset, iso.levels=iso.levels, status=FALSE)
                    }
                    
                    ## Remove hulls SpatialPolygons, leaving just the data frame, and enc.pts if needed
                    if (!save.hulls) hullset[[1]][["hulls"]] <- hulls.data.df
                    if (!save.enc.pts) hullset[[1]][["enc.pts"]] <- NULL
                    
                    res[[lhs.name]] <- hullset[[1]]
                    
                    rm(hullset, hulls.data.df)

                }
                
            }
        }
        }
        }
        }
    }

    if (is.null(names(res))) {cat("No hullsets generated \n"); return(NULL)}
    
    attr(res, "tlocoh_ver") <- packageVersion("tlocoh")
    class(res) <- c("locoh.lhs", "list")

    if (status) {
        cat("The following hullsets were generated:\n")
        if (is.null(names(res))) cat("   none \n")
        for (lhs.name in names(res)) cat("   ", lhs.name, "\n")
        if (!is.null(runs.not.added)) {
            cat(cw("The following hullsets were NOT generated because no suitable nearest-neighbor set was found:\n"))
            for (lhs.name.na in runs.not.added) cat("   ", lhs.name.na, "\n")
        }
        
        time.taken <- difftime(Sys.time(), start.time, units="auto")
        cat("Total time:", round(time.taken,1), units(time.taken), "\n", sep = " ")
        flush.console()
    }
       
    if (beep) {
        flush.console()
        for (i in 1:3) {
            alarm()
            Sys.sleep(0.8)
        }
    }
    
    return(res)
    
}
