hulls2iso.gpc <- function(hulls, points.lst, hm.vals=NULL, iso.levels, decreasing=FALSE, 
                      iso.method=c("pt.quantiles", "hm.vals")[1], hs.name=NULL, iso.cap.method=c(">=", "<=")[1],
                      status=TRUE, total.num.points=NULL) {

    ############################################################################
    ## hulls2iso
    ## creates isopleths by merging hulls in the order they are passed
    ## Returns a SpatialPolygonsDataFrame
    ## Uses gpclib
    ############################################################################
                                           
    ## decreasing means that large values of the hull metric represent higher 'density' (and should therefore be colored red)
    ## hulls = a set of convex hulls (polygons) passed as a SpatialPolygons object,
    ## points.lst =  list of point indices enclosed by each hull
    ## hm.vals is a numeric vector of hull.metrics (only used if iso.method == 'hm.vals')
    ## iso.levels = a set of isopleth levels, 
    ##    If iso.method is pt.quantiles, iso.levels will be proportions of total points enclosed [0..1] in increasing order
    ##    if iso.method == 'hm.vals', iso.levels will be the 'break points' ()
    ## in the order they are passed until the ith % of points is captured

    if (!require(sp)) stop("package sp required")
    if (!require(rgeos)) stop("package rgeos required")
    if (!require(pbapply)) stop("package pbapply required")
    if (!require(gpclib)) stop("package gpclib required")
    if (!is(hulls, "SpatialPolygons")) stop("hulls should be SpatialPolygons")
    if (length(hulls) < length(iso.levels)) return(NULL)
    if (!iso.method %in% c("pt.quantiles", "hm.vals")) stop("Unknown value for iso.method")
    if (!iso.cap.method %in% c(">=", "<=")) stop("Unknown value for iso.cap.method")
    if (is.null(total.num.points)) stop("total.num.points is a required parameter")

    #iso.cap.method.int <- if (iso.cap.method == ">=") 1 else 0
    false.holes.corrected <- 0

    ## If method is quantitles and larger values represent 'density' or 'uniqueness', then 
    ## reverse the order of the hulls, points.lst, hm.vals, and iso.levels
    if (iso.method == "pt.quantiles" && decreasing) {
        bln.hmvals.dec <- TRUE
        rev.idx <- length(hulls):1
        hulls <- hulls[rev.idx,]
        points.lst <- points.lst[rev.idx]
        hm.vals <- hm.vals[rev.idx]
    } else {
        bln.hmvals.dec <- FALSE
    }
                    
    ## Compute the cummulative number of points enclosed for the data frme
    ##bln.pt.enc <- rep(FALSE, max(unlist(points.lst)))
    bln.pt.enc <- rep(FALSE, max(sapply(points.lst,max)))
    enc.pts.cummulative.len <- numeric(length(points.lst))
    for (i in 1:length(points.lst)) {
        bln.pt.enc[points.lst[[i]]] <- TRUE
        enc.pts.cummulative.len[i] <- sum(bln.pt.enc)
    }
    
    ## Compute the cummulative number of points enclosed as a proportion of the total points
    ep.ptp <- enc.pts.cummulative.len / total.num.points
        
    if (status) cat("  Converting SpatialPolygons to gpc.poly objects \n")
    ## Converting from the coordinates was much faster than coercing 
    hulls.gpc <- pblapply(hulls@polygons, function(x) as(x@Polygons[[1]]@coords[-nrow(x@Polygons[[1]]@coords) , ], "gpc.poly"))    
    
    hulls.proj4string <- hulls@proj4string
    rm(hulls)
    
    ## Create an object of class "MethodDefinition" (methods package)
    to_numeric <- selectMethod(coerce, c("gpc.poly", "numeric"))
    
    if (iso.method == "pt.quantiles") {
        
        ## For each iso.level, find the index of the first hull whose cummulative proportion of total points
        ## is less than or equal to iso.level. 0s are converted to 1 because can't have an isopleth with no hull
        last.hull.idx <- pmax.int(1, findInterval(iso.levels, ep.ptp))
        
        ## findInterval returns the index of the last (in case of duplicates) hull whose ptp <= iso.level.
        ## If iso.cap.method is ">=", then we add one to those values where ep.ptp[i] < iso.level so that 
        ## the isopleths enclose >= ith percent of point). We use pmin to make sure we don't go above the number of hulls
        if (iso.cap.method == ">=") {
            less.than.iso.level <- ep.ptp[last.hull.idx] < iso.levels
            last.hull.idx[less.than.iso.level] <- pmin.int(last.hull.idx[less.than.iso.level] + 1, length(ep.ptp))
        }

        ## Finally, if there are duplicate values in ep.ptp, change last.hull.idx to the index of the *first* occurence
        ep.ptp.first.idx <- seq_along(ep.ptp)
        ep.ptp.first.idx[duplicated(ep.ptp)] <- match(ep.ptp[duplicated(ep.ptp)], ep.ptp)
        last.hull.idx <- ep.ptp.first.idx[last.hull.idx] 

        ## Make sure there are enough hulls to enclose enough points for each iso.level. In theory, if a lot of parent points 
        ## didn't have enough nearest neighbors identified to create a hull, you could wind up with an insufficient number of
        ## hulls to enclose for example the 90th isopleth
        not.enuf.hulls <- which(iso.levels > ep.ptp[last.hull.idx])
        if (length(not.enuf.hulls) > 0) {
            if (status) cat("  Not enough hulls available for isopleth(s): ", paste(iso.levels[not.enuf.hulls], collapse=", ", sep=""), "\n", sep="")
            warning(paste(hs.name, ": not enough hulls available for iso level(s): ", paste(iso.levels[not.enuf.hulls], collapse=", ", sep=""), sep=""))
            iso.levels <- iso.levels[-not.enuf.hulls]
            last.hull.idx <- last.hull.idx[-not.enuf.hulls]
        }

        ## Record the hm.val of the last hull in each isopleth
        hm.vals.for.spdf <- hm.vals[last.hull.idx]
        num.hulls.for.spdf <- last.hull.idx

        isop.sp.lst <- vector(mode="list", length=length(last.hull.idx))
        
        i <- 1
        merged.hulls <- to_numeric(hulls.gpc[[1]])
        
        if (status) cat("  Unioning hulls and saving isopleths \n") 
        pb <- txtProgressBar(min=0, max=last.hull.idx[length(last.hull.idx)], style=3, char="+", width=getOption("pboptions")$txt.width - 1)
        for (j in 1:length(last.hull.idx)) {
            while (i <= last.hull.idx[j]) {
                next.hull <- to_numeric(hulls.gpc[[i]])
                merged.hulls <- .Call("Rgpc_polygon_clip", merged.hulls, next.hull, 3, PACKAGE="gpclib")
                i <- i + 1
                setTxtProgressBar(pb, i)
            } 
            
            ## We've gotten through a group, save this aggregation as an isopleth
            isop.sp.lst[[j]] <- try(as(as(merged.hulls, "gpc.poly"), "SpatialPolygons"), silent=TRUE)
            
            if (class(isop.sp.lst[[j]])=="try-error") {
                
                ## Error could occur if one of the polygon holes was actually a line segment. I could not find a way to trap
                ## when a union will produce a sliver above - it doesn't occur because the next poly being unioned is a line 
                ## segment. So instead we just delete all segment 'holes' and try again
                mhp <- as(merged.hulls, "gpc.poly")
                p.three.or.more.nodes <- sapply(mhp@pts, function(ppoly) nrow(unique(cbind(ppoly$x, ppoly$y))) > 2)
                mhp@pts <- mhp@pts[p.three.or.more.nodes]
                isop.sp.lst[[j]] <- try(as(mhp, "SpatialPolygons"), silent=TRUE)
                
                if (class(isop.sp.lst[[j]])=="try-error") {
                    ## This could be a floating point precision problem that prevents unique() from identifying the 
                    ## the rows that are *really* the same. Repeat with format function and only 10 significant digits
                    p.three.or.more.nodes <- sapply(mhp@pts, function(ppoly) nrow(unique(format(cbind(ppoly$x, ppoly$y), digits=10))) > 2)
                    mhp@pts <- mhp@pts[p.three.or.more.nodes]
                    isop.sp.lst[[j]] <- try(as(mhp, "SpatialPolygons"))
                }
                false.holes.corrected <- false.holes.corrected + sum(!p.three.or.more.nodes)
            }
            
            isop.sp.lst[[j]] <- spChFIDs(isop.sp.lst[[j]], as.character(j))
        }
        close(pb)
        
        #print("got thru all of theme");browser()
        
        ## Rbind the individual isopleths into a SpatialPolygons object
        grps.cum.union.sp <- do.call(rbind.SpatialPolygons, isop.sp.lst)
        
        # By this point we have lost proj4string, so restore it now
        grps.cum.union.sp@proj4string <- hulls.proj4string

    } else if (iso.method == "hm.vals") {

        stop("This method needs some checking, especially when high hm values represent density, also if not enough hulls to reach all iso.levels")
        
        # to check this, note the HM levels with a descending iso with quantiles, then try to 
        # recreate those ispoleths using those break points as iso.levels
        
        if (is.null(hm.vals)) stop("To define isopleths by hull metric values, you must pass hm.values")
        
        ## Here, iso.levels represent ranges of the values of hm.vals, not proportions of points enclosed.
        ## We presume the values in iso.levels are sorted, but they could be incerasing or decreasing
        iso.levels.descending <- !is.unsorted(iso.levels, strictly=TRUE)
        
        if (iso.levels[1] > iso.levels[2]) {
            ## Define the indices which divide isopleths. Because iso.levels and hm.vals are both reversed,
            ## to use the findInterval function we have to first reverse them and then flip them back 
            last.hull.idx.rev <- findInterval(rev(iso.levels), rev(hm.vals))
            last.hull.idx <- rev(length(hm.vals) - last.hull.idx.rev)
            
            # Because they are reverse, we need to add the final index (which will be the hull with the lowest hm.val)
            base.idx <- max(1, last.hull.idx[1])
            last.hull.idx <- c(last.hull.idx[-1], length(hm.vals))
            
            hm.vals.for.spdf <- hm.vals[last.hull.idx]
            num.hulls.for.spdf <- last.hull.idx - base.idx + 1

            ## Make a list of the indices of the hulls in each isopleth group        
            idx.groups.lst <- lapply(last.hull.idx, function(i) base.idx:i)

        } else {
            last.hull.idx <- findInterval(iso.levels, hm.vals)
            hm.vals.for.spdf <- hm.vals[last.hull.idx]
            num.hulls.for.spdf <- last.hull.idx
            
            ## Make a list of the indices of the hulls in each isopleth group        
            idx.groups.lst <- lapply(last.hull.idx, function(i) 1:i)
        }
    
    } 

    ## Compute the total length of edge (including holes) for each isopleth
    edge.len <- sapply(1:length(grps.cum.union.sp@polygons), function(i) sum(sapply(grps.cum.union.sp@polygons[[i]]@Polygons, function(p) matperim(p@coords))))

    ## Create the data frame that does with the isopleths
    isos.sp.comb.data <- data.frame(iso.level=iso.levels, area=sapply(grps.cum.union.sp@polygons, function(x) x@area), 
                                    edge.len=edge.len, nep=enc.pts.cummulative.len[last.hull.idx], ptp=ep.ptp[last.hull.idx],
                                    hm.val=hm.vals.for.spdf, num.hulls=num.hulls.for.spdf)
    
    if (status && false.holes.corrected > 0) cat("  ", false.holes.corrected, " linear 'holes' removed \n")
    
    return(SpatialPolygonsDataFrame(grps.cum.union.sp, data=isos.sp.comb.data, match.ID=FALSE) )
    
}
