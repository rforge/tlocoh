#' @import pbapply sp

hulls2iso.rgeos <- function(hulls, points.lst, hm.vals=NULL, iso.levels, decreasing=FALSE, 
                      iso.method=c("pt.quantiles", "hm.vals")[1], iso.cap.method=c(">=", "<=")[1],
                      one.by.one=FALSE, merge.from.hull1=FALSE, hs.name=NULL, status=TRUE, total.num.points=NULL) {

    ############################################################################
    ## hulls2iso.sp
    ## creates isopleths by merging hulls in the order they are passed
    ## Returns a SpatialPolygonsDataFrame
    ## Uses gUnaryUnion function from rgeos package
    ############################################################################
                                           
    ## decreasing means that large values of the hull metric represent higher 'density' (and should therefore be colored red)
    ## hulls = a set of convex hulls (polygons) passed as a SpatialPolygons object,
    ## points.lst =  list of point indices enclosed by each hull
    ## hm.vals is a numeric vector of hull.metrics (only used if iso.method == 'hm.vals')
    ## iso.levels = a set of isopleth levels, 
    ##    If iso.method is pt.quantiles, iso.levels will be proportions of total points enclosed [0..1] in increasing order
    ##    if iso.method == 'hm.vals', iso.levels will be the 'break points' ()
    ## in the order they are passed until the ith % of points is captured

    if (!is(hulls, "SpatialPolygons")) stop("hulls should be SpatialPolygons")
    if (length(hulls) < length(iso.levels)) return(NULL)
    if (!iso.method %in% c("pt.quantiles", "hm.vals")) stop("Unknown value for iso.method")
    if (!iso.cap.method %in% c(">=", "<=")) stop("Unknown value for iso.cap.method")
    if (is.null(total.num.points)) stop("total.num.points is a required parameter")
    if (rgeos::version_GEOS0() < "3.3.0") stop("Please upgrade rgeos, need support for geos 3.3.0 or later is required")

    #iso.cap.method.int <- if (iso.cap.method == ">=") 1 else 0

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
    #bln.pt.enc <- rep(FALSE, max(unlist(points.lst)))
    bln.pt.enc <- rep(FALSE, max(sapply(points.lst,max)))
    
    enc.pts.cummulative.len <- numeric(length(points.lst))
    for (i in 1:length(points.lst)) {
        bln.pt.enc[points.lst[[i]]] <- TRUE
        enc.pts.cummulative.len[i] <- sum(bln.pt.enc)
    }
    
    ## Compute the cummulative number of points enclosed as a proportion of the total points
    ep.ptp <- enc.pts.cummulative.len / total.num.points
    
    ## Make a master list of all the individual polygon objects        
    hulls.p.lst <- lapply(hulls@polygons, function(x) x@Polygons[[1]])
    
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
            if (status) cat("  Not enough hulls available for isopleth level(s): ", paste(iso.levels[not.enuf.hulls], collapse=", ", sep=""), "\n", sep="")
            warning(paste(hs.name, ": not enough hulls available for iso level(s): ", paste(iso.levels[not.enuf.hulls], collapse=", ", sep=""), sep=""))
            iso.levels <- iso.levels[-not.enuf.hulls]
            last.hull.idx <- last.hull.idx[-not.enuf.hulls]
        }
        
        ## Record the hm.val of the last hull in each isopleth
        hm.vals.for.spdf <- hm.vals[last.hull.idx]
        num.hulls.for.spdf <- last.hull.idx

        ## Compute the range of hulls in each isopleth group, remembering that the hulls are already sorted in the order they should be union
        last.hull.idx.with.zero <- c(0, last.hull.idx)
        ind.grps.idx.lst <- lapply(1:length(last.hull.idx), function(i) (last.hull.idx.with.zero[i]+1):last.hull.idx.with.zero[i+1])
        
        ## Create a character vector of unique feature ids. (Can't just use as.character(i) because a bug in sp)
        ## We need to pad id with leading 0s otherwise gUnaryUnion messes up '1' and '10'
        feat.ids <- sprintf("p%03d", 1:length(last.hull.idx))

        ## For each group of hulls, create a list of Polygon objects.
        ind.grps.P.lst <- lapply(1:length(last.hull.idx), function(i) Polygons(hulls.p.lst[ind.grps.idx.lst[[i]]], ID=feat.ids[i]))

        ## Convert the list of Polygons to a multi-part SpatialPolygons object
        ind.grps.mpart.sp <- SpatialPolygons(ind.grps.P.lst)
        
        ## Union the multipart objects together (OLD - didn't error trap typology errors)
        #ind.grps.union.old <- gUnaryUnionPB(ind.grps.mpart.sp, id=sprintf("p%03d", 1:length(ind.grps.mpart.sp)))

        ind.grps.union.lst <- vector(mode="list", length=length(last.hull.idx))
        pb <- txtProgressBar(min=0, max=length(ind.grps.mpart.sp), style=3, char="+", width=getOption("pboptions")$txt.width - 1)
        for (i in 1:length(ind.grps.mpart.sp)) {
            setTxtProgressBar(pb, i)
            ind.grps.union.lst[[i]] <- try(.Call("rgeos_unaryunion", rgeos:::.RGEOS_HANDLE, ind.grps.mpart.sp[i], feat.ids[i], FALSE, PACKAGE = "rgeos"), silent=TRUE)

            ## If there was a typology error, try unioning them one by one
            if (class(ind.grps.union.lst[[i]]) == "try-error") {
                close(pb)
                
                cat("  gUnaryUnion failed for hull group ", i, ". Trying slower one-by-one method \n", sep="")
                cat("  Reformatting multipart polygon object into simple polygons")
                badsp.singlepart.lst <- pblapply(ind.grps.idx.lst[[i]], function(j) SpatialPolygons(list(Polygons(list(hulls.p.lst[[j]]), ID=feat.ids[i]))))

                cat("  Merging pieces together \n")
                p.union <- badsp.singlepart.lst[[1]]
                pb <- txtProgressBar(min=0, max=length(badsp.singlepart.lst), style=3, char="+", width=getOption("pboptions")$txt.width - 1)
                for (j in 2:length(badsp.singlepart.lst)) {
                  setTxtProgressBar(pb, j)
                  p.union <- try(rgeos::gUnion(p.union, badsp.singlepart.lst[[j]], id=feat.ids[i]), silent=TRUE)
                  if (class(p.union) == "try-error") return("error")
                  
                }
                close(pb)
                ind.grps.union.lst[[i]] <- p.union

                ## I also tried putting the hulls in the problem in a SpatialPolygons object with one record per feature,
                ## but that was not faster, probably because extracting individual features with [] notation seems to be slow

                ## Reset the progress bar
                if (i < length(ind.grps.mpart.sp)) {
                    pb <- txtProgressBar(min=0, max=length(ind.grps.mpart.sp), style=3, char="+", width=getOption("pboptions")$txt.width - 1)
                    setTxtProgressBar(pb, i)
                }
            }
        }
        close(pb)
        
        ## Take the list of the individual SP and put them in a single sp object
        ##ind.grps.union.sp <- do.call("rbind.SpatialPolygons", ind.grps.union.lst)
        
        ## Cummulatively union the individual groups into a list of Sp object
        grps.cum.union.lst <- vector("list", length(ind.grps.union.lst))
        grps.cum.union.lst[[1]] <- ind.grps.union.lst[[1]]
        for (i in 2:length(ind.grps.union.lst)) {
            grps.cum.union.lst[[i]] <- try(rgeos::gUnion(grps.cum.union.lst[[i-1]], ind.grps.union.lst[[i]], id=as.character(i)), silent=TRUE)
            if (class(grps.cum.union.lst[[i]]) == "try-error") {
                ## There's been an error summing up the groups
                return("error")
            }
        }

        ## rbind the individual isopleths into a single SpatialPolygons object
        grps.cum.union.sp <- do.call(rbind.SpatialPolygons, grps.cum.union.lst)
        
        ## By this point we have lost proj4string, so restore it now
        grps.cum.union.sp@proj4string <- hulls@proj4string
    
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
    
    return(SpatialPolygonsDataFrame(grps.cum.union.sp, data=isos.sp.comb.data, match.ID=FALSE) )
    
}
