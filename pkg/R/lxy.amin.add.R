#' Finds the value of a such that p percent of points are a nearest neighbor for at least one hull
#'
#' @param lxy A \code{\link{LoCoH-xy}} object
#' @param id The id value(s) to analyze. If \code{NULL} all ids will be used.
#' @param s Value(s) for the s term in the time-scaled-distance equation for point-to-point distance
#' @param ptp The proportion of total points that should be a nearest neighbor for at least one hull (0..1]
#' @param max.iter The maximum number of iterations to try
#' @param prec A numeric value in map units to which the value of 'a' will be found. If \code{NULL}, will default to one-half of the median step legnth of the entire dataset
#' @param status Show messages, T/F
#'
#' @details This function finds the value of 'a' (within a specified threshhold \code{prec}) such that the 
#' proportion \code{ptp} of all points will be a
#' nearest neighbor for at least one hull. This value of 'a' is intended to be a reasonable lower bound for a home range that
#' includes as many points as desired, but minimizes areas where the individual was not observed. This assumes that 
#' duplicate points will be offset by a random amount when creating hullset(s).
#' 
#' Note that the value of 'a' such that \code{ptp} of points are nearest neighbors does not mean that \code{ptp} points are
#' enclosed. Points can be enclosed by hulls that are not a nearest neighbor of any hull parent point.
#'
#' @export

lxy.amin.add <- function(lxy, id=NULL, s=NULL, ptp=1, max.iter=20, nnn=2, prec=NULL, status=TRUE) {

    if (!require(pbapply)) stop("pbapply package required")
    if (!inherits(lxy, "locoh.lxy")) stop("lxy should be of class \"locoh.lxy\"")
    if (!is.null(lxy[["xys"]])) stop("Old data structure detected. Fix with lxy.repair()")
    if (is.null(lxy[["nn"]])) stop("No nearest-neighbor sets found")
    if (max(ptp) > 1) stop("ptp can't be more than 1")  
    
    ## Error check parameter values
    if (is.null(id)) {
        id <- levels(lxy[["pts"]][["id"]])
    } else {
        if (FALSE %in% (id %in% levels(lxy[["pts"]][["id"]]))) stop(paste("Can't find the following ID(s) in lxy: ", paste(id[!(id %in% levels(lxy[["pts"]][["id"]]))], collapse=", ", sep=""), sep=""))
    }
    
    nnn <- as.integer(nnn)
    if (min(nnn) < 2) stop("Minimum value of nnn is 2")
    
    #if (save && is.null(lxy[["amin"]])) lxy[["amin"]] <- data.frame(id="1", s=0, ptp=0, amin=0)[0,]
    # res <- data.frame(id="1", s=0, ptp=0, amin=0)[0,]
    
    start.time = Sys.time()
    #suminfo <- NULL
    #amin.lst <<- list()
    
    ## Starting the first of the mother of all nested loops
    for (idVal in id) {
        cat(idVal, "\n")
        idVal.idx <- which(lxy[["pts"]][["id"]] == idVal)
        idVal.num.pts <- length(idVal.idx)
        
        if (is.null(prec)) {
            prec.use <- lxy[["rw.params"]][lxy[["rw.params"]][["id"]]==idVal , "d.bar"] / 2
        } else {
            prec.use <- prec
        }
        
        nn.info <- do.call(rbind, lapply(lxy[["nn"]], function(x) data.frame(id=x[["id"]], tt=x[["time.term"]], s=x[["s"]], n=length(x[["ptid"]]), kmax=x[["kmax"]], rmax=x[["rmax"]], amax=x[["amax"]])))
        
        if (is.null(nn.info)) stop(paste("No nearest neighbor tables found for id=", idVal, sep=""))
        
        ## Find those nn sets that have the same value of id
        nn.info.idVal.idx <- which(nn.info[["id"]] == idVal)
        
        if (is.null(s)) {
            ## Loop thru all of them
            nn.idx.all <- nn.info.idVal.idx
        } else {
            ## If there are any s values that don't already have nearest neighbor tables available, add those now
            for (sVal in s[!s %in% nn.info[["s"]]]) {
                lxy <- lxy.nn.add(lxy, s=sVal, k=max(nnn))
                nn.info <- do.call(rbind, lapply(lxy[["nn"]], function(x) data.frame(id=x[["id"]], tt=x[["time.term"]], s=x[["s"]], n=length(x[["ptid"]]), kmax=x[["kmax"]], rmax=x[["rmax"]], amax=x[["amax"]])))
                nn.info.idVal.idx <- which(nn.info[["id"]] == idVal)
            }
        
            ## Lop thru the ones that have matching s values
            nn.idx.all <- nn.info.idVal.idx[sapply(nn.info.idVal.idx, function(i) lxy[["nn"]][[i]][["s"]] %in% s)]
            
        }
        
        for (nn.idx in nn.idx.all) {
        
            for (nnnVal in nnn) {

                if (lxy[["nn"]][[nn.idx]][["kmax"]] < nnnVal) {
                    if (status) cat("Insufficient number of nearest neighbors in ", names(lxy[["nn"]])[nn.idx], ". Run lxy.nn.add with k=", nnnVal, "\n", sep="")
                } else {
                    #nn.idx <- which(nn.names[["id"]] == idVal & nn.names[["n"]] == length(idVal.idx.sub) & sapply(nn.names[["s"]], function(x) isTRUE(all.equal(x, sVal))))

                    for (ptpVal in ptp) {
                        if (status) cat(cw(paste("- Finding the value of 'a' (within ", prec.use, ") where ", ptpVal * 100, "% or more points are a nearest neighbor in at least one hull when k=", nnnVal, sep=""), final.cr=TRUE, indent=2, exdent=3))

                        #print("lets look at this");browser()

                        ## First thing we need to do is to find an upper value of 'a' that results in 100% of points being a nearest neighbor
                        a2 <- lxy[["nn"]][[nn.idx]][["nn.df"]][["tsd.cumsum"]][lxy[["nn"]][[nn.idx]][["nn.df"]][["nn.rank"]]==nnnVal]
                        a.high <- sort(a2)[ceiling(length(a2) * ptpVal)]

                        ## For the lower limit, we grab the first nearest neighbor of each point, sort, and take the pth value
                        a1 <- lxy[["nn"]][[nn.idx]][["nn.df"]][["tsd.cumsum"]][  lxy[["nn"]][[nn.idx]][["nn.df"]][["nn.rank"]]==(nnnVal-1)]
                        a.low <- sort(a1)[length(a1) * ptpVal]

                        rm(a1, a2)

                        ## We need to compute the percentage of points that are nearest neighbors for these upper and lower bounds, and keep
                        ## dropping the lower bound until the proportion of points enclosed is less than ptpVal

                        iter.num <- 0
                        blnLowerBoundOK <- FALSE

                        while (!blnLowerBoundOK) {
                            iter.num <- iter.num + 1
                            if (iter.num > 10) stop("Unable to get find an initial lower bound for 'a'. Try increasing ptp.")

                            ## Identify all the rows where tsd.cumsum is less than or equal to a.low
                            less.than.atest <- lxy[["nn"]][[nn.idx]][["nn.df"]][["tsd.cumsum"]] <= a.low

                            #nnsplit <- split(nndf[less.than.atest, "nn.idx"], nndf[less.than.atest, "pp.idx"])

                            ## Of these rows, split into list by pp.idx
                            #nnidx.lst <- with(lxy[["nn"]][[nn.idx]][["nn.df"]][less.than.atest, ], split(nn.idx, pp.idx))
                            nnidx.lst <- split(lxy[["nn"]][[nn.idx]][["nn.df"]][["nn.idx"]][less.than.atest], lxy[["nn"]][[nn.idx]][["nn.df"]][["pp.idx"]][less.than.atest])

                            ## The valid ones are those which are left with three or more points
                            nnidx.lst.good <- sapply(nnidx.lst, function(x) length(x) > nnnVal)

                            ## Grab all the nn.idx values from valid polygons, unlist them, and count the number of unique values
                            nn.pts <- unique(unlist(nnidx.lst[nnidx.lst.good]))

                            ## Compute the proportion of enclosed points
                            atest.ppe <- length(nn.pts) / idVal.num.pts

                            if (atest.ppe <= ptpVal) {
                                blnLowerBoundOK <- TRUE
                                #suminfo <- rbind(suminfo, data.frame(atest=a.low, nep=length(nn.pts), ptp=atest.ppe))
                                #amin.lst[[as.character(a.low)]] <- nn.pts
                            } else {
                                a.low <- a.low * 0.75
                            }

                        }

                        #cat("Initial a.low=", a.low, " (", atest.ppe, "), a.high=", a.high, "\n", sep="")

                        ## So now we have a.low and a.high such that somewhere between them ptpVal points or more will be enclosed
                        ## We want to shrink the difference between a.low and a.high until it is <= prec.use

                        iter.num <- 1

                        while ((a.high - a.low) > prec.use && iter.num < max.iter) {
                            cat("   ", iter.num, ": ", a.low, " - ", a.high, "\n", sep=""); flush.console()

                            ## Take the mid point a.high and a.low
                            a.mid <- mean(c(a.high, a.low))

                            ## Identify all the rows where tsd.cumsum is less than or equal to a.low
                            less.than.atest <- lxy[["nn"]][[nn.idx]][["nn.df"]][["tsd.cumsum"]] <= a.mid

                            ## Of these rows, split into list by pp.idx
                            #nnidx.lst <- with(lxy[["nn"]][[nn.idx]][["nn.df"]][less.than.atest, ], split(nn.idx, pp.idx))
                            nnidx.lst <- split(lxy[["nn"]][[nn.idx]][["nn.df"]][["nn.idx"]][less.than.atest], lxy[["nn"]][[nn.idx]][["nn.df"]][["pp.idx"]][less.than.atest])

                            ## The valid ones are those which are left with three or more points
                            nnidx.lst.good <- sapply(nnidx.lst, function(x) length(x) > nnnVal)

                            ## Grab all the nn.idx values from valid polygons, unlist them, and count the number of unique values
                            nn.pts <- unique(unlist(nnidx.lst[nnidx.lst.good]))

                            ## Compute the proportion of enclosed points
                            atest.ppe <- length(nn.pts) / idVal.num.pts

                            #suminfo <- rbind(suminfo, data.frame(atest=a.mid, nep=length(nn.pts), ptp=atest.ppe))
                            #amin.lst[[as.character(a.mid)]] <- nn.pts

                            #num.pts.enclosed <- length(unique(lxy[["nn"]][[nn.idx]][["nn.df"]][lxy[["nn"]][[nn.idx]][["nn.df"]][["tsd.cumsum"]] <= a.mid , "nn.idx"]))

                            if (atest.ppe >= ptpVal) {
                                a.high <- a.mid
                            } else {
                                a.low <- a.mid
                            }
                            iter.num <- iter.num + 1
                        }

                        cat("   Done: a.min = ", a.high, "\n", sep=""); flush.console()

                        #print("right here what we should do is to save this in nn");browser()
                        aa.df <- data.frame(meth="enc", ptp=ptpVal, nnn=nnnVal, tct=NA, aVal=a.high)
                        lxy[["nn"]][[nn.idx]][["auto.a.df"]] <- unique(rbind(lxy[["nn"]][[nn.idx]][["auto.a.df"]], aa.df))

                    }


                }
            }
        }
        
    }

    time.taken = difftime(Sys.time(), start.time, units="auto")
    if (status) cat("\nTotal time:", round(time.taken,1), units(time.taken), "\n", sep = " ")
    return(lxy)

}

