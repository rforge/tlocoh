#' Identifies nearest neighbors for a LoCoH-xy object
#'
#' @param lxy A LoCoH-xy object
#' @param id The name(s) of individuals to analyze
#' @param k Value for the fixed-k method (identify k nearest neighbors of each point)
#' @param r Value for the fixed-r method (identify all points within radius r)
#' @param a Value for the a method (identify all points within cummulative distance a). Can also be a data frame of parameters returned by the \code{\link{auto.a}} function.
#' @param s The value for s for the time-scaled distance metric
#' @param ptid A vector of ptid indicating which points should have nearest neighbors identified (can be used to speed up processing during testing)
#' @param kmin A minimum number of neighbors to identify for each point
#' @param ptsh The desired proportion of time-selected hulls [0..1], may also be 'all' if the s-ptsh map has already been computed. See details.
#' @param ptsh.idx The index of the saved ptsh-s table. See also \code{\link{summary.locoh.lhs}}
#' @param nn.exists What to do if nearest neighbors have already been saved: "append", "replace", or "skip"
#' @param time.term The space-time transformation to use in the TSD distance metric: 'vmax' for the maximum velocity transformation (default) or 'dif' for the diffusion transformation
#' @param FNN.algorithm  The algorithm to be used in the get.knnx() function in the package FNN
#' @param dec.places The number of decimal places that rmax and amax should be rounded to
#' @param ra.init.samp.size How many randomly selected points to use to come up with the first guess for how many neighbors to find for each point to satisify a (a-method only)
#' @param ra.init.quant The proportion of randomly selected to points to use for the a value when coming up with an initial guess at the number of neighbors to find for each point to satisify a (a-method only)
#' @param tct Temporal continuity threshhold used when \code{a=\link{auto.a}}
#' @param beep Beep when done (T/F)
#' @param status Show status messages (T/F)
#'
#' @details
#' When s > 0, the 'distance' (as defined by the time-scaled distance metric') between two points is a function of 
#' their separation in time as well as space. 
#'
#' If \code{ptsh} is provided, the script will find a value of 's' such that the proportion of hulls which are time-selected (e.g, consecutive points in time) is 
#' equal to \code{ptsh}. In this case, no value for \code{s} should be passed. If \code{ptsh="all"} and the proportion of time selected hulls has already been computed, 
#' those values of 's' will be used.
#' 
#' @return A LoCoH-xy object object with saved information about each points' nearest neighbors
#'
#' @seealso \code{\link{summary.locoh.lxy}} to see what nearest neighbors have already been identified
#' \code{\link{auto.a}}
#' \code{\link{lxy.amin.add}}
#' \code{\link{lxy.ptsh.add}}
#'
#' @examples
#' \dontrun{
#' # lxy <- lxy.nn.add(lxy, k=10, s=0.01)
#' }
#'
#' @export
#' @import pbapply sp FNN

lxy.nn.add <- function(lxy, id=NULL, ptid=NULL, k=NULL, r=NULL, a=NULL, s=NULL, kmin=0, ptsh=NULL, ptsh.idx=1,
                   nn.exists = c("append", "replace", "skip")[1],
                   time.term=c("vmax", "dif")[1], FNN.algorithm = c("cover_tree", "kd_tree", "VR", "CR")[2], dec.places=1, 
                   ra.init.samp.size=30, ra.init.quant=0.8, tct=0.05, beep=FALSE, status=TRUE) {
    
    if (!inherits(lxy, "locoh.lxy")) stop("lxy should be of class \"locoh.lxy\"")
    if (!is.null(lxy[["xys"]])) stop("Old data structure detected. Fix with lxy.repair()")
    if (!is.null(s)) {if (is.null(lxy[["pts"]][["dt"]]) && max(s) > 0) stop("You can't set s>0 because these locations have no time stamps saved")}
    if (!is.null(ptsh) && is.null(lxy[["pts"]][["dt"]])) stop("You can't pass ptsh because these locations have no time stamps saved")
    pbo.orig <- pboptions(type="txt", style=3)
    on.exit(pboptions(pbo.orig))
    
    start.time = Sys.time()
    
    ## Error check parameter values
    if (!(FNN.algorithm %in% c("cover_tree", "kd_tree", "VR", "CR"))) stop(paste(FNN.algorithm, " is not a valid value", sep=""))
    if (!(nn.exists %in% c("append", "replace", "skip"))) stop(paste("unknown value for nn.exists: ", nn.exists, sep=""))
    if (is.null(id)) {
        id <- levels(lxy[["pts"]][["id"]])
    } else {
        if (FALSE %in% (id %in% levels(lxy[["pts"]][["id"]]))) stop(paste("Can't find the following ID(s) in lxy: ", paste(id[!(id %in% levels(lxy[["pts"]][["id"]]))], collapse=", ", sep=""), sep=""))
    }
    if (!time.term %in% c("vmax", "dif")) stop("Unknown value for time.term")
    
    ## Figure out the mode
    if (is.null(a) && is.null(r) && !is.null(k)) {
        k <- vectorize.parameter(k)
        mode <- "Fixed-k"
        str.param <- "k"
    } else if (is.null(a) && !is.null(r) && is.null(k)) {
        mode <- "Fixed-r"
        r <- vectorize.parameter(r)
        str.param <- "r"
    } else if (!is.null(a) && is.null(r) && is.null(k)) {
        str.param <- "a"
        if (is.numeric(a)) {
            mode <- "Fixed-a"
            a <- vectorize.parameter(a)
        } else if (is.data.frame(a)) { 
            mode <- "Auto-a"
            auto.a.names <- c("meth", "ptp", "nnn", "tct")
            if (!identical(sort(auto.a.names), sort(names(a)))) stop(paste("a must contain these columns:", paste(auto.a.names, collapse=", ")))
            if (min(a[["ptp"]]) <= 0 || max(a[,"ptp"]) > 1) stop("a$ptp must be between 0 and 1")
            if (min(a[["nnn"]]) <= 0) stop("auto.a$nnn must be great than 0")
            if (FALSE %in% (a[["meth"]] %in% c("enc","nn"))) stop("unknown value for auto.a$meth")

            ## Set k to be the value(s) of nnn, so we can find at least this number of nearest neighbors for each point
            k <- a[["nnn"]]
            
        } else {
            stop(paste("Unknown method:", a))
        }
    } else {
        stop(cw("Could not determine what algorithm to use. Make sure to pass one and only one of the following parameters: k (Fixed-k method), r (Fixed-r), a (Fixed-a, Auto-a)."))
    }

    if (is.null(ptsh)) {
        if (is.null(s)) s <- 0
        s <- vectorize.parameter(s)
    } else {
        if (!is.null(s)) stop("Do not pass both 's' and 'ptsh'")
        if (!identical(ptsh, "all")) {
            if (min(ptsh)<=0 || max(ptsh)>1) stop("'ptsh' should be between 0 and 1")
        }
    }

    if (!is.null(lxy[["pts"]][["dt"]])) lxy.dt.int <- as.numeric(lxy[["pts"]][["dt"]])
    if (is.null(a)) a <- 0

    ## Define the indices for looping through "a" based on whether it's a nummeric vector or a data frame
    if (is.numeric(a)) {
        a.idxs <- order(a, decreasing=TRUE)
    } else {
        a.idxs <- order(a[["nnn"]], a[["ptp"]], decreasing=TRUE)
    }
    
    ## Down below we will need to see if there are existing nn sets that meets the criteria.
    ## So we will create a data frame of the names of all of the existing nn
    nn.name.added <- NULL
        
    ## Starting the first of the mother of all nested loops
    for (idVal in id) {

        ## If the user specified one or more values of ptsh, then we need to grab the corresponding s value(s)
        ## If they haven't already been saved as part of the lxy object, then we'll need to call lxy.ptsh.add
        if (!is.null(ptsh)) {
        
            blnNeedToAddPtsh <- FALSE
            if (is.null(lxy[["ptsh"]][[idVal]])) {
                blnNeedToAddPtsh <- TRUE
            } else {
                if (!identical(ptsh, "all")) {
                    if (is.null(lxy[["ptsh"]][[idVal]][[ptsh.idx]])) stop("Invalid value for ptsh.idx")
                    s.target.idx <- match(ptsh, lxy[["ptsh"]][[idVal]][[ptsh.idx]][["target.ptsh"]])
                    if (TRUE %in% sapply(s.target.idx, is.na)) blnNeedToAddPtsh <- TRUE
                }
            }
            
            if (blnNeedToAddPtsh) {
                if (status) cat("  - Finding s values that correspond to target proportions of time-selected hulls \n");flush.console
                lxy <- lxy.ptsh.add(lxy, id=idVal, k=10, n=200, ptsh.target=if (identical(ptsh, "all")) 1:9/10 else ptsh, plotme=FALSE, nn.add=FALSE, save=TRUE)
                ptsh.idx <- length(lxy[["ptsh"]][[idVal]])
            }
            
            if (identical(ptsh, "all")) {
                s.target.idx <- 1:length(lxy[["ptsh"]][[idVal]][[ptsh.idx]][["target.ptsh"]])
            } else {
                s.target.idx <- match(ptsh, lxy[["ptsh"]][[idVal]][[ptsh.idx]][["target.ptsh"]])
            }
            
            s <- lxy[["ptsh"]][[idVal]][[ptsh.idx]][["target.s"]][s.target.idx]
        
        }
        

        ## Indentify the indices of the points for this animal
        idVal.idx <- which(lxy[["pts"]][["id"]] == idVal)
        idVal.num.pts <- length(idVal.idx)
        
        xys.idVal <- coordinates(lxy[["pts"]])[idVal.idx,,drop=FALSE]

        tau <- lxy[["rw.params"]][lxy[["rw.params"]][["id"]]==idVal, "time.step.median"]
        d.bar <- lxy[["rw.params"]][lxy[["rw.params"]][["id"]]==idVal, "d.bar"]
        vmax <- lxy[["rw.params"]][lxy[["rw.params"]][["id"]]==idVal, "vmax"]
        
        ## If ptid is passed, compute the indices of the subset of points for which NN will be found
        if (!is.null(ptid)) {
            idVal.idx.sub <- idVal.idx[lxy[["pts"]][["ptid"]][idVal.idx] %in% ptid]
        } else {
            idVal.idx.sub <- idVal.idx
        }

        for (kVal in sort(n2z(k), decreasing=TRUE)) {
        for (rVal in sort(n2z(r), decreasing=TRUE)) {
        
        for (a.idx in a.idxs) {
            
        if (is.numeric(a)) {
            aVal <- a[a.idx]
        } else {
            a.params.str <- paste(unlist(a[a.idx,]), collapse="|", sep="")
        }
        
        for (sVal in n2z(s)) {

            ## Pull out the median sampling interval and the median step length
            if (sVal != 0) {
                if (is.null(lxy[["rw.params"]])) stop("rw.params is null, bailing")
                if (time.term == "vmax" && is.null(vmax)) stop("vmax is null, run lxy.repair and try again")
            }
        
            ## Start the clock running for this iteration
            start.time.this.run <- Sys.time()
            blnNoTime <- (sVal == 0)
            auto.a.df <- NULL
            
            if (blnNoTime) {
                method.str <- "Euclidean"
            } else {
                method.str <- paste("TSD:", time.term, sep="")
            }
                                                       
            if (status) cat(cw(paste("\nFinding nearest neighbors for id=", idVal, " (n=", idVal.num.pts, "), num.parent.pts=", length(idVal.idx.sub), ", mode=", mode, ", ",
                 ifelse(mode == "Auto-a", paste("auto.a=", a.params.str, sep=""), paste(str.param, "=", get(paste(str.param, "Val", sep="")), sep="")), 
                 ", s=", sVal, ", method=", method.str, "\n", sep=""), exdent=2))

            if (length(idVal.idx.sub) == 0) {
                if (status) cat("  - no parent points found with those ptid. Done \n")
                blnCont <- FALSE
            } else {
                blnCont <- TRUE
            }

            ## Look to see if theres a nn set already for this e, ce, and s, and if there is then
            ## see if the kmax, amax, or rmax is > the current kval, rval, or aval    
            
            append.idx <- NULL
            if (blnCont && !is.null(lxy[["nn"]])) {
                ## Get a data frame with the parsed values of all the names of the lxy[["nn"]] list elements
                nn.names <- do.call(rbind, lapply(lxy[["nn"]], function(x) data.frame(id=x$id, tt=x$time.term, s=x$s, n=length(x$ptid), kmax=x$kmax, rmax=x$rmax, amax=x$amax)))

                ## Find those nn sets that have the same value of id, e, ce, and n (number of parent points)
                nn.names.this.s.idx <- which(nn.names[["id"]] == idVal & nn.names[["n"]] == length(idVal.idx.sub) & sapply(nn.names[["s"]], function(x) isTRUE(all.equal(x, sVal))))
                
                for (nn.names.idx in nn.names.this.s.idx) {
                
                    ## See if the ptid are the same
                    if (identical(lxy[["nn"]][[nn.names.idx]][["ptid"]], lxy[["pts"]][["ptid"]][idVal.idx.sub])) {
                        if (status) cat(cw("- there is already a set of nearest neighbors for this set of parent points and value of s. \n", indent=2, exdent=4))
                        
                        if (nn.exists == "skip") {
                            if (status) cat("  - nn.exists='skip', so skipping this one \n\n")
                            blnCont <- FALSE
                        } else {
                            ## Replace or append
                            
                            ## If auto.a, see if there is a saved value of auto.a 
                            if (mode=="Auto-a") {                                
                                aVal.test <- 0
                                blnAutoAOK <- FALSE
                                
                                ## See if there is already a row in the data frame of saved auto.a
                                if (!is.null(lxy[["nn"]][[nn.names.idx]][["auto.a.df"]])) {
                                    matching.saved.auto.a <- with(lxy[["nn"]][[nn.names.idx]][["auto.a.df"]], which(meth==a[a.idx, "meth"] & ptp==a[a.idx, "ptp"] & nnn==a[a.idx, "nnn"] & tct==a[a.idx, "tct"]))
                                    
                                    if (length(matching.saved.auto.a) > 0) {
                                        blnAutoAOK <- TRUE
                                        aVal.test <- lxy[["nn"]][[nn.names.idx]][["auto.a.df"]][matching.saved.auto.a, "aVal"]
                                    }
                                }
                            
                            } else {
                                blnAutoAOK <- TRUE
                                aVal.test <- aVal
                            }
                            
                            ## See if this set of nearest neighbors has enough neighbors in it
                            if (max(kmin,kVal) <= lxy[["nn"]][[nn.names.idx]][["kmax"]] && rVal <= lxy[["nn"]][[nn.names.idx]][["rmax"]]
                                 && aVal.test <= lxy[["nn"]][[nn.names.idx]][["amax"]] && blnAutoAOK) {
                                if (status) cat("  - enough points already saved, no need to find more \n")
                                blnCont = FALSE
                            } else {
                                if (nn.exists == "replace") {
                                    if (status) cat("  - nn.exists='replace', so the current set of points will be replaced \n")
                                    lxy[["nn"]] <- lxy[["nn"]][-nn.names.idx]
                                } else if (nn.exists == "append") {
                                    append.idx <- nn.names.idx
                                    if (status) cat("  - additional neighbors will be identified and appended as needed \n")
                                }
                            }
                        }
                        ## We already found a perfect match, so don't need to check the other rows in nn.names
                        break
                    }
                }
            }
            
            if (blnCont) {
                kmax <- NULL; rmax <- NULL; amax <- NULL
                
                ## initialize a boolean in case we only are calculating auto.a and not actually appending new NN
                new.auto.a.only <- FALSE    
                
                if (mode=="Fixed-k") {
                    kVal <- max(kVal, kmin)
                    if (blnNoTime) {
                        ## Using FNN, find the nearest neighbors and distances to the query points.
                        pp.nn.lst <- FNN::get.knnx(data=xys.idVal, query=coordinates(lxy[["pts"]])[idVal.idx.sub, ,drop=F], k=kVal+1, algorithm=FNN.algorithm)
                        
                        ## Convert the list to a data frame
                        pp.nn.df <- data.frame(pp.idx = rep(idVal.idx.sub, each = kVal+1), nn.rank = rep(0:kVal, times=length(idVal.idx.sub)), 
                                               nn.idx = idVal.idx[c(t(pp.nn.lst$nn.index))], tsd = c(t(pp.nn.lst$nn.dist)))
                    } else {
                        pp.nn.df <- do.call(rbind, pblapply(idVal.idx.sub, function(i) {                                                                                                                          
                            pp.nn.lst <- FNN::get.knnx(data=data.frame(x=xys.idVal[,1], y=xys.idVal[,2], z = tsd.zvals(delta.t=lxy.dt.int[idVal.idx] - lxy.dt.int[i], sVal=sVal, type=time.term, d.bar=d.bar, tau=tau, vmax=vmax)),
                                                  query=cbind(coordinates(lxy[["pts"]])[i, , drop=F],0), k=kVal+1, algorithm=FNN.algorithm)
                            data.frame(pp.idx=i, nn.rank=0:kVal, nn.idx=idVal.idx[as.numeric(pp.nn.lst$nn.index)], tsd=as.numeric(pp.nn.lst$nn.dist))
                            }))
                    }
                    
                    ## Add a column of cumulative distances
                    pp.nn.d.cumsum <- unlist(lapply(split(pp.nn.df$tsd, pp.nn.df$pp.idx), cumsum))
                    pp.nn.df <- cbind(pp.nn.df, tsd.cumsum=pp.nn.d.cumsum)
                
                } else if (mode=="Fixed-r") {
                    
                   ## Use X randomly points to find an inital value of kVal.tmp that will hopefully result in grabbing all points <=r for most other points
                    num.test.pts <- min(ra.init.samp.size, idVal.num.pts)
                    if (status) cat("  - finding an initial value of k using ", num.test.pts, " randomly selected points...", sep="")
                    kVal.tmp <- max(15, kmin)
                    #idx.test <- sample(1:idVal.num.pts, num.test.pts)
                    idVal.idx.test <- sample(idVal.idx, num.test.pts)
                    
                    if (blnNoTime) {
                        pp.nn.lst <- FNN::get.knnx(data=xys.idVal, query=coordinates(lxy[["pts"]])[idVal.idx.test,,drop=FALSE], k=kVal.tmp + 1, algorithm=FNN.algorithm)
                        pp.nn.df <- data.frame(pp.idx = rep(idVal.idx.test, each=kVal.tmp + 1), nn.rank = rep(0:kVal.tmp, times=length(idVal.idx.test)), 
                                               nn.idx = idVal.idx[c(t(pp.nn.lst$nn.index))], tsd = c(t(pp.nn.lst$nn.dist)))
                    
                    } else {
                        pp.nn.df <- do.call(rbind, lapply(idVal.idx.test, function(i) {
                            pp.nn.lst <- FNN::get.knnx(data=data.frame(x=xys.idVal[,1], y=xys.idVal[,2], z = tsd.zvals(delta.t=lxy.dt.int[idVal.idx] - lxy.dt.int[i], sVal=sVal, type=time.term, d.bar=d.bar, tau=tau, vmax=vmax)),
                            query=cbind(coordinates(lxy[["pts"]])[i, , drop=F],0), k=kVal.tmp + 1, algorithm=FNN.algorithm)
                            data.frame(pp.idx=i, nn.rank=0:kVal.tmp, nn.idx=idVal.idx[as.numeric(pp.nn.lst$nn.index)], tsd=as.numeric(pp.nn.lst$nn.dist))
                            }))
                    }
                    
                    ## Identify those parent points who didn't get enough nearest neighbors (i.e., distance to furthest one is < rVal). Then increase kVal.tmp and try again
                    idx.not.enuf.nn.kVal <- which(pp.nn.df$nn.rank == kVal.tmp & pp.nn.df$tsd < rVal )
                    while (length(idx.not.enuf.nn.kVal) > 0 && kVal.tmp < idVal.num.pts - 1) {
                        
                        ## Remove those rows of the data frame because not enough points were found so we have to redo them
                        pp.idx.not.enuf <- pp.nn.df[idx.not.enuf.nn.kVal, "pp.idx"]
                        idx.not.enuf.nn.all <- which(pp.nn.df$pp.idx %in% pp.idx.not.enuf)
                        pp.nn.df <- pp.nn.df[-idx.not.enuf.nn.all, ,drop=FALSE]
                        kVal.tmp <- min(c(idVal.num.pts - 1, kVal.tmp * 2))
                        
                        if (blnNoTime) {
                            pp.nn.lst <- FNN::get.knnx(data=xys.idVal, query=coordinates(lxy[["pts"]])[pp.idx.not.enuf,,drop=FALSE], k=kVal.tmp+1, algorithm=FNN.algorithm)
                            pp.nn.df <- rbind(pp.nn.df,
                                        data.frame(pp.idx = rep(pp.idx.not.enuf, each=kVal.tmp + 1), nn.rank = rep(0:kVal.tmp, times=length(pp.idx.not.enuf)), 
                                                   nn.idx = idVal.idx[c(t(pp.nn.lst$nn.index))], tsd = c(t(pp.nn.lst$nn.dist))))
                        } else {
                            pp.nn.df <- rbind(pp.nn.df,
                                do.call(rbind, lapply(pp.idx.not.enuf, function(i) {
                                pp.nn.lst <- FNN::get.knnx(data=data.frame(x=xys.idVal[,1], y=xys.idVal[,2], z = tsd.zvals(delta.t=lxy.dt.int[idVal.idx] - lxy.dt.int[i], sVal=sVal, type=time.term, d.bar=d.bar, tau=tau, vmax=vmax)),
                                query=cbind(coordinates(lxy[["pts"]])[i, , drop=FALSE],0), k=kVal.tmp + 1, algorithm=FNN.algorithm)
                                data.frame(pp.idx=i, nn.rank=0:kVal.tmp, nn.idx=idVal.idx[as.numeric(pp.nn.lst$nn.index)], tsd=as.numeric(pp.nn.lst$nn.dist))
                                })))
                        }
                                                    
                        idx.not.enuf.nn.kVal <- which(pp.nn.df$nn.rank == kVal.tmp & pp.nn.df$tsd < rVal )
                    }
                    
                    ## The following would confirm each point got enough nn
                    ## pp.nn.df.maxd <- do.call(rbind, lapply(unique(pp.nn.df$pp.idx), function(x) data.frame(pp.idx=x, maxd=max(pp.nn.df[pp.nn.df$pp.idx == x,"d"]))))
                    
                    ## Get rid of all the rows where d > rVal
                    pp.nn.df <- pp.nn.df[pp.nn.df$tsd <= rVal ,,drop=FALSE]
                
                    ## Find the maximum nn.rank for each point
                    pp.nn.df.maxnn <- do.call(rbind, lapply(unique(pp.nn.df$pp.idx), function(x) data.frame(pp.idx=x, maxnn=max(pp.nn.df[pp.nn.df$pp.idx == x,"nn.rank"]))))

                    ## Take the 50th percentile as the starting point for kVal.tmp (plus one because we to get one more than the minimum)
                    kVal.tmp <- max(kmin, sort(pp.nn.df.maxnn$maxnn)[num.test.pts * ra.init.quant] + 1)
                    if (status) cat("Done. \n")
                    
                    #######################################################################################################
                    ## Done finding the initial value of kVal.tmp. Now lets use this value of k to find all neighbors distance 
                    ## rVal or a bit more
                    #######################################################################################################
                    
                    if (status) cat("  - computing distances for k=", kVal.tmp, " nearest neighbors\n", sep="")
                    
                    if (is.null(append.idx)) {
                        ## All parent points need new nearest nearest neighbors
                        idVal.idx.sub.nmnn <- idVal.idx.sub
                    } else {
                        ## Identify the parent points that already have enough points identified
                        pp.idx.enuf.nn <- with(lxy[["nn"]][[append.idx]], unique(nn.df[nn.df$tsd>=rVal, "pp.idx"]))
                        ## Remove the parent points with enuf nn from idVal.idx.sub to get just those that need more nearest neighbors (nmnn)
                        idVal.idx.sub.nmnn <- idVal.idx.sub[!idVal.idx.sub %in% pp.idx.enuf.nn]
                    }

                    
                    if (blnNoTime) {                                                   
                        pp.nn.lst <- FNN::get.knnx(data=, query=coordinates(lxy[["pts"]])[idVal.idx.sub.nmnn, ,drop=FALSE], k=kVal.tmp + 1, algorithm=FNN.algorithm)
                        pp.nn.df <- data.frame(pp.idx = rep(idVal.idx.sub.nmnn, each=kVal.tmp + 1), 
                                               nn.rank = rep(0:kVal.tmp, times=length(idVal.idx.sub.nmnn)), 
                                               nn.idx = idVal.idx[c(t(pp.nn.lst$nn.index))], 
                                               tsd = c(t(pp.nn.lst$nn.dist)))
                    } else {
                        pp.nn.df <- do.call(rbind, pblapply(idVal.idx.sub.nmnn, function(i) {
                            pp.nn.lst <- FNN::get.knnx(data=data.frame(x=xys.idVal[,1], y=xys.idVal[,2], z = tsd.zvals(delta.t=lxy.dt.int[idVal.idx] - lxy.dt.int[i], sVal=sVal, type=time.term, d.bar=d.bar, tau=tau, vmax=vmax)),
                            query=cbind(coordinates(lxy[["pts"]])[i, , drop=FALSE],0), k=kVal.tmp + 1, algorithm=FNN.algorithm)
                            data.frame(pp.idx=i, nn.rank=0:kVal.tmp, nn.idx=idVal.idx[as.numeric(pp.nn.lst$nn.index)], tsd=as.numeric(pp.nn.lst$nn.dist))
                            }))
                            ## z = d.bar * sqrt(sVal * (dt.thisid.int - dt.thisid.int[i]) / tau) 
                    }
                    
                    ## Next we will keep increasing kVal.tmp until all points have enough neighbors such that the furthest neighbor dist > rVal
                    ## (to make sure we've gotten all of the points within the radius)
                    
                    ## Look at the kth nearest neighbor and identify those whose d is still too small to ensure we've got all within r
                    idx.not.enuf.nn.kVal <- which(pp.nn.df$nn.rank == kVal.tmp & pp.nn.df$tsd < rVal )
                    
                    while (length(idx.not.enuf.nn.kVal) > 0 && kVal.tmp < idVal.num.pts - 1) {
                        
                        ## Remove those rows from pp.nn.df because we're going to replace them
                        pp.idx.not.enuf <- pp.nn.df[idx.not.enuf.nn.kVal, "pp.idx"]
                        idx.not.enuf.nn.all <- which(pp.nn.df$pp.idx %in% pp.idx.not.enuf)
                        pp.nn.df <- pp.nn.df[-idx.not.enuf.nn.all, ,drop=FALSE]
                        
                        ## Double kVal.tmp
                        kVal.old <- kVal.tmp
                        kVal.tmp <- min(c(idVal.num.pts - 1, kVal.tmp * 2))
                        
                        if (status) cat(cw(paste("- ", kVal.old, " wasn't large enough for ", length(pp.idx.not.enuf), " points. Computing distances for k=", kVal.tmp, " points\n", sep=""), exdent=4, indent=2))
                        
                        if (blnNoTime) {
                            pp.nn.lst <- FNN::get.knnx(data=xys.idVal, query=coordinates(lxy[["pts"]])[pp.idx.not.enuf,,drop=FALSE], k=kVal.tmp + 1, algorithm=FNN.algorithm)
                            pp.nn.df <- rbind(pp.nn.df,
                                        data.frame(pp.idx = rep(pp.idx.not.enuf, each=kVal.tmp + 1), 
                                                   nn.rank = rep(0:kVal.tmp, times=length(pp.idx.not.enuf)), 
                                                   nn.idx = idVal.idx[c(t(pp.nn.lst$nn.index))], 
                                                   tsd = c(t(pp.nn.lst$nn.dist))))
                        } else {
                            pp.nn.df <- rbind(pp.nn.df,
                                do.call(rbind, pblapply(pp.idx.not.enuf, function(i) {
                                pp.nn.lst <- FNN::get.knnx(data=data.frame(x=xys.idVal[,1], y=xys.idVal[,2], z = tsd.zvals(delta.t=lxy.dt.int[idVal.idx] - lxy.dt.int[i], sVal=sVal, type=time.term, d.bar=d.bar, tau=tau, vmax=vmax)),
                                query=cbind(coordinates(lxy[["pts"]])[i, , drop=FALSE],0), k=kVal.tmp + 1, algorithm=FNN.algorithm)
                                data.frame(pp.idx=i, nn.rank=0:kVal.tmp, nn.idx=idVal.idx[as.numeric(pp.nn.lst$nn.index)], tsd=as.numeric(pp.nn.lst$nn.dist))
                                })))

                                ## z = d.bar * sqrt(sVal * (dt.thisid.int - dt.thisid.int[i]) / tau) 
                        }
                            
                        idx.not.enuf.nn.kVal <- which(pp.nn.df$nn.rank == kVal.tmp & pp.nn.df$tsd < rVal )
                        
                    }
                    
                    ## The following confirms each point got enough nn
                    ## pp.nn.df.maxd <- do.call(rbind, pblapply(unique(pp.nn.df$pp.idx), function(x) data.frame(pp.idx=x, maxd=max(pp.nn.df[pp.nn.df$pp.idx == x,"d"]))))

                    ## Add a column of cumulative distances
                    pp.nn.d.cumsum <- unlist(lapply(split(pp.nn.df$tsd, pp.nn.df$pp.idx), cumsum))
                    pp.nn.df <- cbind(pp.nn.df, tsd.cumsum=pp.nn.d.cumsum)
                    
                } else if (mode=="Fixed-a" || mode=="Auto-a") {
                    #blnNeedAVal <- TRUE
                    if (mode=="Auto-a") {
                        ## Compute the value of a that will result in ptp perecent of points having at least nnn nearest neighbors

                        if (status) cat("  - computing auto-a...\n")
                        
                        if (a[a.idx, "meth"] == "enc") {
                            kVal.tmp <- a[a.idx, "nnn"]

                        } else if (a[a.idx, "meth"] == "nn") {
                            kVal.tmp <- a[a.idx, "nnn"]

                            ## First look to see if the existing NN has identified enuf nn to calculate auto.a
                            if (!is.null(append.idx)) {
                                if (lxy[["nn"]][[append.idx]][["kmax"]] >= kVal.tmp) {
                                    kth.nn.tsd.cumsum <- with(lxy[["nn"]][[append.idx]], nn.df[nn.df$nn.rank == kVal.tmp, "tsd.cumsum"])
                                    aVal <- kth.nn.tsd.cumsum[order(kth.nn.tsd.cumsum)][length(kth.nn.tsd.cumsum) * a[a.idx, "ptp"]]
                                    if (status) cat("  - found from existing NN set. Auto-a is", aVal, "\n")
                                    new.auto.a.only <- TRUE
                                }
                            }

                            if (!new.auto.a.only) {
                                
                                if (status) cat("  - finding nearest neighbors for k=", kVal.tmp, "...\n", sep="")

                                ## Find the nnn nearest neighbors for all points
                                if (blnNoTime) {
                                    pp.nn.lst <- FNN::get.knnx(data=xys.idVal, query=coordinates(lxy[["pts"]])[idVal.idx.sub, ,drop=FALSE], k=kVal.tmp + 1, algorithm=FNN.algorithm)
                                    pp.nn.df <- data.frame(pp.idx = rep(idVal.idx.sub, each=kVal.tmp + 1), 
                                                           nn.rank = rep(0:kVal.tmp, times=length(idVal.idx.sub)), 
                                                           nn.idx = idVal.idx[c(t(pp.nn.lst$nn.index))], 
                                                           tsd = c(t(pp.nn.lst$nn.dist)))
                                } else {
                                    pp.nn.df <- do.call(rbind, pblapply(idVal.idx.sub, function(i) {
                                        pp.nn.lst <- FNN::get.knnx(data=data.frame(x=xys.idVal[,1], y=xys.idVal[,2], z = tsd.zvals(delta.t=lxy.dt.int[idVal.idx] - lxy.dt.int[i], sVal=sVal, type=time.term, d.bar=d.bar, tau=tau, vmax=vmax)),
                                                              query=cbind(coordinates(lxy[["pts"]])[i, , drop=FALSE],0), k=kVal.tmp + 1, algorithm=FNN.algorithm)
                                        data.frame(pp.idx=i, nn.rank=0:kVal.tmp, nn.idx=idVal.idx[as.numeric(pp.nn.lst$nn.index)], tsd=as.numeric(pp.nn.lst$nn.dist))
                                        }))
                                        ## z = d.bar * sqrt(sVal * (dt.thisid.int - dt.thisid.int[i]) / tau) 
                                }
                                
                                ## Sum up the total nearest neighbor distances for each parent points, then take the ptp percentile for the value of "a"
                                pp.nn.sumd <- rowsum(pp.nn.df$tsd, pp.nn.df$pp.idx)
                                aVal <- pp.nn.sumd[order(pp.nn.sumd)][length(pp.nn.sumd) * a[a.idx, "ptp"]]
                                if (status) cat("Done. Auto-a is", aVal, "\n")
                            }
                        } else {
                            stop(paste("Method", a[a.idx, "meth"], "not supported"))
                        } 
                        
                        ## If we're appending to an existing set of nearest neighbors, see if amax < aVal
                        if (!is.null(append.idx)) {
                            if (lxy[["nn"]][[append.idx]][["amax"]] < aVal) new.auto.a.only <- FALSE
                        }
                    }   
                    #####################################
                    ## done finding aVal for mode auto-a, now find enuf nn so the last nn for all pts is >= aVal
                    #####################################

                    if (!new.auto.a.only) {

                        #######################################################################################################
                        ## Use ra.init.samp.size randomly points to find an inital value of kVal.tmp that will hopefully result in grabbing all points <=r for most other points
                        num.test.pts <- min(ra.init.samp.size, idVal.num.pts)
                        if (status) cat("  - finding an initial value of k using ", num.test.pts, " randomly selected points...", sep="")
                        kVal.tmp <- max(15, kmin)
                        idVal.idx.test <- sample(idVal.idx, num.test.pts)

                        ## A-code
                        if (blnNoTime) {
                            pp.nn.lst <- FNN::get.knnx(data=xys.idVal, query=coordinates(lxy[["pts"]])[idVal.idx.test, ,drop=FALSE], k=kVal.tmp + 1, algorithm=FNN.algorithm)
                            pp.nn.df <- data.frame(pp.idx = rep(idVal.idx.test, each=kVal.tmp + 1), 
                                                   nn.rank = rep(0:kVal.tmp, times=length(idVal.idx.test)), 
                                                   nn.idx = idVal.idx[c(t(pp.nn.lst$nn.index))], 
                                                   tsd = c(t(pp.nn.lst$nn.dist)))
                            
                            ## Add a column of cumulative distances
                            pp.nn.d.cumsum <- unlist(lapply(split(pp.nn.df$tsd, pp.nn.df$pp.idx), cumsum))
                            pp.nn.df <- cbind(pp.nn.df, tsd.cumsum=pp.nn.d.cumsum)
    
                        } else {
                            pp.nn.df <- do.call(rbind, lapply(idVal.idx.test, function(i) {
                                pp.nn.lst <- FNN::get.knnx(data=data.frame(x=xys.idVal[,1], y=xys.idVal[,2], z = tsd.zvals(delta.t=lxy.dt.int[idVal.idx] - lxy.dt.int[i], sVal=sVal, type=time.term, d.bar=d.bar, tau=tau, vmax=vmax)),
                                query=cbind(coordinates(lxy[["pts"]])[i, , drop=FALSE],0), k=kVal.tmp + 1, algorithm=FNN.algorithm)
                                data.frame(pp.idx=i, nn.rank=0:kVal.tmp, nn.idx=idVal.idx[as.numeric(pp.nn.lst$nn.index)], tsd=as.numeric(pp.nn.lst$nn.dist), 
                                           tsd.cumsum=cumsum(as.numeric(pp.nn.lst$nn.dist)))
                                }))
                        }
                        
                        ## Look at the kth (last) nearest neighbor and identify those whose cumulative d is still too small to ensure we've got all within a
                        idx.not.enuf.nn.aVal <- which(pp.nn.df$nn.rank == kVal.tmp & pp.nn.df$tsd.cumsum < aVal )

                        while (length(idx.not.enuf.nn.aVal) > 0 && kVal.tmp < idVal.num.pts - 1) {
                            pp.idx.not.enuf <- pp.nn.df[idx.not.enuf.nn.aVal, "pp.idx"]
                            idx.not.enuf.nn.all <- which(pp.nn.df$pp.idx %in% pp.idx.not.enuf)
                            ## Delete the rows where the cumulative sum is not enough, because we're going to recalculate those with a larger value of k
                            pp.nn.df <- pp.nn.df[-idx.not.enuf.nn.all, ,drop=FALSE]

                            ## Double kVal.tmp
                            kVal.old <- kVal.tmp
                            kVal.tmp <- min(c(idVal.num.pts - 1, kVal.tmp * 2))

                            ## Recalculate
                            if (blnNoTime) {
                                pp.nn.lst <- FNN::get.knnx(data=xys.idVal, query=coordinates(lxy[["pts"]])[pp.idx.not.enuf, ,drop=FALSE], k=kVal.tmp + 1, algorithm=FNN.algorithm)
                                pp.nn.df.new <- data.frame(pp.idx = rep(pp.idx.not.enuf, each=kVal.tmp + 1), 
                                                       nn.rank = rep(0:kVal.tmp, times=length(pp.idx.not.enuf)), 
                                                       nn.idx = idVal.idx[c(t(pp.nn.lst$nn.index))], 
                                                       tsd = c(t(pp.nn.lst$nn.dist)))
    
                                ## Add a column of cumulative distances for these new rows
                                pp.nn.d.new.cumsum <- unlist(lapply(split(pp.nn.df.new$tsd, pp.nn.df.new$pp.idx), cumsum))
                                pp.nn.df.new <- cbind(pp.nn.df.new, tsd.cumsum=pp.nn.d.new.cumsum)
                                
                                ## Add new rows to the rest of the records
                                pp.nn.df <- rbind(pp.nn.df, pp.nn.df.new)
                                
                            } else {
                                pp.nn.df <- rbind(pp.nn.df,
                                    do.call(rbind, lapply(pp.idx.not.enuf, function(i) {
                                    pp.nn.lst <- FNN::get.knnx(data=data.frame(x=xys.idVal[,1], y=xys.idVal[,2], z = tsd.zvals(delta.t=lxy.dt.int[idVal.idx] - lxy.dt.int[i], sVal=sVal, type=time.term, d.bar=d.bar, tau=tau, vmax=vmax)),
                                    query=cbind(coordinates(lxy[["pts"]])[i, , drop=FALSE],0), k=kVal.tmp + 1, algorithm=FNN.algorithm)
                                    data.frame(pp.idx=i, nn.rank=0:kVal.tmp, nn.idx=idVal.idx[as.numeric(pp.nn.lst$nn.index)], tsd=as.numeric(pp.nn.lst$nn.dist), 
                                    tsd.cumsum=cumsum(as.numeric(pp.nn.lst$nn.dist)))
                                    })))

                            }
                            
                            idx.not.enuf.nn.aVal <- which(pp.nn.df$nn.rank == kVal.tmp & pp.nn.df$tsd.cumsum < aVal )

                        }
                    
                        ## Find the maximum nn.rank for each point
                        pp.nn.df.nn.rank.aval <- sort(sapply(split(pp.nn.df[pp.nn.df$tsd.cumsum <= aVal, "nn.rank"], pp.nn.df[pp.nn.df$tsd.cumsum <= aVal, "pp.idx"]), max))
                        
                        ## Take the nth percentile as the starting point for kVal.tmp (plus one because we to get one more than the minimum)
                        kVal.tmp <- max(kmin, 1 + pp.nn.df.nn.rank.aval[num.test.pts * ra.init.quant])
                        ##pp.nn.df.maxnn <- do.call(rbind, lapply(unique(pp.nn.df$pp.idx), function(x) data.frame(pp.idx=x, maxnn=max(pp.nn.df[pp.nn.df$pp.idx == x,"nn.rank"]))))
    
                        if (status) cat("Done.\n  - Initial k=", kVal.tmp, "\n", sep="")
                        
                        ### Next, we need to go thru all points whose tsd.cumsum <= aVal and get additional nn as needed. 
                        if (status) cat("  - computing cumulative distances for k=", kVal.tmp, "\n", sep="")
                        
                        if (is.null(append.idx)) {
                            ## All parent points need new nearest nearest neighbors
                            idVal.idx.sub.nmnn <- idVal.idx.sub
                        } else {
                            ## Identify the parent points that already have enough points identified
                            pp.idx.enuf.nn <- with(lxy[["nn"]][[append.idx]], unique(nn.df[nn.df$tsd.cumsum>=aVal, "pp.idx"]))
                            ## Remove the parent points with enuf nn from idVal.idx.sub to get just those that need more nearest neighbors (nmnn)
                            idVal.idx.sub.nmnn <- idVal.idx.sub[!idVal.idx.sub %in% pp.idx.enuf.nn]
                        }
    
                        if (blnNoTime) {
                            pp.nn.lst <- FNN::get.knnx(data=xys.idVal, query=coordinates(lxy[["pts"]])[idVal.idx.sub.nmnn, ,drop=FALSE], k=kVal.tmp + 1, algorithm=FNN.algorithm)
                            pp.nn.df <- data.frame(pp.idx = rep(idVal.idx.sub.nmnn, each=kVal.tmp + 1),
                                                   nn.rank = rep(0:kVal.tmp, times=length(idVal.idx.sub.nmnn)), 
                                                   nn.idx = idVal.idx[c(t(pp.nn.lst$nn.index))], 
                                                   tsd = c(t(pp.nn.lst$nn.dist)))
                            
                            ## Add a column of cumulative distances
                            pp.nn.d.cumsum <- unlist(lapply(split(pp.nn.df$tsd, pp.nn.df$pp.idx), cumsum))
                            pp.nn.df <- cbind(pp.nn.df, tsd.cumsum=pp.nn.d.cumsum)
    
                        } else {
                            pp.nn.df <- do.call(rbind, pblapply(idVal.idx.sub.nmnn, function(i) {
                                pp.nn.lst <- FNN::get.knnx(data=data.frame(x=xys.idVal[,1], y=xys.idVal[,2], z = tsd.zvals(delta.t=lxy.dt.int[idVal.idx] - lxy.dt.int[i], sVal=sVal, type=time.term, d.bar=d.bar, tau=tau, vmax=vmax)),
                                                      query=cbind(coordinates(lxy[["pts"]])[i, , drop=FALSE],0), k=kVal.tmp + 1, algorithm=FNN.algorithm)
                                data.frame(pp.idx=i, nn.rank=0:kVal.tmp, nn.idx=idVal.idx[as.numeric(pp.nn.lst$nn.index)], tsd=as.numeric(pp.nn.lst$nn.dist), 
                                           tsd.cumsum=cumsum(as.numeric(pp.nn.lst$nn.dist)))
                                }))
    
                                ## z = d.bar * sqrt(sVal * (dt.thisid.int - dt.thisid.int[i]) / tau)
                        }
                                                
                        ## Look at the kth (last) nearest neighbor and identify those whose cumulative d is still too small to ensure we've got all within a
                        idx.not.enuf.nn.aVal <- which(pp.nn.df$nn.rank == kVal.tmp & pp.nn.df$tsd.cumsum < aVal )
                        
                        while (length(idx.not.enuf.nn.aVal) > 0 && kVal.tmp < idVal.num.pts - 1) {
                            pp.idx.not.enuf <- pp.nn.df[idx.not.enuf.nn.aVal, "pp.idx"]
                            idx.not.enuf.nn.all <- which(pp.nn.df$pp.idx %in% pp.idx.not.enuf)
                            ## Delete the rows where the cumulative sum is not enough, because we're going to recalculate those with a larger value of k
                            pp.nn.df <- pp.nn.df[-idx.not.enuf.nn.all, ,drop=FALSE]
                            
                            ## Double kVal.tmp
                            kVal.old <- kVal.tmp
                            kVal.tmp <- min(c(idVal.num.pts - 1, kVal.tmp * 2))
                            
                            if (status) cat(cw(paste("  - ", kVal.old, " wasn't large enough for ", length(pp.idx.not.enuf), " points. Computing cumulative distances for k=", kVal.tmp, " points\n", sep=""), exdent=4, indent=1))
                            
                            if (blnNoTime) {
                                pp.nn.lst <- FNN::get.knnx(data=xys.idVal, query=coordinates(lxy[["pts"]])[pp.idx.not.enuf, ,drop=FALSE], k=kVal.tmp + 1, algorithm=FNN.algorithm)
                                pp.nn.df.new <- data.frame(pp.idx = rep(pp.idx.not.enuf, each=kVal.tmp + 1), 
                                                       nn.rank = rep(0:kVal.tmp, times=length(pp.idx.not.enuf)), 
                                                       nn.idx = idVal.idx[c(t(pp.nn.lst$nn.index))], 
                                                       tsd = c(t(pp.nn.lst$nn.dist)))
    
                                ## Add a column of cumulative distances for these new rows
                                pp.nn.d.new.cumsum <- unlist(lapply(split(pp.nn.df.new$tsd, pp.nn.df.new$pp.idx), cumsum))
                                pp.nn.df.new <- cbind(pp.nn.df.new, tsd.cumsum=pp.nn.d.new.cumsum)
                                
                                ## Add new rows to the rest of the records
                                pp.nn.df <- rbind(pp.nn.df, pp.nn.df.new)
                                
                            } else {
                                pp.nn.df <- rbind(pp.nn.df,
                                    do.call(rbind, pblapply(pp.idx.not.enuf, function(i) {
                                    pp.nn.lst <- FNN::get.knnx(data=data.frame(x=xys.idVal[,1], y=xys.idVal[,2], z = tsd.zvals(delta.t=lxy.dt.int[idVal.idx] - lxy.dt.int[i], sVal=sVal, type=time.term, d.bar=d.bar, tau=tau, vmax=vmax)),
                                    query=cbind(coordinates(lxy[["pts"]])[i, , drop=FALSE],0), k=kVal.tmp + 1, algorithm=FNN.algorithm)
                                    data.frame(pp.idx=i, nn.rank=0:kVal.tmp, nn.idx=idVal.idx[as.numeric(pp.nn.lst$nn.index)], tsd=as.numeric(pp.nn.lst$nn.dist), 
                                    tsd.cumsum=cumsum(as.numeric(pp.nn.lst$nn.dist)))
                                    })))

                            }
                            
                            idx.not.enuf.nn.aVal <- which(pp.nn.df$nn.rank == kVal.tmp & pp.nn.df$tsd.cumsum < aVal )
                        }
                    
                    }   #if blnNeedAVal
                }  # if mode==____ or _____ 
                
                
                ## if its new.auto.a.only, then just change 
                if (new.auto.a.only) {
                    ## No new nn were identified, all we need to do is to append a record to lxy[["nn"]]$auto.a.df
                    lxy[["nn"]][[append.idx]][["auto.a.df"]] <- unique(rbind(lxy[["nn"]][[append.idx]][["auto.a.df"]],
                        data.frame(meth = a[a.idx, "meth"], ptp = a[a.idx, "ptp"], nnn = a[a.idx, "nnn"],
                                   tct = a[a.idx, "tct"], aVal = aVal)))

                        if (status) cat("  - auto.a value added to: ", names(lxy[["nn"]])[append.idx], "\n", sep="")
                
                } else {
                
                    ## Append new points found if needed
                    if (!is.null(append.idx)) {
                        
                        ## To uniquely identify the new rows, create a key consisting of pp.idx and nn.rank
                        pp.nn.df.key <- paste(pp.nn.df[, "pp.idx"], pp.nn.df[, "nn.rank"], sep=".")
                        lxy.nn.key <- paste(lxy[["nn"]][[append.idx]]$nn.df[, "pp.idx"], lxy[["nn"]][[append.idx]]$nn.df[, "nn.rank"], sep=".")
                        
                        ## Append the new rows that don't already exist
                        pp.nn.df.new.nn.idx <- which(!is.element(pp.nn.df.key, lxy.nn.key))
                        pp.nn.df <- rbind(lxy[["nn"]][[append.idx]]$nn.df, pp.nn.df[pp.nn.df.new.nn.idx, ,drop=FALSE])
                        
                        ## Sort the merged data frame by pp.idx and then nn.rank
                        pp.nn.df <- pp.nn.df[order(pp.nn.df$pp.idx, pp.nn.df$nn.rank), ,drop=FALSE]
                        
                        auto.a.df <- lxy[["nn"]][[append.idx]]$auto.a.df
                        
                        ## If no new points were added, grab the existing values for kmax, rmax, and amax, because we don't want to recalculate those
                        if (length(pp.nn.df.new.nn.idx)==0) {
                            kmax <- lxy[["nn"]][[append.idx]][["kmax"]]
                            rmax <- lxy[["nn"]][[append.idx]][["rmax"]]
                            amax <- lxy[["nn"]][[append.idx]][["amax"]]
                        }
                        
                        ## Now we can delete the existing list element, we will add the appended one down below
                        lxy[["nn"]] <- lxy[["nn"]][-append.idx]
                        
                        if (status) cat("  - ", length(pp.nn.df.new.nn.idx), " additional neighbors appended \n", sep="")
                        
                    }
                    
                    ## Add a row of this auto-a value to $auto.a.df
                    if (mode=="Auto-a") {
                        auto.a.df <- rbind(auto.a.df, data.frame(meth = a[a.idx, "meth"], 
                                                                 ptp = a[a.idx, "ptp"],
                                                                 nnn = a[a.idx, "nnn"],
                                                                 tct = a[a.idx, "tct"],
                                                                 aVal = aVal))
                    }
    
                    if (is.null(append.idx) || length(pp.nn.df.new.nn.idx) > 0) {
                        ## Come up with values of kmax, rmax, and amax
                        if (status) {
                            if (status) cat("  - computing values of kmax, rmax, and amax...")
                            flush.console()
                        }
                        kmax <- min(unlist(lapply(split(pp.nn.df$nn.rank, pp.nn.df$pp.idx), max)))
                        rmax <- round(min(unlist(lapply(split(pp.nn.df$tsd, pp.nn.df$pp.idx), max))), dec.places)
                        amax <- round(min(unlist(lapply(split(pp.nn.df$tsd.cumsum, pp.nn.df$pp.idx), max))), dec.places)
                        if (status) cat("Done \n")
                    }
    
                    ## Come up with a name for this list element in the form
                    ##   id | s | npp | kmax | rmax | amax
                    ##   "ag208|0|8254|15|2|15.3005630797458"
    
                    nn.name <- paste(idVal, "|", time.term, "|s", sVal, "|n", length(idVal.idx.sub), "|kmax", kmax, "|rmax", rmax, "|amax", amax, sep="")
                    
                    
                    
                    lxy[["nn"]][[nn.name]] <- list(id=idVal, time.term=time.term, kmax=kmax, rmax=rmax, amax=amax, s=sVal, ptid=lxy[["pts"]][["ptid"]][idVal.idx.sub], auto.a.df=auto.a.df,
                                        time.taken = difftime(Sys.time(), start.time.this.run, units="sec"), nn.df=pp.nn.df)
                    
                    nn.name.added <- c(nn.name.added, nn.name)
                    if (status) cat("  - set of neighbors (re)named: ", nn.name, "\n", sep="")
                }
                
            }   ## if (blnCont)
        
        }
        }
        }
        }
    }    ## for (idVal in id)
    
    
    time.taken = difftime(Sys.time(), start.time, units="auto")

    if (status) {
        cat("\nDone.")
        if (!is.null(nn.name.added)) {
            cat(" Nearest neighbor set(s) created / updated: \n", paste("  ", nn.name.added, collapse="\n", sep=""), sep="")
        } else {
            cat("\n")
        }
        cat("\nTotal time:", round(time.taken,1), units(time.taken), "\n", sep = " ")
    }
    
    if (beep) {
        flush.console()
        for (i in 1:3) {
            alarm()
            Sys.sleep(0.8)
        }
    }
        
    return(lxy)
}
