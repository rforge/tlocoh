#' Compute s vs. proportion-time-selected hulls table
#'
#' Identify the values of s that result in proportion p of hulls being time-selected
#'
#' @param lxy A \link{LoCoH-xy} object
#' @param id The name(s) of individuals to analyze
#' @param k Value for the k-method that will be used to create hulls for the sample points (and then used to find the proportion of time-selected hulls)
#' @param n Number of randomly selected points to use as samples (ignored if \code{samp.idx} is passed)
#' @param samp.idx A vector of indices of the points to use as samples
#' @param sinit The initial value of \emph{s} that will be used to start the iteration to find that value of \emph{s} that produces a proportion of time selected hulls >= ptsh.max
#' @param ptsh.target A vector of values 0..1 that will serve as the target \emph{ptsh} values. For each ptsh target, script will 
#' try to find a value of \emph{s} that generates the targeted proportion of time selected hulls (within \code{ptsh.buf}) 
#' @param ptsh.max The highest value of ptsh for which a value of \emph{s} will be computed. It is generally recommended that this be less than 1.
#' @param ptsh.buf The level of accuracy or precison to which the found values of 's' return the targetted ptsh.
#' @param max.iter The maximum number of times the script will double sinit in an effort to find the value of s that produces ptsh.max
#' @param max.loops The maximum number of intermediate values of s the script will try when 'zooming' in on the target ptsh levels
#' @param time.term The space-time transformation to use in the TSD distance metric: \code{'vmax'} for the maximum velocity 
#' transformation (default) or \code{'dif'} for the diffusion transformation.
#' @param FNN.algorithm  The algorithm to be used in the get.knnx() function in the package FNN
#' @param use.nn Whether to use saved nearest neighbor sets for the ptsh computations. T/F
#' @param plotme Whether to plot the ptsh vs. s curve. T/F
#' @param save Whether to save the ptsh-s table in the LoCoH-xy object. T/F
#' @param nn.add Whether to also identify and save nearest neighbor sets for all points for \emph{k}=\code{k} and the values of \emph{s} identified. T/F
#' @param use.pb.n The sample size (i.e., \code{n}) above which a progress bar will be displayed 
#' @param ptsh.exists What to do if there is already a ptsh table in the LoCoH-xy object: \code{'replace'} or \code{'append'}
#' @param beep Whether to beep when done. T/F
#' @param status Display status messages. T/F
#' 
#' @details When \emph{s}=0, nearest neighbors are space-selected (i.e., point-to-point distance is merely 
#' Euclidean distance). For large values of \emph{s}, nearest neighbors are effectively time-selected (e.g., 
#' temporally contiguous). One approach to selecting a value of \emph{s} between these two ends of 
#' the spectrum is to pick a \emph{s} value that returns an intermediate proportion of 
#' time-selected-hulls representing the desired balance between space and time selection 
#' (which in turn is shaped by the research question / objective). This function will find the 
#' value(s) of \emph{s} that produce a desired proportion of time-selected-hulls using a randomly-selected
#' subset of points. A general rule of thumb is to pick a value of \emph{s} that results in a proportion of time-selected hulls
#' from 0.4 to 0.6.
#' 
#' The script iteratively tries a variety of \emph{s} values until it finds one that produces the desired 
#' proportion of time-selected hulls within a tolerance of \code{ptsh.buf}. If for example the vector of target 
#' values \code{ptsh.target} includes 0.5, and \code{ptsh.buf = 0.01}, the script will try to find a 
#' value of \emph{s} that results in a proportion of time-selected hulls between 0.49 and 0.51.
#' 
#' @seealso \code{\link{lxy.nn.add}}
#'
#' @export
#' @import pbapply sp

lxy.ptsh.add <- function(lxy, id=NULL, k=10, n=200, samp.idx=NULL, sinit=0.005, ptsh.target=1:9/10, ptsh.max=0.98, ptsh.buf=0.01, max.iter=15,
                   max.loops=10, time.term=c("vmax", "dif")[1], FNN.algorithm = c("cover_tree", "kd_tree", "VR", "CR")[2], use.nn=FALSE,
                   plotme=TRUE, save=TRUE, nn.add=FALSE, use.pb.n=200, ptsh.exists=c("replace", "append")[2], beep=FALSE, status=TRUE) {

    ## Future enhancements
    ## 1) Option to compute ptsh from saved nearest neighbor sets   *** top priority ***
    ## 2) Option to compute ptsh for r and a methods

    if (!inherits(lxy, "locoh.lxy")) stop("lxy should be of class \"locoh.lxy\"")
    if (!is.null(lxy[["xys"]])) stop("Old data structure detected. Fix with lxy.repair()")
    
    if (is.null(lxy[["pts"]][["dt"]])) stop("No time-stamps found in lxy")
    if (use.nn && is.null(lxy[["nn"]])) stop("No nearest-neighbor sets found")
    if (ptsh.max > 1) stop("ptsh.max should not be greater than 1")
    if (min(ptsh.target) < 0 || max(ptsh.target) > ptsh.max) stop("ptsh.target should be between 0 and ptsh.max")
    if (!FNN.algorithm %in% c("cover_tree", "kd_tree", "VR", "CR")) stop(paste(FNN.algorithm, " is not a valid value", sep=""))
    if (time.term != "vmax") stop("Sorry, that value for time.term not yet supported")
    if (!ptsh.exists %in% c("replace", "append")) stop("Unknown value for ptsh.exists")
    if (!plotme && !save && !nn.add) stop("Nothing to do. Set plotme, save, or nn.add to TRUE")

    ## Error check parameter values
    if (is.null(id)) {
        id <- levels(lxy[["pts"]][["id"]])
    } else {
        if (FALSE %in% (id %in% levels(lxy[["pts"]][["id"]]))) stop(paste("Can't find the following ID(s) in lxy: ", paste(id[!(id %in% levels(lxy[["pts"]][["id"]]))], collapse=", ", sep=""), sep=""))
    }

    #max.tries.getting.close.to.ptsh.target <- 5
    lxy.dt.int <- as.numeric(lxy[["pts"]][["dt"]])
    
    res <- list()

    ## Starting the first of the mother of all nested loops
    for (idVal in id) {
        cat(idVal, "\n")
        idVal.idx <- which(lxy[["pts"]][["id"]] == idVal)

        if (save) {
            if (is.null(lxy[["ptsh"]])) lxy[["ptsh"]] <- list()
            if (is.null(lxy[["ptsh"]][[idVal]])) lxy[["ptsh"]][[idVal]] <- list()
        }

        start.time = Sys.time()

        if (use.nn) {
        
            ## Get the indices of the nearest neighbor sets affiliated with this idVal
            nn.idVal.idx <- which(sapply(lxy[["nn"]], function(x) x[["id"]]==idVal))
            
            ## Loop through these nn sets and construct a matrix of s and ptsh
            s.ptsh <- NULL
            cat("Computing ptsh for each saved nearest neighbor set \n")
            pb <- txtProgressBar(min=0, max=length(nn.idVal.idx), style = 3)
            for (i in 1:length(nn.idVal.idx)) {
                setTxtProgressBar(pb, i)
            
                idx <- nn.idVal.idx[[i]]
                
                if (lxy[["nn"]][[idx]][["kmax"]] < k) stop("Insufficient number of nearest neighbors")
                good.rows.idx <- lxy[["nn"]][[idx]][["nn.df"]][["nn.rank"]] <= k
                
                nn.lst <- with(lxy[["nn"]][[idx]][["nn.df"]][good.rows.idx, c("pp.idx","nn.idx")], split(nn.idx, pp.idx))
                ptsh.cur <- sum(sapply(nn.lst, function(x) max(diff(sort(match(x, idVal.idx))))) == 1)  / length(nn.lst)
                s.ptsh <- rbind(s.ptsh, c(lxy[["nn"]][[idx]][["s"]], ptsh.cur))
            }
            close(pb)
            
            s.ptsh <- s.ptsh[order(s.ptsh[,2]), , drop=FALSE]
            res.idVal <- list(id=idVal, samp.idx=idVal.idx, n=length(idVal.idx), k=k, target.ptsh=signif(s.ptsh[,2],2), target.s=s.ptsh[,1], s.ptsh=s.ptsh, time.taken=difftime(Sys.time(), start.time, units="secs"))            
        
        } else {
        
            ## Find the ptsh using a sample of the points        
    
            ## Identify the indices of the points for this animal
            
            xys.idVal <- coordinates(lxy[["pts"]])[idVal.idx,]
            
            vmax <- lxy[["rw.params"]][lxy[["rw.params"]][["id"]]==idVal, "vmax"]
            if (is.null(vmax)) stop("vmax not found, please run lxy.repair")

            nn.info <- do.call(rbind, lapply(lxy[["nn"]], function(x) data.frame(id=x$id, s=x$s, kmax=x$kmax)))
            nn.idx <- which(nn.info[["s"]]==0 & nn.info[["id"]]==idVal & nn.info[["kmax"]] >= k)
            
            if (length(nn.idx)==0) {
                ## Find nearest neighbors s=0 for the whole dataset (we'll need to do this anyway)
                if (status) cat("  Finding ", k, " nearest neighbors for s=0...", sep=""); flush.console()
                lxy <- lxy.nn.add(lxy, id=idVal, k=k, s=0, status=FALSE)
                if (status) cat("Done\n")
            }
            
            ## Get ptsh for s=0
            nn.info <- do.call(rbind, lapply(lxy[["nn"]], function(x) data.frame(id=x$id, s=x$s, kmax=x$kmax)))
            nn.idx <- which(nn.info[["s"]]==0 & nn.info[["id"]]==idVal & nn.info[["kmax"]] >= k)
            ## nn.idx <- with(nn.info, which(s==0 & id==idVal & kmax >= k))
            if (length(nn.idx)==0) stop("Can't find the nearest neighbor set for s=0")
    
            ## Compute the ptsh for s=0
            nn.df <- lxy[["nn"]][[nn.idx]][["nn.df"]][  lxy[["nn"]][[nn.idx]][["nn.df"]][["nn.rank"]] <=k, c("pp.idx","nn.idx", "nn.rank") ]
            nn.lst <- split(nn.df[["nn.idx"]], nn.df[["pp.idx"]])
            ptsh.cur <- sum(sapply(nn.lst, function(x) max(diff(sort(match(x, idVal.idx))))) == 1)  / length(nn.lst)
    
            ## Create a matrix to hold the results
            s.ptsh <- matrix(c(0, ptsh.cur), ncol=2)
            colnames(s.ptsh) <- c("s", "ptsh")
    
            ## Pick sample for the first run
            if (is.null(samp.idx)) {
                samp.idx <- sample(idVal.idx, min(n, length(idVal.idx)))
                cat("  Selected ", n, " points at random \n", sep=""); flush.console()
            } else {
                n <- length(samp.idx)
                cat("  Using ", n, " samples from passed value of samp.idx \n", sep=""); flush.console()
            }
            cat("  Finding ", k, " nearest neighbors for ", n, " sample points \n", sep=""); flush.console()
    
            use.pb <- (n > use.pb.n)
            post.sequals.str <- if (use.pb) "\n" else ", "
            pbo.orig <- pboptions(type = if (use.pb) "txt" else "none")
            on.exit(pboptions(pbo.orig))
            con.width <- getOption("width")
            
            ## First thing to do is to increase s until we ptsh >= ptsh.max
            cat("  Finding s for ptsh=", ptsh.max, "\n", sep=""); flush.console()
            if (!use.pb) cat("  ")
            count.int <- 0
            sVal.cur <- sinit / 2
            sequals.len <- 2
            
            while (ptsh.cur < ptsh.max) {
                count.int <- count.int + 1
                if (count.int > max.iter) {
                    cat(" - reached the maximum iterations and still haven't reached ptsh.max=", ptsh.max, "\n")
                    cat(" - resetting ptsh.max to ", ptsh.cur, "\n")
                    ptsh.max <- ptsh.cur
                    ptsh.target <- ptsh.target[ptsh.target <= ptsh.max]
                    break
                }
                
                sVal.cur <- sVal.cur * 2
    
                ## Prepare the feedback for the console
                sequals.str <- paste("s=", sVal.cur, post.sequals.str, sep="")
                if (!use.pb) {
                    sequals.len <- sequals.len + nchar(sequals.str)
                    if (sequals.len > con.width) {
                        cat("\n  ")
                        sequals.len <- 2
                    }
                }
                cat(sequals.str); flush.console()
    
                ## Find nn for sVal.cur
                nn.lst <- pblapply(samp.idx, function(i) idVal.idx[as.numeric(FNN::get.knnx(data=data.frame(x=xys.idVal[,1], y=xys.idVal[,2], z=tsd.zvals(delta.t=lxy.dt.int[idVal.idx] - lxy.dt.int[i], sVal=sVal.cur, type="vmax", d.bar=NULL, tau=NULL, vmax=vmax)),
                                          query=cbind(coordinates(lxy[["pts"]])[i, , drop=F], 0), k=k+1, algorithm=FNN.algorithm)[["nn.index"]])])
    
                ptsh.cur <- sum(sapply(nn.lst, function(x) max(diff(sort(match(x, idVal.idx))))) == 1)  / length(nn.lst)
                s.ptsh <- rbind(s.ptsh, c(sVal.cur, ptsh.cur))
    
            }
            if (!use.pb) cat("\n  ")
            
            
            for (ptshVal in sort(ptsh.target, decreasing=FALSE)) {
                cat("Finding s for ptsh=", ptshVal, " (+/- ", ptsh.buf, ")\n", sep=""); flush.console()
                if (!use.pb) cat("  ")
                sequals.len <- 2
                count.int <- 0
    
                while (count.int <= max.loops && sum(s.ptsh[,"ptsh"] >= (ptshVal - ptsh.buf) & s.ptsh[,"ptsh"] <= (ptshVal + ptsh.buf))==0) {
    
                    ## Increment counter, we're only going to do this so many times
                    count.int <- count.int + 1
    
                    ## Want to trap if we get stuck
                    sVal.cur.old <- sVal.cur
    
                    ## Sort s.ptsh by ptsh and then s, because we want to find the sVal above and below the target ptsh
                    s.ptsh <- s.ptsh[order(s.ptsh[,2], s.ptsh[,1]),]
                    base.idx <- findInterval(ptshVal, s.ptsh[,2])
                    
                    if (base.idx == 0) {
                        ## ptshVal is less than ptsh when s=0, so no chance of getting that level
                        break
                    } else if (base.idx == nrow(s.ptsh)) {
                        ## ptshVal is more than ptsh.max, we won't get closer
                        break
                    } else {
                        sVal.cur <- mean(s.ptsh[0:1+base.idx,1])
                        
                        if (isTRUE(all.equal(sVal.cur, sVal.cur.old))) {
                            int.converge.counter <- 0
                            
                            ## We have to use the isTRUE function because s is not an integer
                            while (TRUE %in% sapply(s.ptsh[,1], function(x) isTRUE(all.equal(sVal.cur, x)))) {
                                int.converge.counter <- int.converge.counter + 1
                                if (int.converge.counter > 6) {
                                    cat("Failing to find a value of s which ptsh=", ptshVal, "\n"); browser()
                                    break
                                }
                                sVal.dup.idx <- which(sapply(s.ptsh[,1], function(x) isTRUE(all.equal(sVal.cur, x))))
                                if (sVal.dup.idx==0 || sVal.dup.idx==nrow(s.ptsh)) {
                                    break
                                } else if (s.ptsh[sVal.dup.idx, 2] > ptshVal) {
                                    sVals.to.avg.idx <- c(base.idx, sVal.dup.idx)
                                } else {
                                    sVals.to.avg.idx <- c(base.idx + 1, sVal.dup.idx)
                                }
    
                                sVal.cur <- mean(s.ptsh[sVals.to.avg.idx,1])
                                
                            }
    
                        }
    
                        ## Prepare the feedback for the console
                        sequals.str <- paste("s=", sVal.cur, post.sequals.str, sep="")
                        if (!use.pb) {
                            sequals.len <- sequals.len + nchar(sequals.str)
                            if (sequals.len > con.width) {
                                cat("\n  ")
                                sequals.len <- 2
                            }
                        }
                        cat(sequals.str); flush.console()
    
                        ## Find nn for sVal.cur
                        nn.lst <- pblapply(samp.idx, function(i) idVal.idx[as.numeric(FNN::get.knnx(data=data.frame(x=xys.idVal[,1], y=xys.idVal[,2], z=tsd.zvals(delta.t=lxy.dt.int[idVal.idx] - lxy.dt.int[i], sVal=sVal.cur, type="vmax", d.bar=NULL, tau=NULL, vmax=vmax)),
                                                  query=cbind(coordinates(lxy[["pts"]])[i, , drop=F], 0), k=k+1, algorithm=FNN.algorithm)[["nn.index"]])])
    
                        ptsh.cur <- sum(sapply(nn.lst, function(x) max(diff(sort(match(x, idVal.idx))))) == 1)  / length(nn.lst)
                        s.ptsh <- rbind(s.ptsh, c(sVal.cur, ptsh.cur))
    
                    }
    
                
                }
                if (!use.pb && count.int > 0) cat("\n  ")
                
                if (count.int > max.loops && sum(s.ptsh[,"ptsh"] >= (ptshVal - ptsh.buf) & s.ptsh[,"ptsh"] <= (ptshVal + ptsh.buf))==0) {
                    cat("count.int reached ", count.int, " but still could not find a value of s that worked \n"); browser()
                }
                
            }
            if (!use.pb) cat("\n")
    
            ## Sort matrix by s
            s.ptsh <- s.ptsh[order(s.ptsh[,1]),]
    
            ptsh.target.all <- sort(unique(c(0, ptsh.target, ptsh.max)))
            magic.s <- s.ptsh[FNN::get.knnx(s.ptsh[,2], query=ptsh.target.all, k=1)[["nn.index"]],1]
            res.idVal <- list(id=idVal, samp.idx=samp.idx, n=n, k=k, target.ptsh=ptsh.target.all, target.s=magic.s, s.ptsh=s.ptsh, time.taken=difftime(Sys.time(), start.time, units="secs"))



        }

        cat("Done with ", idVal, "\n", sep="")
        
        if (plotme) {
            plot(s.ptsh, type="l", main=paste("s vs. ptsh\n", idVal, ", n=", n, sep=""))
            points(s.ptsh, pch=20, cex=1)
            abline(v=pretty(c(0,max(s.ptsh[,1])), n=15), lty=3, col="gray", lwd=0.1)
        }

        if (save) lxy[["ptsh"]][[idVal]][[length(lxy[["ptsh"]][[idVal]]) + (ptsh.exists == "append" || length(lxy[["ptsh"]][[idVal]])==0)]] <- res.idVal
        
        if (nn.add) lxy <- lxy.nn.add(lxy, id=idVal, k=k, s=magic.s)

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

