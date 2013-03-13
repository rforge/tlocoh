
multi.proportion.col <- function(pmat, prop.all=NULL, ps.coords=FALSE, testme=FALSE, label.pt.exp=1.1) {

    # pmat is a proportion matrix
    
    # label.pt.exp is the expansion factor for the label locations returned (at the end of each axis)
    # used for making a legend

    # ps.coords - return coordinates of multi-color proportion space

    ## Given a set of proportions and the total number of proportions, will return a vector of colors

    if (is.data.frame(pmat)) {
        pmat <- as.matrix(pmat)
    } else if (!is.matrix(pmat)) {
        stop("pmat must be a matrix or a data frame")
    }
    
    twopi <- 2*pi

    if (max(pmat) > 1 || min(pmat) < 0) stop("Proportions must be between 0 and 1")
    if (is.null(prop.all)) {
        prop.all <- rep(1/ncol(pmat), ncol(pmat))
    } else {
        if (ncol(pmat) != length(prop.all)) stop("Length of prop.all must equal number of columns")
        if (sum(prop.all) != 1) stop("prop.all should add up to 1")
    }

    # Define the angle between adjacent axes
    theta.int <- twopi / ncol(pmat)
    theta <- (1:ncol(pmat) - 1) * theta.int

    ## Compute the magnitude at the tip of each axis
    theta.mag <- 1 / prop.all 
    

    # Define the nodes of the polygon
    nodes.mat <- matrix(c(theta.mag * cos(theta), theta.mag * sin(theta)), ncol=2, byrow=FALSE)

    ## For each node, compute the angle from the origin to the next node
    v1.mat <- nodes.mat
    v2.mat <- cbind(nodes.mat[,2] - nodes.mat[c(2:length(theta),1),2], nodes.mat[c(2:length(theta),1),1] - nodes.mat[,1])
    theta.afo <- asin(sapply(1:length(theta), function(i) -crossprod(v2.mat[i,,drop=T], v1.mat[i,,drop=T]) / (sqrt(sum( v1.mat[i,,drop=T] ^2)) * sqrt(sum(v2.mat[i,,drop=T]^2)))))
    
    if (testme) {
        ## Plot the boundary of the envelope
        plot(v1.mat[c(1:length(theta),1), ], type="b", col="gray20", asp=1)
        abline(h=0, v=0, col="gray80")
        for (i in 1:length(theta)) points(rbind(c(0,0),v1.mat[i,,drop=T]), type="l", col="red")

        ## Plot the points along the outside edges
        alpha <- seq(from=0, to=2*pi, by=2*pi/45)
        alpha.prev.theta.idx <- findInterval(alpha, theta)
        alpha.dist.to.edge <- sin(theta.afo[alpha.prev.theta.idx]) * theta.mag[alpha.prev.theta.idx] / sin(pi - theta.afo[alpha.prev.theta.idx] - (alpha - theta[alpha.prev.theta.idx]))
        xs <- alpha.dist.to.edge * cos(alpha); ys <- alpha.dist.to.edge * sin(alpha)
        points(xs, ys, col="blue")
        print("hows that");browser()
    }

    # Convert the set of proportion to an xy value in color space by mapping it to each axes
    csx <- apply(pmat, 1, function(x) sum(x * cos(theta) / prop.all))
    csy <- apply(pmat, 1, function(x) sum(x * sin(theta) / prop.all))

    ## Compute the values in proportion space if we need to return those
    if (ps.coords) {
        ps.coords.mat <- matrix(data=c(psx=apply(pmat, 1, function(x) sum(x * cos(theta))), psy=apply(pmat, 1, function(x) sum(x * sin(theta)))), ncol=2, byrow=FALSE)
        ps.nodes <- matrix(c(label.pt.exp * cos(theta), label.pt.exp * sin(theta)), ncol=2, byrow=FALSE)
    } else {
        ps.coords.mat <- NULL
        ps.nodes <- NULL
    }

    ## Find the color space angle from x-axis (0..2*pi)
    alpha <- (atan2(csy, csx) + twopi) %% twopi

    ## For each alpha, find the maximum magnitude (used below to assign a saturation value)
    alpha.prev.theta.idx <- findInterval(alpha, theta)
    alpha.dist.to.edge <- sin(theta.afo[alpha.prev.theta.idx]) * theta.mag[alpha.prev.theta.idx] / sin(pi - theta.afo[alpha.prev.theta.idx] - (alpha - theta[alpha.prev.theta.idx]))

    # Old technique - linear interpolation - bad
    #cs.max.mag.this.theta <- 1/prop.all[prev.theta.idx] + (1/prop.all[1 + prev.theta.idx %% length(theta)] - 1/prop.all[prev.theta.idx]) * ((csxy.theta - theta[prev.theta.idx]) / theta.int)

    ## Compute the magnitude of each set of proportions
    ## We autoamtically take the min with max possible length to deal with rounding errors
    csxy.mag <- pmin((csx^2 + csy^2)^0.5, alpha.dist.to.edge)

    ## Create the color values based on the angle from the origin and the magnitude.
    vmid <- 0.6
    return(list(col=hsv(h=alpha / twopi, s=csxy.mag/alpha.dist.to.edge, v=vmid+(csxy.mag/alpha.dist.to.edge*(1-vmid))), ps.coords=ps.coords.mat, ps.nodes=ps.nodes))
    
}

pm.legend.plot <- function(pm=NULL, incr=10, prop.all=NULL, lhs=NULL, hs.idx=1, plotme=TRUE, cex.xlim=1, cex.ylim=1, mar=c(0.5,0.5,0.5,0.5), rotate=pi/2, prop.names=NULL, testme=FALSE, show.origin=FALSE, show.colnames=TRUE, cex.lbl=1.5, label.pt.exp=1.1) {

    ## This will create a proportion matrix (if needed), which is a positive matrix of proportions where the sum of each row equals 1
    ## It will then project these proportions in a two-dimensional color space for a legend
    ## prop.all is an optional vector with length equal to the number of columns in pm containing the total proportions of each column
    ## It can be used to 'offset' the center at which the proportions are considered 'equal'
    
    ## If lhs is not null, then prop.all and prop.names will taken from lhs pep hull metric
    ## cex.xlim and cex.ylim will expand the range of the x and y axis (to make more room for large labels). Note that
    ## the lower bound of x and y lim probably negative so you want these to > 1 to make more room

    ## Get the proportion matrix
    if (is.null(pm)) pm <- prop.mat(incr)

    if (!is.null(lhs)) {
        xtab.df <- data.frame(as.list(table(lhs[[hs.idx]][["hulls"]]@data[[lhs[[hs.idx]][["hm"]][which(sapply(lhs[[hs.idx]][["hm"]], function(x) x$type == "pep"))[1]][[1]][["aux"]][["pep.var"]]]])))
        oid.vals <- as.character(sapply(lhs[[hs.idx]][["hm"]][sapply(lhs[[hs.idx]][["hm"]], function(x) x$type == "pep")], function(x) x$aux$pep.val))
        if (length(oid.vals) != ncol(pm)) stop("The number of original id values in lhs is not the same as the number of cols in pm")
        prop.all <- as.numeric(xtab.df[oid.vals  ] / sum(xtab.df[oid.vals ]))
        prop.names <- oid.vals
    }

    mpc <- multi.proportion.col(pm, ps.coords=T, prop.all=prop.all, testme=testme, label.pt.exp=label.pt.exp)

    ##pts <- do.call(rbind, pblapply(1:nrow(prop.mat), function(i) data.frame(tri.col.one(prop.mat[i,]))))
    if (plotme) {
        if (is.null(mpc[["ps.coords"]])) stop("ps.coords must be TRUE to plot")
        if (rotate > 0) {
            rot.mat <- -1 * matrix(data=c(cos(rotate), sin(rotate), -sin(rotate), cos(rotate)), ncol=2, byrow=FALSE)
            mpc[["ps.coords"]] <- mpc[["ps.coords"]] %*% rot.mat
            mpc[["ps.nodes"]] <- mpc[["ps.nodes"]] %*% rot.mat
        }
        
        if (show.colnames) {
            if (is.null(prop.names)) {
                if (is.null(colnames(pm))) {
                    stop("Can't show proportion names. Please provide prop.names or a matrix with column names")
                } else {
                    prop.names <- colnames(pm)
                }
            } else {
                if (length(prop.names) != ncol(pm)) stop("prop.names must be the same length as the number of columns in pm")
            }
        }

        if (show.colnames) {
            coords.for.ext <- "ps.nodes"
        } else {
            coords.for.ext <- "ps.coords"
        }
        
        par(mar=mar)
        plot(NULL, xlim=range(mpc[[coords.for.ext]][,1,drop=T])*cex.xlim, ylim=range(mpc[[coords.for.ext]][,2,drop=T])*cex.ylim, asp=1, axes=F, xlab="", ylab="")
        points(mpc[["ps.coords"]], col=mpc[["col"]], pch=16)
        if (show.origin) abline(h=0, v=0, col="gray80")
        if (show.colnames && !is.null(colnames(pm))) {
            text(mpc[["ps.nodes"]], labels=prop.names, cex=cex.lbl)
        }
    }

}

prop.mat <- function(incr=10) {

  # Need a version of this where you can specify the number of columns also

  # Returns a matrix of proportions, such that the sum of each row is equal to 1
  # Number of rows / detail is determined by incr which should be 1..25

  res <- NULL
  for (p1 in seq(from=0, to=100, by=incr)) {
      for (p2 in seq(from=0, to=100-p1, by=incr)) {
          #for (p3 in seq(from=100-p1-p2, to=100, by=incr)) {
              res <- rbind(res, c(p1, p2, 100-p1-p2))
          #}
      }
  }
  colnames(res) <- paste("p", 1:3, sep="")

  return(res / 100)


}
