hsp.col.spiral <-
function(x,y, hue.offset=0, sat.base=0.5, val.base=0.9, center.method=c("bbox","mean")[1]) {

    ## Returns a vector of color values based on where each point falls in the plane relative to the center of a scatter plot of x and y
    ## It does this by overlaying a HSV color wheel over the 'center' of the data splot, where the center is either defined by the bounding box or data mean
    ## Color values go from a base saturation value at the center to full saturation at the edges, and base value up to value=1 (no black)
    
    ## hue.offset is a value in radians [0..2*pi] for the color wheel rotation, which controls the color of points directly to the right of the center
    ## sat.base [0..1] is the base staturation (how much of the color is present) of the center color (s=0.4 gives pastels), ramps up to 1 (full color) at edges
    ## val.base [0..1] controls how much black is in the center color (0 is total black), ramps out to no blackness at the edges
    
    if (is.null(hue.offset)) hue.offset <- 0
    if (is.null(sat.base)) sat.base <- 0.5
    if (is.null(val.base)) val.base <- 0.9

    ## If x or y are dates, convert them to seconds
    if (is(x, "POSIXt")) x <- as.numeric(x)
    if (is(y, "POSIXt")) y <- as.numeric(y)
    
    ## Scale x and y to 0..1 so they're on similar scales
    x.dist <- (x - min(x)) / (max(x) - min(x))
    y.dist <- (y - min(y)) / (max(y) - min(y))

    ## Calculate the distance of each point to the center
    if (center.method == "bbox") {
        ctr.dist <- cbind(mean(range(x.dist)), mean(range(y.dist)))
    } else if (center.method == "mean") {
        ctr.dist <- cbind(mean(x.dist), mean(y.dist))
    } else {
        stop(paste("Unkown value for center.method: ", center.method, sep=""))
    }
    

    dist <- sqrt((y.dist - ctr.dist[2])^2 + (x.dist - ctr.dist[1])^2)
    
    ## Calculate the angle of each point on a scale from 0..2*pi
    theta <- (atan2(y.dist - ctr.dist[2], x.dist - ctr.dist[1]) + hue.offset + (2*pi)) %% (2*pi)
    hue <- theta / (2*pi)
    
    sat <- sat.base + (1-sat.base) * (dist / max(dist))
    v <- val.base + (1 - val.base) * (dist / max(dist))
    
    res <- hsv(h=hue, s=sat, v=v)
    
    return(res)

}
