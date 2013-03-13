#' Returns a preset for a group of indices for hull metric auxillary parameters
#'
#' @export

hmap.grp <- function(n=c(3,5,7)[2], hlim=c(0.6667, 0.6667), slim=c(0.1225, 1), vlim=c(0.96, 1)) {

    if (!n %in% c(3,5,7)) stop("n must be 3, 5, or 7")

    lim <- floor(n/2)
    kernal.idx <- seq(from=-lim, to=lim)
    ord <- match(unique(as.numeric(t(do.call(rbind, lapply(lim:0, function(i) i * c(-1,1)))))), kernal.idx)
    
    #h.min <- h.max <- 2 / 3
    #s.min <- 0.1225
    #s.max <- 1
    #v.min <- 0.96
    #v.max <- 1
    #v.max <- 1

    #col.ramp is a character vector of named colors

    num.int <- ceiling(n/2)
    col.ramp.one.half <- hsv(h=seq(from=hlim[1], to=hlim[2], length.out=num.int+1), s=seq(from=slim[1], to=slim[2], length.out=num.int+1),
                             v=seq(from=vlim[1], to=vlim[2], length.out=num.int+1))

    start.col.idx <- c(1:num.int, (num.int-1):1)
    col.ramp <- paste( col.ramp.one.half[start.col.idx], ",", col.ramp.one.half[start.col.idx +1], sep="")

    ##print("im confused");browser()
    res <- list(kernal.idx=kernal.idx, ord=ord, col.ramp=col.ramp)
    return(res)

}

