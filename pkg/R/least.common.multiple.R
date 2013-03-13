least.common.multiple <- function(x, max.iter=300, show.err=TRUE) {

    ## Find the lcm of a bunch of integers
    ## Based on 'A Simple Algorithm'
    ## http://en.wikipedia.org/wiki/Least_common_multiple#A_simple_algorithm
    
    if (!is.integer(x)) x <- as.integer(x)

    x.orig <- x
    multiply.by <- rep(1, length(x))
    tol <- .Machine$double.eps ^ 0.5
    i <- 0
    
    
    ## Loop until all of the elements of x are equal
    while (diff(range(x)) >= tol) {
        i <- i + 1
        if (i > max.iter) {
            if (show.err) cat(max.iter, " loops and still can't find a lcm! :-(")
            return(NULL)
        }
        
        ## Identify which of the elements of x is the smallest
        wm <- which.min(x)
        
        ## Increase the muliplication factor of the minimum element by one
        multiply.by[wm] <- multiply.by[wm] + 1

        ## Update the value of the minimum element (only)
        x[wm] <- x.orig[wm] * multiply.by[wm]
        
    }
    
    return(x[1])

}
