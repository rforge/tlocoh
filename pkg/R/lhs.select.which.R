lhs.select.which <- function (lhs, id = NULL, k = NULL, r = NULL, a = NULL, s = NULL, hs.names = NULL) {
    
    if (!inherits(lhs, "locoh.lhs")) stop("lhs should be of class \"locoh.lhs\"")
    
    for (str.param in c("id", "k", "r", "a", "s", "hs.names")) {
        assign(paste("dont.test.", str.param, sep=""), value=is.null(get(str.param)))
    }

    if (dont.test.id && dont.test.k && dont.test.r && dont.test.a && dont.test.s && dont.test.hs.names) return(1:length(lhs))

    kVals <- vectorize.parameter(k)
    aVals <- vectorize.parameter(a)
    rVals <- vectorize.parameter(r)
    sVals <- vectorize.parameter(s)
    idVals <- id

    lhs.params <- cbind(hs.name=names(lhs), do.call(rbind, lapply(lhs, function(x) data.frame(id=x[["id"]], k=n2z(x$k), r=n2z(x$r), a=n2z(x$a), s=x$s))))
    lhs.idx.good <- with(lhs.params, which((dont.test.id | id %in% idVals) & (dont.test.k | k %in% kVals)
                                     & (dont.test.r | r %in% rVals) & (dont.test.a | a %in% aVals)
                                     & (dont.test.s | s %in% sVals) & (dont.test.hs.names | hs.name %in% hs.names)))
    return(lhs.idx.good)
}
