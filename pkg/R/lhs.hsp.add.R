#' Adds a hull scatterplot to a LoCoH-hullset object
#'
#' @param lhs A LoCoH-hullset object
#' @param hsp.lst A list of object(s) of class locoh.hsp
#'
#' @note
#' hsp objects are typically created with \code{\link{lhs.plot.scatter}} function. They can then be 'saved' in a LoCoH-hullset object with this function.
#'
#' @examples
#' ## Display a scatter plot and manually draw four regions on it
#' ## hsp <- lhs.plot.scatter(lhs, x="area", y="par", regions=4)
#' 
#' ## Save hull scatter plot as part of the hullset
#' ## lhs <- lhs.hsp.add(lhs, hsp.lst=hsp)
#' ## summary(lhs)
#' 
#' @export

lhs.hsp.add <- function(lhs, hsp.lst) {
  
  if (!inherits(lhs, "locoh.lhs")) stop("lhs should be of class \"locoh.lhs\"")
  err.msg <- "hsp.lst should be a list of objects of class 'locoh.hsp'"
  if (!is.list(hsp.lst)) stop(err.msg)
  if (FALSE %in% (sapply(hsp.lst, function(x) class(x)=="locoh.hsp"))) stop(err.msg)
  
  for (j in 1:length(hsp.lst)) {
      if (is.null(lhs[[hsp.lst[[j]][["hs.name"]]]])) {
          stop("Can't find a hull set named ", names(hsp.lst)[j])
      } else {
          if (is.null(lhs[[hsp.lst[[j]][["hs.name"]]]][["hsp"]])) lhs[[hsp.lst[[j]][["hs.name"]]]][["hsp"]] <- list()
          
          ## Need to find a new name
          hsp.base.name <- substr(names(hsp.lst)[j], 1, nchar(names(hsp.lst)[j])-3)
          
          i <- 0
          hsp.in.lhs.name <- NULL
          while (is.null(hsp.in.lhs.name) || !is.null(lhs[[hsp.lst[[j]][["hs.name"]]]][["hsp"]][[hsp.in.lhs.name]])) {
              i <- i + 1
              if (i > 99) stop("Can't find a new name for this scatter plot")
              hsp.in.lhs.name <- paste(hsp.base.name, ".", sprintf("%02d", i), sep="")
          }
          
          lhs[[hsp.lst[[j]][["hs.name"]]]][["hsp"]][[hsp.in.lhs.name]] <- hsp.lst[[j]]
          cat(" - ", hsp.in.lhs.name, " added to ", hsp.lst[[j]][["hs.name"]], "\n", sep="") 
      }
  }
  return(lhs)
}
