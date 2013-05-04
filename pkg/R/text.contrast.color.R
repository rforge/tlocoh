#' Choose a white or black plot background for a particular color of text 
#' @param x A color value
#' @param ... Other arguments (not used)
#' @export

text.contrast.color <- function(x, ...) {
    ifelse( mean(col2rgb(x)) > 127, "black", "white")
}
