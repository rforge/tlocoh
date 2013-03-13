#' text.contrast.color
#' @export
text.contrast.color <-
function(color) {
    ifelse( mean(col2rgb(color)) > 127, "black", "white")
}
