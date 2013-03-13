#' matperim
#'
#' @param mat 2-column matrix representing the xy coordinates of a closed polygon or line segment
#'
#' @return The length of the perimeter

matperim <- function(mat) {
    return(sum(sqrt((mat[2:nrow(mat),1]-mat[1:(nrow(mat)-1),1])^2 + (mat[2:nrow(mat),2]-mat[1:(nrow(mat)-1),2])^2)))
}
