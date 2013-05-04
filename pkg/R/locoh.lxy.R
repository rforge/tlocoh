#' @name locoh.lxy
#' @aliases LoCoH-xy
#'
#' @title Class for a series of locations
#'
#' @description Class for locations with associated dates, nearest neighbor info, parameters for a random walk null model, and assorted meta-data
#'
#' @details
#' An object of class LoCoH.lxy is a list containing a series of point locations and ancillary
#' variables that go with those locations (e.g., time stamps, point IDs, animal IDs). These items
#' are bundled together and 'ready to go' as inputs into T-LoCoH functions.
#'
#' The reasons for bundling all of the objects related to a set of point locations in a single list
#' object includes:
#'
#' \itemize{
#'
#'     \item the cleaning and error checking only has to be done once when the lxy object is created with the xyt.lxy function
#'
#'     \item having all of the ancillary variables together simplifies the task of passing parameters to other functions as well as saving/retrieving your work to disk
#'
#'     \item it allows re-use of the nearest neighbor lookup-table (which can take a long time to compute)
#'
#'     }
#'
#' In code examples, objects of class LoCoH.lxy are often named 'lxy'. 
#'
#' @section Data Structure:
#'
#' The named elements in the list include:
#'
#' \describe{
#'    \item{xys}{a two-column data frame containing the x and y coordinates of each point}
#'
#'    \item{col}{a vector (or factor) of color values. May also be NULL}
#'
#'    \item{ptid}{an integer vector of unique id values for each point}
#'
#'    \item{id}{a character vector or factor containing the ids of the individuals (e.g., name of the animal, GPS device) associated with each points }
#'
#'    \item{dt}{a vector of date-stamps (class POSIXct) for each point. May also be NULL.}
#'
#'    \item{anv}{a data frame of ancillary variables for each location, each column is a variable, and the number of rows equals the number of points}
#'
#'    \item{dt.int}{a four-column data frame containing a frequency table of the time interval between points. Columns include: id, interval, count, and rtn (where rtn is the round-to-nearest value (in seconds) that was used in binning the delta.t values).}
#'
#'    \item{rw.params}{a three-column data frame containing the parameters used to compute the predicted 'diffusion distance' for any pair of points as a function of the difference in time. Columns of the data frame includ: id, time.step.median and d.bar (median step-length)}
#'
#'    \item{aux}{either NULL or a list object of auxillary data associated with the points. This can include spatial elements to be overlaid when lxy is plotted, or other meta-data associated with the lxy. There is one list element per ID, and each of those elements is another list containing two named elements: \code{type}, and \code{data}:
#'       \tabular{rll}{
#'       \tab \strong{type} \tab \strong{data}\cr
#'       \tab \code{text} \tab a data frame with columns x, y, label, pos\cr
#'       \tab \code{lines} \tab a list containing named elements 'xy' and 'lty' and 'col'\cr
#'       \tab \code{data.frame} \tab a data frame\cr
#'       \tab \code{range.expand} \tab a two-element numeric vector that will be added to both xlim and ylim in \link{plot.locoh.lxy}. To reduce the range on the x-axis, for example to make space for labels, the first element should be negative.\cr
#'       }
#'   }
#'
#'    \item{comment}{a named list of descriptive text. One list element per id; the item name is the id. The default is a constructed string consisting of the ID(s) and number of points per ID. May also be NULL.}
#'
#'    \item{nn}{Nearest neighbor lookup table. Can also be NULL. Before hulls can be constructed, nearest neighbors have to be identified using \code{\link{lxy.nn.add}}. Note:
#'
#'         \itemize{
#'         \item A different set of nearest neighbors will exist for each value of s, because s determines how point-to-point distance is computed.
#'         \item Once nearest neighbors have been identified, they can be used to construct hulls for a variety of parameters and
#'               nearest neighbor selection method (e.g., k, r, or a method).
#'         \item For a set of nearest neighbors, the maximum possible value for k, r, and a are saved as list elements. If you want to construct
#'               hulls for a larger value of k, r, or a, you must first identify additional nearest neighbors using \code{\link{lxy.nn.add}}.
#'         }
#'
#'       The nearest neighbor lookup table is a list of lists. The name of each list element is a pipe-delimited character string that uniquely
#'       identifies the parameters for that list. Each element of this list is another list with the following elements:
#'
#'         \itemize{
#'            \item id
#'            \item s
#'            \item kmax
#'            \item rmax
#'            \item amax
#'            \item ptid (a vector of ptid values that have nearest neighbors identified for the current ID)
#'            \item auto.a.df - a data frame of the auto-a values that have been calculated. See \code{\link{auto.a}}. Columns include:
#'               \enumerate{
#'                  \item a.meth
#'                  \item a.pp
#'                  \item a.nn
#'                  \item a.h
#'                  \item a.tct
#'                  \item auto.a
#'                 }
#'            \item time.taken = time taken to find the nearest neighbors (in seconds)
#'            \item nn.df = a data frame with the following five columns:
#'               \enumerate{
#'                  \item pp.idx = index of the parent point from lxy$xys
#'                  \item nn.rank = nearest neighbor rank (integer), starting from 0 for the parent point. Points with no eligible
#'                        parent points will be have only the parent point itself as the 0th neighbor
#'                  \item nn.idx = index of the nearest neighbor point from lxy$xys. Integer. For the 0th neighbor this will be the parent point index
#'                  \item tsd = time-scaled-distance
#'                  \item tsd.cumsum = the cumulative tsd
#'               }
#'         }
#'
#'    }
#'
#'
#' }
#'
#' @seealso \code{\link{xyt.lxy}}, \code{\link{lxy.repair}}, \code{\link{lxy.subset}}, \code{\link{lxy.merge}}, \code{\link{lxy.exp.csv}}

NULL
