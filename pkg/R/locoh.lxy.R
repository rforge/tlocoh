#' @name locoh.lxy
#' @aliases LoCoH-xy locoh.lxy-class
#'
#' @title Class for location data
#'
#' @description Data class for locations with associated dates, nearest neighbor info, parameters for a random walk null model, and assorted meta-data
#'
#' @details
#' An object of class locoh.lxy is a list containing a series of point locations and ancillary
#' variables that go with those locations (e.g., time stamps, point IDs, animal IDs). These items
#' are bundled together and 'ready to go' as inputs into T-LoCoH functions. The benefits of bundling all 
#' of the objects related to a set of point locations in a single list object includes:
#'
#' \itemize{
#'     \item cleaning and error checking only has to be done once when the lxy object is created (e.g., with \code{\link{xyt.lxy}})
#'     \item having all of the ancillary variables together simplifies the task of passing parameters to other functions as well as saving/retrieving your work to disk
#'     \item the nearest neighbor lookup-table (which can take a long time to compute) can be reused
#'     \item locations for multiple individuals can be saved in one object
#'     }
#'
#' In code examples, objects of class locoh.lxy are often named 'lxy'. 
#'
#' @section Data Structure:
#'
#' The named elements in the list include:
#'
#' \describe{
#'    \item{pts}{a \link{SpatialPointsDataFrame} of the locations. Columns in the associated data frame vary but typically include:}
#'
#'         \itemize{
#'            \item \emph{ptid} - an integer vector of unique id values for each point
#'            \item \emph{id} - a character vector or factor containing the ids of the individuals (e.g., name of the animal, GPS device) associated with each point
#'            \item \emph{dt} - a vector of date-stamps (class POSIXct) for each point
#'            \item \emph{col} - a vector (or factor) of color values
#'            \item various other ancillary variables associated with each location
#'         }
#'
#'
#'    \item{anv}{A data frame of meta data of the ancillary variables associated with each point (or NULL if none). Column names are \emph{anv} (corresponding to the column name in \code{pts} and \emph{desc} (description)}
#'
#'    \item{dt.int}{a four-column data frame containing a frequency table of the time interval between points. Columns include: id, interval, count, and rtn (where rtn is the round-to-nearest value (in seconds) that was used in binning the delta.t values).}
#'
#'    \item{rw.params}{a four-column data frame containing the parameters used to compute the predicted 'diffusion distance' for any pair of points as a function of the difference in time. Columns of the data frame includ: \emph{id}, \emph{time.step.median}, \emph{d.bar} (median step-length), and \emph{vmax} (maximum observed velocity)}
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
#'    \item{ptsh}{List of the proportion of time-selected hulls for different values of s (created with lxy.ptsh.add), based on a random sample of hulls. One 
#'                list element for each id, and in each of those there will be an (unnamed) list element for each time lxy.ptsh was run. 
#'                Each of those will be a list containing elements: \emph{id, samp.idx, n, k, target.ptsh, target.s, s.ptsh, time.taken}.
#'                See \code{\link{lxy.ptsh.add}}.}
#'
#' }
#'
#' @seealso \code{\link{xyt.lxy}}, \code{\link{move.lxy}}, \code{\link{lxy.repair}}, \code{\link{lxy.subset}}, \code{\link{lxy.merge}}, \code{\link{lxy.exp.csv}}

NULL
