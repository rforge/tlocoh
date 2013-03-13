#' @name toni
#'
#' @docType data
#'
#' @title Tracking data for one buffalo in South Africa
#'
#' @description This dataframe contains the locations from a GPS collar fitted to a buffalo in Kruger National Park, South Africa in 2005-06.
#'
#' @usage data(toni)
#'
#' @format A dataframe with 6371 rows. Columns are id, lat, long, and timestamp (in UTC). Datum presumed to be WGS84.
#'
#' @source MoveBank \url{http://www.movebank.org}
#'
#' Name: Kruger African Buffalo, GPS tracking, South Africa
#'
#' Acknowledgements: Collection of Kruger Park Buffalo data funded by NSF Grant DEB-0090323 to Wayne M. Getz
#'
#' Principal Investigator Name: Paul Cross
#'
#' @examples
#' data(toni)
#' head(toni)
#' plot(toni[,2:3])
#'
#' @keywords datasets 
NULL
