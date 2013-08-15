\name{iso2raster}
\alias{iso2raster}
\title{Converts a SpatialPolygonsDataFrame to a raster
This presumes the SPDF contains isopleths ordered from lowest level to highest
ext is an extent object or NULL
dimSize is numeric. dimSize is only used if raster is not set. dimSize is interpreted as the number of cells along the largest dimension of the track. The according raster will be calculated internally. Default is 10}
\usage{
  iso2raster(polys, raster = NULL, ext = NULL,
    dimSize = 100, cell.size = NULL, sf.cell.size = 2,
    ll.round = TRUE, status = TRUE, debug = FALSE)
}
\description{
  Converts a SpatialPolygonsDataFrame to a raster This
  presumes the SPDF contains isopleths ordered from lowest
  level to highest ext is an extent object or NULL dimSize
  is numeric. dimSize is only used if raster is not set.
  dimSize is interpreted as the number of cells along the
  largest dimension of the track. The according raster will
  be calculated internally. Default is 10
}

