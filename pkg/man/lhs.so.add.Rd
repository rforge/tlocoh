\name{lhs.so.add}
\alias{lhs.so.add}
\title{Add hull metrics for association analysis}
\usage{
  lhs.so.add(lhs, id = "all", hs2.id = "all", tbuff = 0,
    ivg = NULL, test = 0, skip.dups = TRUE,
    save.hso = TRUE, status = TRUE,
    fn.log = "segment.intersections.txt",
    piFUN = c("pIntersect", "pIntersectSat")[1])
}
\arguments{
  \item{lhs}{A LoCoH-hullset object}

  \item{save.hso}{Whether to save the hull intersection
  list, T/F}

  \item{skip.dups}{Skip duplicate hulls (faster)}
}
\value{
  A LoCoH-hullset object
}
\description{
  Add hull metrics for association analysis
}

