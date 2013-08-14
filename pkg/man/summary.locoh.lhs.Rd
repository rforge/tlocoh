\name{summary.locoh.lhs}
\alias{summary.locoh.lhs}
\title{Show summary of a LoCoH-hullset}
\usage{
  \method{summary}{locoh.lhs} (lhs, file = "", id = NULL,
    k = NULL, r = NULL, a = NULL, s = NULL,
    hs.names = NULL, iso.details = FALSE, desc = FALSE,
    compact = FALSE, ...)
}
\arguments{
  \item{object}{A \link{LoCoH-hullset} object}

  \item{file}{A file name}

  \item{id}{The name(s) of individuals to summarize}

  \item{k}{The k value of hullsets to summarize}

  \item{r}{The r value of hullsets to summarize}

  \item{a}{The a value of hullsets to summarize}

  \item{s}{The s value of hullsets to summarize}

  \item{hs.names}{The name(s) of hullsets to summarize}

  \item{iso.details}{Display details of the isopleths. T/F}

  \item{desc}{Display the hullset description. T/F}

  \item{compact}{Use a compact format. T/F}

  \item{...}{Other arguments}
}
\description{
  Displays a summary of a LoCoH-hullset object
}

