\name{lhs.filter.hsp}
\alias{lhs.filter.hsp}
\title{Define subsets of hulls based on the location of the parent point in hull scatterplot space}
\usage{
  lhs.filter.hsp(lhs, id = NULL, k = NULL, r = NULL,
    a = NULL, s = NULL, hs.names = NULL, hsp = NULL,
    reg.idx = NULL, label = NULL, col = NULL,
    short.list = TRUE)
}
\arguments{
  \item{lhs}{A LoCoH-hullset object}

  \item{id}{The name(s) of the individual(s) to include in
  the plot. Character vector or comma-delimited character.}

  \item{k}{The k value of hullsets to create isopleths for}

  \item{r}{The r value of hullsets to create isopleths for}

  \item{a}{The a value of hullsets to create isopleths for}

  \item{s}{The s value of hullsets to create isopleths for}

  \item{hs.names}{The name(s) of saved hullsets to create
  isopleths for}

  \item{hsp}{Either the index of a hull scatterplot(s)
  saved in lhs (use \code{\link{summary.locoh.lhs}} to
  which how many hsp objects have been saved), or a list of
  objects of class \code{locoh.hsp} (i.e., the return value
  of \code{\link{lhs.plot.scatter}}.}
}
\value{
  A list that defines subsets of hulls, with one element
  for each region in \code{hsp}. Each element is a list
  with three elements: \describe{ \item{idx}{indices of the
  hull parent points} \item{label}{the label of the region
  of hsp} \item{col}{the color of the region in hsp} }
}
\description{
  Define subsets of hulls based on the location of the
  parent point in hull scatterplot space
}
\details{
  This will return a list that defines subsets of hulls
  grouped according to which manually-digitized region in
  scatterplot space the hull parent point falls. This can
  be passed to several functions, including
  \code{\link{plot.locoh.lhs}} (future), and
  \code{\link{lhs.plot.scatter}}, to create plots of
  subsets of hulls.

  The scatterplot space, including the manually defined
  regions, are saved in a \code{locoh.hsp} object.
  Typically \code{hsp} will be created using the
  \code{\link{lhs.plot.scatter}} function. \code{hsp}
  objects can also be saved in the \code{lhs} object (see
  \code{\link{lhs.hsp.add}}.

  Note that this function can only return subsets for *one*
  hullset and *one* hsp object. This means that either
  \code{lhs} must contain a single hullset, or other
  parameters (e.g., \code{id}, \code{hs.names}, \code{k},
  \code{r}, and/or \code{a}) are passed to select one and
  only one of the hullsets in \code{lhs}.
}
\examples{
# Load a hullset
data(toni.lhs)

# View a hull scatter plot, manually define 4 regions using the mouse, and give each region a name
hsp <- lhs.plot.scatter(toni.lhs, x.axis="area", y.axis="par", regions=4)

# For the hulls in each manually defined region, create a separate scatter plot of day and hour
toni.hsp.grps <- lhs.filter.hsp(toni.lhs, hsp=hsp)
lhs.plot.scatter(toni.lhs, x.axis="dt", y.axis="dt.hour", filter=toni.hsp.grps, figs.per.page=4, desc=0, title.hs.name.include=F)
}

