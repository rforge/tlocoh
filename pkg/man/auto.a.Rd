\name{auto.a}
\alias{auto.a}
\title{Parameters to select a reasonable value of 'a'}
\usage{
  auto.a(meth = c("enc", "nn")[1], ptp = 1, nnn = 2,
    tct = 1.05)
}
\arguments{
  \item{meth}{The name of the method to use}

  \item{ptp}{The proportion of total points [0..1]}

  \item{nnn}{The number of nearest neighbors}

  \item{tct}{The temporal continuity thresshold, used to
  filter out of the calculation points that are isolated in
  time to avoid bias from internal time gaps. Expressed as
  a proportion of the median sampling frequency, such that
  if a point is temporally disconnected from its a.nn / 2
  nearest temporal neighbors by more thanthan a.tct times
  the median sampling frequency, the point will be be
  excluded from the computation of a}
}
\value{
  A data frame with columns meth, ptp nnn, and tct
}
\description{
  Returns a list of parameters that other functions accept
  for the 'a' parameter
}
\note{
  If multiple values are passed for any of the parameters,
  other parameters will be recycled as needed.

  This function does not actually find nearest neighgors.
  Rather it returns a list of parameters that can be used
  as input in other functions. To actually find nearest
  neighbors, use \code{\link{lxy.nn.add}}.
}
\examples{
# lxy <- lxy.nn.add(lxy, ptsh=0.5, a=auto.a())
}
\seealso{
  \code{\link{lxy.nn.add}}
}

