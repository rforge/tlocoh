\name{lxy.ptsh.add}
\alias{lxy.ptsh.add}
\title{Compute s vs. proportion-time-selected hulls table}
\usage{
  lxy.ptsh.add(lxy, id = NULL, k = 10, n = 200,
    samp.idx = NULL, sinit = 0.005, ptsh.target = 1:9/10,
    ptsh.max = 0.98, ptsh.buf = 0.01, max.iter = 15,
    max.loops = 10, time.term = c("vmax", "dif")[1],
    FNN.algorithm = c("cover_tree", "kd_tree", "VR", "CR")[2],
    use.nn = FALSE, plotme = TRUE, save = TRUE,
    nn.add = FALSE, use.pb.n = 200,
    ptsh.exists = c("replace", "append")[2], beep = FALSE,
    status = TRUE)
}
\arguments{
  \item{lxy}{A \link{LoCoH-xy} object}

  \item{id}{The name(s) of individuals to analyze}

  \item{k}{Value for the k-method that will be used to
  create hulls for the sample points (and then used to find
  the proportion of time-selected hulls)}

  \item{n}{Number of randomly selected points to use as
  samples (ignored if \code{samp.idx} is passed)}

  \item{samp.idx}{A vector of indices of the points to use
  as samples}

  \item{sinit}{The initial value of \emph{s} that will be
  used to start the iteration to find that value of
  \emph{s} that produces a proportion of time selected
  hulls >= ptsh.max}

  \item{ptsh.target}{A vector of values 0..1 that will
  serve as the target \emph{ptsh} values. For each ptsh
  target, script will try to find a value of \emph{s} that
  generates the targeted proportion of time selected hulls
  (within \code{ptsh.buf})}

  \item{ptsh.max}{The highest value of ptsh for which a
  value of \emph{s} will be computed. It is generally
  recommended that this be less than 1.}

  \item{ptsh.buf}{The level of accuracy or precison to
  which the found values of 's' return the targetted ptsh.}

  \item{max.iter}{The maximum number of times the script
  will double sinit in an effort to find the value of s
  that produces ptsh.max}

  \item{max.loops}{The maximum number of intermediate
  values of s the script will try when 'zooming' in on the
  target ptsh levels}

  \item{time.term}{The space-time transformation to use in
  the TSD distance metric: \code{'vmax'} for the maximum
  velocity transformation (default) or \code{'dif'} for the
  diffusion transformation.}

  \item{FNN.algorithm}{The algorithm to be used in the
  get.knnx() function in the package FNN}

  \item{use.nn}{Whether to use saved nearest neighbor sets
  for the ptsh computations. T/F}

  \item{plotme}{Whether to plot the ptsh vs. s curve. T/F}

  \item{save}{Whether to save the ptsh-s table in the
  LoCoH-xy object. T/F}

  \item{nn.add}{Whether to also identify and save nearest
  neighbor sets for all points for \emph{k}=\code{k} and
  the values of \emph{s} identified. T/F}

  \item{use.pb.n}{The sample size (i.e., \code{n}) above
  which a progress bar will be displayed}

  \item{ptsh.exists}{What to do if there is already a ptsh
  table in the LoCoH-xy object: \code{'replace'} or
  \code{'append'}}

  \item{beep}{Whether to beep when done. T/F}

  \item{status}{Display status messages. T/F}
}
\description{
  Identify the values of s that result in proportion p of
  hulls being time-selected
}
\details{
  When \emph{s}=0, nearest neighbors are space-selected
  (i.e., point-to-point distance is merely Euclidean
  distance). For large values of \emph{s}, nearest
  neighbors are effectively time-selected (e.g., temporally
  contiguous). One approach to selecting a value of
  \emph{s} between these two ends of the spectrum is to
  pick a \emph{s} value that returns an intermediate
  proportion of time-selected-hulls representing the
  desired balance between space and time selection (which
  in turn is shaped by the research question / objective).
  This function will find the value(s) of \emph{s} that
  produce a desired proportion of time-selected-hulls using
  a randomly-selected subset of points. A general rule of
  thumb is to pick a value of \emph{s} that results in a
  proportion of time-selected hulls from 0.4 to 0.6.

  The script iteratively tries a variety of \emph{s} values
  until it finds one that produces the desired proportion
  of time-selected hulls within a tolerance of
  \code{ptsh.buf}. If for example the vector of target
  values \code{ptsh.target} includes 0.5, and
  \code{ptsh.buf = 0.01}, the script will try to find a
  value of \emph{s} that produces 0.49 to 0.51
  time-selected hulls.
}
\seealso{
  \code{\link{lxy.nn.add}}
}

