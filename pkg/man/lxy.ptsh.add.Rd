\name{lxy.ptsh.add}
\alias{lxy.ptsh.add}
\title{Identify the values of s that result in proportion p of hulls being time-selected}
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
\description{
  Identify the values of s that result in proportion p of
  hulls being time-selected
}

