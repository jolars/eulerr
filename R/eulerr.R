#' @useDynLib eulerr, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom grDevices adjustcolor dev.off png xy.coords
#' @importFrom graphics plot
#' @importFrom stats optim nlm as.formula coef optimize runif
#' @importFrom utils capture.output modifyList
#' @importFrom assertthat assert_that is.number is.flag has_attr not_empty
#' @importFrom lattice trellis.par.get xyplot panel.text lattice.options
#'   panel.superpose packet.number
#' @importFrom grid gpar grid.circle
NULL
