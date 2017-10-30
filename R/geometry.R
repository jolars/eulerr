#' Optimize Distance Between Circles Based On Overlap
#'
#' @param r1 Radius of circle one
#' @param r2 Radius of circle two
#' @param overlap Overlap (area) of the two circles
#'
#' @return The necessary distance between sets to achieve the desired overlap
#'   area.
#' @keywords internal
separate_two_discs <- function(r1, r2, overlap) {
  stats::optimize(discdisc,
                  interval = c(abs(r1 - r2), sum(r1, r2)),
                  r1 = r1,
                  r2 = r2,
                  overlap = overlap,
                  tol = sqrt(.Machine$double.eps))$minimum
}

#' Get the bounding box of an ellipse
#'
#' @param h x-coordinate for the center
#' @param k y-coordinate for the center
#' @param a radius or semi-major axis
#' @param b semi-minor axis
#' @param phi rotation
#'
#' @return The bounding box as a list with xlim and ylim
#' @export
#' @keywords internal
get_bounding_box <- function(h, k, a, b = NULL, phi = NULL) {
  if (is.null(b))
    b <- a
  if (is.null(phi))
    phi <- 0

  xlim <- sqrt(a^2*cos(phi)^2 + b^2*sin(phi)^2)
  ylim <- sqrt(a^2*sin(phi)^2 + b^2*cos(phi)^2)

  list(xlim = range(xlim + h, -xlim + h),
       ylim = range(ylim + k, -ylim + k))
}
