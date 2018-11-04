#' Optimize Distance Between Circles Based On Overlap
#'
#' @param r1 radius of circle one
#' @param r2 radius of circle two
#' @param overlap overlap (area) of the two circles
#'
#' @return The necessary distance between sets to achieve the desired overlap
#'   area.
#' @keywords internal
separate_two_discs <- function(r1, r2, overlap) {
  if (r1 > 0 && r2 > 0) {
    stats::optimize(discdisc,
                    interval = c(abs(r1 - r2), sum(r1, r2)),
                    r1 = r1,
                    r2 = r2,
                    overlap = overlap,
                    tol = sqrt(.Machine$double.eps))$minimum
  } else {
    0
  }
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

#' Plotting coordinates for an ellipse
#'
#' @param h x coordinates
#' @param k y coordinates
#' @param a semimajor axis
#' @param b semiminor axis
#' @param phi rotation
#' @param n number of plotting points
#'
#' @return A list of matrices of coordinates for the ellipses.
#' @keywords internal
ellipse <- function(h, k, a, b = a, phi = 0, n = 200L) {
  theta <- seq.int(0, 2*pi, length.out = n)
  m <- length(h)
  out <- vector("list", m)
  for (i in seq_along(h)) {
    out[[i]]$x <-
      h[i] + a[i]*cos(theta)*cos(phi[i]) - b[i]*sin(theta)*sin(phi[i])
    out[[i]]$y <-
      k[i] + b[i]*sin(theta)*cos(phi[i]) + a[i]*cos(theta)*sin(phi[i])
  }
  out
}

#' Polygon Clipping
#'
#' This function is provided to efficiently and safely handle clipping
#' operations. It wraps around [polyclip::polyclip()], which is an
#' interface to the **Clipper** C++ library.
#'
#' @param x polygon
#' @param y polygon
#' @param op operation
#'
#' @return A list of lists.
#' @keywords internal
poly_clip <- function(a, b, op = c("intersection", "union", "minus", "xor")) {
  op <- match.arg(op)
  a0 <- identical(length(a), 0L)
  b0 <- identical(length(b), 0L)

  if (op == "intersection") {
    if (a0 || b0)
      return(list())
  } else if (op == "union") {
    if (a0 && !b0)
      return(b)
    else if (!a0 && b0)
      return(a)
    else if (!a0 && !b0) {
      if (all(unlist(a) == unlist(b)))
        return(a)
    } else
      return(list())
  } else if (op == "minus") {
    if (!a0 && b0)
      return(a)
    else if (a0)
      return(list())
  }
  polyclip::polyclip(a, b, op = op)
}
