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
  # TODO: Move the entire discdisc function to c++
  stats::optimize(
    function(x, r1, r2, overlap) (discdisc(r1, r2, d = x) - overlap) ^ 2L,
    interval = c(abs(r1 - r2), sum(r1, r2)),
    r1 = r1,
    r2 = r2,
    overlap = overlap,
    tol = sqrt(.Machine$double.eps)
  )$minimum
}

