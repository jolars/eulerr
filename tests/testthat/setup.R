#' Suppress printing
#'
#' @param x object to (not) print
#' @param ... arguments to `x`
#'
#' @return Invisibly returns the output of running print on `x`.
#' @keywords internal
dont_print <- function(x, ...) {
  utils::capture.output(y <- print(x, ...))
  invisible(y)
}
