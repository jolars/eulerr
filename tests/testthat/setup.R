#' Suppress plotting
#'
#' @param x object to call [graphics::plot()] on
#' @param ... arguments to pass to `x`
#'
#' @return Invisibly returns whatever `plot(x)` would normally return, but
#'   does not plot anything (which is the point).
#' @keywords internal
dont_plot <- function(x, ..., .f = graphics::plot) {
  tmp <- tempfile()
  grDevices::png(tmp)
  p <- .f(x, ...)
  grDevices::dev.off()
  unlink(tmp)
  invisible(p)
}

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
