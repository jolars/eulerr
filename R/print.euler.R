#' Print Euler Fits
#'
#' Prints a data frame of the original set relationships and the fitted
#' values as well as `diag_error` and `stress` statistics.
#'
#' @param x Euler diagram specification from [euler()].
#' @param round Number of decimal places to round to.
#' @param ... Arguments passed to [base::print.data.frame()].
#'
#' @return Prints the results of the fit.
#'
#' @export
print.euler <- function(x, round = 3, ...) {
  assertthat::assert_that(assertthat::is.number(round), round > 0L)

  out <- data.frame(
    "original" = x$original.values,
    "fitted" = x$fitted.values,
    "residuals" = x$residuals,
    "region_error" = x$region_error
  )
  print(round(out, digits = round), ...)
  cat("\n")
  cat("diag_error: ", round(x$diag_error, digits = round), "\n")
  cat("stress:     ", round(x$stress, digits = round), "\n")
}
