# Methods for the eulerr object -------------------------------------------

#' Squared residuals from a eulerr fit.
#'
#' @param object A euler specification of class \code{eulerr}.
#' @param ... Currently ignored.
#' @return Squared residuals.
#' @seealso \code{\link{plot.residuals.eulerr}}, \code{\link{eulerr}}
#' @examples
#' fit <- eulerr(c("A" = 1, "B" = 0.4, "C" = 3, "A&B" = 0.2))
#' residuals(fit)
#'
#' @export

residuals.eulerr <- function(object, ...) {
  assert_that(inherits(object, "eulerr"))
  structure(
    object$residuals,
    class = c("residuals.eulerr", "residuals", "eulerr")
  )
}

resid.eulerr <- function(object, ...) {
  residuals.eulerr(object, ...)
}

#' Fitted values from a eulerr fit
#'
#' Returns a matrix of the circle centers in x and y coordinates, as well as
#' the radiuses.
#'
#' @param object An object of class \code{eulerr}.
#' @param ... Currently ignored
#' @return A matrix of x and y coordinates and radiuses.
#'
#' @export

fitted.eulerr <- function(object, ...) {
  assert_that(inherits(x, "eulerr"))
  x$fitted_areas
}