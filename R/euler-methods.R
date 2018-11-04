#' Fitted values of euler object
#'
#' @param object object of class `'euler'`
#' @param ... ignored
#' @keywords internal
#' @export
#' @return fitted values
fitted.euler <- function(object, ...) {
  object$fitted.values
}

#' Return ellipses from the euler object
#'
#' @param object object of class `'euler'`
#' @param ... ignored
#' @export
#' @keywords internal
#' @return a data frame of the ellipses in the fit
coef.euler <- function(object, ...) {
  object$ellipses
}
