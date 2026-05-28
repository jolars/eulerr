#' Fitted values of euler object
#'
#' @param object object of class `'euler'`
#' @param dense if `TRUE`, return a vector covering every combination of the
#'   non-empty sets (`2^n - 1` entries), filling absent combinations with 0.
#'   The default (`FALSE`) returns the sparse vector stored on the object,
#'   which only contains entries that were either requested as input or fit
#'   to a non-zero area. Use `dense = TRUE` only for diagrams with few sets
#'   --- the full enumeration is exponential in the number of non-empty sets.
#' @param ... ignored
#' @export
#' @return A named numeric vector of fitted areas keyed by combination label
#'   (set names joined by `&`).
fitted.euler <- function(object, dense = FALSE, ...) {
  if (!isTRUE(dense)) {
    return(object$fitted.values)
  }
  expand_dense(object$fitted.values, object$shapes)
}

#' Residuals of euler object
#'
#' @param object object of class `'euler'`
#' @param dense same meaning as in [fitted.euler()]
#' @param ... ignored
#' @export
#' @return A named numeric vector of residuals (input minus fit) keyed by
#'   combination label.
residuals.euler <- function(object, dense = FALSE, ...) {
  if (is.null(object$residuals)) {
    return(NULL)
  }
  if (!isTRUE(dense)) {
    return(object$residuals)
  }
  expand_dense(object$residuals, object$shapes)
}

#' Pad a sparse combination-keyed vector to cover every combination of the
#' non-empty sets in `shapes`, filling absent entries with 0. The lookup is
#' the wide `$shapes` table keyed by row names; empty sets carry NA in `h`
#' so they're filtered out.
#' @keywords internal
#' @noRd
expand_dense <- function(sparse_values, shapes) {
  setnames <- rownames(shapes)
  non_empty <- !is.na(shapes$h)
  all_combos <- all_set_combinations(setnames[non_empty])
  out <- stats::setNames(numeric(length(all_combos)), all_combos)
  present <- intersect(all_combos, names(sparse_values))
  out[present] <- sparse_values[present]
  out
}

#' Return the fitted shape parameters from the euler object
#'
#' Returns the `$shapes` data frame — a tagged uniform schema with one row
#' per set, a `type` column, and shape-specific columns
#' (`h, k, a, b, phi, width, height, side`) populated according to the
#' chosen shape (other columns are `NA`). For circle/ellipse fits the
#' legacy `$ellipses` slot is still populated for back-compat; consumers
#' that need the new shapes (rectangle, square) must read `$shapes`.
#'
#' @param object object of class `'euler'`
#' @param ... ignored
#' @export
#' @keywords internal
#' @return a data frame of the fitted shape parameters
coef.euler <- function(object, ...) {
  if (!is.null(object$shapes)) {
    object$shapes
  } else {
    object$ellipses
  }
}
