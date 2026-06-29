#' Get the bounding box for a row-wise collection of fitted shapes.
#'
#' Dispatches on the shape kind. Ellipse/circle use the rotated-ellipse
#' bounding-box formula; rectangle/square use width/height (or side)
#' directly since both are axis-aligned in eunoia.
#'
#' @param shapes the `$shapes` data frame of a fitted euler object (after
#'   dropping NA rows for empty sets), or — for legacy callers — the
#'   numeric `h` vector. When `h` is numeric the function falls back to
#'   the legacy ellipse signature for back-compat with external code.
#' @param k,a,b,phi legacy ellipse parameters; only used when `shapes` is
#'   numeric (the legacy signature).
#'
#' @return The bounding box as a list with `xlim` and `ylim`.
#' @keywords internal
get_bounding_box <- function(shapes, k = NULL, a = NULL, b = NULL, phi = NULL) {
  if (is.data.frame(shapes)) {
    return(shape_bounding_box(shapes))
  }
  # Legacy numeric signature: get_bounding_box(h, k, a, b, phi).
  h <- shapes
  if (is.null(b)) {
    b <- a
  }
  if (is.null(phi)) {
    phi <- 0
  }
  ellipse_bounding_box(h, k, a, b, phi)
}

#' Bounding box of a vector of rotated ellipses.
#' @keywords internal
ellipse_bounding_box <- function(h, k, a, b, phi) {
  xlim <- sqrt(a^2 * cos(phi)^2 + b^2 * sin(phi)^2)
  ylim <- sqrt(a^2 * sin(phi)^2 + b^2 * cos(phi)^2)

  list(
    xlim = range(xlim + h, -xlim + h),
    ylim = range(ylim + k, -ylim + k)
  )
}

#' Per-shape bounding box dispatch. Reads the `type` tag on `shapes` (rows
#' are assumed to share a tag since a diagram fixes one shape kind), then
#' picks the appropriate width/height calculation. Falls back to the
#' rotated-ellipse formula when the type is unknown so external callers
#' constructing ad-hoc `$shapes` frames still get a sensible box.
#' @keywords internal
shape_bounding_box <- function(shapes) {
  if (NROW(shapes) == 0L) {
    return(list(xlim = c(-1, 1), ylim = c(-1, 1)))
  }
  type <- shapes$type[1L]
  h <- shapes$h
  k <- shapes$k
  switch(
    type,
    rectangle = {
      half_w <- shapes$width / 2
      half_h <- shapes$height / 2
      list(
        xlim = range(c(h - half_w, h + half_w)),
        ylim = range(c(k - half_h, k + half_h))
      )
    },
    square = {
      half <- shapes$side / 2
      list(
        xlim = range(c(h - half, h + half)),
        ylim = range(c(k - half, k + half))
      )
    },
    rotated_rectangle = {
      # Axis-aligned extent of a rectangle rotated by `phi` about its center.
      half_w <- shapes$width / 2
      half_h <- shapes$height / 2
      c_phi <- abs(cos(shapes$phi))
      s_phi <- abs(sin(shapes$phi))
      ext_x <- half_w * c_phi + half_h * s_phi
      ext_y <- half_w * s_phi + half_h * c_phi
      list(
        xlim = range(c(h - ext_x, h + ext_x)),
        ylim = range(c(k - ext_y, k + ext_y))
      )
    },
    ellipse_bounding_box(h, k, shapes$a, shapes$b, shapes$phi)
  )
}
