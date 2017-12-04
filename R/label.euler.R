#' Compute Locations for Overlaps
#'
#' Runs the same algorithm as in [plot.euler()] / [panel.euler.labels()] to
#' label the overlaps in the Euler diagram. This is useful if you want to
#' use your own solution to plot the final diagram.
#'
#' In the cases where `x` is a list of Euler diagrams (if the `by` argument
#' was used in the call to [euler()]), this function returns a list of
#' matrices with coordinates.
#'
#' @param x An object of class 'euler'
#' @param labels An optional character vector of labels for the diagram.
#'
#' @return A numeric matrix of x and y coordinates for the labels, as well as
#'   the quantities and proportions for the overlaps depicted in the labels.
#' @export
#'
#' @examples
#' fit <- euler(c(A = 1, B = 3, "A&B" = 0.9))
#' label(fit)
label <- function(x, labels = NULL) UseMethod("label")

#' @rdname label
#' @export
label.euler <- function(x, labels = NULL) {
  if (inherits(x, "by")) {
    out <- lapply(x, label)

    d <- dim(x)
    dn <- dimnames(x)
    dnn <- names(dn)
    gg <- lapply(seq_along(x), function(i, x) {
      ii <- i - 1L
      nms <- character(length(dn))
      for (j in seq_along(dn)) {
        iii <- ii%%d[j] + 1L
        ii <- ii%/%d[j]

        nms[j] <- dn[[j]][iii]
      }
      paste(nms, collapse = "_")
    }, x)
    names(out) <- unlist(gg)

    out
  } else {
    coefs <- x$coefficients
    fitted.values <- x$fitted.values
    quantity <- x$original.values

    h <- coefs[, 1L]
    k <- coefs[, 2L]

    if (ncol(coefs) == 3L) {
      # Circles
      a <- b <- coefs[, 3L]
      phi <- rep.int(0, length(a))
    } else {
      # Ellipses
      a <- coefs[, 3L]
      b <- coefs[, 4L]
      phi <- coefs[, 5L]
    }

    n <- length(h)
    id <- bit_indexr(n)
    singles <- rowSums(id) == 1
    empty <- abs(fitted.values) < sqrt(.Machine$double.eps)

    if (is.null(labels))
      labels <- names(quantity)

    stopifnot(length(labels) == nrow(id))

    centers <- locate_centers(h = h,
                              k = k,
                              a = a,
                              b = b,
                              phi = phi,
                              fitted = fitted.values)

    centers <- t(centers)
    dimnames(centers) <- list(labels, c("x", "y"))

    centers <- cbind(centers, quantity)

    out <- stats::na.omit(centers)
    cbind(out, proportion = out[, 3L]/sum(quantity))
  }
}
