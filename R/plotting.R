#' Plot Euler diagrams
#'
#' Generic plotting of euler diagrams created with eulerr using base R graphics.
#'
#' @param eulerr euler diagram specifications of type \code{'eulerr'}
#' @param pal Palette from RColorBrewer.
#' @param alpha Alpha for the colors to be plotted
#' @return Plots euler diagram using Base R graphics.
#' @examples
#' fit <- eulerr(c(A = 10, B = 5, "A&B" = 3))
#' plot(fit, pal = "Set2", alpha = .3)
#'
#' @export

plot.eulerr <- function(eulerr, pal = "Accent", alpha = .4, ...) {
  x <- eulerr[["Circles"]][, 1]
  y <- eulerr[["Circles"]][, 2]
  r <- eulerr[["Circles"]][, 3]

  n <- ifelse(length(x) < 3, 3, length(x))

  pal <- grDevices::adjustcolor(RColorBrewer::brewer.pal(length(x), pal),
                                alpha.f = alpha)
  u <- seq(0, 2 * pi, length = 500)

  plot(
    NULL,
    axes = F,
    ann = F,
    xlim = range(c(x + r, x - r)),
    ylim = range(c(y + r, y - r)),
    asp = 1
  )

  for (i in seq_along(x)) {
    polygon(
      r[i] * cos(u) + x[i],
      r[i] * sin(u) + y[i],
      col = pal[i],
      ...
    )
  }
  for (i in seq_along(x)) {
    text(
      x[i],
      y[i],
      labels = names(r)[i],
      ...
    )
  }
}
