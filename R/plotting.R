#' Plot Euler diagrams
#'
#' @param eulerr
#' @return Plots a eulerr diagram using Base R graphics.
#' @examples
#'
#' @export

plot.eulerr <- function(eulerr, pal = "Accent", alpha = 80, ...) {
  x <- eulerr[["Circles"]][, 1]
  y <- eulerr[["Circles"]][, 2]
  r <- eulerr[["Circles"]][, 3]

  pal <- vapply(RColorBrewer::brewer.pal(length(x), pal),
                function (x) paste0(x, alpha), FUN.VALUE = character(1))

  u <- seq(0, 2 * pi, length = 200)

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

# # Draw the diagram
# library(lattice)
# library(RColorBrewer)
#
# colrs <- brewer.pal(nrow(circles), "Accent")
#
# # Custom panel function to create circles
# panel.venn <- function (x, y, radius, groups = NULL, ...) {
#   grid::grid.circle(x, y, r = radius, default.units = "native",
#                     gp = grid::gpar(fill = colrs, col = "transparent", lex = 0,
#                                     lwd = 0, alpha = .6), ...)
# }
#
# xyplot(y ~ x, data = circles, radius = circles$r, aspect = "iso", scales = list(draw = F),
#        xlab = NULL, ylab = NULL,
#        prepanel = function(x, y, radius, ...) {
#          # Return new limits
#          list(xlim =  range(c(x + radius, x - radius)),
#               ylim =  range(c(y + radius, y - radius)))
#        },
#        panel = function(x, y, radius, ...) {
#          panel.venn(x, y, radius)
#          panel.text(x, y, rownames(circles))
#        }
# )
#
