#' Plot Euler diagrams
#'
#' Plot euler diagrams using base R graphics.
#'
#' @param eulerr euler diagram specifications of class \code{'eulerr'}
#' @param col Colors to fill the circles with. The default uses the
#'   "Accent" palette from RColorBrewer.
#' @param alpha Alpha for the colors to be plotted
#' @return Plots euler diagram using Base R graphics.
#' @examples
#' fit <- eulerr(c(A = 10, B = 5, "A&B" = 3))
#' plot(fit, pal = "Set2", alpha = .3)
#'
#' @export

plot.eulerr <- function(eulerr, alpha = .4, ...) {
  x <- eulerr[["circles"]][, 1]
  y <- eulerr[["circles"]][, 2]
  r <- eulerr[["circles"]][, 3]

  # Find good positions for the text centers
  text_x <- double(length(x))
  text_y <- double(length(y))

  rpoints <- randtoolbox::sobol(100, 2)
  u <- scale_runif(rpoints[, 1], 0, 2 * pi)
  v <- sqrt(rpoints[, 2])

  for (i in seq_along(r)) {
    rs <- scale_runif(v, 0, r[i])
    xs <- x[i] + rs * cos(u)
    ys <- y[i] + rs * sin(u)

    locs <- colSums(apply(cbind(xs, ys), 1, find_sets_containing_points,
                          x, y, r))

    outskirts <- which(locs == min(locs))

    text_x[i] <- mean(xs[outskirts])
    text_y[i] <- mean(ys[outskirts])
  }

  pal <- grDevices::adjustcolor(color_palette, alpha = alpha)
  u <- seq(0, 2 * pi, length = 500)

  graphics::plot(
    NULL,
    axes = F,
    ann = F,
    xlim = range(c(x + r, x - r)),
    ylim = range(c(y + r, y - r)),
    asp = 1
  )

  for (i in seq_along(x)) {
    graphics::polygon(
      r[i] * cos(u) + x[i],
      r[i] * sin(u) + y[i],
      col = pal[i]
    )
  }
  for (i in seq_along(x)) {
    graphics::text(
      text_x[i],
      text_y[i],
      labels = names(r)[i]
    )
  }
}


# Color palette from qualpalr ---------------------------------------------

color_palette <- c("#6ACB69", "#C96CC6", "#76AACE", "#CF9D74", "#E4CCDD",
                   "#C0EADF", "#6A71CC", "#E0E1A8", "#C86D7A", "#AFA1DD",
                   "#C19DA0", "#77BDA5", "#C6D4E8", "#E6CCB9", "#E1A7DC",
                   "#B0C2A3", "#C8B778", "#8BCDD7", "#9F79CC", "#7391CE")

# Helper functions --------------------------------------------------------

scale_runif <- function(x, new_min, new_max) {
  (new_max - new_min) * (x - 1) + new_max
}
