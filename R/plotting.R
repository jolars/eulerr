#' Plot Euler diagrams
#'
#' Plot euler diagrams using base R graphics.
#'
#' @param x euler diagram specifications of class \code{'eulerr'}
#' @param alpha Alpha for the colors to be plotted
#' @param ... Arguments to pass to text, polygon, or plot.
#' @return Plots euler diagram using Base R graphics.
#' @examples
#' fit <- eulerr(c(A = 10, B = 5, "A&B" = 3))
#' plot(fit, alpha = .7)
#'
#' @export

plot.eulerr <- function(x, alpha = .4, ...) {
  X <- x[["circles"]][, 1]
  Y <- x[["circles"]][, 2]
  r <- x[["circles"]][, 3]

  # Find good positions for the text centers
  text_x <- double(length(X))
  text_y <- double(length(Y))

  rpoints <- randtoolbox::sobol(100, 2)
  u <- 2 * pi * (rpoints[, 1] - 1) + 2 * pi
  v <- sqrt(rpoints[, 2])
  for (i in seq_along(r)) {
    rs <- r[i] * (v - 1) + r[i]
    xs <- X[i] + rs * cos(u)
    ys <- Y[i] + rs * sin(u)

    locs <- colSums(apply(cbind(xs, ys), 1, find_sets_containing_points,
                          X, Y, r))

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
    xlim = range(c(X + r, X - r)),
    ylim = range(c(Y + r, Y - r)),
    asp = 1
  )

  for (i in seq_along(X)) {
    graphics::polygon(
      r[i] * cos(u) + X[i],
      r[i] * sin(u) + Y[i],
      col = pal[i]
    )
  }
  for (i in seq_along(X)) {
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

