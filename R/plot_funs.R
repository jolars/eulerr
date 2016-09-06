library(RColorBrewer)
library(lattice)

plot.vennr <- function(vennr, pal = "Accent", pal.alpha = 80, ...) {
  x <- vennr$x
  y <- vennr$y
  r <- vennr$r

  pal <- vapply(brewer.pal(length(x), pal),
                function (x) paste0(x, pal.alpha), FUN.VALUE = character(1))

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