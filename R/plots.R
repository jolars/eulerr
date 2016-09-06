# Draw the diagram
library(lattice)
library(RColorBrewer)

colrs <- brewer.pal(nrow(circles), "Accent")

# Custom panel function to create circles
panel.venn <- function (x, y, radius, groups = NULL, ...) {
  grid::grid.circle(x, y, r = radius, default.units = "native",
                    gp = grid::gpar(fill = colrs, col = "transparent", lex = 0,
                                    lwd = 0, alpha = .6), ...)
}

xyplot(y ~ x, data = circles, radius = circles$r, aspect = "iso", scales = list(draw = F),
       xlab = NULL, ylab = NULL,
       prepanel = function(x, y, radius, ...) {
         # Return new limits
         list(xlim =  range(c(x + radius, x - radius)),
              ylim =  range(c(y + radius, y - radius)))
       },
       panel = function(x, y, radius, ...) {
         panel.venn(x, y, radius)
         panel.text(x, y, rownames(circles))
       }
)

