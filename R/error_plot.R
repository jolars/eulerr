#' Error plot for `euler` objects
#'
#' This is a diagnostic tool for evaluating the fit from a call
#' to [euler()] visually. A color key is provided by default, which
#' represents the chosen error metric so that one can easily detect
#' which areas in the diagram to be skeptical about.
#'
#' Notice that this function is purely provided for diagnostic reasons
#' and does not come with the same kind of customization that
#' [plot.euler()] provides: the color legend can only be customized
#' in regards to its color palette and another key (instead of labels)
#' is completely turned off.
#'
#' @param x an object of class `euler`, typically the result of
#'   a call to [euler()].
#' @param type error metric. `'regionError'` is the difference in
#'   *percentage points* from the input
#' @param quantities whether to draw the error metric on the plot
#' @param pal color palette for the fills in the legend
#' @param ... arguments passed down to [plot.euler()]. Currently,
#'   providing `fills`, `legend`, or `strips` are not allowed and
#'   will return a warning.
#'
#' @return Returns an object of class `eulergram`, which will be
#' plotted on the device in the same manner as objects from
#' [plot.euler()]. See [plot.eulergram()] for details.
#'
#' @export
#'
#' @seealso [plot.euler()], [euler()],
#'   [plot.eulergram()]
#'
#' @examples
#' error_plot(euler(organisms), quantities = FALSE)
error_plot <- function(x,
                       type = c("regionError", "residuals"),
                       quantities = TRUE,
                       pal = NULL,
                       ...)
{
  type <- match.arg(type)
  dots <- list(...)
  n <- 101 # number of colors in legend

  if (!is.null(dots$fills))
    stop("values for 'fills' are not allowed here and will be ignored.")

  if (!is.null(dots$legend))
    stop("values for 'legend' are not allowed here and will be ignored.")

  if (!is.null(dots$strips))
    stop("values for 'strips' are not allowed here and will be ignored.")

  r <- switch(type,
              residuals = stats::residuals(x),
              regionError = sign(stats::residuals(x))*x$regionError)

  if (is.null(pal)) {
    pal <- grDevices::colorRampPalette(c("#67001F", "#B2182B", "#D6604D",
                                         "#F4A582", "#FDDBC7", "#F7F7F7",
                                         "#D1E5F0", "#92C5DE", "#4393C3",
                                         "#2166AC", "#053061"),
                                       space = "rgb")
  }

  lim <- max(c(abs(r), ifelse(type == "regionError", 0.01, 0)))

  rng <- grDevices::extendrange(c(-lim, lim))

  labels <- grid::grid.pretty(rng)
  n_labels <- length(labels)

  s <- seq(rng[1], rng[2], length.out = n)

  ind <- findInterval(r, s)

  labels_grob <- grid::textGrob(label = labels,
                                x = rep(0, length(labels)),
                                y = labels,
                                just = "left",
                                vp = grid::viewport(yscale = rng),
                                default.units = "native",
                                name = paste("eulerr", "error_plot", sep = "."))

  heights <- grid::unit(c(1, 1, 1), c("lines", "null", "lines"))
  widths <- grid::unit(c(1, 1, 1),
                       c("lines", "lines", "grobwidth"),
                       data = list(NULL, NULL, labels_grob))

  layout <- grid::grid.layout(nrow = 3, ncol = 3,
                              heights = heights,
                              widths = widths,
                              respect = TRUE)

  frame <- grid::frameGrob(
    layout = layout,
    vp = NULL,
    name = paste("eulerr", "error_plot", "key", sep = ".")
  )

  step <- diff(range(s))/(n - 1)
  y_rect <- seq(min(s) + step/2, max(s) - step/2, length.out = n - 1)

  ticks_grob <- grid::segmentsGrob(
    x0 = rep(0, n_labels),
    y0 = labels,
    x1 = rep(0.7, n_labels),
    y1 = labels,
    default.units = "native",
    name = paste("eulerr", "error_plot", "key", "ticks", sep = "."),
    vp = grid::viewport(yscale = rng)
  )

  box_grob <- grid::rectGrob(
    vp = grid::viewport(yscale = rng),
    height = diff(rng),
    default.units = "native",
    name = paste("eulerr", "error_plot", "key", "box", sep = "."),
    gp = grid::gpar(fill = "transparent")
  )

  rect_grob <- grid::rectGrob(
    x = rep(0.5, length(y_rect)),
    y = y_rect,
    default.units = "native",
    height = step,
    vp = grid::viewport(yscale = rng),
    name = paste("eulerr", "error_plot", "key", "rect", sep = "."),
    gp = grid::gpar(fill = pal(n), col = "transparent")
  )

  frame <- grid::placeGrob(frame, rect_grob, row = 2, col = 1)
  frame <- grid::placeGrob(frame, box_grob, row = 2, col = 1)
  frame <- grid::placeGrob(frame, ticks_grob, row = 2, col = 2)
  frame <- grid::placeGrob(frame, labels_grob, row = 2, col = 3)

  graphics::plot(x,
                 fills = pal(n)[ind],
                 labels = TRUE,
                 quantities = if (quantities) round(r, 3) else FALSE,
                 legend = frame,
                 ...)
}


