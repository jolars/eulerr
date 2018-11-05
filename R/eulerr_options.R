#' Get or set global graphical parameters for eulerr
#'
#' This function provides a means to set default parameters for functions
#' in eulerr. Query [eulerr_options()] (without any
#' argument) to see all the available options and read more about
#' the plot-related ones in [grid::gpar()] and [graphics::par()].
#'
#' Currently, the following items will be considered:
#' \describe{
#'   \item{pointsize}{size in pts to be used as basis for fontsizes and
#'   some margin sizes in the resulting plot}
#'   \item{fills}{a list of items `fill` and `alpha`}
#'   \item{edges}{a list of items `col`, `alpha`, `lex`, `lwd`, and `lty`}
#'   \item{labels}{a list of items `rot`,
#'   `col`, `alpha`, `fontsize`, `cex`, `fontfamily`, `fontface`,
#'   `lineheight`, and `font`}
#'   \item{quantities}{a list of items `rot`,
#'   `col`, `alpha`, `fontsize`, `cex`, `fontfamily`,
#'   `lineheight`, and `font`}
#'   \item{strips}{`col`, `alpha`, `fontsize`, `cex`, `fontfamily`,
#'   `lineheight`, and `font`}
#'   \item{legend}{arguments to [grid::legendGrob()] as well as `col`, `alpha`,
#'   `fontsize`, `cex`, `fontfamily`, `lineheight`, and `font`}
#'   \item{main}{arguments to [grid::textGrob()]}
#' }
#'
#' @param ... objects to update the global graphical parameters for \pkg{eulerr}
#'   with.
#'
#' @return This function gets or sets updates in the global environment
#'   that are used in [plot.euler()].
#' @export
#' @seealso [plot.euler()], [grid::gpar()], [graphics::par()]
#'
#' @examples
#' eulerr_options(edges = list(col = "blue"), fontsize = 10)
#' eulerr_options(n_threads = 2)
eulerr_options <- function(...) {
  new <- list(...)
  if (is.null(names(new)) && length(new) == 1L && is.list(new[[1L]]))
    new <- new[[1L]]
  old <- .eulerr_env$options
  if (length(new) == 0L)
    return(old)
  nm <- names(new)
  if (is.null(nm))
    return(old[unlist(new)])
  is_named <- nm != ""
  if (any(!is_named))
    nm[!is_named] <- unlist(new[!is_named])
  out <- old[nm]
  names(out) <- nm
  nm <- nm[is_named]
  .eulerr_env$options <- update_list(old, new[nm])
  pointsize <- new$pointsize
  if (!is.null(pointsize))
    .eulerr_env$options <- update_list(
      .eulerr_env$options,
      list(labels = list(fontsize = pointsize),
           quantities = list(fontsize = pointsize),
           strips = list(fontsize = pointsize),
           legend = list(fontsize = pointsize),
           main = list(fontsize = pointsize),
           pointsize = pointsize)
    )
  invisible(out)
}

#' Default options for eulerr
#'
#' @return default options for eulerr
#' @keywords internal
eulerr_default_options <- function() {
  list(
    pointsize = 12,
    fills = list(
      fill = function(n) {
        c("white",
          "grey85",
          "lightblue",
          "lightcoral",
          "lemonchiffon",
          "plum3",
          "aquamarine2",
          "grey55",
          "steelblue2",
          "lightsalmon",
          "lightpink",
          "lightgoldenrod")[seq_len(n)]
      },
      alpha = 1
    ),
    edges = list(
      col = 1L,
      alpha = 1,
      lty = 1L,
      lwd = 1,
      lex = 1
    ),
    labels = list(
      rot = 0,
      col = 1L,
      alpha = 1,
      fontsize = 12,
      cex = 1,
      fontfamily = "",
      lineheight = 1.2,
      font = 2
    ),
    quantities = list(
      rot = 0,
      col = 1L,
      alpha = 1,
      fontsize = 12,
      cex = 1,
      fontfamily = "",
      lineheight = 1.2,
      font = 1
    ),
    strips = list(
      col = 1L,
      alpha = 1,
      fontsize = 12,
      fontfamily = "",
      lineheight = 1.2,
      font = 4
    ),
    legend = list(
      side = "right",
      cex = 1,
      fontsize = 12,
      font = 1,
      fontfamily = "",
      labels = NULL,
      byrow = FALSE,
      do.lines = FALSE,
      lines.first = TRUE,
      hgap = grid::unit(1, "lines"),
      vgap = grid::unit(0.25, "lines"),
      default.units = "lines",
      pch = 21
    ),
    main = list(
      label = NULL,
      x = grid::unit(0.5, "npc"),
      y = grid::unit(0.5, "npc"),
      just = "center",
      hjust = NULL,
      vjust = NULL,
      rot = 0,
      check.overlap = FALSE,
      default.units = "npc",
      cex = 1.5,
      fontsize = 12,
      font = 1,
      fontfamily = "",
      col = 1,
      lineheight = 1.2,
      alpha = 1
    )
  )
}

