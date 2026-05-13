#' Compose Euler Diagrams
#'
#' Arrange two `eulergram` objects side-by-side or stacked, building up
#' multi-panel layouts with operator syntax. Compositions can be nested
#' arbitrarily, e.g. `(p1 | p2) / p3`.
#'
#' @param e1,e2 `eulergram` objects, typically returned by [plot.euler()].
#'
#' @details
#' `|` arranges the two plots horizontally; `/` stacks them vertically.
#' The result is itself an `eulergram`, so further composition chains
#' naturally.
#'
#' The gap between adjacent plots is controlled by the
#' `composition$spacing` entry of [eulerr_options()], which must be a
#' [grid::unit()] and defaults to `grid::unit(1, "lines")`.
#'
#' Because composition is binary and recursive, panels at different
#' nesting levels are not size-aligned. In `(p1 | p2) / p3`, `p3` spans
#' the full bottom row while `p1` and `p2` split the top row equally.
#'
#' @return An `eulergram` containing the composed layout.
#'
#' @examples
#' p1 <- plot(euler(c(A = 1, B = 8, "A&B" = 1)))
#' p2 <- plot(euler(c(A = 1, C = 1, "A&C" = 1)))
#'
#' p1 | p2
#' p1 / p2
#'
#' p3 <- plot(euler(c(X = 3, Y = 2, "X&Y" = 1)))
#' (p1 | p2) / p3
#'
#' @name eulergram-compose
#' @seealso [plot.euler()], [eulerr_options()]
NULL

#' @rdname eulergram-compose
#' @export
"|.eulergram" <- function(e1, e2) {
  compose_eulergrams(e1, e2, horizontal = TRUE)
}

#' @rdname eulergram-compose
#' @export
"/.eulergram" <- function(e1, e2) {
  compose_eulergrams(e1, e2, horizontal = FALSE)
}

compose_eulergrams <- function(e1, e2, horizontal = TRUE) {
  if (!inherits(e1, "eulergram") || !inherits(e2, "eulergram")) {
    stop("Both operands must be `eulergram` objects.", call. = FALSE)
  }

  spacing <- .eulerr_env$options$composition$spacing
  if (is.null(spacing)) {
    spacing <- grid::unit(1, "lines")
  }
  if (!grid::is.unit(spacing)) {
    stop(
      "`eulerr_options()$composition$spacing` must be a `grid::unit()`.",
      call. = FALSE
    )
  }

  if (horizontal) {
    layout_vp <- grid::viewport(
      layout = grid::grid.layout(
        nrow = 1,
        ncol = 3,
        widths = grid::unit.c(
          grid::unit(1, "null"),
          spacing,
          grid::unit(1, "null")
        )
      ),
      name = "euler.composed.vp"
    )

    first <- grid::gTree(
      children = grid::gList(e1),
      vp = grid::viewport(layout.pos.col = 1),
      name = "euler.composed.left"
    )

    second <- grid::gTree(
      children = grid::gList(e2),
      vp = grid::viewport(layout.pos.col = 3),
      name = "euler.composed.right"
    )
  } else {
    layout_vp <- grid::viewport(
      layout = grid::grid.layout(
        nrow = 3,
        ncol = 1,
        heights = grid::unit.c(
          grid::unit(1, "null"),
          spacing,
          grid::unit(1, "null")
        )
      ),
      name = "euler.composed.vp"
    )

    first <- grid::gTree(
      children = grid::gList(e1),
      vp = grid::viewport(layout.pos.row = 1),
      name = "euler.composed.top"
    )

    second <- grid::gTree(
      children = grid::gList(e2),
      vp = grid::viewport(layout.pos.row = 3),
      name = "euler.composed.bottom"
    )
  }

  grid::gTree(
    children = grid::gList(first, second),
    vp = layout_vp,
    cl = "eulergram",
    name = "euler.composed"
  )
}
