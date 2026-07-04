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
#' Consecutive operators of the same direction are flattened into a
#' single row or column of equally sized panels. Thus
#' `p1 | p2 | p3 | p4` produces one row of four equal-width panels rather
#' than a lopsided nest of binary splits. Use parentheses (or mix `|` and
#' `/`) to force sub-groups: in `(p1 | p2) / p3`, `p3` spans the full
#' bottom row while `p1` and `p2` split the top row equally.
#'
#' The gap between adjacent plots is controlled by the
#' `composition$spacing` entry of [eulerr_options()], which must be a
#' [grid::unit()] and defaults to `grid::unit(1, "lines")`.
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

  # Flatten runs of same-direction operators into a single row/column so
  # that, e.g., `p1 | p2 | p3` yields three equal-width panels instead of
  # a lopsided binary nest. Operands composed in the other direction stay
  # as single panels, preserving intended sub-groups.
  panels <- c(
    composition_panels(e1, horizontal),
    composition_panels(e2, horizontal)
  )

  build_composition(panels, spacing, horizontal)
}

# Return the list of flat panels for an operand: the panels of a same-
# direction composition, otherwise the operand itself as a single panel.
composition_panels <- function(e, horizontal) {
  dir <- attr(e, "euler_dir", exact = TRUE)
  panels <- attr(e, "euler_panels", exact = TRUE)
  if (!is.null(dir) && identical(dir, horizontal) && !is.null(panels)) {
    panels
  } else {
    list(e)
  }
}

# Lay out `panels` in a single row (horizontal) or column, separated by
# `spacing`, with each panel getting an equal `1null` share.
build_composition <- function(panels, spacing, horizontal) {
  n <- length(panels)

  # Interleave panel slots with spacing: panel, gap, panel, ..., panel.
  panel_size <- grid::unit(1, "null")
  sizes <- panel_size
  for (i in seq_len(n - 1)) {
    sizes <- grid::unit.c(sizes, spacing, panel_size)
  }
  n_slots <- 2L * n - 1L

  if (horizontal) {
    layout_vp <- grid::viewport(
      layout = grid::grid.layout(nrow = 1, ncol = n_slots, widths = sizes),
      name = "euler.composed.vp"
    )
  } else {
    layout_vp <- grid::viewport(
      layout = grid::grid.layout(nrow = n_slots, ncol = 1, heights = sizes),
      name = "euler.composed.vp"
    )
  }

  children <- lapply(seq_len(n), function(i) {
    slot <- 2L * i - 1L
    vp <- if (horizontal) {
      grid::viewport(layout.pos.col = slot)
    } else {
      grid::viewport(layout.pos.row = slot)
    }
    grid::gTree(
      children = grid::gList(panels[[i]]),
      vp = vp,
      name = paste0("euler.composed.panel", i)
    )
  })

  grob <- grid::gTree(
    children = do.call(grid::gList, children),
    vp = layout_vp,
    cl = "eulergram",
    name = "euler.composed"
  )

  # Record the flat panel list and direction so a subsequent same-
  # direction operator can extend this composition instead of nesting it.
  attr(grob, "euler_panels") <- panels
  attr(grob, "euler_dir") <- horizontal
  grob
}
