euler_legend_grob <- function(
  labels,
  gp,
  symbol_size = 1,
  hgap = grid::unit(1, "lines"),
  vgap = grid::unit(0.25, "lines"),
  ncol = 1L,
  nrow = length(labels),
  byrow = FALSE
) {
  n <- length(labels)
  sym_unit <- grid::unit(symbol_size, "char")

  t_pts <- seq(0, 2 * pi, length.out = 64)
  cx <- 0.5 + 0.48 * cos(t_pts)
  cy <- 0.5 + 0.48 * sin(t_pts)

  symbol_grobs <- lapply(seq_len(n), function(i) {
    base <- grid::pathGrob(
      cx, cy,
      default.units = "npc",
      gp = grid::gpar(
        fill = gp$fill[i],
        col = gp$col[i],
        lwd = gp$lwd[i],
        lex = gp$lex[i],
        alpha = gp$alpha[i]
      )
    )
    children <- grid::gList(base)

    if (isTRUE(gp$pattern_type[i] == "stripes")) {
      pcol <- gp$pattern_col[i]
      if (is.na(pcol)) {
        pcol <- gp$fill[i]
      }
      clipped <- apply_stripe_pattern(
        fill_data = list(x = cx, y = cy, id.lengths = length(cx)),
        pattern_gp = list(
          type = "stripes",
          angle = gp$pattern_angle[i],
          col = pcol,
          lwd = gp$pattern_lwd[i],
          alpha = gp$pattern_alpha[i]
        ),
        spacing_scale = 3
      )
      if (!is.null(clipped)) {
        stripe_grob <- grid::pathGrob(
          x = unlist(lapply(clipped, "[[", "x"), use.names = FALSE),
          y = unlist(lapply(clipped, "[[", "y"), use.names = FALSE),
          id.lengths = lengths(lapply(clipped, "[[", "x")),
          default.units = "npc",
          gp = grid::gpar(fill = pcol, col = "transparent", alpha = gp$pattern_alpha[i])
        )
        children <- grid::gList(base, stripe_grob)
      }
    }

    # Explicit square viewport so the circle stays round regardless of row height.
    grid::gTree(
      children = children,
      vp = grid::viewport(width = sym_unit, height = sym_unit)
    )
  })

  text_grobs <- lapply(seq_len(n), function(i) {
    grid::textGrob(
      labels[i],
      x = 0, y = 0.5,
      just = c("left", "centre"),
      gp = grid::gpar(
        fontsize = gp$fontsize[1L],
        font = gp$font[1L],
        fontfamily = gp$fontfamily[1L],
        cex = gp$cex[1L]
      )
    )
  })

  if (byrow) {
    entry_rows <- ((seq_len(n) - 1L) %/% ncol) + 1L
    entry_cols <- ((seq_len(n) - 1L) %% ncol) + 1L
  } else {
    entry_rows <- ((seq_len(n) - 1L) %% nrow) + 1L
    entry_cols <- ((seq_len(n) - 1L) %/% nrow) + 1L
  }

  # Per legend-column, use the widest label to size the text column.
  text_col_widths <- lapply(seq_len(ncol), function(j) {
    entries <- which(entry_cols == j)
    if (length(entries) == 0L) {
      return(grid::unit(0, "mm"))
    }
    best <- entries[which.max(nchar(labels[entries]))]
    grid::unit(1, "grobwidth", list(text_grobs[[best]]))
  })

  # Frame columns: [sym | hgap | text] per legend-column, with 1-line sep between.
  # Total cols = 4*ncol - 1.
  frame_widths <- vector("list", 4L * ncol - 1L)
  for (j in seq_len(ncol)) {
    base_fc <- (j - 1L) * 4L + 1L
    frame_widths[[base_fc]]      <- sym_unit
    frame_widths[[base_fc + 1L]] <- hgap
    frame_widths[[base_fc + 2L]] <- text_col_widths[[j]]
    if (j < ncol) {
      frame_widths[[base_fc + 3L]] <- grid::unit(1, "lines")
    }
  }
  frame_widths <- do.call(grid::unit.c, frame_widths)

  # Row height = max(symbol height, text height).
  # Both are in "char" units (1 char = fontsize pt): symbol = symbol_size char,
  # text = cex char. Grid has no unit.pmax, so compute the max numerically.
  row_height <- grid::unit(max(symbol_size, gp$cex[1L]), "char")

  frame_heights <- vector("list", 2L * nrow - 1L)
  for (r in seq_len(nrow)) {
    frame_heights[[2L * r - 1L]] <- row_height
    if (r < nrow) {
      frame_heights[[2L * r]] <- vgap
    }
  }
  frame_heights <- do.call(grid::unit.c, frame_heights)

  sym_frame_cols  <- (seq_len(ncol) - 1L) * 4L + 1L
  text_frame_cols <- (seq_len(ncol) - 1L) * 4L + 3L

  fg <- grid::frameGrob(
    layout = grid::grid.layout(
      nrow = length(frame_heights),
      ncol = length(frame_widths),
      widths = frame_widths,
      heights = frame_heights
    )
  )

  for (i in seq_len(n)) {
    frame_r <- 2L * entry_rows[i] - 1L
    j <- entry_cols[i]
    fg <- grid::placeGrob(fg, symbol_grobs[[i]], row = frame_r, col = sym_frame_cols[j])
    fg <- grid::placeGrob(fg, text_grobs[[i]], row = frame_r, col = text_frame_cols[j])
  }

  fg
}
