stripe_segments <- function(
  xlim,
  ylim,
  angle = 45,
  spacing = NULL
) {
  xrng <- diff(xlim)
  yrng <- diff(ylim)
  if (is.null(spacing)) {
    spacing <- max(xrng, yrng) / 25
  }
  if (spacing <= 0) {
    return(NULL)
  }

  theta <- (angle %% 180) * pi / 180
  n_vec <- c(-sin(theta), cos(theta))
  d_vec <- c(cos(theta), sin(theta))

  corners <- rbind(
    c(xlim[1], ylim[1]),
    c(xlim[1], ylim[2]),
    c(xlim[2], ylim[1]),
    c(xlim[2], ylim[2])
  )
  tvals <- as.numeric(corners %*% n_vec)
  t_seq <- seq(min(tvals) - spacing, max(tvals) + spacing, by = spacing)

  bbox_intersections <- function(t0) {
    pts <- list()
    if (abs(d_vec[1]) > .Machine$double.eps) {
      s <- (xlim[1] - t0 * n_vec[1]) / d_vec[1]
      y <- t0 * n_vec[2] + s * d_vec[2]
      if (y >= ylim[1] - 1e-9 && y <= ylim[2] + 1e-9) {
        pts[[length(pts) + 1L]] <- c(xlim[1], y)
      }
      s <- (xlim[2] - t0 * n_vec[1]) / d_vec[1]
      y <- t0 * n_vec[2] + s * d_vec[2]
      if (y >= ylim[1] - 1e-9 && y <= ylim[2] + 1e-9) {
        pts[[length(pts) + 1L]] <- c(xlim[2], y)
      }
    }
    if (abs(d_vec[2]) > .Machine$double.eps) {
      s <- (ylim[1] - t0 * n_vec[2]) / d_vec[2]
      x <- t0 * n_vec[1] + s * d_vec[1]
      if (x >= xlim[1] - 1e-9 && x <= xlim[2] + 1e-9) {
        pts[[length(pts) + 1L]] <- c(x, ylim[1])
      }
      s <- (ylim[2] - t0 * n_vec[2]) / d_vec[2]
      x <- t0 * n_vec[1] + s * d_vec[1]
      if (x >= xlim[1] - 1e-9 && x <= xlim[2] + 1e-9) {
        pts[[length(pts) + 1L]] <- c(x, ylim[2])
      }
    }
    if (length(pts) < 2L) {
      return(NULL)
    }
    m <- do.call(rbind, pts)
    m <- unique(round(m, 9))
    if (NROW(m) < 2L) {
      return(NULL)
    }
    dist2 <- as.matrix(stats::dist(m))^2
    idx <- which(dist2 == max(dist2), arr.ind = TRUE)[1L, ]
    list(p1 = m[idx[1], ], p2 = m[idx[2], ])
  }

  out <- lapply(t_seq, bbox_intersections)
  out <- out[!vapply(out, is.null, logical(1))]
  if (length(out) == 0L) {
    return(NULL)
  }
  list(
    x0 = vapply(out, function(z) z$p1[1], numeric(1)),
    y0 = vapply(out, function(z) z$p1[2], numeric(1)),
    x1 = vapply(out, function(z) z$p2[1], numeric(1)),
    y1 = vapply(out, function(z) z$p2[2], numeric(1))
  )
}

split_fill_paths <- function(fill_data) {
  idx <- cumsum(fill_data$id.lengths)
  starts <- c(1L, utils::head(idx + 1L, -1L))
  mapply(
    function(s, e) {
      list(
        x = fill_data$x[s:e],
        y = fill_data$y[s:e]
      )
    },
    starts,
    idx,
    SIMPLIFY = FALSE
  )
}

stripe_polygons <- function(
  xlim,
  ylim,
  angle = 45,
  spacing = NULL,
  stripe_width = NULL
) {
  seg <- stripe_segments(xlim, ylim, angle = angle, spacing = spacing)
  if (is.null(seg)) {
    return(list())
  }

  if (is.null(spacing)) {
    spacing <- max(diff(xlim), diff(ylim)) / 25
  }
  if (is.null(stripe_width)) {
    stripe_width <- spacing / 3
  }
  stripe_width <- max(stripe_width, .Machine$double.eps)

  dx <- seg$x1 - seg$x0
  dy <- seg$y1 - seg$y0
  len <- sqrt(dx^2 + dy^2)
  ok <- len > .Machine$double.eps
  if (!any(ok)) {
    return(list())
  }

  dx <- dx[ok]
  dy <- dy[ok]
  len <- len[ok]
  x0 <- seg$x0[ok]
  y0 <- seg$y0[ok]
  x1 <- seg$x1[ok]
  y1 <- seg$y1[ok]

  nx <- -dy / len
  ny <- dx / len
  h <- stripe_width / 2

  lapply(seq_along(x0), function(i) {
    list(
      x = c(
        x0[i] + h * nx[i],
        x1[i] + h * nx[i],
        x1[i] - h * nx[i],
        x0[i] - h * nx[i]
      ),
      y = c(
        y0[i] + h * ny[i],
        y1[i] + h * ny[i],
        y1[i] - h * ny[i],
        y0[i] - h * ny[i]
      )
    )
  })
}

apply_stripe_pattern <- function(fill_data, pattern_gp, spacing_scale = 25) {
  if (is.null(pattern_gp$type) || !identical(pattern_gp$type, "stripes")) {
    return(NULL)
  }

  spacing <- max(diff(range(fill_data$x)), diff(range(fill_data$y))) /
    spacing_scale
  stripe_width <- spacing * pattern_gp$lwd / 2

  stripes <- stripe_polygons(
    xlim = range(fill_data$x),
    ylim = range(fill_data$y),
    angle = pattern_gp$angle,
    spacing = spacing,
    stripe_width = stripe_width
  )

  if (length(stripes) == 0L) {
    return(NULL)
  }

  fill_paths <- split_fill_paths(fill_data)
  subject_x <- unlist(lapply(fill_paths, "[[", "x"), use.names = FALSE)
  subject_y <- unlist(lapply(fill_paths, "[[", "y"), use.names = FALSE)
  subject_id_lengths <- as.integer(lengths(lapply(fill_paths, "[[", "x")))

  clipped <- list()
  idx <- 1L
  for (stripe in stripes) {
    piece <- polygon_clip_rust(
      subject_x = subject_x,
      subject_y = subject_y,
      subject_id_lengths = subject_id_lengths,
      clip_x = stripe$x,
      clip_y = stripe$y,
      op = "intersection"
    )
    n_pieces <- length(piece$id_lengths)
    if (n_pieces > 0L) {
      starts <- c(1L, utils::head(cumsum(piece$id_lengths) + 1L, -1L))
      ends <- cumsum(piece$id_lengths)
      for (j in seq_len(n_pieces)) {
        s <- starts[j]
        e <- ends[j]
        clipped[[idx]] <- list(x = piece$x[s:e], y = piece$y[s:e])
        idx <- idx + 1L
      }
    }
  }

  if (length(clipped) == 0L) {
    return(NULL)
  }

  clipped
}

add_fill_pattern <- function(fill_grob, fill_data, pattern_gp) {
  clipped <- apply_stripe_pattern(fill_data, pattern_gp, spacing_scale = 25)
  if (is.null(clipped)) {
    return(fill_grob)
  }

  stripe_grob <- grid::pathGrob(
    x = unlist(lapply(clipped, "[[", "x"), use.names = FALSE),
    y = unlist(lapply(clipped, "[[", "y"), use.names = FALSE),
    id.lengths = lengths(lapply(clipped, "[[", "x")),
    default.units = "native",
    gp = grid::gpar(
      fill = pattern_gp$col,
      col = "transparent",
      alpha = pattern_gp$alpha
    )
  )

  grid::grobTree(fill_grob, stripe_grob)
}

#' Grobify Euler objects
#'
#' @param x geometry data
#' @param fills fills params
#' @param patterns pattern params
#' @param edges edges params
#' @param labels labels params
#' @param quantities quantities params
#' @param number current diagram number
#' @param merged_sets sets that are the same and have been merged
#'
#' @return A [grid::gList()] is returned.
#' @keywords internal
setup_grobs <- function(
  x,
  fills,
  patterns,
  edges,
  labels,
  quantities,
  complement = NULL,
  number,
  merged_sets,
  n_vertices = 200L,
  placement_opts = NULL
) {
  data_edges <- x$edges
  data_fills <- x$fills
  data_tags <- x$centers
  fitted <- x$fitted.values
  empty_sets <- x$empty_sets
  empty_subsets <- x$empty_subsets

  do_tags <- !is.null(data_tags)
  do_edges <- !is.null(data_edges)
  do_fills <- !is.null(data_fills)
  do_patterns <- !is.null(patterns)
  do_labels <- !is.null(labels)
  do_quantities <- !is.null(quantities)

  xlim <- x$xlim
  ylim <- x$ylim

  n_e <- NROW(x$ellipses)
  n_id <- length(x$fitted.values)

  #edges
  if (do_edges) {
    # edges
    if (is.null(data_edges$x)) {
      edges_grob <- grid::nullGrob()
    } else {
      edges_grob <- grid::polylineGrob(
        data_edges$x,
        data_edges$y,
        id.lengths = data_edges$id.lengths,
        default.units = "native",
        name = "edges.grob",
        gp = edges$gp[which(!empty_sets & !merged_sets)]
      )
    }
  }

  # patterns by shape
  if (do_patterns && identical(patterns$mode, "shape")) {
    data_sets <- x$set_polygons
    pattern_grobs <- vector("list", n_e)

    for (i in seq_len(n_e)) {
      set_data <- data_sets[[i]]
      set_fill <- list(
        x = set_data$x,
        y = set_data$y,
        id.lengths = length(set_data$x)
      )
      clipped <- apply_stripe_pattern(
        set_fill,
        patterns$set_gp[i],
        spacing_scale = 25
      )
      if (is.null(clipped)) {
        pattern_grobs[[i]] <- grid::nullGrob(
          name = paste0("set.pattern.grob.", i)
        )
      } else {
        pattern_grobs[[i]] <- grid::pathGrob(
          x = unlist(lapply(clipped, "[[", "x"), use.names = FALSE),
          y = unlist(lapply(clipped, "[[", "y"), use.names = FALSE),
          id.lengths = lengths(lapply(clipped, "[[", "x")),
          default.units = "native",
          name = paste0("set.pattern.grob.", i),
          gp = grid::gpar(
            fill = patterns$set_gp[i]$col,
            col = "transparent",
            alpha = patterns$set_gp[i]$alpha
          )
        )
      }
    }
    patterns_grob <- do.call(grid::gList, pattern_grobs)
  }

  # fills
  if (do_fills) {
    if (n_e == 0) {
      fills_grob <- grid::nullGrob()
    } else if (n_e == 1) {
      fill_idx <- which(!empty_subsets)[1L]
      fill_grob <- grid::polygonGrob(
        data_fills[[1]]$x,
        data_fills[[1]]$y,
        default.units = "native",
        name = "fills.grob",
        gp = fills$gp[fill_idx]
      )
      fill_grob <- add_fill_pattern(
        fill_grob = fill_grob,
        fill_data = data_fills[[1]],
        pattern_gp = if (
          do_patterns && identical(patterns$mode, "intersection")
        ) {
          patterns$gp[fill_idx]
        } else {
          NULL
        }
      )
      fills_grob <- grid::gList(fill_grob)
    } else {
      fills_grob <- vector("list", n_id)

      for (i in seq_len(n_id)) {
        if (is.null(data_fills[[i]])) {
          fills_grob[[i]] <- grid::nullGrob(name = paste0("fills.grob.", i))
        } else {
          fill_idx <- which(!empty_subsets)[i]
          fill_grob <- grid::pathGrob(
            data_fills[[i]]$x,
            data_fills[[i]]$y,
            id.lengths = data_fills[[i]]$id.lengths,
            rule = "winding",
            default.units = "native",
            name = paste0("fills.grob.", i),
            gp = fills$gp[fill_idx]
          )
          fills_grob[[i]] <- add_fill_pattern(
            fill_grob = fill_grob,
            fill_data = data_fills[[i]],
            pattern_gp = if (
              do_patterns && identical(patterns$mode, "intersection")
            ) {
              patterns$gp[fill_idx]
            } else {
              NULL
            }
          )
        }
      }
      fills_grob <- do.call(grid::gList, fills_grob)
    }
  }

  container_data <- x$container
  do_container <- !is.null(container_data) && !is.null(complement)

  container_fill_grob <- NULL
  container_edge_grob <- NULL

  if (do_container) {
    cgp <- complement$gp

    cp <- container_data$complement_polygon
    if (length(cp$id_lengths) > 0L) {
      container_fill_grob <- grid::pathGrob(
        cp$x,
        cp$y,
        id.lengths = cp$id_lengths,
        rule = "winding",
        default.units = "native",
        name = "complement.fill.grob",
        gp = grid::gpar(
          fill = cgp$fill[1L],
          col = "transparent",
          alpha = cgp$alpha[1L]
        )
      )
    }

    container_edge_grob <- grid::polylineGrob(
      container_data$outline$x,
      container_data$outline$y,
      default.units = "native",
      name = "container.edge.grob",
      gp = grid::gpar(
        col = cgp$col[1L],
        lwd = cgp$lwd[1L],
        lty = cgp$lty[1L],
        lex = cgp$lex[1L],
        alpha = cgp$alpha[1L]
      )
    )
  }

  # ------------------------------------------------------------------
  # EulerTags: one gTree per panel holding every label/quantity/leader
  # plus the optional complement count. makeContent.EulerTags re-runs
  # the eunoia placement at draw time so resizing the device replaces
  # labels cleanly — no clipping on shrink.
  # ------------------------------------------------------------------
  tag_grobs <- gList()
  if ((do_quantities || do_labels) && !is.null(data_tags)) {
    for (i in seq_len(NROW(data_tags))) {
      tag_grobs[[length(tag_grobs) + 1L]] <- setup_tag(
        data_tags[i, , drop = FALSE],
        labels,
        quantities,
        number = i
      )
    }
  }
  if (do_container) {
    comp_tag <- setup_complement_tag(
      container_data,
      complement,
      number = length(tag_grobs) + 1L
    )
    if (!is.null(comp_tag)) {
      tag_grobs[[length(tag_grobs) + 1L]] <- comp_tag
    }
  }

  do_tags <- length(tag_grobs) > 0L
  tags_gtree <- NULL
  if (do_tags) {
    label_precision <- max(diff(xlim), diff(ylim)) / 100
    tags_gtree <- gTree(
      xlim = xlim,
      ylim = ylim,
      ellipses = x$ellipses,
      container = container_data,
      n_vertices = as.integer(n_vertices),
      placement_opts = placement_opts,
      label_precision = label_precision,
      padding = eulerr_options()$padding,
      children = tag_grobs,
      name = "tags",
      cl = "EulerTags"
    )
  }

  panel_children <- grid::gList(
    if (do_container && !is.null(container_fill_grob)) container_fill_grob,
    if (do_fills) fills_grob,
    if (do_patterns && identical(patterns$mode, "shape")) patterns_grob,
    if (do_edges) edges_grob,
    if (do_container && !is.null(container_edge_grob)) container_edge_grob,
    if (do_tags) tags_gtree
  )

  # Wrap each panel in a custom `EulerPanel` gTree so its viewport can
  # be sized at draw time. `makeContext.EulerPanel` measures labels
  # against the live device, runs eunoia's placement, and rewrites
  # the panel viewport's `xscale`/`yscale` to encompass the labels.
  # On window resize grid re-fires `makeContext`, so the bbox tracks
  # the current device and labels can't be clipped at the panel edge.
  #
  # The PURE geometry bbox (no labels) is used to seed the measurement
  # viewport — that's the smallest plausible viewport, which gives the
  # smallest label native size and the most conservative placement.
  # The (label-aware) `x$xlim`/`x$ylim` from setup_geometry are used as
  # the fallback when measurement isn't possible.
  if (NROW(x$ellipses) > 0L) {
    geom_box <- get_bounding_box(
      x$ellipses$h,
      x$ellipses$k,
      x$ellipses$a,
      x$ellipses$b,
      x$ellipses$phi
    )
    geom_xlim <- geom_box$xlim
    geom_ylim <- geom_box$ylim
    if (!is.null(x$container)) {
      cx <- x$container$h
      cy <- x$container$k
      chw <- x$container$width / 2
      chh <- x$container$height / 2
      geom_xlim <- range(c(geom_xlim, cx - chw, cx + chw))
      geom_ylim <- range(c(geom_ylim, cy - chh, cy + chh))
    }
  } else {
    geom_xlim <- xlim
    geom_ylim <- ylim
  }
  label_precision_base <- max(diff(geom_xlim), diff(geom_ylim)) / 100
  grid::gTree(
    children = panel_children,
    ellipses = x$ellipses,
    container = if (do_container) x$container else NULL,
    n_vertices = as.integer(n_vertices),
    placement_opts = placement_opts,
    geom_xlim = geom_xlim,
    geom_ylim = geom_ylim,
    label_precision = label_precision_base,
    padding = eulerr_options()$padding,
    name = paste0("diagram.grob.", number),
    cl = "EulerPanel"
  )
}
