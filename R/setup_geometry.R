#' Compute geometries and label locations
#'
#' @param x an object of class 'euler'
#' @param fills fills
#' @param edges edges
#' @param labels labels
#' @param quantities quantities
#' @param n number of vertices to use to render each ellipse
#' @param merged_sets which sets have been merged?
#'
#' @return a list object with slots for the various objects
#' @keywords internal
setup_geometry <- function(
  x,
  fills,
  edges,
  labels,
  quantities,
  annotations,
  n,
  merged_sets,
  placement_opts = NULL,
  do_complement_label = FALSE
) {
  dd <- x$ellipses
  empty_sets <- is.na(dd[, 1L]) & !merged_sets
  setnames_full <- rownames(dd)
  empty_set_names <- setnames_full[empty_sets]

  combo_labels_full <- names(x$fitted.values)
  combo_sets_full <- strsplit(combo_labels_full, "&", fixed = TRUE)
  empty_subsets <- vapply(
    combo_sets_full,
    function(s) any(s %in% empty_set_names),
    logical(1)
  )

  orig <- x$original.values[!empty_subsets]
  fitted <- x$fitted.values[!empty_subsets]
  combo_labels <- combo_labels_full[!empty_subsets]
  combo_sets <- combo_sets_full[!empty_subsets]

  dd <- dd[!empty_sets, , drop = FALSE]

  nonzero <- nonzero_fit(fitted)
  nonzero <- ifelse(is.na(nonzero), FALSE, nonzero)

  do_fills <- !is.null(fills)
  do_edges <- !is.null(edges)
  do_labels <- !is.null(labels)
  do_quantities <- !is.null(quantities)
  do_annotations <- !is.null(annotations)

  h <- dd$h
  k <- dd$k
  a <- dd$a
  b <- dd$b
  phi <- dd$phi

  n_e <- NROW(dd)
  n_id <- length(combo_labels)

  container <- x$container
  do_container <- !is.null(container)

  if (n_e > 0L) {
    limits <- get_bounding_box(h, k, a, b, phi)
  } else {
    limits <- list(xlim = c(-1, 1), ylim = c(-1, 1))
  }

  if (do_container) {
    cx_half <- container$width / 2
    cy_half <- container$height / 2
    container_xlim <- c(container$h - cx_half, container$h + cx_half)
    container_ylim <- c(container$k - cy_half, container$k + cy_half)
    limits$xlim <- range(c(limits$xlim, container_xlim))
    limits$ylim <- range(c(limits$ylim, container_ylim))
  }

  width <- abs(limits$xlim[1] - limits$xlim[2])
  height <- abs(limits$ylim[1] - limits$ylim[2])

  if (n_e > 0L) {
    plot_data <- euler_plot_data(
      set_names = rownames(dd),
      h = h,
      k = k,
      a = a,
      b = b,
      phi = phi,
      container_h = if (do_container) container$h else NULL,
      container_k = if (do_container) container$k else NULL,
      container_width = if (do_container) container$width else NULL,
      container_height = if (do_container) container$height else NULL,
      n_vertices = as.integer(n),
      label_precision = max(width, height) / 100
    )
    set_polygons <- plot_data$set_polygons
    region_labels_geom <- plot_data$region_labels
    region_polygons_geom <- plot_data$region_polygons
    region_centers_x_geom <- plot_data$region_centers_x
    region_centers_y_geom <- plot_data$region_centers_y

    align_idx <- match(combo_labels, region_labels_geom)
    region_polygons <- vector("list", n_id)
    region_centers_x <- rep(NA_real_, n_id)
    region_centers_y <- rep(NA_real_, n_id)
    has_geom <- !is.na(align_idx)
    if (any(has_geom)) {
      region_polygons[has_geom] <- region_polygons_geom[align_idx[has_geom]]
      region_centers_x[has_geom] <- region_centers_x_geom[align_idx[has_geom]]
      region_centers_y[has_geom] <- region_centers_y_geom[align_idx[has_geom]]
    }
  } else {
    plot_data <- NULL
    set_polygons <- list()
    region_polygons <- list()
    region_centers_x <- double(0)
    region_centers_y <- double(0)
  }

  e_x <- c(lapply(set_polygons, "[[", "x"), recursive = TRUE)
  e_y <- c(lapply(set_polygons, "[[", "y"), recursive = TRUE)

  # setup edges
  if (do_edges) {
    edges <- list(
      x = e_x,
      y = e_y,
      id.lengths = vapply(set_polygons, function(p) length(p$x), integer(1))
    )
  }

  if (do_fills || do_labels || do_quantities || do_annotations) {
    fills <- vector("list", n_id)
    for (i in seq_len(n_id)) {
      if (!nonzero[i]) {
        next
      }
      rp <- region_polygons[[i]]
      if (is.null(rp) || length(rp$id_lengths) == 0L) {
        next
      }
      fills[[i]]$x <- rp$x
      fills[[i]]$y <- rp$y
      fills[[i]]$id.lengths <- rp$id_lengths
    }
  }

  if (do_labels || do_quantities || do_annotations) {
    empty <- !nonzero_fit(fitted)

    centers_x <- region_centers_x
    centers_y <- region_centers_y
    centers_x[!nonzero] <- NA_real_
    centers_y[!nonzero] <- NA_real_
    centers_id <- seq_len(n_id)

    centers <- data.frame(
      x = centers_x,
      y = centers_y,
      id = centers_id,
      labels = rep(NA_character_, n_id),
      quantities = rep(NA, n_id),
      annotations = rep(NA_character_, n_id),
      labels_par_id = rep(NA_integer_, n_id),
      quantities_par_id = rep(NA_integer_, n_id),
      annotations_par_id = rep(NA_integer_, n_id),
      row.names = combo_labels,
      stringsAsFactors = FALSE
    )

    has_center <- !is.na(centers$x) & !is.na(centers$y)

    if (do_labels) {
      labels$labels <- labels$labels[which(!empty_sets)]
    }

    singles <- logical(NROW(centers))

    setnames <- rownames(dd)
    for (i in seq_len(n_e)) {
      set_i <- setnames[i]
      in_set <- vapply(combo_sets, function(s) set_i %in% s, logical(1))
      ind <- which(in_set & !empty & has_center)[1]

      if (!is.na(ind)) {
        if (do_labels) {
          centers$labels[ind] <- labels$labels[i]
          centers$labels_par_id[ind] <- i
        }
        singles[ind] <- TRUE
      }
    }

    others <- has_center & !singles & !empty

    if (do_quantities) {
      num <- orig[centers$id[singles | others]]

      if (is.null(quantities$labels)) {
        template <- quantities$template
        valid_types <- c("counts", "percent", "fraction")
        if (!is.null(template)) {
          if (!is.character(template) || length(template) != 1L) {
            stop("`quantities$template` must be a single string.")
          }
          placeholders <- regmatches(
            template,
            gregexpr("\\{([^{}]+)\\}", template)
          )[[1]]
          placeholders <- gsub("[{}]", "", placeholders)
          unknown <- setdiff(placeholders, valid_types)
          if (length(unknown) > 0L) {
            stop(
              "Unknown placeholder(s) in `quantities$template`: ",
              paste0("{", unknown, "}", collapse = ", "),
              ". Valid placeholders: ",
              paste0("{", valid_types, "}", collapse = ", "),
              "."
            )
          }
          type <- unique(placeholders)
        } else {
          type <- quantities$type
        }
        perc <- frac <- NULL
        fmt_fun <- quantities$format$fun
        fmt_args <- quantities$format$args

        total <- quantities$total
        if (is.null(total)) {
          total <- sum(num, na.rm = TRUE)
        }

        format_numeric <- function(x, default_fun, default_args = list()) {
          if (is.null(fmt_fun)) {
            out <- do.call(default_fun, c(list(x), default_args))
          } else {
            out <- do.call(fmt_fun, c(list(x), fmt_args))
          }
          as.character(out)
        }

        if ("percent" %in% type) {
          perc <- num / total * 100
          perc <- format_numeric(
            perc,
            function(x) ifelse(x >= 1, as.character(round(x)), "< 1")
          )
          perc <- sapply(perc[!is.na(perc)], function(x) paste0(x, " %"))
        }

        if ("fraction" %in% type) {
          frac <- num / total
          frac <- format_numeric(
            frac,
            signif,
            list(digits = options("digits")$digits)
          )
        }

        cnt <- format_numeric(
          num,
          signif,
          list(digits = options("digits")$digits)
        )

        values <- list(counts = cnt, percent = perc, fraction = frac)

        if (!is.null(template)) {
          n_q <- length(values[[type[1]]])
          qnt <- vapply(
            seq_len(n_q),
            function(i) {
              s <- template
              for (t in type) {
                s <- gsub(
                  paste0("{", t, "}"),
                  values[[t]][i],
                  s,
                  fixed = TRUE
                )
              }
              s
            },
            character(1)
          )
          centers$quantities[singles | others] <- qnt
        } else if (length(type) == 1) {
          centers$quantities[singles | others] <- values[[type]]
        } else if (length(type) == 2) {
          qnt <- paste0(values[[type[1]]], " (", values[[type[2]]], ")")
          centers$quantities[singles | others] <- qnt
        } else {
          qnt <- paste0(
            values[[type[1]]],
            " (",
            values[[type[2]]],
            "; ",
            values[[type[3]]],
            ")"
          )
          centers$quantities[singles | others] <- qnt
        }
      } else {
        if (!is.null(names(quantities$labels))) {
          named_quantities <- quantities$labels[rownames(centers)[
            singles | others
          ]]
          centers$quantities[singles | others] <- ifelse(
            is.na(named_quantities),
            NA_character_,
            unname(named_quantities)
          )
        } else {
          centers$quantities[singles | others] <-
            quantities$labels[which(!empty_subsets)][centers$id[
              singles | others
            ]]
        }
      }
    }

    if (do_annotations && !is.null(annotations$labels)) {
      rows <- singles | others
      annot_named <- annotations$labels[rownames(centers)[rows]]
      centers$annotations[rows] <- ifelse(
        is.na(annot_named),
        NA_character_,
        unname(annot_named)
      )
    }

    centers <- centers[has_center, , drop = FALSE]

    n_q <- sum(!is.na(centers$quantities))
    if (n_q > 0L) {
      centers$quantities_par_id[!is.na(centers$quantities)] <- seq_len(n_q)
    }

    n_a <- sum(!is.na(centers$annotations))
    if (n_a > 0L) {
      centers$annotations_par_id[!is.na(centers$annotations)] <- seq_len(n_a)
    }

    has_tag <- !is.na(centers$quantities_par_id) |
      !is.na(centers$labels_par_id) |
      !is.na(centers$annotations_par_id)

    centers <- centers[has_tag, , drop = FALSE]
  } else {
    centers <- NULL
  }

  container_data <- NULL
  if (do_container) {
    if (n_e > 0L) {
      cd <- list(
        outline_x = plot_data$container_outline_x,
        outline_y = plot_data$container_outline_y,
        complement_polygon = plot_data$complement_polygon,
        label_x = plot_data$complement_label_x,
        label_y = plot_data$complement_label_y
      )
    } else {
      # No fitted shapes: the complement *is* the whole container, so the
      # rectangle minus nothing is the rectangle itself. Synthesise it on the
      # R side rather than calling into the (shape-driven) Rust pipeline.
      cd <- list(
        outline_x = c(
          container$h - cx_half,
          container$h + cx_half,
          container$h + cx_half,
          container$h - cx_half,
          container$h - cx_half
        ),
        outline_y = c(
          container$k - cy_half,
          container$k - cy_half,
          container$k + cy_half,
          container$k + cy_half,
          container$k - cy_half
        ),
        complement_polygon = list(
          x = c(
            container$h - cx_half,
            container$h + cx_half,
            container$h + cx_half,
            container$h - cx_half
          ),
          y = c(
            container$k - cy_half,
            container$k - cy_half,
            container$k + cy_half,
            container$k + cy_half
          ),
          id_lengths = 4L
        ),
        label_x = container$h,
        label_y = container$k
      )
    }

    quantity_label <- NA_character_
    val <- container$complement
    if (is.null(val) || !is.finite(val)) {
      val <- container$complement_fitted
    }
    if (is.finite(val)) {
      quantity_label <- format(
        signif(val, digits = options("digits")$digits)
      )
    }

    container_data <- list(
      h = container$h,
      k = container$k,
      width = container$width,
      height = container$height,
      complement = container$complement,
      complement_fitted = container$complement_fitted,
      outline = list(x = cd$outline_x, y = cd$outline_y),
      complement_polygon = cd$complement_polygon,
      label_x = cd$label_x,
      label_y = cd$label_y,
      quantity_label = quantity_label
    )
  }

  # Refine label positions via eunoia's `place_labels` API: anchors come
  # from POI when the label fits inside its region, otherwise eunoia
  # places them exterior with a leader line. Also widens xlim/ylim so
  # exterior labels stay inside the panel viewport — labels are never
  # clipped.
  if (n_e > 0L) {
    placement_label <- do_complement_label &&
      !is.null(container_data) &&
      !is.null(container_data$quantity_label) &&
      !is.na(container_data$quantity_label)
    placed <- apply_label_placement(
      centers = centers,
      container_data = container_data,
      ellipses = dd,
      labels = labels,
      quantities = quantities,
      annotations = annotations,
      placement_opts = placement_opts,
      do_complement_label = placement_label,
      limits = limits,
      n_vertices = as.integer(n),
      label_precision = max(width, height) / 100
    )
    centers <- placed$centers
    container_data <- placed$container_data
    limits <- placed$limits
  }

  list(
    ellipses = dd,
    set_polygons = set_polygons,
    fitted.values = fitted,
    original.values = orig,
    fills = fills,
    edges = edges,
    labels = labels,
    quantities = quantities,
    annotations = annotations,
    centers = centers,
    empty_sets = empty_sets,
    empty_subsets = empty_subsets,
    container = container_data,
    xlim = limits$xlim,
    ylim = limits$ylim
  )
}
