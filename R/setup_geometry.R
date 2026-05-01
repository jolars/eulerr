#' Compute geometries and label locations
#'
#' @param fills fills
#' @param edges edges
#' @param labels labels
#' @param quantities quantities
#' @param n number of sets
#' @param id identity matrix
#' @param merged_sets which sets have been merged?
#' @param x an object of class 'euler'
#'
#' @return a list object with slots for the various objects
#' @keywords internal
setup_geometry <- function(
  x,
  fills,
  edges,
  labels,
  quantities,
  n,
  id,
  merged_sets
) {
  dd <- x$ellipses
  empty_sets <- is.na(dd[, 1L]) & !merged_sets
  empty_subsets <- rowSums(id[, empty_sets, drop = FALSE]) > 0

  orig <- x$original.values[!empty_subsets]
  fitted <- x$fitted.values[!empty_subsets]
  dd <- dd[!empty_sets, , drop = FALSE]

  # avoid plotting very small intersections
  nonzero <- nonzero_fit(fitted)
  nonzero <- ifelse(is.na(nonzero), FALSE, nonzero)

  do_fills <- !is.null(fills)
  do_edges <- !is.null(edges)
  do_labels <- !is.null(labels)
  do_quantities <- !is.null(quantities)

  id <- id[!empty_subsets, !empty_sets, drop = FALSE]

  h <- dd$h
  k <- dd$k
  a <- dd$a
  b <- dd$b
  phi <- dd$phi

  n_e <- NROW(dd)
  n_id <- 2L^n_e - 1L

  limits <- get_bounding_box(h, k, a, b, phi)

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
      n_vertices = as.integer(n),
      label_precision = max(width, height) / 100
    )
    set_polygons <- plot_data$set_polygons
    region_polygons <- plot_data$region_polygons
    region_centers_x <- plot_data$region_centers_x
    region_centers_y <- plot_data$region_centers_y
  } else {
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

  if (do_fills || do_labels || do_quantities) {
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

  if (do_labels || do_quantities) {
    n_singles <- sum(rowSums(id) == 1)
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
      labels = NA_character_,
      quantities = NA,
      labels_par_id = NA_integer_,
      quantities_par_id = NA_integer_,
      row.names = names(orig),
      stringsAsFactors = FALSE
    )

    has_center <- !is.na(centers$x) & !is.na(centers$y)

    if (do_labels) {
      labels <- list(labels = labels$labels[which(!empty_sets)])
    }

    singles <- logical(NROW(centers))

    for (i in seq_len(n_singles)) {
      ind <- which((id[, i] & !empty & has_center))[1]

      if (do_labels) {
        centers$labels[ind] <- labels$labels[i]

        centers$labels_par_id[ind] <- i
      }

      singles[ind] <- TRUE
    }

    others <- has_center & !singles & !empty

    if (do_quantities) {
      num <- orig[centers$id[singles | others]]

      if (is.null(quantities$labels)) {
        type <- quantities$type
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

        if (length(type) == 1) {
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
          named_quantities <- quantities$labels[rownames(centers)[singles | others]]
          centers$quantities[singles | others] <- ifelse(
            is.na(named_quantities),
            NA_character_,
            unname(named_quantities)
          )
        } else {
          centers$quantities[singles | others] <-
            quantities$labels[which(!empty_subsets)][centers$id[singles | others]]
        }
      }
    }

    centers <- centers[has_center, , drop = FALSE]

    n_q <- sum(!is.na(centers$quantities))
    if (n_q > 0L) {
      centers$quantities_par_id[!is.na(centers$quantities)] <- seq_len(n_q)
    }

    has_tag <- !is.na(centers$quantities_par_id) | !is.na(centers$labels_par_id)

    centers <- centers[has_tag, , drop = FALSE]
  } else {
    centers <- NULL
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
    centers = centers,
    empty_sets = empty_sets,
    empty_subsets = empty_subsets,
    xlim = limits$xlim,
    ylim = limits$ylim
  )
}
