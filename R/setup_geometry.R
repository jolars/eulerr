
#' Compute geometries and label locations
#'
#' @param x an object of class 'euler'
#' @param do_fills do fills?
#' @param do_edges do edges?
#' @param do_labels do labels?
#' @param do_quantities do quantities?
#'
#' @return a list object with slots for the various objects
#' @keywords internal
setup_geometry <- function(x,
                           fills,
                           edges,
                           labels,
                           quantities,
                           percentages,
                           n,
                           id) {
  dd <- x$ellipses
  empty_sets <- is.na(dd[, 1L])
  empty_subsets <- rowSums(id[, empty_sets, drop = FALSE]) > 0

  orig <- x$original.values[!empty_subsets]
  fitted <- x$fitted.values[!empty_subsets]
  dd <- dd[!empty_sets, , drop = FALSE]

  # avoid plotting very small intersections
  nonzero <- abs(fitted)/max(abs(fitted)) > 1e-3
  nonzero <- ifelse(is.na(nonzero), FALSE, nonzero)

  do_fills <- !is.null(fills)
  do_edges <- !is.null(edges)
  do_labels <- !is.null(labels)
  do_quantities <- !is.null(quantities)
  do_percentages <- !is.null(percentages)

  id <- id[!empty_subsets, !empty_sets, drop = FALSE]

  h <- dd$h
  k <- dd$k
  a <- dd$a
  b <- dd$b
  phi <- dd$phi

  n_e <- NROW(dd)
  n_id <- 2L^n_e - 1L

  e <- ellipse(h, k, a, b, phi, n)
  e_x <- c(lapply(e, "[[", "x"), recursive = TRUE)
  e_y <- c(lapply(e, "[[", "y"), recursive = TRUE)

  limits <- get_bounding_box(h, k, a, b, phi)

  # setup edges
  if (do_edges)
    edges <- list(x = e_x, y = e_y, id.lengths = rep.int(n, n_e))

  if (do_fills || do_labels || do_quantities || do_percentages) {
    # decompose ellipse polygons into intersections
    pieces <- fills <- vector("list", n_id)
    for (i in rev(seq_len(n_id))) {
      if (nonzero[i]) {
        idx <- which(id[i, ])
        n_idx <- length(idx)

        if (n_idx == 1L) {
          pieces[[i]] <- list(e[[idx[1]]])
        } else {
          pieces[[i]] <- poly_clip(e[[idx[1L]]], e[[idx[2L]]], "intersection")
          if (n_idx > 2L) {
            for (j in 3L:n_idx) {
              pieces[[i]] <- poly_clip(pieces[[i]], e[[idx[j]]], "intersection")
            }
          }
        }

        for (j in which(!id[i, ])) {
          pieces[[i]] <- poly_clip(pieces[[i]], e[[j]], "minus")
        }
      }
    }

    for (i in seq_along(pieces)) {
      x0 <- lapply(pieces[[i]], "[[", "x")
      y0 <- lapply(pieces[[i]], "[[", "y")

      if (length(x0) > 0L) {
        fills[[i]]$x <- c(x0, recursive = TRUE)
        fills[[i]]$y <- c(y0, recursive = TRUE)
        fills[[i]]$id.lengths <- lengths(x0)
      }
    }
  }

  if (do_labels || do_quantities || do_percentages) {
    n_singles <- sum(rowSums(id) == 1)
    empty <- abs(fitted) < sqrt(.Machine$double.eps)

    width <- abs(limits$xlim[1] - limits$xlim[2])
    height <- abs(limits$ylim[1] - limits$ylim[2])

    prec <- max(width, height)/100

    centers <- lapply(pieces, locate_centers, precision = prec)

    centers_x <- vapply(centers, "[[", "x", FUN.VALUE = double(1))
    centers_y <- vapply(centers, "[[", "y", FUN.VALUE = double(1))
    centers_id <- seq_len(n_id)

    centers <- data.frame(x = centers_x,
                          y = centers_y,
                          id = centers_id,
                          labels = NA_character_,
                          quantities = NA,
                          percentages = NA_character_,
                          labels_par_id = NA_integer_,
                          others_par_id = NA_integer_,
                          row.names = names(orig),
                          stringsAsFactors = FALSE)

    has_center <- !is.na(centers$x) & !is.na(centers$y)

    if (do_labels) {
      labels <- list(labels = labels$labels[which(!empty_sets)])
    }

    singles <- logical(NROW(centers))

    for (i in seq_len(n_singles)) {
      ind <- which((id[, i] & !empty & has_center))[1]

      if (do_labels) {
        if (is.na(centers$labels[ind])) {
          centers$labels[ind] <- labels$labels[i]
        } else {
          centers$labels[ind] <- paste(centers$labels[ind],
                                       labels$labels[i],
                                       sep = ",")
        }
      }

      singles[ind] <- TRUE
    }

    others <- has_center & !singles & !empty

    if (do_quantities || do_percentages) {
      digits <- options("digits")$digits

      if (is.null(quantities$labels)) {
        centers$quantities[singles | others] <-
          signif(orig[centers$id[singles | others]], digits)
      } else {
        centers$quantities[singles | others] <-
          signif(quantities$labels[which(!empty_subsets)][centers$id[singles | others]], digits)
      }
    }

    if (do_percentages) {
      if (is.null(percentages$labels)) {
        perc <- centers$quantities/sum(centers$quantities, na.rm = TRUE)*100
        perc <- ifelse(perc >= 1, as.character(round(perc)), "< 1")

        centers$percentages[!is.na(perc)] <-
          sapply(perc[!is.na(perc)], function(x) {
            if (do_quantities)
              paste0("(", x, " %)")
            else
              paste0(x, " %")
          })
      } else {
        centers$percentages[others | singles] <-
          percentages$labels[which(!empty_subsets)][centers$id[others | singles]]
      }
    }

    centers <- centers[has_center, , drop = FALSE]

    centers$labels_par_id[which(!is.na(centers$labels))] <-
      seq_len(sum(singles))

    centers$others_par_id[!is.na(centers$quantities) | !is.na(centers$percentages)] <-
      seq_len(sum(others | singles))

    naornull <- function(x)
      is.na(x) | is.null(x)

    has_tag <- !naornull(centers$quantities) |
      !naornull(centers$labels) |
      !naornull(centers$percentages)

    centers <- centers[has_tag, , drop = FALSE]
  } else {
    centers <- NULL
  }

  list(ellipses = dd,
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
       ylim = limits$ylim)
}
