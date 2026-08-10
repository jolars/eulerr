# Shared geometry-index and fill-resolution helpers used by both the static
# `grid` renderer (`plot.euler()`) and the interactive `euler_widget()` backend.
# Keeping these here means both renderers agree on region ordering and on the
# resolved per-region fill colors (including Lab-space blending of overlaps), so
# a diagram never looks different between the two backends.

#' Build the region index for a fitted diagram
#'
#' Constructs the working set of regions shared across the plotting pipeline:
#' singletons (in input order) first, then multi-set combinations in cardinality
#' and lexicographic order, together with the sparse `id` membership matrix.
#'
#' @param x an object of class `euler` (or `venn`), possibly a faceted list with
#'   a `groups` attribute.
#'
#' @return a list with `shapes`, `n_e` (number of sets), `setnames`,
#'   `combo_labels`, `combo_sets`, `n_id` (number of regions), and the `id`
#'   matrix (`id[i, j]` is `TRUE` iff region `i` includes set `j`).
#' @keywords internal
build_region_index <- function(x) {
  groups <- attr(x, "groups")
  do_groups <- !is.null(groups)

  shapes <- if (do_groups) x[[1L]]$shapes else x$shapes
  n_e <- NROW(shapes)
  setnames <- rownames(shapes)

  # Build a sparse combo_labels that places singletons (in input order) first,
  # then multi-set combos in cardinality + lexicographic order. This is the
  # working set of regions for the entire plot pipeline.
  if (do_groups) {
    all_labels <- unique(unlist(
      lapply(x, function(xi) names(xi$fitted.values)),
      use.names = FALSE
    ))
  } else {
    all_labels <- names(x$fitted.values)
  }
  singletons_present <- intersect(setnames, all_labels)
  multi_labels <- setdiff(all_labels, singletons_present)
  multi_card <- lengths(strsplit(multi_labels, "&", fixed = TRUE))
  multi_labels <- multi_labels[order(multi_card, multi_labels)]
  combo_labels <- c(singletons_present, multi_labels)
  combo_sets <- strsplit(combo_labels, "&", fixed = TRUE)
  n_id <- length(combo_labels)

  # Sparse equivalent of the legacy bit_indexr `id` matrix: rows are populated
  # combinations (in `combo_labels` order), columns are sets (in `setnames`
  # order). id[i, j] is TRUE iff combination i includes set j.
  if (n_id > 0L && n_e > 0L) {
    id <- t(vapply(
      combo_sets,
      function(s) setnames %in% s,
      logical(n_e)
    ))
    if (n_id == 1L) {
      id <- matrix(id, nrow = 1L)
    }
    dimnames(id) <- list(combo_labels, setnames)
  } else {
    id <- matrix(
      FALSE,
      nrow = n_id,
      ncol = n_e,
      dimnames = list(combo_labels, setnames)
    )
  }

  list(
    shapes = shapes,
    n_e = n_e,
    setnames = setnames,
    combo_labels = combo_labels,
    combo_sets = combo_sets,
    n_id = n_id,
    id = id
  )
}

#' Resolve per-region fill colors and alphas
#'
#' Normalizes the user's `fills` specification into a per-region fill and alpha,
#' handling named-fill matching in `disjoint`/`union` mode, length-1/n_sets/
#' n_subsets expansion, and Lab-space blending of overlaps via [mix_colors()].
#'
#' @param fills the user's `fills` argument (already stripped of any `by_group`
#'   overrides): `TRUE`, a color vector, or a list.
#' @param dots additional named styling arguments (from `...`).
#' @param id the region membership matrix from [build_region_index()].
#' @param setnames set names, in `id` column order (possibly merged).
#' @param n_e number of sets.
#' @param n_id number of regions.
#' @param opar package options (from [eulerr_options()]).
#'
#' @return a list with `gp` (a [grid::gpar()] of length `n_id`, as consumed by
#'   `setup_grobs()`). The resolved per-region fills and alphas are available as
#'   `gp$fill` and `gp$alpha` for reuse by other renderers.
#' @keywords internal
resolve_region_fills <- function(fills, dots, id, setnames, n_e, n_id, opar) {
  fills_out <- replace_list(
    list(
      fill = opar$fills$fill,
      alpha = opar$fills$alpha,
      mode = opar$fills$mode
    ),
    if (is.list(fills)) {
      fills
    } else if (isTRUE(fills)) {
      list()
    } else {
      list(fill = fills)
    }
  )
  fills_out <- replace_list(fills_out, dots)
  fills_out$col <- "transparent"

  if (is.function(fills_out$fill)) {
    fills_out$fill <- fills_out$fill(n_e)
  }

  fill_names <- names(fills_out$fill)
  if (!is.null(fill_names)) {
    all_named <- all(nzchar(fill_names))
    any_named <- any(nzchar(fill_names))
    if (any_named && !all_named) {
      stop("`fills$fill` must be either fully named or fully unnamed.")
    }
    if (all_named) {
      if (!fills_out$mode %in% c("disjoint", "union")) {
        stop("`fills$mode` must be either 'disjoint' or 'union'.")
      }
      subset_names <- rownames(id)
      valid_fill_names <- c(setnames, subset_names)
      unknown <- setdiff(fill_names, valid_fill_names)
      if (length(unknown) > 0L) {
        stop(
          "`fills$fill` has unknown names: ",
          paste(unknown, collapse = ", ")
        )
      }

      default_fill <- opar$fills$fill
      if (is.function(default_fill)) {
        default_fill <- default_fill(n_e)
      }
      n_default <- length(default_fill)
      if (n_default == n_e && n_default != n_id) {
        per_set <- default_fill
        default_fill <- character(n_id)
        for (ii in seq_len(n_id)) {
          set_idx <- which(id[ii, ])
          if (length(set_idx) == 1L) {
            default_fill[ii] <- per_set[set_idx]
          } else if (length(set_idx) > 1L) {
            default_fill[ii] <- mix_colors(per_set[set_idx])
          }
        }
      } else if (n_default == 1L || n_default == n_id) {
        default_fill <- rep_len(default_fill, n_id)
      } else {
        stop("Default `fills$fill` must have length 1, n_sets, or n_subsets.")
      }

      fill_map <- default_fill
      names(fill_map) <- subset_names
      named_sets <- intersect(fill_names, setnames)
      if (identical(fills_out$mode, "union") && length(named_sets) > 0L) {
        for (set_name in named_sets) {
          fill_map[id[, set_name]] <- fills_out$fill[[set_name]]
        }
      }
      named_subsets <- intersect(fill_names, subset_names)
      fill_map[named_subsets] <- unname(fills_out$fill[named_subsets])
      fills_out$fill <- fill_map
    }
  }

  n_fills <- length(fills_out$fill)
  if (n_fills == n_e && n_fills != n_id) {
    per_set <- fills_out$fill
    fills_out$fill <- character(n_id)
    for (i in seq_len(n_id)) {
      set_idx <- which(id[i, ])
      if (length(set_idx) == 1L) {
        fills_out$fill[i] <- per_set[set_idx]
      } else if (length(set_idx) > 1L) {
        fills_out$fill[i] <- mix_colors(per_set[set_idx])
      }
    }
  } else if (!(n_fills %in% c(1L, n_id))) {
    stop("`fills$fill` must have length 1, n_sets, or n_subsets.")
  }

  fills <- list()
  fills_gp <- fills_out
  fills_gp$mode <- NULL
  fills$gp <- setup_gpar(fills_gp, list(), n_id)
  fills
}
