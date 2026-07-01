# Convert a named vector of *union* (inclusive) combination areas into the
# areas of the disjoint (exclusive) regions they imply. Mirrors the
# decomposition used in the venn branch, but over whatever combinations are
# named (unnamed combinations are treated as 0 elsewhere).
union_to_disjoint <- function(combinations) {
  labels <- names(combinations)
  set_lists <- strsplit(labels, "&", fixed = TRUE)
  cards <- lengths(set_lists)
  disjoint <- numeric(length(labels))

  for (i in order(cards, decreasing = TRUE)) {
    this_sets <- set_lists[[i]]
    is_strict_super <- cards > cards[i] &
      vapply(set_lists, function(s) all(this_sets %in% s), logical(1))
    disjoint[i] <- combinations[i] - sum(disjoint[is_strict_super])
  }

  if (any(disjoint < 0)) {
    stop("Check your set configuration. Some disjoint areas are negative.")
  }

  stats::setNames(disjoint, labels)
}

fit_diagram <- function(
  combinations,
  type = c("euler", "venn"),
  input = c("disjoint", "union"),
  transform = identity,
  shape = c("circle", "ellipse", "rectangle", "square", "rotated_rectangle"),
  loss = c(
    "sum_squared",
    "sum_absolute",
    "sum_absolute_region_error",
    "sum_squared_region_error",
    "max_absolute",
    "max_squared",
    "root_mean_squared",
    "stress",
    "diag_error",
    "log_sum_absolute",
    "smooth_sum_absolute",
    "smooth_sum_absolute_region_error",
    "smooth_max_absolute",
    "smooth_max_squared",
    "smooth_diag_error",
    "smooth_log_sum_absolute"
  ),
  loss_aggregator = NULL,
  complement = NULL,
  control = list(),
  ...
) {
  input <- match.arg(input)
  shape <- match.arg(shape)
  type <- match.arg(type)
  loss <- resolve_loss(loss, loss_aggregator)

  if (!is.function(transform)) {
    stop("`transform` must be a function")
  }

  if (!is.numeric(combinations)) {
    stop("`combinations` must be numeric")
  }

  if (any(combinations < 0)) {
    stop("values in `combinations` cannot be negative")
  }

  if (is.null(attr(combinations, "names")) || any(names(combinations) == "")) {
    stop("every element in `combinations` needs to be named")
  }

  if (anyDuplicated(names(combinations)) > 0) {
    stop("names of elements in `combinations` cannot be duplicated")
  }

  if (!is.null(complement)) {
    if (
      !is.numeric(complement) ||
        length(complement) != 1L ||
        !is.finite(complement) ||
        complement < 0
    ) {
      stop("`complement` must be a single non-negative number.")
    }
    complement <- as.double(complement)
  }

  combo_name_parts <- strsplit(names(combinations), split = "&", fixed = TRUE)
  setnames <- unique(unlist(combo_name_parts, use.names = FALSE))
  n <- length(setnames)

  if (type == "venn" && !is.null(complement)) {
    stop("`complement` is not supported for `venn()` diagrams.")
  }

  # Venn early return (uses lookup table, no optimization)
  if (type == "venn") {
    combo_labels <- all_set_combinations(setnames)
    N <- length(combo_labels)
    combo_set_lists <- strsplit(combo_labels, "&", fixed = TRUE)
    cards <- lengths(combo_set_lists)

    areas <- numeric(N)
    for (j in seq_along(combo_name_parts)) {
      parts <- combo_name_parts[[j]]
      hit <- vapply(
        combo_set_lists,
        function(s) length(s) == length(parts) && all(s %in% parts),
        logical(1)
      )
      areas[hit] <- combinations[j]
    }

    if (input == "disjoint") {
      areas_disjoint <- areas
    } else {
      areas_disjoint <- numeric(N)
      for (i in order(cards, decreasing = TRUE)) {
        this_sets <- combo_set_lists[[i]]
        is_strict_super <- cards > cards[i] &
          vapply(combo_set_lists, function(s) all(this_sets %in% s), logical(1))
        areas_disjoint[i] <- areas[i] - sum(areas_disjoint[is_strict_super])
      }
      if (any(areas_disjoint < 0)) {
        stop("Check your set configuration. Some disjoint areas are negative.")
      }
    }

    orig <- stats::setNames(areas_disjoint, combo_labels)
    fit <- stats::setNames(rep.int(1, N), combo_labels)

    if (shape == "rotated_rectangle") {
      # Rotated-rectangle Venn geometry comes from eunoia's canonical layout
      # (rotation is what lets four rectangles open all 15 regions). It only
      # supports up to four sets, unlike the ellipse lookup.
      if (n > 4) {
        stop(
          "rotated-rectangle Venn diagrams support at most four sets; ",
          "use the default ellipse shape for five sets"
        )
      }
      layout <- venn_layout(setnames, "rotated_rectangle")
      shapes <- new_shape_frame("rotated_rectangle", n, setnames)
      shapes$h <- layout$h
      shapes$k <- layout$k
      shapes$width <- layout$width
      shapes$height <- layout$height
      shapes$phi <- layout$phi

      return(structure(
        list(
          shapes = shapes,
          original.values = orig,
          fitted.values = fit
        ),
        class = c("eulerr_venn", "venn", "euler", "list")
      ))
    }

    fpar <- venn_spec[[n]]
    rownames(fpar) <- setnames
    shapes <- ellipse_frame_to_shapes(fpar, "ellipse")

    return(structure(
      list(
        shapes = shapes,
        ellipses = fpar,
        original.values = orig,
        fitted.values = fit
      ),
      class = c("eulerr_venn", "venn", "euler", "list")
    ))
  }

  # Euler fit: delegate to Rust

  # `modifyList()` drops NULL entries, so an explicit `n_threads = NULL` (the
  # "use all cores" sentinel) would otherwise silently revert to the default.
  # Detect it before merging.
  n_threads_auto <- "n_threads" %in%
    names(control) &&
    is.null(control$n_threads)

  control <- utils::modifyList(
    list(
      extraopt = (n == 3) && (shape == "ellipse"),
      extraopt_threshold = 0.001,
      extraopt_control = list(),
      tolerance = 1e-3,
      max_sets = NULL,
      n_threads = default_n_threads(),
      optimizer = "auto",
      n_restarts = NULL,
      loss_eps = 0.01
    ),
    control
  )

  # Keep the global-search fallback gated on ellipse fits for now — eunoia's
  # rectangle/square fitters haven't been tuned to benefit from CMA-ES the
  # way the ellipse path has.
  extraopt_threshold <- if (isTRUE(control$extraopt) && shape == "ellipse") {
    as.numeric(control$extraopt_threshold)
  } else {
    NULL
  }

  tolerance <- if (is.null(control$tolerance)) {
    NULL
  } else {
    as.numeric(control$tolerance)
  }

  hard_cap <- max_sets_hard_cap()
  max_sets <- control$max_sets
  if (!is.null(max_sets)) {
    if (
      !is.numeric(max_sets) ||
        length(max_sets) != 1L ||
        !is.finite(max_sets) ||
        max_sets < 1 ||
        max_sets != as.integer(max_sets)
    ) {
      stop("`control$max_sets` must be a positive integer scalar")
    }
    max_sets <- as.integer(max_sets)
    if (max_sets > hard_cap) {
      stop(sprintf(
        "`control$max_sets` (%d) exceeds the hard cap of %d",
        max_sets,
        hard_cap
      ))
    }
  }

  # `n_threads` controls how many threads the restart loop is fanned across.
  # NULL (signalled by `n_threads_auto`) means "use all available cores"; the
  # default of 1 keeps the fit single-threaded. Results are identical
  # regardless of the thread count.
  if (n_threads_auto) {
    n_threads <- NULL
  } else {
    n_threads <- control$n_threads
    if (
      !is.numeric(n_threads) ||
        length(n_threads) != 1L ||
        !is.finite(n_threads) ||
        n_threads < 1 ||
        n_threads != as.integer(n_threads)
    ) {
      stop(
        "`control$n_threads` must be a positive integer scalar, or NULL for automatic"
      )
    }
    n_threads <- as.integer(n_threads)
  }

  optimizer_choices <- c(
    "auto",
    "levenberg_marquardt",
    "lbfgs",
    "nelder_mead",
    "mads",
    "cma_es",
    "cma_es_lm",
    "trf",
    "cma_es_trf"
  )
  optimizer <- match.arg(control$optimizer, optimizer_choices)

  loss_eps <- as.numeric(control$loss_eps)
  if (length(loss_eps) != 1L || !is.finite(loss_eps) || loss_eps <= 0) {
    stop("`control$loss_eps` must be a single positive number")
  }

  n_restarts <- control$n_restarts
  if (!is.null(n_restarts)) {
    if (
      !is.numeric(n_restarts) ||
        length(n_restarts) != 1L ||
        !is.finite(n_restarts) ||
        n_restarts < 1 ||
        n_restarts != as.integer(n_restarts)
    ) {
      stop("`control$n_restarts` must be a positive integer scalar, or NULL")
    }
    n_restarts <- as.integer(n_restarts)
  }

  effective_cap <- if (is.null(max_sets)) max_sets_default() else max_sets
  if (n > effective_cap) {
    stop(sprintf(
      "too many sets: %d requested, but maximum supported is %d (raise `control$max_sets` up to %d to override)",
      n,
      effective_cap,
      hard_cap
    ))
  }

  # Derive integer seed from R's RNG so set.seed() works
  seed <- sample.int(.Machine$integer.max, 1L)

  # A `transform` applies to the *disjoint* (exclusive) regions, since those are
  # the additive atoms the fitter targets. When the input is given as unions we
  # must decompose to disjoint areas first so the transform lands on the right
  # quantities, then hand the result to Rust as disjoint input.
  combo_names <- names(combinations)
  combo_values <- as.double(combinations)
  input_used <- input

  if (!identical(transform, identity)) {
    if (input == "union") {
      disjoint <- union_to_disjoint(combinations)
      combo_names <- names(disjoint)
      combo_values <- as.double(disjoint)
      input_used <- "disjoint"
    }

    combo_values <- as.double(transform(combo_values))

    if (
      length(combo_values) != length(combo_names) ||
        !all(is.finite(combo_values)) ||
        any(combo_values < 0)
    ) {
      stop(
        "`transform` must return a non-negative, finite value for every region"
      )
    }

    if (!is.null(complement)) {
      complement <- as.double(transform(complement))
      if (
        length(complement) != 1L || !is.finite(complement) || complement < 0
      ) {
        stop(
          "`transform` must return a single non-negative value for `complement`"
        )
      }
    }
  }

  result <- fit_euler_diagram(
    combo_names = combo_names,
    combo_values = combo_values,
    input = input_used,
    shape = shape,
    loss = loss,
    loss_eps = loss_eps,
    optimizer = optimizer,
    n_restarts = if (is.null(n_restarts)) NULL else as.numeric(n_restarts),
    extraopt_threshold = extraopt_threshold,
    tolerance = tolerance,
    max_sets = if (is.null(max_sets)) NULL else as.numeric(max_sets),
    complement = complement,
    seed = seed,
    n_threads = if (is.null(n_threads)) NULL else as.numeric(n_threads)
  )

  # Empty sets stay NA so downstream code (setup_geometry, plotting) detects
  # them via `is.na(shapes$h)` and skips polygonization — eunoia rejects
  # zero-area shapes, so we must not feed those through.
  n_all <- length(result$all_set_names)

  shapes <- new_shape_frame(shape, n_all, result$all_set_names)

  if (length(result$fitted_set_names) > 0L) {
    fidx <- match(result$fitted_set_names, result$all_set_names)
    shapes$h[fidx] <- result$h
    shapes$k[fidx] <- result$k
    shapes$a[fidx] <- result$a
    shapes$b[fidx] <- result$b
    shapes$phi[fidx] <- result$phi
    shapes$width[fidx] <- result$width
    shapes$height[fidx] <- result$height
    shapes$side[fidx] <- result$side
  }

  # The legacy `$ellipses` column is still populated for circle/ellipse fits
  # so consumers reading `fit$ellipses$h` etc. keep working without change.
  # For rectangle/square fits there is no equivalent ellipse representation,
  # so `$ellipses` is omitted — callers must read `$shapes` (see ?euler).
  fpar <- if (shape %in% c("circle", "ellipse")) {
    shapes_to_ellipse_frame(shapes)
  } else {
    NULL
  }

  labs <- result$combo_labels
  orig <- stats::setNames(result$original_values, labs)
  fit <- stats::setNames(result$fitted_values, labs)

  container <- if (isTRUE(result$has_container)) {
    container_area <- result$container_width * result$container_height
    list(
      h = result$container_h,
      k = result$container_k,
      width = result$container_width,
      height = result$container_height,
      complement = complement,
      complement_fitted = container_area - sum(result$fitted_values)
    )
  } else if (!is.null(complement)) {
    # eunoia rejects 0-/1-set specs with a complement, so synthesise a
    # square container around the (possibly empty) closed-form layout.
    fitted_total <- sum(result$fitted_values)
    side <- sqrt(fitted_total + complement)
    if (length(result$h) >= 1L) {
      cx <- mean(result$h)
      cy <- mean(result$k)
    } else {
      cx <- 0
      cy <- 0
    }
    list(
      h = cx,
      k = cy,
      width = side,
      height = side,
      complement = complement,
      complement_fitted = complement
    )
  } else {
    NULL
  }

  out <- list(
    shapes = shapes,
    ellipses = fpar,
    original.values = orig,
    fitted.values = fit,
    residuals = stats::setNames(result$residuals, labs),
    regionError = stats::setNames(result$region_error, labs),
    diagError = result$diag_error,
    stress = result$stress,
    container = container
  )
  # Drop `$ellipses` rather than carry an explicit NULL slot — `coef()` and
  # any other consumer that checks `is.null(fit$ellipses)` then sees a
  # missing field, which is the deprecation contract.
  if (is.null(fpar)) {
    out$ellipses <- NULL
  }

  structure(out, class = c("euler", "list"))
}

#' Allocate a fresh `$shapes` data frame for `n_all` sets. Rows for empty
#' sets keep NA in every column so downstream plotting can detect them via
#' `is.na(shapes$h)` regardless of shape kind.
#' @keywords internal
new_shape_frame <- function(shape, n_all, row_names) {
  data.frame(
    type = rep(shape, n_all),
    h = rep(NA_real_, n_all),
    k = rep(NA_real_, n_all),
    a = rep(NA_real_, n_all),
    b = rep(NA_real_, n_all),
    phi = rep(NA_real_, n_all),
    width = rep(NA_real_, n_all),
    height = rep(NA_real_, n_all),
    side = rep(NA_real_, n_all),
    row.names = row_names,
    stringsAsFactors = FALSE
  )
}

#' Promote a legacy 5-column ellipse data frame (h, k, a, b, phi) into the
#' wide `$shapes` schema. Used by the `venn()` path, where the precomputed
#' venn-shape lookup is still expressed as ellipses.
#' @keywords internal
ellipse_frame_to_shapes <- function(fpar, shape) {
  shapes <- new_shape_frame(shape, NROW(fpar), rownames(fpar))
  shapes$h <- fpar$h
  shapes$k <- fpar$k
  shapes$a <- fpar$a
  shapes$b <- fpar$b
  shapes$phi <- fpar$phi
  shapes
}

#' Project the wide `$shapes` schema back into the legacy 5-column
#' (h, k, a, b, phi) ellipse data frame for circle/ellipse fits. Preserves
#' the row order and row names so back-compat consumers see exactly the
#' shape they used to.
#' @keywords internal
shapes_to_ellipse_frame <- function(shapes) {
  data.frame(
    h = shapes$h,
    k = shapes$k,
    a = shapes$a,
    b = shapes$b,
    phi = shapes$phi,
    row.names = rownames(shapes),
    stringsAsFactors = TRUE
  )
}

# Translate the legacy (loss, loss_aggregator) pair into a single
# eunoia-style loss name, warning on use of either deprecated form.
resolve_loss <- function(loss, loss_aggregator) {
  loss_choices <- c(
    "sum_squared",
    "sum_absolute",
    "sum_absolute_region_error",
    "sum_squared_region_error",
    "max_absolute",
    "max_squared",
    "root_mean_squared",
    "stress",
    "diag_error",
    "log_sum_absolute",
    "smooth_sum_absolute",
    "smooth_sum_absolute_region_error",
    "smooth_max_absolute",
    "smooth_max_squared",
    "smooth_diag_error",
    "smooth_log_sum_absolute"
  )
  legacy_loss <- c("square", "abs", "region")

  # The unmodified default vector means the caller supplied no `loss`; pick
  # the first option without requiring the upstream defaults to be a literal
  # match for `loss_choices`.
  if (length(loss) > 1L) {
    if (!is.null(loss_aggregator)) {
      warning(
        "`loss_aggregator` is deprecated and will be removed in a future ",
        "version of eulerr. Pick a `loss` value directly instead.",
        call. = FALSE
      )
    }
    return(loss_choices[1L])
  }

  if (!is.null(loss_aggregator)) {
    warning(
      "`loss_aggregator` is deprecated and will be removed in a future ",
      "version of eulerr. Pick a `loss` value directly instead.",
      call. = FALSE
    )
    loss_aggregator <- match.arg(loss_aggregator, c("sum", "max"))
  }

  if (loss %in% legacy_loss) {
    agg <- if (is.null(loss_aggregator)) "sum" else loss_aggregator
    new_loss <- switch(
      paste(loss, agg, sep = "_"),
      square_sum = "sum_squared",
      square_max = "max_squared",
      abs_sum = "sum_absolute",
      abs_max = "max_absolute",
      region_sum = "sum_absolute_region_error",
      region_max = "diag_error"
    )
    warning(
      "Loss value '",
      loss,
      "' is deprecated. Use loss = '",
      new_loss,
      "' instead.",
      call. = FALSE
    )
    loss <- new_loss
  }

  match.arg(loss, loss_choices)
}
