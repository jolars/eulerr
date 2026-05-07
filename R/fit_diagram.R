fit_diagram <- function(
  combinations,
  type = c("euler", "venn"),
  input = c("disjoint", "union"),
  shape = c("circle", "ellipse"),
  loss = c(
    "sum_squared",
    "sum_absolute",
    "sum_absolute_region_error",
    "sum_squared_region_error",
    "max_absolute",
    "max_squared",
    "root_mean_squared",
    "stress",
    "diag_error"
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

  if (!is.numeric(combinations)) {
    stop("`combinations` must be numeric")
  }

  if (any(combinations < 0)) {
    stop("values in `combinations` cannot be negative")
  }

  if (is.null(attr(combinations, "names")) || any(names(combinations) == "")) {
    stop("every element in `combinations` needs to be named")
  }

  if (any(duplicated(names(combinations)))) {
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

    fpar <- venn_spec[[n]]
    rownames(fpar) <- setnames

    return(structure(
      list(
        ellipses = fpar,
        original.values = orig,
        fitted.values = fit
      ),
      class = c("eulerr_venn", "venn", "euler", "list")
    ))
  }

  # Euler fit: delegate to Rust
  control <- utils::modifyList(
    list(
      extraopt = (n == 3) && (shape == "ellipse"),
      extraopt_threshold = 0.001,
      extraopt_control = list(),
      tolerance = 1e-3,
      max_sets = NULL
    ),
    control
  )

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

  result <- fit_euler_diagram(
    combo_names = names(combinations),
    combo_values = as.double(combinations),
    input = input,
    shape = shape,
    loss = loss,
    extraopt_threshold = extraopt_threshold,
    tolerance = tolerance,
    max_sets = if (is.null(max_sets)) NULL else as.numeric(max_sets),
    complement = complement,
    seed = seed
  )

  # Empty sets stay NA so downstream code (setup_geometry, plotting) detects
  # them via `is.na(ellipses$h)` and skips polygonization — eunoia rejects
  # zero-radius ellipses, so we must not feed those through.
  n_all <- length(result$all_set_names)

  fpar <- data.frame(
    h = rep(NA_real_, n_all),
    k = rep(NA_real_, n_all),
    a = rep(NA_real_, n_all),
    b = rep(NA_real_, n_all),
    phi = rep(NA_real_, n_all),
    row.names = result$all_set_names,
    stringsAsFactors = TRUE
  )

  if (length(result$fitted_set_names) > 0L) {
    fidx <- match(result$fitted_set_names, result$all_set_names)
    fpar$h[fidx] <- result$h
    fpar$k[fidx] <- result$k
    fpar$a[fidx] <- result$a
    fpar$b[fidx] <- result$b
    fpar$phi[fidx] <- result$phi
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

  structure(
    list(
      ellipses = fpar,
      original.values = orig,
      fitted.values = fit,
      residuals = stats::setNames(result$residuals, labs),
      regionError = stats::setNames(result$region_error, labs),
      diagError = result$diag_error,
      stress = result$stress,
      container = container
    ),
    class = c("euler", "list")
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
    "diag_error"
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
