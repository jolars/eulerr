fit_diagram <- function(
  combinations,
  type = c("euler", "venn"),
  input = c("disjoint", "union"),
  shape = c("circle", "ellipse"),
  loss = c("square", "abs", "region"),
  loss_aggregator = c("sum", "max"),
  control = list(),
  ...
) {
  input <- match.arg(input)
  shape <- match.arg(shape)
  type <- match.arg(type)
  loss <- match.arg(loss)
  loss_aggregator <- match.arg(loss_aggregator)

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

  combo_name_parts <- strsplit(names(combinations), split = "&", fixed = TRUE)
  setnames <- unique(unlist(combo_name_parts, use.names = FALSE))
  n <- length(setnames)

  # Venn early return (uses lookup table, no optimization)
  if (type == "venn") {
    id <- bit_indexr(n)
    N <- NROW(id)

    areas <- double(N)
    for (i in seq_len(N)) {
      s <- setnames[id[i, ]]
      for (j in seq_along(combo_name_parts)) {
        if (setequal(s, combo_name_parts[[j]])) {
          areas[i] <- combinations[j]
        }
      }
    }

    if (input == "disjoint") {
      areas_disjoint <- areas
    } else {
      areas_disjoint <- double(N)
      for (i in rev(seq_along(areas))) {
        prev <- rowSums(id[, id[i, ], drop = FALSE]) == sum(id[i, ])
        areas_disjoint[i] <- areas[i] - sum(areas_disjoint[prev])
      }
      if (any(areas_disjoint < 0)) {
        stop("Check your set configuration. Some disjoint areas are negative.")
      }
    }

    combo_labels <- apply(id, 1L, function(x) {
      paste0(setnames[x], collapse = "&")
    })
    orig <- stats::setNames(areas_disjoint, combo_labels)

    fpar <- venn_spec[[n]]
    rownames(fpar) <- setnames

    return(structure(
      list(
        ellipses = fpar,
        original.values = orig,
        fitted.values = rep(1, N)
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
      tolerance = 1e-3
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

  # Derive integer seed from R's RNG so set.seed() works
  seed <- sample.int(.Machine$integer.max, 1L)

  result <- fit_euler_diagram(
    combo_names = names(combinations),
    combo_values = as.double(combinations),
    input = input,
    shape = shape,
    loss = loss,
    loss_aggregator = loss_aggregator,
    extraopt_threshold = extraopt_threshold,
    tolerance = tolerance,
    seed = seed
  )

  # Assemble ellipses data frame. Initial values:
  #   - 0 if no sets were fitted at all (all-zero input) — matches legacy behavior
  #   - NA otherwise; non-empty rows are filled in below
  n_all <- length(result$all_set_names)
  init <- if (length(result$fitted_set_names) == 0L) 0 else NA_real_

  fpar <- data.frame(
    h = rep(init, n_all),
    k = rep(init, n_all),
    a = rep(init, n_all),
    b = rep(init, n_all),
    phi = rep(init, n_all),
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

  structure(
    list(
      ellipses = fpar,
      original.values = orig,
      fitted.values = fit,
      residuals = stats::setNames(result$residuals, labs),
      regionError = stats::setNames(result$region_error, labs),
      diagError = result$diag_error,
      stress = result$stress
    ),
    class = c("euler", "list")
  )
}
