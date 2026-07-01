#' Tally set relationships
#'
#' @param sets a data.frame with set relationships and weights
#' @param weights a numeric vector
#'
#' @return
#' Calls [euler()] after the set relationships have been coerced to a named
#' numeric vector.
#' @keywords internal
tally_combinations <- function(sets, weights) {
  if (!is.matrix(sets)) {
    sets <- as.matrix(sets)
  }
  setnames <- colnames(sets)

  if (NROW(sets) == 0L || NCOL(sets) == 0L) {
    return(numeric(0))
  }

  m <- sets != 0
  patterns <- apply(m, 1L, function(row) paste(setnames[row], collapse = "&"))
  keep <- nzchar(patterns)

  if (any(keep)) {
    tap <- tapply(weights[keep], patterns[keep], sum)
    counts <- as.numeric(tap)
    names(counts) <- names(tap)
  } else {
    counts <- numeric(0)
  }

  singletons <- stats::setNames(numeric(length(setnames)), setnames)
  shared <- intersect(setnames, names(counts))
  if (length(shared) > 0L) {
    singletons[shared] <- counts[shared]
  }
  multi <- counts[setdiff(names(counts), setnames)]

  c(singletons, multi)
}

#' Rescale values to new range
#'
#' @param x numeric vector
#' @param new_min new min
#' @param new_max new max
#'
#' @return Rescaled vector
#' @keywords internal
rescale <- function(x, new_min, new_max) {
  (new_max - new_min) / (max(x) - min(x)) * (x - max(x)) + new_max
}

#' Update list with input
#'
#' A wrapper for [utils::modifyList()] that attempts to coerce non-lists to
#' lists before updating.
#'
#' @param old a list to be updated
#' @param new stuff to update `x` with
#'
#' @seealso [utils::modifyList()]
#' @return Returns an updated list.
#' @keywords internal
update_list <- function(old, new) {
  if (is.null(old)) {
    old <- list()
  }
  if (!is.list(new)) {
    tryCatch(new <- as.list(new))
  }
  if (!is.list(old)) {
    tryCatch(old <- as.list(old))
  }
  utils::modifyList(old, new)
}

#' Replace (refresh) a list
#'
#' Unlike `update_list`, this function only modifies, and does not add, items in
#' the list.
#'
#' @param old the original list
#' @param new the things to update `old` with
#'
#' @return A refreshed list.
#' @keywords internal
replace_list <- function(old, new) {
  update_list(old, new[names(new) %in% names(old)])
}

#' Check if object is strictly FALSE
#'
#' @param x object to check
#'
#' @return A logical.
#' @keywords internal
is_false <- function(x) identical(x, FALSE)

#' Null-coalesce: returns `y` when `x` is `NULL`, else `x`. Local
#' polyfill so we keep the R >= 4.2 floor (base `%||%` is 4.4+).
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Check if a vector is an integer
#'
#' @param x a vector
#'
#' @return TRUE of FALSE.
#' @keywords internal
is_integer <- function(x, tol = .Machine$double.eps^0.5) {
  all(abs(x - round(x)) < tol)
}

#' Check if vector is a real (numeric non-integer)
#'
#' @param x a vector
#'
#' @return A logical.
#' @keywords internal
is_real <- function(x, tol = .Machine$double.eps^0.5) {
  is.numeric(x) && !is_integer(x, tol = )
}

#' Number of CPU cores eulerr may use
#'
#' Collects the core-count limits we trust and returns the smallest, never less
#' than one. This mirrors the (much more elaborate) min-of-signals design of
#' `parallelly::availableCores()`, but only the durable, non-platform-specific
#' signals: the detected core count, `R CMD check`'s `_R_CHECK_LIMIT_CORES_`
#' (capped at two), and `OMP_THREAD_LIMIT`. We deliberately do not parse cgroup
#' quotas or HPC scheduler variables.
#'
#' @return A positive integer scalar.
#' @keywords internal
detect_available_cores <- function() {
  n_cores <- parallel::detectCores(logical = TRUE)
  caps <- if (is.na(n_cores)) 1L else as.integer(n_cores)

  # `R CMD check --as-cran` sets `_R_CHECK_LIMIT_CORES_`; the CRAN check farm
  # sets `OMP_THREAD_LIMIT`. Both cap how many cores we may use.
  if (nzchar(Sys.getenv("_R_CHECK_LIMIT_CORES_"))) {
    caps <- c(caps, 2L)
  }
  omp <- suppressWarnings(as.integer(Sys.getenv("OMP_THREAD_LIMIT", "")))
  if (!is.na(omp)) {
    caps <- c(caps, omp)
  }

  max(1L, min(caps))
}

#' Default number of threads for the fitter's restart loop
#'
#' Mirrors the approach taken by **data.table**: use half the available cores at
#' runtime, but stay single-threaded under `R CMD check` to honor CRAN's
#' two-core policy. eunoia parallelizes through `rayon`, which ignores the
#' `OMP_THREAD_LIMIT` signal that CRAN's check farm sets, so we throttle here
#' instead. Overrides are honored in order: the `eulerr.n_threads` option, the
#' `EULERR_NUM_THREADS` environment variable, and then R's conventional
#' `mc.cores` option (or `MC_CORES` environment variable), the last of which is
#' treated as an exact request bounded by the available cores.
#'
#' @return
#' A positive integer scalar (or whatever the `eulerr.n_threads` option is set
#' to, validated downstream).
#' @keywords internal
default_n_threads <- function() {
  # Explicit, eulerr-specific overrides win outright.
  opt <- getOption("eulerr.n_threads", NULL)
  if (!is.null(opt)) {
    return(opt)
  }

  env <- suppressWarnings(as.integer(Sys.getenv("EULERR_NUM_THREADS", "")))
  if (!is.na(env) && env >= 1L) {
    return(env)
  }

  available <- detect_available_cores()

  # Respect R's conventional `mc.cores` knob as an exact request, bounded by
  # what's actually available.
  mc <- getOption("mc.cores", NULL)
  if (is.null(mc)) {
    mc <- suppressWarnings(as.integer(Sys.getenv("MC_CORES", "")))
  }
  if (!is.null(mc) && !is.na(mc) && mc >= 1L) {
    return(max(1L, min(as.integer(mc), available)))
  }

  # Otherwise default to half of the available cores.
  max(1L, available %/% 2L)
}

#' Enumerate all 2^n - 1 combination labels for a set of names
#'
#' Generates labels in cardinality-first order: singletons first, then pairs,
#' then triples, etc. Within each cardinality, set names appear in their
#' original order. Used only by the Venn path (bounded at n = 5).
#'
#' @param setnames a character vector of set names
#'
#' @return A character vector of length 2^n - 1.
#' @keywords internal
all_set_combinations <- function(setnames) {
  n <- length(setnames)
  if (n == 0L) {
    return(character(0))
  }
  out <- character(0)
  for (k in seq_len(n)) {
    cmb <- utils::combn(setnames, k)
    labs <- if (k == 1L) {
      as.vector(cmb)
    } else {
      apply(cmb, 2L, paste, collapse = "&")
    }
    out <- c(out, labs)
  }
  out
}

#' Blend (average) colors
#'
#' @param rcol_in a vector of R colors
#'
#' @return A single R color
#' @keywords internal
mix_colors <- function(rcol_in) {
  rgb_in <- t(grDevices::col2rgb(rcol_in))
  lab_in <- grDevices::convertColor(rgb_in, "sRGB", "Lab", scale.in = 255)
  mean_col <- colMeans(lab_in)
  rgb_out <- grDevices::convertColor(mean_col, "Lab", "sRGB", scale.out = 1)
  grDevices::rgb(rgb_out)
}

#' Setup gpars
#'
#' @param default default values
#' @param user user-inputted values
#' @param n required number of items
#'
#' @return a `gpar` object
#' @keywords internal
setup_gpar <- function(default = list(), user = list(), n) {
  # set up gpars
  if (is.list(user)) {
    gp <- replace_list(default, user)
  } else {
    gp <- default
  }
  gp <- lapply(gp, function(x) if (is.function(x)) x(n) else x)

  do.call(grid::gpar, lapply(gp, rep_len, n))
}

#' Overlay per-panel overrides onto a styling parameter
#'
#' Used to apply `by_group` overrides to `fills`, `patterns`, `edges`, `labels`,
#' and `quantities` for a single panel produced by `euler(..., by = ...)`. The
#' override list contains gpar-level fields (and optionally `rot`), which are
#' overlaid onto `param$gp` (and `param$set_gp` for shape-mode patterns).
#' Structural fields are not overlaid here — they must be filtered upstream.
#'
#' @param param a list with `$gp` (a `grid::gpar`) and optionally `$rot` and/or
#'   `$set_gp`.
#' @param override a flat named list of fields to overlay.
#'
#' @return The param list with overlaid fields.
#' @keywords internal
apply_panel_overrides <- function(param, override) {
  if (is.null(param) || is.null(override) || length(override) == 0L) {
    return(param)
  }
  overlay_gp <- function(gp) {
    if (is.null(gp)) {
      return(gp)
    }
    n <- if (length(gp) > 0L) max(lengths(gp), 1L) else 1L
    for (nm in names(override)) {
      if (nm == "rot") {
        next
      }
      gp[[nm]] <- rep_len(override[[nm]], n)
    }
    gp
  }
  if (!is.null(param$gp)) {
    param$gp <- overlay_gp(param$gp)
  }
  if (!is.null(param$set_gp)) {
    param$set_gp <- overlay_gp(param$set_gp)
  }
  if (!is.null(override$rot) && !is.null(param$rot)) {
    param$rot <- rep_len(override$rot, length(param$rot))
  }
  param
}

#' Dummy code a data.frame
#'
#' @param x a data.frame
#' @param sep character for separating dummy code factors and their levels when
#'   constructing names
#' @param factor_names whether to include factor names when creating new names
#'   for dummy codes
#'
#' @return A dummy-coded version of x.
#'
#' @keywords internal
dummy_code <- function(x, sep = "_", factor_names = TRUE) {
  fac_chr <- vapply(x, function(x) is.character(x) || is.factor(x), logical(1))

  tmp <- x[, fac_chr, drop = FALSE]

  # convert characters into factors
  for (i in seq_len(ncol(tmp))) {
    tmp[, i] <- as.factor(tmp[, i])
  }

  lvls <- lapply(tmp, function(x) levels(x))

  n_levels <- vapply(lvls, function(x) length(x) - 1, double(1))
  dummy_levels <- lapply(lvls, function(x) x[-length(x)])
  n_levels_tot <- sum(n_levels)

  dummy_names <- dummy_levels

  if (isTRUE(factor_names)) {
    for (i in seq_along(dummy_names)) {
      dummy_names[[i]] <- paste(
        names(dummy_levels)[i],
        dummy_levels[[i]],
        sep = sep
      )
    }
  }

  dummy_names <- unlist(dummy_names)

  if (anyDuplicated(dummy_names) > 0) {
    stop(paste(
      "duplicated names for dummy coded factors were generated;",
      "please consider specifying 'factor_names = TRUE'."
    ))
  }

  out <- matrix(
    FALSE,
    nrow(x),
    n_levels_tot,
    dimnames = list(NULL, dummy_names)
  )

  k <- 1

  for (i in seq_along(n_levels)) {
    kk <- as.numeric(tmp[, i])
    for (j in seq_len(n_levels[i])) {
      out[, k] <- kk == j
      k <- 1 + k
    }
  }

  cbind(x[, !fac_chr, drop = FALSE], out)
}

nonzero_fit <- function(x) abs(x) / sum(abs(x) + .Machine$double.eps) > 1e-4
