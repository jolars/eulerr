parse_list <- function(combinations) {
  if (is.null(attr(combinations, "names"))) {
    stop(
      "when `combinations` is a list, all vectors in that list must be named"
    )
  }

  if (any(names(combinations) == "")) {
    stop("all elements of `combinations` must be named")
  }

  if (!all(sapply(combinations, anyDuplicated) == 0)) {
    stop("vectors in `combinations` cannot contain duplicates")
  }

  if (any(duplicated(names(combinations)))) {
    stop("names of elements in `combinations` must be unique")
  }

  sets <- names(combinations)

  all_elems <- unlist(lapply(combinations, as.character), use.names = FALSE)
  all_set_idx <- rep.int(seq_along(combinations), lengths(combinations))

  if (length(all_elems) == 0L) {
    counts <- numeric(0)
  } else {
    by_elem <- split(all_set_idx, all_elems)
    patterns <- vapply(
      by_elem,
      function(idx) paste(sets[sort.int(unique(idx))], collapse = "&"),
      character(1)
    )
    tab <- table(patterns)
    counts <- as.numeric(tab)
    names(counts) <- names(tab)
  }

  singletons <- stats::setNames(numeric(length(sets)), sets)
  shared <- intersect(sets, names(counts))
  if (length(shared) > 0L) {
    singletons[shared] <- counts[shared]
  }
  multi <- counts[setdiff(names(counts), sets)]

  c(singletons, multi)
}

parse_dataframe <- function(
  combinations,
  weights = NULL,
  by = NULL,
  facs,
  sep = "_",
  factor_names = TRUE,
  ...
) {
  if (any(grepl("&", colnames(combinations), fixed = TRUE))) {
    stop("names of columns in `combinations` must not contain '$'")
  }

  if (!is.null(facs)) {
    if (is.list(facs)) {
      if (!is.null(names(facs))) {
        nms <- names(facs)
      } else {
        nms <- sapply(by[-1L], deparse)
      }
    } else {
      nms <- deparse(by)
    }

    if (is.list(facs)) {
      stopifnot(length(facs) < 3)
    } else {
      facs <- list(facs)
    }

    dd <- as.data.frame(facs, col.names = nms, stringsAsFactors = TRUE)
    groups <- unique(dd)
    rownames(groups) <- NULL

    out <- g <- vector("list", NROW(groups))

    by_ind <- match(nms, colnames(combinations))

    for (i in seq_len(NROW(groups))) {
      ind <- apply(dd, 1, function(x) all(x == groups[i, ]))
      out[[i]] <- combinations[ind, -by_ind, drop = FALSE]
      names(out)[[i]] <- paste(unlist(groups[i, , drop = TRUE]), collapse = ".")
    }

    attr(out, "groups") <- groups
  } else {
    is_factor <- vapply(
      combinations,
      function(x) is.factor(x) || is.character(x),
      logical(1)
    )

    reals <- vapply(combinations, is_real, FUN.VALUE = logical(1))

    if (any(reals)) {
      stop("you cannot use non-integer numeric variables.")
    }

    if (any(is_factor)) {
      combinations <- dummy_code(
        combinations,
        sep = sep,
        factor_names = factor_names
      )
    }

    if (is.null(weights)) {
      weights <- rep.int(1L, NROW(combinations))
    }

    out <- tally_combinations(combinations, weights)
  }
  out
}
