parse_list <- function(combinations)
{
  stopifnot(!is.null(attr(combinations, "names")),
            !any(names(combinations) == ""),
            !any(duplicated(names(combinations))),
            all(sapply(combinations, anyDuplicated) == 0))

  sets <- names(combinations)
  n <- length(sets)

  id <- bit_indexr(n)

  out <- integer(nrow(id))
  names(out) <- apply(id, 1L, function(x) paste(sets[x], collapse = "&"))

  for (i in seq_len(nrow(id)))
    out[i] <- length(Reduce(intersect, combinations[id[i, ]]))

  out
}

parse_dataframe <- function(combinations,
                            weights = NULL,
                            by = NULL,
                            facs,
                            sep = "_",
                            factor_names = TRUE,
                            ...)
{
  stopifnot(!any(grepl("&", colnames(combinations), fixed = TRUE)))

  if (!is.null(facs)) {
    if (is.list(facs)) {
      if (!is.null(names(facs)))
        nms <- names(facs)
      else {
        nms <- sapply(by[-1L], deparse)
      }
    } else
      nms <- deparse(by)

    if (is.list(facs))
      stopifnot(length(facs) < 3)
    else
      facs <- list(facs)

    dd <- as.data.frame(facs, col.names = nms)
    groups <- unique(dd)
    rownames(groups) <- NULL

    out <- g <- vector("list", NROW(groups))

    by_ind <- match(nms, colnames(combinations))

    for (i in seq_len(NROW(groups))) {
      ind <- apply(dd, 1, function(x) all(x == groups[i, ]))
      out[[i]] <- combinations[ind, -by_ind, drop = FALSE]
      names(out)[[i]] <- paste(unlist(groups[i, , drop = TRUE]),
                               collapse = ".")
    }

    attr(out, "groups") <- groups
  } else {
    is_factor <- vapply(combinations,
                        function(x) is.factor(x) || is.character(x),
                        logical(1))

    reals <- vapply(combinations,
                    is_real,
                    FUN.VALUE = logical(1))

    if (any(reals))
      stop("you cannot use non-integer numeric variables.")

    if (any(is_factor))
      combinations <- dummy_code(combinations,
                                 sep = sep,
                                 factor_names = factor_names)

    if (is.null(weights))
      weights <- rep.int(1L, NROW(combinations))

    out <- tally_combinations(combinations, weights)
  }
  out
}
