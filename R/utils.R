# Tally set relationships from a matrix of logicals (set interactions)

tally_sets <- function(sets, ...) {
  for (i in seq_along(sets)) {
    assertthat::assert_that(
      any(is.numeric(sets[[i]]), is.logical(sets[[i]]))
    )
  }
  if (!is.matrix(sets)) {
    sets <- as.matrix(sets)
  }
  setlist <- vector("list", length = ncol(sets))

  for (i in seq_along(colnames(sets))) {
    setlist[[i]] <- utils::combn(colnames(sets), i)
  }

  tally <- double(0L)

  for (i in seq_along(setlist)) {
    for (j in 1:ncol(setlist[[i]])) {
      combos <- setlist[[i]][, j]
      if (i == 1) {
        intersections <- sets[, combos]
      } else {
        intersections <- apply(sets[, combos], 1, all)
      }
      sum_intersections <- sum(intersections)
      names(sum_intersections) <- paste0(combos, collapse = "&")
      tally <- c(tally, sum_intersections)
    }
  }
  eulerr(tally, ...)
}


# Set up binary indexing --------------------------------------------------

binary_indexing <- function(n) {
  no_combos <- choose(n, 1L:n)
  id <- matrix(FALSE, sum(no_combos), n)
  cum_combos <- c(0, cumsum(no_combos)[-n])

  k <- 1
  for (i in cum_combos) {
    permutations <- utils::combn(n, k)
    for (j in 1:(ncol(permutations))) {
      id[i + j, permutations[, j]] <- TRUE
    }
    k <- k + 1
  }

  return(id)
}

# Faster expand.grid ------------------------------------------------------

expand_grid <- function(x, y) {
  cbind(rep.int(x, length(y)),
        rep.int(x, rep.int(length(x), length(y))))
}

# Scale values to range ---------------------------------------------------

rescale <- function(x, new_min, new_max) {
  (new_max - new_min) / (max(x) - min(x)) * (x - max(x)) + new_max
}
