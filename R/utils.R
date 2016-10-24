# Tally set relationships from a matrix of logicals (set interactions)

tally_sets <- function(sets, ...) {
  for (i in seq_along(sets)) {
    assert_that(is.numeric(sets[[i]]) | is.logical(sets[[i]]))
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

