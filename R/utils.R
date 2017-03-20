# Wrapper for bit_index to return a logical matrix

bit_indexr <- function(x) {
  out <- bit_index(x)
  mode(out) <- "logical"
  out
}

# Tally set relationships from a matrix of logicals ----------------------

tally_combinations <- function(sets, ...) {
  if (!is.matrix(sets)) {
    sets <- as.matrix(sets)
  }

  id <- bit_indexr(ncol(sets))
  tally <- double(nrow(id))

  for (i in 1:nrow(id)) {
    tally[i] <- sum(apply(sets, 1, function(x) all(x == id[i, ])))
    names(tally)[i] <- paste0(colnames(sets)[id[i, ]], collapse = "&")
  }

  euler(combinations = tally, ...)
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

# Find min value of each column -------------------------------------------

col_mins <- function(mat) {
  which.max(mat[(1:ncol(mat) - 1) * nrow(mat) + max.col(t(-mat))])
}

# Center circles on coordinate plane --------------------------------------

center_circles <- function(pars) {
  x <- pars[, 1]
  y <- pars[, 2]
  r <- pars[, 3]

  xlim <- range(c(x + r, x - r))
  ylim <- range(c(y + r, y - r))

  pars[, 1] <- x + abs(xlim[1] - xlim[2]) / 2 - xlim[2]
  pars[, 2] <- y + abs(ylim[1] - ylim[2]) / 2 - ylim[2]

  pars
}
