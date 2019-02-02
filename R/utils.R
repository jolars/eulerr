#' Tally set relationships
#'
#' @param sets a data.frame with set relationships and weights
#' @param weights a numeric vector
#'
#' @return Calls [euler()] after the set relationships have been coerced to a
#'   named numeric vector.
#' @keywords internal
tally_combinations <- function(sets, weights) {
  if (!is.matrix(sets))
    sets <- as.matrix(sets)

  id <- bit_indexr(NCOL(sets))
  tally <- double(NROW(id))

  for (i in seq_len(NROW(id))) {
    tally[i] <-
      sum(as.numeric(colSums(t(sets) == id[i, ]) == NCOL(sets))*weights)
    names(tally)[i] <- paste0(colnames(sets)[id[i, ]], collapse = "&")
  }
  tally
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
  (new_max - new_min)/(max(x) - min(x))*(x - max(x)) + new_max
}

#' Update list with input
#'
#' A wrapper for [utils::modifyList()] that attempts to coerce non-lists to
#' lists before updating.
#'
#' @param x a list to be updated
#' @param val stuff to update `x` with
#'
#' @seealso [utils::modifyList()]
#' @return Returns an updated list.
#' @keywords internal
update_list <- function(old, new) {
  if (is.null(old))
    old <- list()
  if (!is.list(new))
    tryCatch(new <- as.list(new))
  if (!is.list(old))
    tryCatch(old <- as.list(old))
  utils::modifyList(old, new)
}

#' Replace (refresh) a list
#'
#' Unlike `update_list`, this function only modifies, and does not add,
#' items in the list.
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
is_false <- function(x) {
  identical(x, FALSE)
}

#' Check if a vector is an integer
#'
#' @param x a vector
#'
#' @return TRUE of FALSE.
#' @keywords internal
is_integer <- function(x, tol = .Machine$double.eps^0.5)
{
  all(abs(x - round(x)) < tol)
}

#' Check if vector is a real (numeric non-integer)
#'
#' @param x
#'
#' @return A logical.
#' @keywords internal
is_real <- function(x, tol = .Machine$double.eps^0.5) {
  is.numeric(x) && !is_integer(x, tol = )
}

#' Binary indices
#'
#' Wraps around bit_indexr().
#'
#' @param n number of items to generate permutations from
#'
#' @return A matrix of logicals.
#' @keywords internal
bit_indexr <- function(n) {
  m <- bit_index_cpp(n)
  mode(m) <- "logical"
  m
}

#' regionError
#'
#' @param fit fitted values
#' @param orig original values
#'
#' @return regionError
#' @keywords internal
regionError <- function(fit, orig) {
  abs(fit/sum(fit) - orig/sum(orig))
}

#' diagError
#'
#' @param fit fitted values
#' @param orig original values
#' @param regionError regionError
#'
#' @return diagError
#' @keywords internal
diagError <- function(fit, orig, regionError = NULL) {
  if(!is.null(regionError)) {
    max(regionError)
  } else {
    max(abs(fit/sum(fit) - orig/sum(orig)))
  }
}

#' Get the number of sets in he input
#'
#' @param combinations a vector of combinations (see [euler()])
#'
#' @return The number of sets in the input
#' @keywords internal
n_sets <- function(combinations) {
  combo_names <- strsplit(names(combinations), split = "&", fixed = TRUE)
  length(unique(unlist(combo_names, use.names = FALSE)))
}


#' Set up constraints for optimization
#'
#' @param newpars parameters from the first optimizer
#'
#' @return A list of lower and upper constraints
#' @keywords internal
get_constraints <- function(newpars) {
  h   <- newpars[, 1L]
  k   <- newpars[, 2L]
  a   <- newpars[, 3L]
  b   <- newpars[, 4L]
  phi <- newpars[, 5L]

  n <- length(h)

  xlim <- sqrt(a^2*cos(phi)^2 + b^2*sin(phi)^2)
  ylim <- sqrt(a^2*sin(phi)^2 + b^2*cos(phi)^2)
  xbnd <- range(xlim + h, -xlim + h)
  ybnd <- range(ylim + k, -ylim + k)

  lwr <- upr <- double(5L*n)

  for (i in seq_along(h)) {
    ii <- 5L*(i - 1L)

    lwr[ii + 1L] <- xbnd[1L]
    lwr[ii + 2L] <- ybnd[1L]
    lwr[ii + 3L] <- a[i]/3
    lwr[ii + 4L] <- b[i]/3
    lwr[ii + 5L] <- 0

    upr[ii + 1L] <- xbnd[2L]
    upr[ii + 2L] <- ybnd[2L]
    upr[ii + 3L] <- a[i]*3
    upr[ii + 4L] <- b[i]*3
    upr[ii + 5L] <- pi
  }
  list(lwr = lwr, upr = upr)
}

#' Normalize an angle to [-pi, pi)
#'
#' @param x angle in radians
#'
#' @return A normalized angle.
#' @keywords internal
normalize_angle <- function(x) {
  a <- (x + pi) %% (2*pi)
  ifelse(a >= 0, a - pi, a + pi)
}

#' Normalize parameters (semiaxes and rotation)
#'
#' @param m pars
#'
#' @return `m`, normalized
#' @keywords internal
normalize_pars <- function(m) {
  n <- NCOL(m)
  if (n == 3L) {
    m[, 3L] <- abs(m[, 3L])
  } else {
    m[, 3L:4L] <- abs(m[, 3L:4L])
    m[, 5L] <- normalize_angle(m[, 5L])
  }
  m
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
#' @param x input
#' @param n required number of items
#' @param default default values
#' @param user user-inputted values
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

#' Dummy code a data.frame
#'
#' @param x a data.frame
#' @param sep character for separating dummy code factors and their levels
#'   when constructing names
#' @param factor_names whether to include factor names when creating new
#'   names for dummy codes
#'
#' @return A dummy-coded version of x.
#'
#' @keywords internal
dummy_code <- function(x, sep = "_", factor_names = TRUE) {
  fac_chr <- vapply(x, function(x) is.character(x) || is.factor(x), logical(1))
  tmp <- x[, fac_chr, drop = FALSE]

  lvls <- lapply(tmp, function(x) levels(as.factor(x)))

  n_levels <- vapply(lvls, function(x) length(x) - 1, double(1))
  dummy_levels <- lapply(lvls, function(x) x[-length(x)])
  n_levels_tot <- sum(n_levels)

  dummy_names <- dummy_levels

  if (isTRUE(factor_names)) {
    for (i in seq_along(dummy_names)) {
      dummy_names[[i]] <- paste(names(dummy_levels)[i],
                                dummy_levels[[i]],
                                sep = sep)
    }
  }

  dummy_names <- unlist(dummy_names)

  if (any(duplicated(dummy_names)))
    stop(paste("duplicated names for dummy coded factors were generated;",
               "please consider specifying 'factor_names = TRUE'."))

  out <- matrix(FALSE,
                nrow(x),
                n_levels_tot,
                dimnames = list(NULL, dummy_names))

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

#' Stress
#'
#' @param orig original values
#' @param fit fitted values
#'
#' @return Stress metric.
#'
#' @keywords internal
stress <- function(orig, fit)
{
  sst <- sum(fit^2)
  slope <- sum(orig*fit)/sum(orig^2)
  sse   <- sum((fit - orig*slope)^2)
  sse/sst
}
