#' Euler diagrams
#'
#' @param sets
#' @return A list object of class 'vennr'.
#' @examples
#'
#' @export

eulerr <- function(sets) {
  setnames <- strsplit(names(sets), split = "&", fixed = T)
  one_sets <- unlist(setnames[lengths(setnames) == 1])

  # Set up names matrix
  names <- vector("list", length = length(one_sets))
  for (i in seq_along(one_sets)) {
    names[[i]] <- utils::combn(one_sets, i)
  }

  # Set up area matrix
  areas <- vector("list", length = length(names))
  for (i in seq_along(names)) {
    for (j in 1:ncol(names[[i]])) {
      tmp <- lapply(setnames, is.element, unlist(names[[i]][, j]))
      ind <- vapply(tmp, all, FUN.VALUE = logical(1)) &
          (vapply(tmp, sum, FUN.VALUE = integer(1)) == i)
      areas[[i]][j] <- ifelse(!any(ind), 0, sets[ind])
    }
  }

  radiuses <- sqrt(areas[[1]] / pi)

  # Set up index matrix
  id <- vector("list", length = length(areas))
  for (i in seq_along(id)) {
    id[[i]] <- utils::combn(length(areas[[1]]), i)
  }

  disjoint <- areas[[2]] == 0

  distances <- mapply(separate_two_discs,
                      r1 = radiuses[id[[2]][1,]],
                      r2 = radiuses[id[[2]][2,]],
                      overlap = areas[[2]])

  # Establish identities of disjoint and contained sets
  tmp <- matrix(areas[[1]][id[[2]]], nrow = 2)
  contained <- areas[[2]] == tmp[1, ] | areas[[2]] == tmp[2, ]

  # Compute an initial layout
  initial_layout <- stats::optim(
    par = stats::runif(length(areas[[1]]) * 2, 0, min(radiuses)),
    fn = initial_layout_optimizer,
    gr = initial_layout_gradient,
    distances = distances,
    disjoint = disjoint,
    contained = contained,
    id = id,
    lower =  rep(0, times = length(areas[[1]]) * 3),
    upper = c(rep(sum(radiuses) * 2 - min(radiuses) *  2,
                  times = length(areas[[1]]) * 2), max(radiuses) * 2),
    method = c("L-BFGS-B")
  )

  final_layout <- stats::optim(
    fn = final_layout_optimizer,
    par = c(initial_layout$par, radiuses),
    areas = areas,
    names = names,
    id = id,
    method = c("Nelder-Mead")
  )

  fpar <- matrix(final_layout$par,
                 ncol = 3,
                 dimnames = list(names[[1]], c("x", "y", "r")))

  fit <- structure(
    list(
      Circles = fpar,
      Stress = final_layout$value
    ),
    class = c("eulerr", "list"))
}

# Utils -----------------------------------------------------------------

is_equal <- function(x, y, tol = .Machine$double.eps ^ 0.5) {
  abs(x - y) < tol
}