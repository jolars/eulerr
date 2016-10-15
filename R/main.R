#' Area-proportional Euler diagrams
#'
#' eulerr computes eulerr diagrams (a generalization of Venn diagrams) using
#' numerical optimization to find exact or the best possible approximations
#' to a input of set combinations.
#'
#' @param sets Set relationships in the form of a named numeric vector, with
#'   interactions seperated by an ampersand, for instance \code{`A&B` = 10}.
#'   Missing interactions are treated as being 0.
#' @param matrix A matrix of logicals with columns representing sets and rows
#'   representing each observations set relationships (see examples).
#' @param data.frame A data frame of
#' @param ... Currently ignored.
#' @return A list object of class 'vennr' with the following parameters
#'   \item{circles}{A matrix of x and y coordinates for the centers of the
#'   circles and their radiuses.}
#'   \item{original_areas}{The areas the user supplied \pkg{eulerr} with.}
#'   \item{fitted_areas}{The areas of the Euler diagram solution \pkg{eulerr}
#'   came up with.}
#'   \item{residuals}{Squared deviations between the original areas and the
#'   fitted areas.}
#'   \item{stress}{The stress of the solution, computed as
#'   squared residuals over the sum of squared residuals.}
#' @seealso \code{\link{plot.eulerr}}, \code{\link{residuals.eulerr}}
#' @examples
#'
#' fit1 <- eulerr(c("A" = 1, "B" = 0.4, "C" = 3, "A&B" = 0.2))
#'
#' # Same result as above
#' fit2 <- eulerr(c("A" = 1, "B" = 0.4, "C" = 3,
#'                  "A&B" = 0.2, "A&C" = 0, "B&C" = 0,
#'                  "A&B&C" = 0) )
#'
#' # Using the matrix method
#' mat <- cbind(A = sample(c(TRUE, TRUE, FALSE), size = 50, replace = TRUE),
#'              B = sample(c(TRUE, FALSE), size = 50, replace = TRUE))
#' fit3 <- eulerr(mat)
#'
#' @export
#' @import assertthat

eulerr <- function(sets, ...) UseMethod("eulerr")

#' @rdname eulerr
#' @export

eulerr.default <- function(sets, ...) {
  assert_that(not_empty(sets))
  assert_that(length(sets) > 0)
  assert_that(has_attr(sets, "names"))
  assert_that(all(names(sets) != ""))
  assert_that(is.numeric(sets))
  assert_that(!any(duplicated(names(sets))))

  setnames <- strsplit(names(sets), split = "&", fixed = T)
  one_sets <- unlist(setnames[lengths(setnames) == 1])

  # Set up names matrix
  names <- vector("list", length = length(one_sets))
  for (i in seq_along(one_sets)) {
    names[[i]] <- utils::combn(one_sets, i)
  }

  # Scale the values to fractions
  scale_factor <- sum(sets)
  sets <- sets / scale_factor

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

  assert_that(all(areas[[1]] > 0))

  radiuses <- sqrt(areas[[1]] / pi)

  # Set up index matrix
  id <- vector("list", length = length(areas))
  for (i in seq_along(id)) {
    id[[i]] <- utils::combn(length(areas[[1]]), i)
  }

  # Make sure that no intersections are larger than their components
  for (i in seq_along(id[-1])) {
    i <- i + 1
    for (j in 1:ncol(id[[i]])) {
      m <- id[[1]] %in% id[[i]][1, j] | id[[1]] %in% id[[i]][2, j]
      if(any(areas[[i]][j] > areas[[1]][m])) {
        stop("Intersection areas cannot exceed their components' areas.")
      }
    }
  }

  disjoint <- areas[[2]] == 0

  two <- id[[2]]

  distances <- mapply(separate_two_discs,
                      r1 = radiuses[two[1, ]],
                      r2 = radiuses[two[2, ]],
                      overlap = areas[[2]])

  # Establish identities of disjoint and contained sets
  tmp <- matrix(areas[[1]][two], nrow = 2)
  contained <- areas[[2]] == tmp[1, ] | areas[[2]] == tmp[2, ]

  # Compute an initial layout
  initial_layout <- stats::optim(
    par = stats::runif(length(areas[[1]]) * 2, 0, min(radiuses)),
    fn = initial_layout_optimizer,
    gr = initial_layout_gradient,
    distances = distances,
    disjoint = disjoint,
    contained = contained,
    two = two,
    lower = rep(0, times = length(areas[[1]]) * 2),
    upper = rep(sum(radiuses) * 2 - min(radiuses) - max(radiuses)),
    method = c("L-BFGS-B")
  )

  final_layout <- stats::optim(
    fn = final_layout_optimizer,
    par = c(initial_layout$par, radiuses),
    areas = areas,
    id = id,
    method = c("Nelder-Mead")
  )

  fit <- unlist(return_intersections(par = final_layout$par,
                                     areas = areas,
                                     id = id)) * scale_factor
  names(fit) <- unlist(lapply(names, apply, 2, paste0, collapse = "&"))

  orig <- unlist(areas) * scale_factor
  names(orig) <- names(fit)

  fpar <- matrix(final_layout$par,
                 ncol = 3,
                 dimnames = list(names[[1]], c("x", "y", "r")))
  output <- structure(
    list(
      circles = fpar * scale_factor,
      original_areas = orig,
      fitted_areas = fit,
      residuals = orig - fit,
      stress = final_layout$value
    ),
    class = c("eulerr", "list"))
}

#' @rdname eulerr
#' @export

eulerr.matrix <- function(matrix, ...) {
  sets <- vector("list", length = ncol(matrix))

  for (i in seq_along(colnames(matrix))) {
    sets[[i]] <- utils::combn(colnames(matrix), i)
  }

  tally <- double(0)

  for (i in seq_along(sets)) {
    for (j in 1:ncol(sets[[i]])) {
      combos <- sets[[i]][, j]
      if (i == 1) {
        intersections <- matrix[, combos]
      } else {
        intersections <- apply(matrix[, combos], 1, all)
      }
      sum_intersections <- sum(intersections)
      names(sum_intersections) <- c(setnames, paste0(combos, collapse = "&"))
      tally <- c(tally, sum_intersections)
    }
  }

  eulerr(tally, ...)
}

#' @rdname eulerr
#' @export

eulerr.data.frame <- function(data.frame, ...) {
  matrix <- as.matrix(data.frame)
  eulerr(matrix, ...)
}
