#' Area-proportional Euler diagrams
#'
#' eulerr computes Euler diagrams (a generalization of Venn diagrams) using
#' numerical optimization to find exact or optimal approximations for an
#' input of set relationships.
#'
#' If \code{by} is specified, \code{eulerr} returns a list of euler diagrams
#' for which there is a separate plot method that plots a grid of diagrams.
#'
#' @param sets Set relationships as a named numeric vector, matrix, or
#'   data.frame. (See the methods (by class) section for details.)
#' @param by A factor or character vector used in \code{\link[base]{by}} to
#'   split the data.frame or matrix and compute euler diagrams for each split.
#' @param \dots Currently ignored.
#' @return A list object of class 'eulerr' with the following parameters.
#'   \item{coefficients}{A matrix of x and y coordinates for the centers of the
#'     circles and their radiuses.}
#'   \item{original.values}{The set relationships provided by the user.}
#'   \item{fitted.values}{The set relationships in the solution given by
#'     \pkg{eulerr}.}
#'   \item{residuals}{Squared residuals between the original areas and the
#'     fitted areas.}
#'   \item{stress}{The stress of the solution, computed as
#'   squared residuals over the sum of squared residuals.}
#' @family eulerr functions
#' @seealso \code{\link{plot.eulerr}}
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
#' # Using grouping via the 'by' argument
#' dat <- data.frame(
#'   A      = sample(c(TRUE, FALSE), size = 100, replace = TRUE),
#'   B      = sample(c(TRUE, TRUE, FALSE), size = 100, replace = TRUE),
#'   gender = sample(c("Men", "Women"), size = 100, replace = TRUE),
#'   nation = sample(c("Sweden", "Denmark"), size = 100, replace = TRUE)
#' )
#'
#' fit4 <- eulerr(dat[, 1:2], by = dat[, 3:4])
#'
#' @export
#' @import assertthat

eulerr <- function(sets, ...) UseMethod("eulerr")

#' @describeIn eulerr A named numeric vector, with
#'   interactions seperated by an ampersand, for instance \code{`A&B` = 10}.
#'   Missing interactions are treated as being 0.
#' @export

eulerr.default <- function(sets, ...) {
  assert_that(
    is.numeric(sets),
    not_empty(sets),
    length(sets) > 0,
    has_attr(sets, "names"),
    all(names(sets) != ""),
    !any(duplicated(names(sets)))
  )

  setnames <- strsplit(names(sets), split = "&", fixed = T)
  one_sets <- unique(unlist(setnames, use.names = FALSE))

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
      tmp <- lapply(setnames, is.element, unlist(names[[i]][, j],
                                                 use.names = FALSE))
      ind <- vapply(tmp, all, FUN.VALUE = logical(1L)) &
          (vapply(tmp, sum, FUN.VALUE = integer(1L)) == i)
      areas[[i]][j] <- ifelse(!any(ind), 0, sets[ind])
    }
  }

  assert_that(all(areas[[1]] > 0L))

  radiuses <- sqrt(areas[[1]] / pi)

  # Set up index matrix
  id <- vector("list", length = length(areas))
  for (i in seq_along(id)) {
    id[[i]] <- utils::combn(length(areas[[1]]), i)
  }

  # Make sure that no intersections are larger than their components
  for (i in seq_along(id[-1])) {
    i <- i + 1L
    for (j in 1:ncol(id[[i]])) {
      m <- id[[1]] %in% id[[i]][1, j] | id[[1]] %in% id[[i]][2, j]
      if(any(areas[[i]][j] > areas[[1]][m])) {
        stop("Intersection areas cannot exceed their components' areas.")
      }
    }
  }

  disjoint <- areas[[2]] == 0L

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
    par = stats::runif(length(areas[[1]]) * 2L, 0L, min(radiuses)),
    fn = initial_layout_optimizer,
    gr = initial_layout_gradient,
    distances = distances,
    disjoint = disjoint,
    contained = contained,
    two = two,
    lower = rep(0, times = length(areas[[1]]) * 2L),
    upper = rep(sum(radiuses) * 2L - min(radiuses) - max(radiuses)),
    method = c("L-BFGS-B")
  )

  final_layout <- stats::optim(
    fn = final_layout_optimizer,
    par = c(initial_layout$par, radiuses),
    areas = areas,
    id = id,
    control = list(reltol = 1e-3),
    method = c("Nelder-Mead")
  )

  fit <- unlist(return_intersections(final_layout$par, areas, id),
                use.names = FALSE) * scale_factor
  names(fit) <- unlist(lapply(names, apply, 2, paste0, collapse = "&"),
                       use.names = FALSE)

  orig <- unlist(areas, use.names = FALSE) * scale_factor
  names(orig) <- names(fit)

  fpar <- matrix(final_layout$par,
                 ncol = 3,
                 dimnames = list(names[[1]], c("x", "y", "r")))
  structure(
    list(
      coefficients = fpar * scale_factor,
      original.values = orig,
      fitted.values = fit,
      residuals = orig - fit,
      stress = final_layout$value
    ),
    class = c("eulerr", "list"))
}

#' @describeIn eulerr A matrix of logical vectors with columns representing sets
#'   and rows representing each observation's set relationships (see examples).
#' @export

eulerr.matrix <- function(sets, by = NULL, ...) {
  if (!is.null(ncol(by)))
    if (ncol(by) > 2)
      stop("Currently no more than two grouping variables are allowed.")

  if (!is.null(by)) {
    assert_that(
      is.character(by) | is.factor(by) | is.data.frame(by) | is.matrix(by)
    )
    if (any(is.data.frame(by), is.matrix(by))) {
      assert_that(
        all(
          vapply(
            by,
            function(x) any(is.character(x), is.factor(x)),
            FUN.VALUE = logical(1)
          )
        )
      )
    } else {
      assert_that(is.character(by) | is.factor(by))
    }
  }

  assert_that(
    any(is.logical(sets), is.numeric(sets)),
    max(sets, na.rm = TRUE) == 1L,
    min(sets, na.rm = TRUE) == 0L,
    !any(grepl("&", colnames(sets), fixed = TRUE))
  )

  if (is.null(by)) {
    out <- tally_sets(sets)
  } else {
    out <- by(sets, by, tally_sets, simplify = FALSE)
    class(out) <- c("eulerr_grid", "by")
  }
  out
}

#' @describeIn eulerr A data.frame that can be converted to a matrix of logicals
#'   (as in the description above) via \code{\link[base]{as.matrix}}.
#' @export

eulerr.data.frame <- function(sets, by = NULL, ...) {
  eulerr(as.matrix(sets), by = by, ...)
}