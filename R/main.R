#' Area-Proportional Euler Diagrams
#'
#' Compute Euler diagrams (a generalization of Venn diagrams) using
#' numerical optimization to find exact or approximate solutions to a
#' specification of set relationships.
#'
#' If \code{by} is specified, \code{eulerr} returns a list of euler diagrams
#' that can be plotted in facets via a special plot method.
#'
#' The fit is optimized using either the cost function used in the
#' eulerAPE software package or the stress statistic of the R package
#' \pkg{venneuler}. The eulerAPE cost function is defined as
#'
#' \deqn{%
#'   \frac{1}{n} \sum_{i=1}^{n} \frac{(y_i - \hat{y}_i) ^ 2}{\hat{y}_i}
#' }{%
#'   (1 / n) \sum (orig - fit) / \sum fit
#' }
#'
#' where \eqn{\hat{y}}{fit} are the estimates of \eqn{y} that are currently
#' explored during optimization. For \code{venneuler}, the stress function is
#' defined as
#'
#' \deqn{%
#'   \frac{\sum_{i=1}^{n} (y_i - \hat{y}_i) ^ 2}{\sum_{i=1}^{n} y_i ^ 2}
#' }{%
#'   (\sum (fit - orig) ^ 2) / (\sum orig ^ 2)
#' }
#'
#' where \eqn{\hat{y}}{fit} are OLS estimates from the regression of the fitted
#' areas on the original areas that are currently being explored during
#' optimization.
#'
#' @param sets Set relationships as a named numeric vector, matrix, or
#'   data.frame. (See the methods (by class) section for details.)
#' @param by A factor or character vector used in \code{\link[base]{by}} to
#'   split the data.frame or matrix and compute euler diagrams for each split.
#' @param cost Cost function to use in optimizing the fit. See details.
#' @param \dots Currently ignored.
#'
#' @return A list object of class 'eulerr' with the following parameters.
#'   \item{coefficients}{A matrix of x and y coordinates for the centers of the
#'     circles and their radiuses.}
#'   \item{original.values}{Set relationships provided by the user.}
#'   \item{fitted.values}{Set relationships in the solution given by
#'     \pkg{eulerr}.}
#'   \item{residuals}{Residuals.}
#'   \item{diagError}{The largest absolute residual in percentage points
#'     between the original and fitted areas.}
#'   \item{stress}{The stress of the solution, computed as the sum of squared
#'     residuals over the total sum of squares.}
#'
#' @seealso \code{\link{plot.eulerr}}, \code{\link{print.eulerr}}
#'
#' @examples
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
#' # A set with no perfect solution
#' rel <- c("a" = 3491, "b" = 3409, "c" = 3503,
#'          "a&b" = 120, "a&c" = 114, "b&c" = 132, "a&b&c" = 126)
#'
#' # Use the cost function from eulerAPE (the default)
#' fit5 <- eulerr(rel, cost = "eulerAPE")
#'
#' # Use the stress function from venneuler
#' fit6 <- eulerr(rel, cost = "venneuler")
#'
#' par(mfrow = c(1, 2))
#' plot(fit5); plot(fit6)
#'
#' @references Wilkinson L. Exact and Approximate Area-Proportional Circular
#' Venn and Euler Diagrams. IEEE Transactions on Visualization and Computer
#' Graphics [Internet]. 2012 Feb [cited 2016 Apr 9];18(2):321–31. Available
#' from: \url{http://doi.org/10.1109/TVCG.2011.56}
#'
#' Micallef L, Rodgers P. eulerAPE: Drawing Area-Proportional 3-Venn Diagrams
#' Using Ellipses. PLOS ONE [Internet]. 2014 Jul [cited 2016 Dec
#' 10];9(7):e101717. Available from:
#' \url{http://dx.doi.org/10.1371/journal.pone.0101717}
#'
#' @export

eulerr <- function(sets, ...) UseMethod("eulerr")

#' @describeIn eulerr A named numeric vector, with
#'   interactions seperated by an ampersand, for instance \code{`A&B` = 10}.
#'   Missing interactions are treated as being 0.
#'
#' @export

eulerr.default <- function(sets, cost = c("eulerAPE", "venneuler"), ...) {
  assertthat::assert_that(
    is.numeric(sets),
    assertthat::not_empty(sets),
    length(sets) > 0,
    all(sets >= 0),
    assertthat::has_attr(sets, "names"),
    !any(names(sets) == ""),
    !any(duplicated(names(sets)))
  )

  setnames <- strsplit(names(sets), split = "&", fixed = T)
  one_sets <- unique(unlist(setnames, use.names = FALSE))
  n <- length(one_sets)

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

  # Scale the values to fractions
  scale_factor <- 100 / min(sets[sets > 0])
  sets <- sets * scale_factor

  areas <- double(nrow(id))
  for (i in 1:nrow(id)) {
    s <- one_sets[id[i, ]]
    for (j in seq_along(setnames)) {
      if (setequal(s, setnames[[j]])) {
        areas[i] <- sets[j]
      }
    }
  }

  id_sums <- rowSums(id)
  ones <- id_sums == 1
  twos <- id_sums == 2

  two <- choose_two(1:n)

  r <- sqrt(areas[ones] / pi)

  # Establish identities of disjoint and contained sets
  disjoint <- areas[twos] == 0
  tmp <- matrix(areas[ones][two], ncol = 2)
  contained <- areas[twos] == tmp[, 1] | areas[twos] == tmp[, 2]

  distances <- mapply(
    separate_two_discs,
    r1 = r[two[, 1]],
    r2 = r[two[, 2]],
    overlap = areas[twos],
    USE.NAMES = FALSE
  )

  # Starting layout
  initial_layout <- stats::optim(
    par = stats::runif(n * 2L, 0L, min(r)),
    fn = initial_layout_optimizer,
    gr = initial_layout_gradient,
    distances = distances,
    disjoint = disjoint,
    contained = contained,
    two = two,
    lower = 0,
    upper = sqrt(sum(r ^ 2 * pi)),
    method = c("L-BFGS-B")
  )

  # Final layout
  final_layout <- stats::optim(
    fn = compute_fit,
    par = c(initial_layout$par, r),
    areas = areas,
    id = id,
    two = two,
    twos = twos,
    ones = ones,
    cost = switch(match.arg(cost), "eulerAPE" = 0, "venneuler" = 1),
    method = c("Nelder-Mead"),
    control = list(maxit = 500)
  )

  fit <- return_intersections(final_layout$par,areas, id, two, twos,
                              ones) / scale_factor

  names(fit) <- apply(id, 1, function(x) paste0(one_sets[x], collapse = "&"))
  orig <- areas / scale_factor

  region_error <- abs(fit / sum(fit) - orig / sum(orig))
  diag_error <- max(region_error)

  names(orig) <- names(fit)
  fpar <- matrix(final_layout$par, ncol = 3,
                 dimnames = list(one_sets, c("x", "y", "r"))) / scale_factor
  structure(
    list(
      coefficients = fpar,
      original.values = orig,
      fitted.values = fit,
      residuals = orig - fit,
      region_error = region_error,
      diag_error = diag_error,
      stress = stress(orig, fit)
    ),
    class = c("eulerr", "list"))
}

#' @describeIn eulerr A matrix of logical vectors with columns representing sets
#'   and rows representing each observation's set relationships (see examples).
#'
#' @export

eulerr.matrix <- function(sets, by = NULL, cost = c("eulerAPE", "venneuler"),
                          ...) {
  if (!is.null(ncol(by)))

    if (ncol(by) > 2)
      stop("Currently, no more than two grouping variables are allowed.")

  if (!is.null(by)) {

    assertthat::assert_that(
      any(is.character(by), is.factor(by), is.data.frame(by), is.matrix(by))
    )

    if (any(is.data.frame(by), is.matrix(by))) {

      assertthat::assert_that(
        all(
          vapply(
            by,
            function(x) any(is.character(x), is.factor(x)),
            FUN.VALUE = logical(1)
          )
        )
      )

    } else {

      assertthat::assert_that(any(is.character(by), is.factor(by)))

    }
  }

  assertthat::assert_that(
    any(is.logical(sets), is.numeric(sets)),
    max(sets, na.rm = TRUE) == 1,
    min(sets, na.rm = TRUE) == 0,
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

eulerr.data.frame <- function(sets, by = NULL,
                              cost = c("eulerAPE", "venneuler"), ...) {
  eulerr(as.matrix(sets), by = by, cost = cost, ...)
}

#' Print eulerr fits
#'
#' Prints a data frame of the original set relationships and the fitted
#' values as well as diagError and stress statistics.
#'
#' @param x Euler diagram specification from \pkg{eulerr}.
#' @param round Number of decimal places to round to.
#' @param ... Arguments passed to \code{\link[base]{print.data.frame}}.
#'
#' @return Prints the results of the fit.
#'
#' @export

print.eulerr <- function(x, round = 3, ...) {
  assertthat::assert_that(
    assertthat::is.number(round)
  )

  out <- data.frame(
    "original" = x$original.values,
    "fitted" = x$fitted.values,
    "residuals" = x$residuals,
    "region_error" = x$region_error
  )

  print(round(out, digits = round), ...)
  cat("\n")
  cat("diagError: ", round(x$diag_error, digits = round), "\n")
  cat("stress:    ", round(x$stress, digits = round), "\n")
}
