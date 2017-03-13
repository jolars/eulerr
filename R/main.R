#' Area-proportional euler diagrams
#'
#' Fit euler diagrams (a generalization of venn diagrams) using numerical
#' optimization to find exact or approximate solutions to a specification of set
#' relationships.
#'
#' If the input is a matrix or data frame and argument \code{by} is specified,
#' the function returns a list of euler diagrams.
#'
#' The function minimizes the sums of squared errors between the disjoint areas
#' in the euler diagram and the user's input, namely
#'
#' \deqn{\sum_{i=1}^{n} (y_i - \hat{y}_i) ^ 2 }{\sum (orig - fit) ^ 2}
#'
#' where \eqn{\hat{y}}{fit} are estimates of \eqn{y} that are currently being
#' explored.
#'
#' The stress statistic from \pkg{venneuler} is returned to give an indication
#' of the goodness of the fit:
#'
#' \deqn{
#'   \frac{\sum_{i=1}^{n} (y_i - \hat{y}_i) ^ 2}{\sum_{i=1}^{n} y_i ^ 2}
#'   }{
#'   \sum (fit - original) ^ 2 / \sum original ^ 2
#' },
#'
#' where \eqn{\hat{y}}{fit} are ordinary least squares estimates from the
#' regression of the fitted areas on the original areas that are currently being
#' explored.
#'
#' \code{euler()} also returns \code{diag_error} and \code{region_error} from
#' \emph{eulerAPE}. \code{region_error} is computed as
#'
#' \deqn{
#'   \left| \frac{y_i}{\sum y_i} - \frac{\hat{y}_i}{\sum \hat{y}_i}\right|
#'   }{
#'   max|fit / \sum fit  - original / \sum original|
#' }.
#'
#' \code{diag_error} is simply the maximum of region_error.
#'
#' @param combinations Set relationships as a named numeric vector, matrix, or
#'   data.frame. (See the methods (by class) section for details.)
#' @param by A factor or character matrix to be used in \code{\link[base]{by}} to
#'   split the data.frame or matrix of set combinations.
#' @param input The type of input: disjoint class combinations
#'   (\code{disjoint}) or unions (\code{union}).
#' @param \dots Currently ignored.
#'
#' @return A list object of class 'euler' with the following parameters.
#'   \item{coefficients}{A matrix of x and y coordinates for the centers of the
#'     circles and their radiuses.}
#'   \item{original.values}{Set relationships provided by the user.}
#'   \item{fitted.values}{Set relationships in the solution.}
#'   \item{residuals}{Residuals.}
#'   \item{diag_error}{The largest absolute residual in percentage points
#'     between the original and fitted areas.}
#'   \item{stress}{The stress of the solution, computed as the sum of squared
#'     residuals over the total sum of squares.}
#'
#' @seealso \code{\link{plot.euler}}, \code{\link{print.euler}}
#'
#' @examples
#' # First fit the euler specification
#' fit <- euler(c("A" = 1, "B" = 0.4, "C" = 3, "A&B" = 0.2))
#'
#' # Then plot it
#' plot(fit)
#'
#' # Same result as above
#' euler(c("A" = 1, "B" = 0.4, "C" = 3,
#'         "A&B" = 0.2, "A&C" = 0, "B&C" = 0,
#'         "A&B&C" = 0))
#'
#' # A euler diagram from a list of sample spaces (the list method)
#' euler(list(c("a", "ab", "ac", "abc"),
#'            c("b", "ab", "bc", "abc"),
#'            c("c", "ac", "bc", "abc")))
#'
#' # Using the matrix method
#' mat <- cbind(A = sample(c(TRUE, TRUE, FALSE), size = 50, replace = TRUE),
#'              B = sample(c(TRUE, FALSE), size = 50, replace = TRUE))
#' euler(mat)
#'
#' # Using grouping via the 'by' argument
#' dat <- data.frame(
#'   A = sample(c(TRUE, FALSE), size = 100, replace = TRUE),
#'   B = sample(c(TRUE, TRUE, FALSE), size = 100, replace = TRUE),
#'   gender = sample(c("Men", "Women"), size = 100, replace = TRUE),
#'   nation = sample(c("Sweden", "Denmark"), size = 100, replace = TRUE)
#' )
#'
#' euler(dat[, 1:2], by = dat[, 3:4])
#'
#' # A set with no perfect solution
#' euler(c("a" = 3491, "b" = 3409, "c" = 3503,
#'         "a&b" = 120, "a&c" = 114, "b&c" = 132,
#'         "a&b&c" = 50))
#'
#' @references Wilkinson L. Exact and Approximate Area-Proportional Circular
#'   Venn and Euler Diagrams. IEEE Transactions on Visualization and Computer
#'   Graphics [Internet]. 2012 Feb [cited 2016 Apr 9];18(2):321–31. Available
#'   from: \url{http://doi.org/10.1109/TVCG.2011.56}
#'
#'   Micallef L, Rodgers P. eulerAPE: Drawing Area-Proportional 3-Venn Diagrams
#'   Using Ellipses. PLOS ONE [Internet]. 2014 Jul [cited 2016 Dec
#'   10];9(7):e101717. Available from:
#'   \url{http://dx.doi.org/10.1371/journal.pone.0101717}
#'
#' @export
euler <- function(combinations, ...) UseMethod("euler")

#' @describeIn euler A named numeric vector, with
#'   combinations seperated by an ampersand, for instance \code{`A&B` = 10}.
#'   Missing combinations are treated as being 0.
#'
#' @export
euler.default <- function(combinations, input = c("disjoint", "union"), ...) {
  assertthat::assert_that(
    is.numeric(combinations),
    assertthat::not_empty(combinations),
    all(combinations >= 0),
    assertthat::has_attr(combinations, "names"),
    !any(names(combinations) == ""),
    !any(duplicated(names(combinations)))
  )
  combo_names <- strsplit(names(combinations), split = "&", fixed = TRUE)
  setnames <- unique(unlist(combo_names, use.names = FALSE))
  n <- length(setnames)

  id <- bit_index(n)
  mode(id) <- "logical"

  areas <- double(nrow(id))
  for (i in 1:nrow(id)) {
    s <- setnames[id[i, ]]
    for (j in seq_along(combo_names)) {
      if (setequal(s, combo_names[[j]])) {
        areas[i] <- combinations[j]
      }
    }
  }

  # Decompose or collect set volumes depending on input
  if (match.arg(input) == "disjoint") {
    areas_disjoint <- areas
    areas[] <- 0
    for (i in rev(seq_along(areas))) {
      prev_areas <- rowSums(id[, id[i, ], drop = FALSE]) == sum(id[i, ])
      areas[i] <- sum(areas_disjoint[prev_areas])
    }
  } else if (match.arg(input) == "union") {
    areas_disjoint <- double(length(areas))
    for (i in rev(seq_along(areas))) {
      prev_areas <- rowSums(id[, id[i, ], drop = FALSE]) == sum(id[i, ])
      areas_disjoint[i] <- areas[i] - sum(areas_disjoint[prev_areas])
    }
    if (any(areas_disjoint < 0))
      stop("Check your set configuration. Your specification resulted in some
         disjoint areas being set to 0.")
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
    par = stats::runif(n * 2L, 0L, sqrt(sum(r ^ 2L * pi))),
    fn = initial_layout_optimizer,
    gr = initial_layout_gradient,
    distances = distances,
    disjoint = disjoint,
    contained = contained,
    two = two,
    lower = 0L,
    upper = sqrt(sum(r ^ 2L * pi)),
    method = c("L-BFGS-B")
  )

  final_layout <- stats::nlm(
    f = loss_final,
    p = c(initial_layout$par, r),
    areas = areas_disjoint
  )

  fit <- as.vector(return_intersections(final_layout$estimate))

  orig <- areas_disjoint

  names(orig) <- names(fit) <-
    apply(id, 1, function(x) paste0(setnames[x], collapse = "&"))

  region_error <- abs(fit / sum(fit) - orig / sum(orig))
  diag_error <- max(region_error)

  fpar <- matrix(final_layout$estimate, ncol = 3,
                 dimnames = list(setnames, c("x", "y", "r")))

  # Center the solution on the coordinate plane
  fpar <- center_circles(fpar)

  # Return eulerr structure
  structure(
    list(
      coefficients = fpar,
      original.values = orig,
      fitted.values = fit,
      residuals = orig - fit,
      region_error = region_error,
      diag_error = diag_error,
      stress = venneuler_stress(orig, fit)
    ),
    class = c("euler", "list"))
}

#' @describeIn euler A matrix of logical vectors with columns representing sets
#'   and rows representing each observation's set relationships (see examples).
#'
#' @export
euler.matrix <- function(combinations, by = NULL, ...) {
  if (!is.null(by)) {
    vapply(by,
           function(x) assertthat::assert_that(is.factor(x) || is.character(x)),
           FUN.VALUE = logical(1))
    if (is.matrix(by) || is.data.frame(by)) {
      if (ncol(by) > 2)
        stop("Currently, no more than two grouping variables are allowed.")
    }
  }
  assertthat::assert_that(
    any(is.logical(combinations), is.numeric(combinations)),
    max(combinations, na.rm = TRUE) == 1,
    min(combinations, na.rm = TRUE) == 0,
    !any(grepl("&", colnames(combinations), fixed = TRUE))
  )
  if (is.null(by)) {
    out <- tally_combinations(combinations)
  } else {
    out <- by(combinations, by, tally_combinations, simplify = FALSE)
    class(out) <- c("by", "euler", "list")
  }
  out
}

#' @describeIn euler A data.frame that can be converted to a matrix of logicals
#'   (as in the description above) via \code{\link[base]{as.matrix}}.
#' @export
euler.data.frame <- function(combinations, by = NULL, ...) {
  euler(as.matrix(combinations), by = by, ...)
}

#' Print euler fits
#'
#' Prints a data frame of the original set relationships and the fitted
#' values as well as diagError and stress statistics.
#'
#' @param x Euler diagram specification from \code{\link{euler}}.
#' @param round Number of decimal places to round to.
#' @param ... Arguments passed to \code{\link[base]{print.data.frame}}.
#'
#' @return Prints the results of the fit.
#'
#' @export
print.euler <- function(x, round = 3, ...) {
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
  cat("diag_error: ", round(x$diag_error, digits = round), "\n")
  cat("stress:     ", round(x$stress, digits = round), "\n")
}

#' @describeIn euler A list of vectors, each vector giving the contents of
#'   that set. Vectors in the list do not need to be named. (Broken.)
#' @export
euler.list <- function(combinations, ...) {
  assertthat::assert_that(
    assertthat::has_attr(combinations, "names")
  )

  sets <- names(combinations)
  n <- length(sets)

  id <- bit_index(n)
  mode(id) <- "logical"

  out <- integer(nrow(id))
  names(out) <- apply(id, 1, function(x) paste(sets[x], collapse = "&"))

  for (i in 1:nrow(id)) {
    out[i] <- length(Reduce(intersect, combinations[id[i, ]]))
  }

  euler(out, input = "union", ...)
}
