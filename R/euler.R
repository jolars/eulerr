#' Area-Proportional Euler Diagrams
#'
#' Fit euler diagrams (a generalization of venn diagrams) using numerical
#' optimization to find exact or approximate solutions to a specification of set
#' relationships.
#'
#' If the input is a matrix or data frame and argument `by` is specified,
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
#' `euler()` also returns `diag_error` and `region_error` from
#' *eulerAPE*. `region_error` is computed as
#'
#' \deqn{
#'   \left| \frac{y_i}{\sum y_i} - \frac{\hat{y}_i}{\sum \hat{y}_i}\right|
#'   }{
#'   max|fit / \sum fit  - original / \sum original|
#' }.
#'
#' `diag_error` is simply the maximum of region_error.
#'
#' @param combinations Set relationships as a named numeric vector, matrix, or
#'   data.frame. (See the methods (by class) section for details.)
#' @param by A factor or character matrix to be used in [base::by()] to
#'   split the data.frame or matrix of set combinations.
#' @param input The type of input: disjoint class combinations
#'   (`disjoint`) or unions (`union`).
#' @param ... Arguments passed down to other methods.
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
#' @seealso [plot.euler()], [print.euler()]
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
#' euler(list(A = c("a", "ab", "ac", "abc"),
#'            B = c("b", "ab", "bc", "abc"),
#'            C = c("c", "ac", "bc", "abc")))
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
#'   Graphics (Internet). 2012 Feb (cited 2016 Apr 9);18(2):321–31. Available
#'   from:
#'   [http://doi.org/10.1109/TVCG.2011.56](http://doi.org/10.1109/TVCG.2011.56)
#'
#'   Micallef L, Rodgers P. eulerAPE: Drawing Area-Proportional 3-Venn Diagrams
#'   Using Ellipses. PLOS ONE (Internet). 2014 Jul (cited 2016 Dec 10);9(7):e101717. Available from:
#'   [http://dx.doi.org/10.1371/journal.pone.0101717](http://dx.doi.org/10.1371/journal.pone.0101717)
#'
#' @export
euler <- function(combinations, ...) UseMethod("euler")

#' @describeIn euler A named numeric vector, with
#'   combinations seperated by an ampersand, for instance `A&B = 10`.
#'   Missing combinations are treated as being 0.
#'
#' @export
euler.default <- function(combinations, input = c("disjoint", "union"), ...) {
  # Assertions
  assert_that(is.numeric(combinations),
              not_empty(combinations),
              all(combinations >= 0),
              has_attr(combinations, "names"),
              !any(names(combinations) == ""),
              !any(duplicated(names(combinations))))

  combo_names <- strsplit(names(combinations), split = "&", fixed = TRUE)
  setnames <- unique(unlist(combo_names, use.names = FALSE))

  n <- length(setnames)
  id <- bit_indexr(n)

  areas <- double(nrow(id))
  for (i in 1:nrow(id)) {
    s <- setnames[id[i, ]]
    for (j in seq_along(combo_names)) {
      if (setequal(s, combo_names[[j]])) {
        areas[i] <- combinations[j]
      }
    }
  }

  if (n > 1) {
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
        stop("Check your set configuration. Your specification resulted in some disjoint areas being set to 0.")
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

    distances <- mapply(separate_two_discs,
                        r1 = r[two[, 1]],
                        r2 = r[two[, 2]],
                        overlap = areas[twos],
                        USE.NAMES = FALSE)

    # Starting layout
    initial_layout <- optim(
      par = runif(n * 2L, 0L, sqrt(sum(r ^ 2L * pi))),
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

    # Final layout
    # TO DO: Allow user customization here?
    final_layout <- nlm(f = loss_final,
                        p = c(initial_layout$par, r),
                        areas = areas_disjoint)

    fit <- as.vector(return_intersections(final_layout$estimate))

    orig <- areas_disjoint

    names(orig) <- names(fit) <-
      apply(id, 1, function(x) paste0(setnames[x], collapse = "&"))

    region_error <- abs(fit / sum(fit) - orig / sum(orig))
    diag_error <- max(region_error)

    fpar <- matrix(final_layout$estimate,
                   ncol = 3,
                   dimnames = list(setnames, c("x", "y", "r")))
    stress <- venneuler_stress(orig, fit)

    # Center the solution on the coordinate plane
    fpar <- center_circles(fpar)
  } else {
    # Just one circle
    fpar <-  matrix(c(0, 0, sqrt(areas / pi)),
                    ncol = 3,
                    dimnames = list(setnames, c("x", "y", "r")))
    region_error <- diag_error <- stress <- 0
    orig <- fit <- areas
    names(orig) <- names(fit) <- setnames
  }

  # Return eulerr structure
  structure(list(coefficients = fpar,
                 original.values = orig,
                 fitted.values = fit,
                 residuals = orig - fit,
                 region_error = region_error,
                 diag_error = diag_error,
                 stress = stress),
            class = c("euler", "list"))
}

#' @describeIn euler A data.frame of logicals, two-level factors (see examples).
#' @param weights A numeric vector of weights of the same length as `by` and
#'   the number of rows in `combinations`.
#' @export
euler.data.frame <- function(combinations, weights = NULL, by = NULL, ...) {
  assert_that(!any(grepl("&", colnames(combinations), fixed = TRUE)))

  if (is.null(weights))
    weights <- rep.int(1, NROW(combinations))
  if (!is.null(by)) {
    vapply(by,
           function(x) assert_that(is.factor(x) || is.character(x)),
           FUN.VALUE = logical(1))
    if (NCOL(by) > 2L)
      stop("No more than two conditioning variables are allowed.")
  }

  out <- matrix(NA, nrow = NROW(combinations), ncol = NCOL(combinations))
  colnames(out) <- colnames(combinations)
  for (i in seq_along(combinations)) {
    y <- combinations[, i]
    if (is.factor(y) || is.character(y)) {
      facs <- unique(as.character(y))
      if (length(facs) > 2L)
        stop("No more than 2 levels allowed.")
      out[, i] <- y == facs[1L]
      colnames(out)[i] <- facs[1L]
    } else if (is.numeric(y)) {
      out[, i] <- as.logical(y)
    } else if (is.logical(y)) {
      out[, i] <- y
    } else {
      stop("Unsupported type of variables.")
    }
  }
  combinations <- as.data.frame(out)
  combinations$weights <- weights

  if (is.null(by)) {
    out <- tally_combinations(combinations)
  } else {
    out <- by(combinations, by, tally_combinations, simplify = FALSE)
    class(out) <- c("by", "euler", "list")
  }

  out
}

#' @describeIn euler A matrix that can be converted to a data.frame of logicals
#'   (as in the description above) via [base::as.data.frame.matrix()].
#' @export
euler.matrix <- function(combinations, ...) {
  euler(as.data.frame(combinations), ...)
}

#' @describeIn euler A table with `max(dim(x)) < 3`.
#' @export
#' @examples
#' plot(euler(as.table(apply(Titanic, 2:4, sum))))
euler.table <- function(combinations, ...) {
  if (max(dim(combinations)) > 2L)
    stop("No table dimension may exceed 2.")
  x <- as.data.frame(combinations)
  euler(x[, !(names(x) == "Freq")], weights = x$Freq, ...)
}


#' @describeIn euler A list of vectors, each vector giving the contents of
#'   that set. Vectors in the list do not need to be named.
#' @export
euler.list <- function(combinations, ...) {
  assert_that(has_attr(combinations, "names"),
              !any(names(combinations) == ""),
              !any(duplicated(names(combinations))))

  sets <- names(combinations)
  n <- length(sets)

  id <- bit_indexr(n)

  out <- integer(nrow(id))
  names(out) <- apply(id, 1, function(x) paste(sets[x], collapse = "&"))

  for (i in 1:nrow(id))
    out[i] <- length(Reduce(intersect, combinations[id[i, ]]))

  euler(out, input = "union")
}

#' Area-Proportional Euler Diagrams (defunct)
#'
#' * Note: This function is defunct; please use [euler()].
#' instead.*
#'
#' @param ... Ignored
#'
#' @export

eulerr <- function(...) {
  .Defunct("euler")
}
