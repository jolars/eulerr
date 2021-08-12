#' Area-proportional Euler diagrams
#'
#' Fit Euler diagrams (a generalization of Venn diagrams) using numerical
#' optimization to find exact or approximate solutions to a specification of set
#' relationships. The shape of the diagram may be a circle or an ellipse.
#'
#' If the input is a matrix or data frame and argument `by` is specified,
#' the function returns a list of euler diagrams.
#'
#' The function minimizes the residual sums of squares,
#' \deqn{
#'   \sum_{i=1}^n (A_i - \omega_i)^2,
#' }{
#'   \sum (A_i - \omega_i)^2,
#' }
#' where \eqn{\omega_i} the size of the ith disjoint subset, and \eqn{A_i} the
#' corresponding area in the diagram, that is, the unique contribution to the
#' total area from this overlap.
#'
#' [euler()] also returns `stress` (from \pkg{venneuler}), as well as
#' `diagError`, and `regionError` from \pkg{eulerAPE}.
#'
#' The *stress* statistic is computed as
#'
#' \deqn{
#'   \frac{\sum_{i=1}^n (A_i - \beta\omega_i)^2}{\sum_{i=1}^n A_i^2},
#' }{
#'   \sum (A_i - \beta\omega_i)^2 / \sum A_i^2,
#' }
#' where
#' \deqn{
#'   \beta = \sum_{i=1}^n A_i\omega_i / \sum_{i=1}^n \omega_i^2.
#' }{
#'   \beta = \sum A_i\omega_i / \sum \omega_i^2.
#' }
#'
#' `regionError` is computed as
#'
#' \deqn{
#'   \left| \frac{A_i}{\sum_{i=1}^n A_i} - \frac{\omega_i}{\sum_{i=1}^n \omega_i}\right|.
#' }{
#'   max|A_i / \sum A  - \omega_i / \sum \omega|.
#' }
#'
#' `diagError` is simply the maximum of regionError.
#'
#' @param combinations set relationships as a named numeric vector, matrix, or
#'   data.frame (see **methods (by class)**)
#' @param by a factor or character matrix to be used in [base::by()] to
#'   split the data.frame or matrix of set combinations
#' @param input type of input: disjoint identities
#'   (`'disjoint'`) or unions (`'union'`).
#' @param shape geometric shape used in the diagram
#' @param control a list of control parameters.
#'   * `extraopt`: should the more thorough optimizer (currently
#'   [GenSA::GenSA()]) kick in (provided `extraopt_threshold` is exceeded)? The
#'   default is `TRUE` for ellipses and three sets and `FALSE` otherwise.
#'   * `extraopt_threshold`: threshold, in terms of `diagError`, for when
#'   the extra optimizer kicks in. This will almost always slow down the
#'   process considerably. A value of 0 means
#'   that the extra optimizer will kick in if there is *any* error. A value of
#'   1 means that it will never kick in. The default is `0.001`.
#'   * `extraopt_control`: a list of control parameters to pass to the
#'   extra optimizer, such as `max.call`. See [GenSA::GenSA()].
#' @param ... arguments passed down to other methods
#'
#' @return A list object of class `'euler'` with the following parameters.
#'   \item{ellipses}{a matrix of `h` and `k` (x and y-coordinates for the
#'     centers of the shapes), semiaxes `a` and `b`, and rotation angle `phi`}
#'   \item{original.values}{set relationships in the input}
#'   \item{fitted.values}{set relationships in the solution}
#'   \item{residuals}{residuals}
#'   \item{regionError}{the difference in percentage points between each
#'     disjoint subset in the input and the respective area in the output}
#'   \item{diagError}{the largest `regionError`}
#'   \item{stress}{normalized residual sums of squares}
#'
#' @seealso [plot.euler()], [print.euler()], [eulerr_options()], [venn()]
#'
#' @examples
#' # Fit a diagram with circles
#' combo <- c(A = 2, B = 2, C = 2, "A&B" = 1, "A&C" = 1, "B&C" = 1)
#' fit1 <- euler(combo)
#'
#' # Investigate the fit
#' fit1
#'
#' # Refit using ellipses instead
#' fit2 <- euler(combo, shape = "ellipse")
#'
#' # Investigate the fit again (which is now exact)
#' fit2
#'
#' # Plot it
#' plot(fit2)
#'
#' # A set with no perfect solution
#' euler(c("a" = 3491, "b" = 3409, "c" = 3503,
#'         "a&b" = 120, "a&c" = 114, "b&c" = 132,
#'         "a&b&c" = 50))
#'
#' @references Wilkinson L. Exact and Approximate Area-Proportional Circular
#'   Venn and Euler Diagrams. IEEE Transactions on Visualization and Computer
#'   Graphics (Internet). 2012 Feb (cited 2016 Apr 9);18(2):321-31. Available
#'   from: \doi{10.1109/TVCG.2011.56}
#'
#'   Micallef L, Rodgers P. eulerAPE: Drawing Area-Proportional 3-Venn Diagrams
#'   Using Ellipses. PLOS ONE (Internet). 2014 Jul (cited 2016 Dec
#'   10);9(7):e101717. Available from:
#'   \doi{10.1371/journal.pone.0101717}
#'
#' @export
euler <- function(combinations, ...) UseMethod("euler")

#' @describeIn euler a named numeric vector, with
#'   combinations separated by an ampersand, for instance `A&B = 10`.
#'   Missing combinations are treated as being 0.
#'
#' @export
euler.default <- function(combinations,
                          input = c("disjoint", "union"),
                          shape = c("circle", "ellipse"),
                          control = list(),
                          ...)
{
  fit_diagram(combinations,
              "euler",
              input,
              shape,
              control,
              ...)
}

#' @describeIn euler a `data.frame` of logicals, binary integers, or
#'   factors.
#' @param weights a numeric vector of weights of the same length as
#'   the number of rows in `combinations`.
#' @param sep a character to use to separate the dummy-coded factors
#'   if there are factor or character vectors in 'combinations'.
#' @param factor_names whether to include factor names when
#'   constructing dummy codes
#' @export
#' @examples
#'
#' # Using grouping via the 'by' argument through the data.frame method
#' euler(fruits, by = list(sex, age))
#'
euler.data.frame <- function(combinations,
                             weights = NULL,
                             by = NULL,
                             sep = "_",
                             factor_names = TRUE,
                             ...)
{
  by <- substitute(by)
  facs <- eval(by, combinations)

  combinations <- parse_dataframe(combinations,
                                  weights,
                                  by,
                                  facs,
                                  sep,
                                  factor_names)

  if (is.list(combinations)) {
    out <- lapply(combinations, euler, ...)
    class(out) <- c("euler", "list")
    attr(out, "groups") <- attr(combinations, "groups")
  } else {
    out <- euler(combinations, ...)
  }

  out
}

#' @describeIn euler a matrix that can be converted to a data.frame of logicals
#'   (as in the description above) via [base::as.data.frame.matrix()].
#' @export
#'
#' @examples
#'
#' # Using the matrix method
#' euler(organisms)
#'
#' # Using weights
#' euler(organisms, weights = c(10, 20, 5, 4, 8, 9, 2))
euler.matrix <- function(combinations, ...)
{
  euler(as.data.frame(combinations), ...)
}

#' @describeIn euler A table with `max(dim(x)) < 3`.
#' @export
#'
#' @examples
#'
#' # The table method
#' euler(pain, factor_names = FALSE)
euler.table <- function(combinations, ...)
{
  x <- as.data.frame(combinations)
  euler(x[, !(names(x) == "Freq")], weights = x$Freq, ...)
}

#' @describeIn euler a list of vectors, each vector giving the contents of
#'   that set (with no duplicates). Vectors in the list must be named.
#' @export
#' @examples
#'
#' # A euler diagram from a list of sample spaces (the list method)
#' euler(plants[c("erigenia", "solanum", "cynodon")])
euler.list <- function(combinations, ...)
{
  out <- parse_list(combinations)
  euler(out, input = "union", ...)
}
