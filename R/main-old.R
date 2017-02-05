#' Area-Proportional Euler Diagrams (deprecated)
#'
#' \emph{Note: This function has been deprecated; please use \code{\link{euler}}
#' instead.}
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
#' @param cost Deprecated.
#' @param \dots Currently ignored.
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
#' @export

eulerr.default <- function(sets, cost = NULL, ...) {
  assertthat::assert_that(
    is.numeric(sets),
    assertthat::not_empty(sets),
    length(sets) > 0,
    all(sets >= 0),
    assertthat::has_attr(sets, "names"),
    !any(names(sets) == ""),
    !any(duplicated(names(sets)))
  )

  .Deprecated("euler")

  if (!missing(cost)) {
    warning("The option to choose cost function has been deprecated and sums
            of squared errors will be used at all times.",
            call. = FALSE)
  }

  euler(combinations = sets, input = "union")
}


#' @describeIn eulerr A matrix of logical vectors with columns representing sets
#'   and rows representing each observation's set relationships (see examples).
#' @export

eulerr.matrix <- function(sets, by = NULL, cost = NULL, ...) {
  if (!is.null(ncol(by)))
    if (ncol(by) > 2)
      stop("Currently, no more than two grouping variables are allowed.",
           .call = FALSE)

  if (!is.null(by)) {
    assertthat::assert_that(
      is.character(by) | is.factor(by) | is.data.frame(by) | is.matrix(by)
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
      assertthat::assert_that(is.character(by) | is.factor(by))
    }
  }

  assertthat::assert_that(
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

eulerr.data.frame <- function(sets, by = NULL, cost = NULL, ...) {
  eulerr(as.matrix(sets), by = by, cost = cost, ...)
}
