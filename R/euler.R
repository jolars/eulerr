#' Area-proportional Euler diagrams
#'
#' Fit Euler diagrams (a generalization of Venn diagrams) using numerical
#' optimization to find exact or approximate solutions to a specification of set
#' relationships. The shape of the diagram may be a circle, an ellipse, an
#' axis-aligned rectangle, or an axis-aligned square.
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
#' by default, where \eqn{\omega_i} the size of the ith disjoint subset, and
#' \eqn{A_i} the corresponding area in the diagram, that is, the unique
#' contribution to the total area from this overlap. The loss function
#' can, however, be controlled via the `loss` argument.
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
#' @param transform a function applied to the areas of the *disjoint*
#'   (exclusive) regions before fitting. The default, [base::identity()], leaves
#'   the areas untouched. A monotone transform such as [base::log1p()] can keep
#'   small regions legible when set sizes span several orders of magnitude. The
#'   transform is applied to the exclusive regions because those are the
#'   additive atoms the diagram fits to; as a consequence the area of a whole
#'   set or union no longer equals `transform()` of its size, only the
#'   individual visible regions carry the transformed scale. The function must
#'   return a non-negative, finite value for each region (and for `complement`,
#'   when given). Has no effect on [venn()] diagrams, whose geometry is fixed.
#' @param shape geometric shape used in the diagram
#' @param loss type of loss to minimize over. The default,
#'   `"sum_squared"`, minimizes the sum of squared errors. The available
#'   options mirror the loss functions exposed by the `eunoia` Rust crate
#'   that powers the optimizer:
#'   * `"sum_squared"` --- normalized sum of squared errors (default).
#'   * `"sum_absolute"` --- normalized sum of absolute errors.
#'   * `"sum_absolute_region_error"` --- normalized sum of absolute
#'     region errors.
#'   * `"sum_squared_region_error"` --- normalized sum of squared region
#'     errors.
#'   * `"max_absolute"` --- normalized maximum absolute error.
#'   * `"max_squared"` --- normalized maximum squared error.
#'   * `"root_mean_squared"` --- normalized root-mean-squared error.
#'   * `"stress"` --- venneuler-style stress.
#'   * `"diag_error"` --- eulerAPE-style `diagError`.
#' @param loss_aggregator deprecated; use `loss` directly instead. Pre-1.0
#'   code that combined `loss` (`"square"`/`"abs"`/`"region"`) with
#'   `loss_aggregator` (`"sum"`/`"max"`) still works but emits a warning;
#'   the combination is mapped to the equivalent new `loss` value.
#' @param complement an optional single non-negative number giving the area
#'   of the *complement* — that is, the universe outside every named set.
#'   When supplied, the fitter jointly optimizes a containing rectangle
#'   together with the diagram shapes so that the area of the rectangle
#'   minus the union of (clipped) shapes matches `complement`. This is the
#'   classical "everything not in any set" region; see [plot.euler()] for
#'   how it is rendered. Defaults to `NULL` (no container; classical
#'   shape-only fit). Not supported for [venn()].
#' @param control a list of control parameters.
#'   * `extraopt`: should the global-search fallback optimizer (CMA-ES) kick
#'   in when the primary optimizer's `diagError` exceeds `extraopt_threshold`?
#'   The default is `TRUE` for three-set ellipse fits and `FALSE` otherwise.
#'   * `extraopt_threshold`: threshold, in terms of `diagError`, for when
#'   the CMA-ES fallback kicks in. A value of 0 means it will kick in for
#'   *any* error; a value of 1 means it will never kick in. Default `0.001`.
#'   * `tolerance`: convergence tolerance passed to the underlying solver.
#'   Tighter values give more accurate fits at higher cost. Default `1e-8`.
#'   * `max_sets`: maximum number of sets the underlying engine will accept.
#'   Defaults to `NULL`, which uses the engine's built-in default of 32.
#'   Region masks are stored in a bitset, so values may be raised up to 63
#'   (the absolute hard cap). Going higher is rarely useful in practice
#'   since fully-overlapping diagrams have `2^n - 1` regions.
#'   * `n_threads`: number of threads used to fan out the optimizer's restart
#'   loop. A positive integer pins a private thread pool of that size, while
#'   `NULL` uses all available cores. This is purely a wall-time knob: the
#'   fitted diagram is identical regardless of the thread count. The default
#'   uses half of the available logical cores (but a single thread under
#'   `R CMD check`, to respect CRAN's two-core policy). It can be overridden
#'   globally with the `eulerr.n_threads` option or the `EULERR_NUM_THREADS`
#'   environment variable, and otherwise honors R's conventional `mc.cores`
#'   option (or `MC_CORES` environment variable).
#' @param ... arguments passed down to other methods
#'
#' @return A list object of class `'euler'` with the following parameters.
#'   \item{shapes}{a data frame of fitted shape parameters. One row per set
#'     with a `type` column (one of `"circle"`, `"ellipse"`, `"rectangle"`,
#'     `"square"`), the center coordinates `h` and `k`, and the
#'     shape-specific columns: `a`, `b`, `phi` for ellipses/circles; `width`
#'     and `height` for rectangles; `side` (plus mirrored `width`/`height`)
#'     for squares. Columns that don't apply to the chosen shape are `NA`.}
#'   \item{ellipses}{for `shape = "circle"` and `shape = "ellipse"` fits,
#'     the legacy 5-column data frame of `h`, `k`, `a`, `b`, `phi`. This
#'     slot is deprecated in favour of `shapes` and is not populated for
#'     rectangle/square fits.}
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
#' euler(c(
#'   "a" = 3491, "b" = 3409, "c" = 3503,
#'   "a&b" = 120, "a&c" = 114, "b&c" = 132,
#'   "a&b&c" = 50
#' ))
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
euler.default <- function(
  combinations,
  input = c("disjoint", "union"),
  transform = identity,
  shape = c("circle", "ellipse", "rectangle", "square"),
  loss = c(
    "sum_squared",
    "sum_absolute",
    "sum_absolute_region_error",
    "sum_squared_region_error",
    "max_absolute",
    "max_squared",
    "root_mean_squared",
    "stress",
    "diag_error"
  ),
  loss_aggregator = NULL,
  complement = NULL,
  control = list(),
  ...
) {
  fit_diagram(
    combinations,
    "euler",
    input,
    transform = transform,
    shape = shape,
    loss = loss,
    loss_aggregator = loss_aggregator,
    complement = complement,
    control = control,
    ...
  )
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
euler.data.frame <- function(
  combinations,
  weights = NULL,
  by = NULL,
  sep = "_",
  factor_names = TRUE,
  ...
) {
  by <- substitute(by)
  facs <- eval(by, combinations)

  combinations <- parse_dataframe(
    combinations,
    weights,
    by,
    facs,
    sep,
    factor_names
  )

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
euler.matrix <- function(combinations, ...) {
  euler(as.data.frame(combinations), ...)
}

#' @describeIn euler A table with `max(dim(x)) < 3`.
#' @export
#'
#' @examples
#'
#' # The table method
#' euler(pain, factor_names = FALSE)
euler.table <- function(combinations, ...) {
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
euler.list <- function(combinations, ...) {
  out <- parse_list(combinations)
  euler(out, input = "disjoint", ...)
}
