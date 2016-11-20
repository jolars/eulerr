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
#' @useDynLib eulerr
#' @importFrom Rcpp sourceCpp
#' @import assertthat
#'
#' @export

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
    all(sets >= 0),
    has_attr(sets, "names"),
    !any(names(sets) == ""),
    !any(duplicated(names(sets)))
  )

  setnames <- strsplit(names(sets), split = "&", fixed = T)
  one_sets <- unique(unlist(setnames, use.names = FALSE))
  n <- length(one_sets)

  id <- as.matrix(expand.grid(
    lapply(seq_along(one_sets), function(x) c(FALSE, TRUE)),
    stringsAsFactors = FALSE,
    KEEP.OUT.ATTRS = FALSE
  ))
  id <- id[-1, ] # get rid of row of empty

  # Scale the values to fractions
  scale_factor <- sum(sets)
  sets <- sets / scale_factor

  areas <- apply(id, 1, function(x) {
    i <- names(sets) == paste(one_sets[x], collapse = "&")
    if (any(i)) sets[i] else 0
  })

  id_sums <- rowSums(id)
  ones <- id_sums == 1
  twos <- id_sums == 2

  two <- apply(id[twos, , drop = FALSE], 1, which)
  two_a <- two[1, ]
  two_b <- two[2, ]

  r <- sqrt(areas[ones] / pi)
  disjoint <- areas[twos] == 0

  distances <- mapply(
    separate_two_discs,
    r1 = r[two_a],
    r2 = r[two_b],
    overlap = areas[twos],
    USE.NAMES = FALSE
  )

  # Establish identities of disjoint and contained sets
  tmp <- matrix(areas[ones][two], nrow = 2)
  contained <- areas[twos] == tmp[1, ] | areas[twos] == tmp[2, ]

  # Make sure that no two set intersections are larger than their parent sets
  assert_that(!any(areas[twos] > tmp[1, ] | areas[twos] > tmp[2, ]))

  initial_layout <- stats::optim(
    par = stats::runif(n * 2L, 0L, min(r)),
    fn = initial_layout_optimizer,
    gr = initial_layout_gradient,
    distances = distances,
    disjoint = disjoint,
    contained = contained,
    two = two,
    lower = rep(0, times = n * 2L),
    upper = rep(sum(r) * 2L - min(r) - max(r)),
    method = c("L-BFGS-B")
  )

  final_layout <- stats::optim(
    fn = final_layout_optimizer,
    par = c(initial_layout$par, r),
    areas = areas,
    id = id,
    two = two,
    twos = twos,
    ones = ones,
    method = c("Nelder-Mead")
  )

  fit <- return_intersections(final_layout$par, areas, id, two, twos, ones) *
    scale_factor
  names(fit) <- apply(id, 1, function(x) paste0(one_sets[x], collapse = "&"))
  orig <- areas * scale_factor
  names(orig) <- names(fit)
  fpar <- matrix(final_layout$par,
                 ncol = 3,
                 dimnames = list(one_sets, c("x", "y", "r")))
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