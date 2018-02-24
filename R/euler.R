# eulerr: Area-Proportional Euler and Venn Diagrams with Circles or Ellipses
# Copyright (C) 2018 Johan Larsson <johanlarsson@outlook.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Area-proportional Euler diagrams
#'
#' Fit Euler diagrams (a generalization of Venn diagrams) using numerical
#' optimization to find exact or approximate solutions to a specification of set
#' relationships. The shape of the diagram may be a circle or an ellipse.
#'
#' If the input is a matrix or data frame and argument `by` is specified,
#' the function returns a list of euler diagrams.
#'
#' The function minimizes the *stress* statistic from \pkg{venneuler},
#'
#' \deqn{
#'   \frac{
#'     \sum_{i=1}^{n} (y_i - \hat{y}_i) ^ 2}{\sum_{i=1}^{n} y_i ^ 2},
#'   }{
#'   \sum (fit - original) ^ 2 / \sum original ^ 2,
#' }
#'
#' where \eqn{\hat{y}}{fit} are ordinary least squares estimates from the
#' regression of the fitted areas on the original areas that are currently being
#' explored. The stress statistic can also be used as a goodness of fit
#' measure.
#'
#' [euler()] also returns `diagError` and `regionError` from
#' \pkg{eulerAPE}. `regionError` is computed as
#'
#' \deqn{
#'     \left| \frac{y_i}{\sum y_i} - \frac{\hat{y}_i}{\sum \hat{y}_i}\right|.
#'   }{
#'     max|fit / \sum fit  - original / \sum original|.
#'  }
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
#'   [RcppDE::DEoptim()]) kick in (provided `extraopt_threshold` is exceeded)? The
#'   default is `TRUE` for ellipses and three sets and `FALSE` otherwise.
#'   * `extraopt_threshold`: threshold, in terms of `diagError`, for when
#'   the extra optimizer kicks in. This will almost always slow down the
#'   process considerably. A value of 0 means
#'   that the extra optimizer will kick in if there is *any* error. A value of
#'   1 means that it will never kick in. The default is `0.001`.
#'   * `extraopt_control`: a list of control parameters to pass to the
#'   extra optimizer, such as `itermax`. See [RcppDE::DEoptim.control()].
#' @param ... arguments passed down to other methods
#'
#' @return A list object of class `'euler'` with the following parameters.
#'   \item{coefficients}{a matrix of `h` and `k` (x and y-coordinates for the
#'     centers of the shapes), semiaxes `a` and `b`, and rotation angle `phi`}
#'   \item{original.values}{set relationships in the input}
#'   \item{fitted.values}{set relationships in the solution}
#'   \item{residuals}{residuals}
#'   \item{regionError}{the difference in percentage points between each
#'     disjoint subset in the input and the respective area in the output}
#'   \item{diagError}{the largest `regionError`}
#'   \item{stress}{normalized residual sums of squares}
#'
#' @seealso [plot.euler()], [print.euler()]
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
#'   from:
#'   [http://doi.org/10.1109/TVCG.2011.56](http://doi.org/10.1109/TVCG.2011.56)
#'
#'   Micallef L, Rodgers P. eulerAPE: Drawing Area-Proportional 3-Venn Diagrams
#'   Using Ellipses. PLOS ONE (Internet). 2014 Jul (cited 2016 Dec 10);9(7):e101717. Available from:
#'   [http://dx.doi.org/10.1371/journal.pone.0101717](http://dx.doi.org/10.1371/journal.pone.0101717)
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
  shape = c("circle", "ellipse"),
  control = list(),
  ...
) {
  stopifnot(is.numeric(combinations),
            !any(combinations < 0),
            !is.null(attr(combinations, "names")),
            !any(names(combinations) == ""),
            !any(duplicated(names(combinations))))

  combo_names <- strsplit(names(combinations), split = "&", fixed = TRUE)
  setnames <- unique(unlist(combo_names, use.names = FALSE))

  n <- length(setnames)
  id <- bit_indexr(n)
  N <- NROW(id)
  n_restarts <- 10L # should this be made an argument?
  small <- sqrt(.Machine$double.eps)

  control <- utils::modifyList(
    list(extraopt = (n == 3) && (match.arg(shape) == "ellipse"),
         extraopt_threshold = 0.001,
         extraopt_control = list()),
    control)

  areas <- double(N)
  for (i in 1L:N) {
    s <- setnames[id[i, ]]
    for (j in seq_along(combo_names)) {
      if (setequal(s, combo_names[[j]])) {
        areas[i] <- combinations[j]
      }
    }
  }

  if (n > 1L) {
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
        stop("Check your set configuration. Some disjoint areas are negative.")
    }

    if (all(areas == 0)) {
      # all sets are zero
      fpar <- matrix(data = rep.int(0, 5L*n),
                     ncol = 5L,
                     dimnames = list(setnames, c("h", "k", "a", "b", "phi")))
      regionError <- diagError <- stress <- 0
      orig <- fit <- areas
      names(orig) <- names(fit) <-
        apply(id, 1L, function(x) paste0(setnames[x], collapse = "&"))
    } else {
      id_sums <- rowSums(id)
      ones <- id_sums == 1L
      twos <- id_sums == 2L
      two <- choose_two(1L:n)
      r <- sqrt(areas[ones]/pi)

      # Establish identities of disjoint and subset sets
      subset <- disjoint <- matrix(FALSE, ncol = n, nrow = n)
      distances <- area_mat <- matrix(0, ncol = n, nrow = n)

      lwrtri <- lower.tri(subset)

      tmp <- matrix(areas[ones][two], ncol = 2L)

      subset[lwrtri] <- areas[twos] == tmp[, 1L] | areas[twos] == tmp[, 2L]
      disjoint[lwrtri] <- areas[twos] == 0
      distances[lwrtri] <- mapply(separate_two_discs,
                                  r1 = r[two[, 1L]],
                                  r2 = r[two[, 2L]],
                                  overlap = areas[twos],
                                  USE.NAMES = FALSE)

      # Starting layout
      loss <- Inf
      initial_layouts <- vector("list", n_restarts)
      bnd <- sqrt(sum(r^2*pi))

      i <- 1L
      while (loss > small && i <= n_restarts) {
        initial_layouts[[i]] <- stats::nlm(
          f = optim_init,
          p = stats::runif(n*2, 0, bnd),
          d = distances,
          disjoint = disjoint,
          subset = subset,
          iterlim = 1000L
        )
        loss <- initial_layouts[[i]]$minimum
        i <- i + 1L
      }

      # Find the best initial layout
      best_init <- which.min(lapply(initial_layouts[1L:(i - 1L)],
                                    "[[",
                                    "minimum"))
      initial_layout <- initial_layouts[[best_init]]

      # Final layout
      circle <- match.arg(shape) == "circle"

      if (circle) {
        pars <- as.vector(matrix(c(initial_layout$estimate, r), 3L,
                                 byrow = TRUE))
        lwr <- rep.int(0, 3L)
        upr <- rep.int(bnd, 3L)
      } else {
        pars <- as.vector(rbind(matrix(initial_layout$estimate, 2L,
                                       byrow = TRUE),
                                r, r, 0, deparse.level = 0L))
        lwr <- c(rep.int(0, 4L), -2*pi)
        upr <- c(rep.int(bnd, 4L), 2*pi)
      }

      orig <- areas_disjoint

      # Try to find a solution using nlm() first (faster)
      # TODO: Allow user options here?
      nlm_solution <- stats::nlm(
        f = optim_final_loss,
        p = pars,
        areas = areas_disjoint,
        circle = circle,
        iterlim = 1e4L
      )$estimate

      tpar <- as.data.frame(matrix(
        data = nlm_solution,
        ncol = if (circle) 3L else 5L,
        dimnames = list(
          setnames,
          if (circle) c("h", "k", "r") else c("h", "k", "a", "b", "phi")
        ),
        byrow = TRUE
      ))
      if (circle)
        tpar <- cbind(tpar, tpar[, 3L], 0)

      # Normalize layout
      nlm_fit <- as.vector(intersect_ellipses(nlm_solution, circle))

      nlm_pars <- compress_layout(normalize_pars(tpar), id, nlm_fit)

      nlm_diagError <- diagError(nlm_fit, orig)

      # If inadequate solution, try with a second optimizer (slower, better)
      if (!circle && control$extraopt &&
          nlm_diagError > control$extraopt_threshold) {
        # Set bounds for the parameters
        newpars <- matrix(
          data = as.vector(t(nlm_pars)),
          ncol = 5L,
          dimnames = list(setnames, c("h", "k", "a", "b", "phi")),
          byrow = TRUE
        )

        constraints <- get_constraints(compress_layout(newpars, id, nlm_fit))

        # TODO: Set up initial population in some clever fashion.

        deoptim <- RcppDE::DEoptim(
          fn = optim_final_loss,
          lower = constraints$lwr,
          upper = constraints$upr,
          control = do.call(
            RcppDE::DEoptim.control,
            utils::modifyList(
              list(VTR = -Inf,
                   NP = length(newpars)*10,
                   CR = 0.6,
                   F = 0.2,
                   itermax = 1000L,
                   trace = FALSE),
              control$extraopt_control
            )
          ),
          areas = areas_disjoint,
          circle = circle
        )

        # Fine tune the fit from DEoptim
        last_ditch_effort <- stats::nlm(
          f = optim_final_loss,
          p = deoptim$optim$bestmem,
          areas = areas_disjoint,
          circle = circle,
          iterlim = 1e4L
        )$estimate

        last_ditch_fit <- as.vector(intersect_ellipses(last_ditch_effort,
                                                       circle))
        last_ditch_diagError <- diagError(last_ditch_fit, orig)

        # Check for the best solution
        if (last_ditch_diagError < nlm_diagError) {
          final_par <- last_ditch_effort
          fit <- last_ditch_fit
        } else {
          final_par <- nlm_solution
          fit <- nlm_fit
        }
      } else {
        final_par <- nlm_solution
        fit <- nlm_fit
      }

      names(orig) <- names(fit) <-
        apply(id, 1L, function(x) paste0(setnames[x], collapse = "&"))

      regionError <- regionError(fit, orig)
      diagError <- diagError(regionError = regionError)
      stress <- stress(orig, fit)

      fpar <- matrix(data = final_par,
                     ncol = if (circle) 3L else 5L,
                     byrow = TRUE)

      if (circle)
        fpar <- cbind(fpar, fpar[, 3L], 0)

      dimnames(fpar) <- list(setnames, c("h", "k", "a", "b", "phi"))

      # Normalize semiaxes and rotation
      fpar <- normalize_pars(fpar)

      # Find disjoint clusters and compress the layout
      fpar <- compress_layout(fpar, id, fit)

      # Center the solution on the coordinate plane
      fpar <- center_layout(fpar)
    }
  } else {
    # One set
    fpar <- matrix(data = c(0, 0, sqrt(areas/pi), sqrt(areas/pi), 0),
                   ncol = 5L,
                   dimnames = list(setnames, c("h", "k", "a", "b", "phi")))
    regionError <- diagError <- stress <- 0
    orig <- fit <- areas
    names(orig) <- names(fit) <- setnames
  }

  # Return eulerr structure
  structure(list(ellipses = as.data.frame(fpar),
                 original.values = orig,
                 fitted.values = fit,
                 residuals = orig - fit,
                 regionError = regionError,
                 diagError = diagError,
                 stress = stress),
            class = c("euler", "list"))
}

#' @describeIn euler a data.frame of logicals, two-level factors (see examples).
#' @param weights a numeric vector of weights of the same length as
#'   the number of rows in `combinations`.
#' @export
#' @examples
#' # Using grouping via the 'by' argument through the data.frame method
#' dat <- data.frame(
#'   A = sample(c(TRUE, FALSE), size = 100, replace = TRUE),
#'   B = sample(c(TRUE, TRUE, FALSE), size = 100, replace = TRUE),
#'   gender = sample(c("Men", "Women"), size = 100, replace = TRUE),
#'   nation = sample(c("Sweden", "Denmark"), size = 100, replace = TRUE)
#' )
#'
#' euler(dat, by = list(gender, nation))
#'
#'
#' dat2 <- data.frame(A = c(TRUE, FALSE, TRUE, TRUE),
#'                    B = c(FALSE, TRUE, TRUE, FALSE))
#' euler(dat2, weights = c(3, 2, 1, 1))
#'
euler.data.frame <- function(combinations, weights = NULL, by = NULL, ...) {
  stopifnot(!any(grepl("&", colnames(combinations), fixed = TRUE)))

  facs <- eval(substitute(by), combinations)

  if (!is.null(facs)) {
    if (is.list(facs)) {
      if (!is.null(names(facs)))
        nms <- names(facs)
      else {
        nms <- sapply(substitute(by)[-1L], deparse)
      }
    } else
      nms <- deparse(substitute(by))

    if (is.list(facs))
      stopifnot(length(facs) < 3)
    else
      facs <- list(facs)

    dd <- as.data.frame(facs, col.names = nms)
    groups <- unique(dd)
    rownames(groups) <- NULL

    out <- g <- vector("list", NROW(groups))
    int_or_log <- vapply(combinations,
                         function(x) is.numeric(x) || is.logical(x),
                         FUN.VALUE = logical(1L))
    for (i in seq_len(NROW(groups))) {
      ind <- apply(dd, 1, function(x) all(x == groups[i, ]))
      out[[i]] <- euler(combinations[ind, int_or_log])
      names(out)[[i]] <- paste(unlist(groups[i, , drop = TRUE]),
                               collapse = ".")
    }

    class(out) <- c("euler", "list")
    attr(out, "groups") <- groups
  } else {
    if (is.null(weights)) {
      combinations <- combinations[,
                                   vapply(combinations,
                                          function(y)
                                            is.integer(y) || is.logical(y),
                                          FUN.VALUE = logical(1L))]
    } else {
      combinations <- combinations[,
                                   vapply(combinations,
                                          function(y)
                                            is.integer(y) ||
                                            is.logical(y) ||
                                            is.factor(y) ||
                                            is.character(y),
                                          FUN.VALUE = logical(1L))]
    }

    if (is.null(weights))
      weights <- rep.int(1L, NROW(combinations))

    out <- matrix(NA, nrow = NROW(combinations), ncol = NCOL(combinations))
    colnames(out) <- colnames(combinations)

    for (i in seq_along(combinations)) {
      y <- combinations[, i]
      if (is.factor(y) || is.character(y)) {
        facs <- unique(as.character(y))
        if (length(facs) > 2L)
          stop("no more than 2 levels allowed")
        out[, i] <- y == facs[1L]
        colnames(out)[i] <- facs[1L]
      } else if (is.numeric(y)) {
        out[, i] <- as.logical(y)
      } else if (is.logical(y)) {
        out[, i] <- y
      } else {
        stop("unsupported type of variables")
      }
    }
    combinations <- as.data.frame(out)
    out <- tally_combinations(combinations, weights)
    out <- euler(out, ...)
  }
  out
}

#' @describeIn euler a matrix that can be converted to a data.frame of logicals
#'   (as in the description above) via [base::as.data.frame.matrix()].
#' @export
#' @examples
#' # Using the matrix method
#' mat <- cbind(A = sample(c(TRUE, TRUE, FALSE), size = 50, replace = TRUE),
#'              B = sample(c(TRUE, FALSE), size = 50, replace = TRUE))
#' euler(mat)
euler.matrix <- function(combinations, ...) {
  euler(as.data.frame(combinations), ...)
}

#' @describeIn euler A table with `max(dim(x)) < 3`.
#' @export
#' @examples
#' # The table method
#' plot(euler(as.table(apply(Titanic, 2:4, sum))))
euler.table <- function(combinations, ...) {
  if (max(dim(combinations)) > 2L)
    stop("no table dimension may exceed 2")
  x <- as.data.frame(combinations)
  euler(x[, !(names(x) == "Freq")], weights = x$Freq, ...)
}

#' @describeIn euler a list of vectors, each vector giving the contents of
#'   that set (with no duplicates). Vectors in the list do not need to be named.
#' @export
#' @examples
#' # A euler diagram from a list of sample spaces (the list method)
#' euler(list(A = c("a", "ab", "ac", "abc"),
#'            B = c("b", "ab", "bc", "abc"),
#'            C = c("c", "ac", "bc", "abc")))
euler.list <- function(combinations, ...) {
  stopifnot(!is.null(attr(combinations, "names")),
            !any(names(combinations) == ""),
            !any(duplicated(names(combinations))),
            all(sapply(combinations, anyDuplicated) == 0))

  sets <- names(combinations)
  n <- length(sets)

  id <- bit_indexr(n)

  out <- integer(nrow(id))
  names(out) <- apply(id, 1L, function(x) paste(sets[x], collapse = "&"))

  for (i in 1L:nrow(id))
    out[i] <- length(Reduce(intersect, combinations[id[i, ]]))

  euler(out, input = "union")
}
