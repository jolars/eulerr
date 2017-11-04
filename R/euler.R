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
#' \deqn{\sum_{i=1}^{n} (y_i - \hat{y}_i) ^ 2,}{\sum (orig - fit) ^ 2,}
#'
#' where \eqn{\hat{y}}{fit} are estimates of \eqn{y} that are currently being
#' explored.
#'
#' The stress statistic from \pkg{venneuler} is returned to give an indication
#' of the goodness of the fit:
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
#' explored.
#'
#' `euler()` also returns `diagError` and `regionError` from
#' *eulerAPE*. `regionError` is computed as
#'
#' \deqn{
#'     \left| \frac{y_i}{\sum y_i} - \frac{\hat{y}_i}{\sum \hat{y}_i}\right|.
#'   }{
#'     max|fit / \sum fit  - original / \sum original|.
#'  }
#'
#' `diagError` is simply the maximum of regionError.
#'
#' @param combinations Set relationships as a named numeric vector, matrix, or
#'   data.frame. (See the methods (by class) section for details.)
#' @param by A factor or character matrix to be used in [base::by()] to
#'   split the data.frame or matrix of set combinations.
#' @param input The type of input: disjoint class combinations
#'   (`disjoint`) or unions (`union`).
#' @param shape The geometric shape used in the diagram: circles or ellipses.
#' @param n_threads The number of threads to use if OpenMP is supported (and
#'   the package was compiled with it). Can be either set to an integer
#'   or, if set to '"auto"', will be automatically detected using
#'   [future::availableCores()].
#' @param extraopt Should the more thorough optimizer (currently
#'   [GenSA::GenSA()]) kick in (provided `extraopt_threshold` is exceeded)?
#' @param extraopt_threshold The threshold, in terms of `diagError`, for when
#'   the extra optimizer kicks. This will
#'   almost always slow down the process considerably. A value of 0 means
#'   that the extra optimizer will kick in if there is *any* error. A value of
#'   1 means that it will never kick in.
#' @param extraopt_control A list of control parameters to pass to the
#'   extra optimizer, such as `max.call`. See [GenSA::GenSA()].
#' @param ... Arguments passed down to other methods.
#'
#' @return A list object of class 'euler' with the following parameters.
#'   \item{coefficients}{A matrix of x and y coordinates for the centers of the
#'     circles and their radii.}
#'   \item{original.values}{Set relationships provided by the user.}
#'   \item{fitted.values}{Set relationships in the solution.}
#'   \item{residuals}{Residuals.}
#'   \item{diagError}{The largest absolute residual in percentage points
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
#'   combinations separated by an ampersand, for instance `A&B = 10`.
#'   Missing combinations are treated as being 0.
#'
#' @export
euler.default <- function(
  combinations,
  input = c("disjoint", "union"),
  shape = c("circle", "ellipse"),
  n_threads = 1,
  extraopt = n_sets(combinations) == 3 && match.arg(shape) == "ellipse",
  extraopt_threshold = 0.001,
  extraopt_control = list(),
  ...
) {
  stopifnot(
    is.numeric(combinations),
    !any(combinations < 0),
    !is.null(attr(combinations, "names")),
    !any(names(combinations) == ""),
    !any(duplicated(names(combinations))),
    is.character(n_threads) || (is.numeric(n_threads) && n_threads >= 0)
  )

  combo_names <- strsplit(names(combinations), split = "&", fixed = TRUE)
  setnames <- unique(unlist(combo_names, use.names = FALSE))

  n <- length(setnames)
  id <- bit_indexr(n)
  N <- NROW(id)
  restarts <- 10L # should this be made an argument?

  if (n_threads == "auto")
    n_threads <- future::availableCores()

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
        stop("Check your set configuration. Your specification resulted in some disjoint areas being set to 0.")
    }

    id_sums <- rowSums(id)
    ones <- id_sums == 1L
    twos <- id_sums == 2L
    two <- choose_two(1L:n)
    r <- sqrt(areas[ones]/pi)

    # Establish identities of disjoint and contained sets
    disjoint <- areas[twos] == 0
    tmp <- matrix(areas[ones][two], ncol = 2L)
    contained <- areas[twos] == tmp[, 1L] | areas[twos] == tmp[, 2L]

    distances <- mapply(separate_two_discs,
                        r1 = r[two[, 1L]],
                        r2 = r[two[, 2L]],
                        overlap = areas[twos],
                        USE.NAMES = FALSE)

    # Starting layout
    loss <- Inf
    i <- 1L
    initial_layouts <- vector("list", restarts)
    bnd <- sqrt(sum(r^2*pi))

    while (loss > sqrt(.Machine$double.eps) && i <= restarts) {
      initial_layouts[[i]] <- stats::nlm(
        f = optim_init,
        p = stats::runif(n*2, 0, bnd),
        d = distances,
        disjoint = disjoint,
        contained = contained,
        iterlim = 200L,
        check.analyticals = FALSE
      )
      loss <- initial_layouts[[i]]$minimum
      i <- i + 1L
    }

    # Find the best initial layout
    best_init <- which.min(lapply(initial_layouts[1:(i - 1)], "[[", "minimum"))
    initial_layout <- initial_layouts[[best_init]]

    # Final layout
    circle <- match.arg(shape) == "circle"

    if (circle) {
      pars <- as.vector(matrix(c(initial_layout$estimate, r), 3L, byrow = TRUE))
    } else {
      pars <- as.vector(rbind(matrix(initial_layout$estimate, 2L, byrow = TRUE),
                              r, r, 0, deparse.level = 0L))
    }

    orig <- areas_disjoint

    # Try to find a solution using nlm() first (faster)
    # TODO: Allow user options here?
    nlm_solution <- stats::nlm(
      f = optim_final_loss,
      p = pars,
      areas = areas_disjoint,
      circles = circle,
      n_threads = n_threads,
      iterlim = 1e5
    )$estimate

    nlm_fit <- as.vector(intersect_ellipses(nlm_solution, circle, n_threads))
    nlm_diagError <- diagError(nlm_fit, orig)

    # If inadequate solution, try with GenSA (slower, better)
    if (extraopt && nlm_diagError > extraopt_threshold) {
      # Set bounds for the parameters
      newpars <- matrix(
        data = nlm_solution,
        ncol = 5L,
        dimnames = list(setnames, c("h", "k", "a", "b", "phi")),
        byrow = TRUE
      )

      newpars <- compress_layout(newpars, id, nlm_fit)
      h   <- newpars[, 1L]
      k   <- newpars[, 2L]
      a   <- newpars[, 3L]
      b   <- newpars[, 4L]
      phi <- newpars[, 5L]

      xlim <- sqrt(a^2*cos(phi)^2 + b^2*sin(phi)^2)
      ylim <- sqrt(a^2*sin(phi)^2 + b^2*cos(phi)^2)

      if (any(!is.finite(xlim), !is.finite(ylim))) {
        pmab <- pmax.int(a, b)
        xbnd <- range(pmax.int(h + pmab), pmin.int(h - pmab))
        ybnd <- range(pmax.int(k + pmab), pmin.int(k - pmab))
      } else {
        xbnd <- range(xlim + h, -xlim + h)
        ybnd <- range(ylim + k, -ylim + k)
      }

      lwr <- double(5L*n)
      upr <- double(5L*n)
      for (i in seq_along(r)) {
        ii <- 5L*(i - 1L)

        lwr[ii + 1L] <- xbnd[1L]
        lwr[ii + 2L] <- ybnd[1L]
        lwr[ii + 3L:4L] <- sqrt(r[i])/4
        lwr[ii + 5L] <- 0

        upr[ii + 1L] <- xbnd[2L]
        upr[ii + 2L] <- ybnd[2L]
        upr[ii + 3L:4L] <- sqrt(r[i])*4
        upr[ii + 5L] <- pi
      }

      GenSA_solution <- GenSA::GenSA(
        par = as.vector(newpars),
        fn = optim_final_loss,
        lower = lwr,
        upper = upr,
        circles = circle,
        areas = areas_disjoint,
        n_threads = n_threads,
        control = utils::modifyList(
          list(threshold.stop = 0,
               smooth = TRUE,
               max.call = 4e3*3L^n),
          extraopt_control
        )
      )$par

      GenSA_fit <- as.vector(intersect_ellipses(GenSA_solution,
                                                circle,
                                                n_threads))
      GenSA_diagError <- diagError(GenSA_fit, orig)

      # Check for the best solution
      if (GenSA_diagError < nlm_diagError) {
        final_par <- GenSA_solution
        fit <- GenSA_fit
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

    fpar <- matrix(
      data = final_par,
      ncol = if (circle) 3L else 5L,
      dimnames = list(
        setnames,
        if (circle) c("h", "k", "r") else c("h", "k", "a", "b", "phi")
      ),
      byrow = TRUE
    )

    # Find disjoint clusters and compress the layout
    fpar <- compress_layout(fpar, id, fit)

    # Center the solution on the coordinate plane
    fpar <- center_layout(fpar)
  } else {
    circle <- match.arg(shape) == "circle"
    # One set
    fpar <- matrix(
      data = if (circle)
        c(0, 0, sqrt(areas/pi))
      else
        c(0, 0, sqrt(areas/pi), sqrt(areas/pi), 0),
      ncol = if (circle) 3L else 5L,
      dimnames = list(
        setnames,
        if (circle) c("h", "k", "r") else c("h", "k", "a", "b", "phi")
      ),
      byrow = TRUE
    )
    regionError <- diagError <- stress <- 0
    orig <- fit <- areas
    names(orig) <- names(fit) <- setnames
  }

  # Return eulerr structure
  structure(list(coefficients = fpar,
                 original.values = orig,
                 fitted.values = fit,
                 residuals = orig - fit,
                 regionError = regionError,
                 diagError = diagError,
                 stress = stress),
            class = c("euler", "list"))
}

#' @describeIn euler A data.frame of logicals, two-level factors (see examples).
#' @param weights A numeric vector of weights of the same length as `by` and
#'   the number of rows in `combinations`.
#' @export
euler.data.frame <- function(combinations, weights = NULL, by = NULL, ...) {
  stopifnot(!any(grepl("&", colnames(combinations), fixed = TRUE)))

  if (is.null(weights))
    weights <- rep.int(1L, nrow(combinations))

  if (!is.null(by)) {
    stopifnot(all(vapply(by,
                         function(x) (is.factor(x) || is.character(x)),
                         FUN.VALUE = logical(1))))
    if (NCOL(by) > 2L)
      stop("No more than two conditioning variables are allowed.")
  }

  out <- matrix(NA, nrow = nrow(combinations), ncol = ncol(combinations))
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
#' # The table method
#' plot(euler(as.table(apply(Titanic, 2:4, sum))))
euler.table <- function(combinations, ...) {
  if (max(dim(combinations)) > 2L)
    stop("No table dimension may exceed 2.")
  x <- as.data.frame(combinations)
  euler(x[, !(names(x) == "Freq")], weights = x$Freq, ...)
}

#' @describeIn euler A list of vectors, each vector giving the contents of
#'   that set (with no duplicates). Vectors in the list do not need to be named.
#' @export
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
