fit_diagram <- function(combinations,
                        type = c("euler", "venn"),
                        input = c("disjoint", "union"),
                        shape = c("circle", "ellipse"),
                        loss = c(
                            "sum_sq",
                            "sum_abs",
                            "max_sq",
                            "max_abs",
                            "diag_error"
                        ),
                        control = list(),
                        ...)
{
  input <- match.arg(input)
  shape <- match.arg(shape)
  type <- match.arg(type)
  loss <- match.arg(loss)

  n_restarts <- 10L # should this be made an argument?
  small <- sqrt(.Machine$double.eps)

  if (!is.numeric(combinations))
    stop("`combinations` must be numeric")

  if (any(combinations < 0))
    stop("values in `combinations` cannot be negative")

  if (is.null(attr(combinations, "names")) || any(names(combinations) == ""))
    stop("every element in `combinations` needs to be named")

  if (any(duplicated(names(combinations))))
    stop("names of elements in `combinations` cannot be duplicated")

  combo_names <- strsplit(names(combinations), split = "&", fixed = TRUE)
  setnames <- unique(unlist(combo_names, use.names = FALSE))

  # setup preliminary return pars

  n <- length(setnames)
  id <- bit_indexr(n)
  N <- NROW(id)

  areas <- double(N)
  for (i in 1L:N) {
    s <- setnames[id[i, ]]
    for (j in seq_along(combo_names)) {
      if (setequal(s, combo_names[[j]])) {
        areas[i] <- combinations[j]
      }
    }
  }

  # Decompose or collect set volumes depending on input
  if (input == "disjoint") {
    areas_disjoint <- areas
    areas[] <- 0
    for (i in rev(seq_along(areas))) {
      prev_areas <- rowSums(id[, id[i, ], drop = FALSE]) == sum(id[i, ])
      areas[i] <- sum(areas_disjoint[prev_areas])
    }
  } else if (input == "union") {
    areas_disjoint <- double(length(areas))
    for (i in rev(seq_along(areas))) {
      prev_areas <- rowSums(id[, id[i, ], drop = FALSE]) == sum(id[i, ])
      areas_disjoint[i] <- areas[i] - sum(areas_disjoint[prev_areas])
    }
    if (any(areas_disjoint < 0))
      stop("Check your set configuration. Some disjoint areas are negative.")
  }

  # setup return values
  orig <- rep.int(0, N)
  fit <- rep.int(0, N)
  names(orig) <- names(fit) <-
    apply(id, 1L, function(x) paste0(setnames[x], collapse = "&"))

  # return venn diagram early if requested
  if (type == "venn") {
    fpar <- venn_spec[[n]]
    rownames(fpar) <- setnames

    orig[] <- areas_disjoint

    out <- structure(list(ellipses = fpar,
                          original.values = orig,
                          fitted.values = rep(1, length(orig))),
                     class = c("venn", "euler", "list"))
    return(out)
  }

  # setup return object
  fpar <- as.data.frame(matrix(
    NA,
    ncol = 5L,
    nrow = n,
    dimnames = list(setnames, c("h", "k", "a", "b", "phi"))
  ), stringsAsFactors = TRUE)

  # find empty sets
  empty_sets <- areas[seq_len(n)] < sqrt(.Machine$double.eps)
  empty_subsets <- rowSums(id[, empty_sets, drop = FALSE]) > 0

  id <- id[!empty_subsets, !empty_sets, drop = FALSE]
  N <- NROW(id)
  n <- sum(!empty_sets)
  areas <- areas[!empty_subsets]
  areas_disjoint <- areas_disjoint[!empty_subsets]

  control <- utils::modifyList(
    list(extraopt = (n == 3) && (match.arg(shape) == "ellipse"),
         extraopt_threshold = 0.001,
         extraopt_control = list()),
    control)

  if (n > 1L) {
    if (all(areas == 0)) {
      # all sets are zero
      fpar[] <- 0
    } else {
      id_sums <- rowSums(id)
      ones <- id_sums == 1L
      twos <- id_sums == 2L
      two <- choose_two(1:n)
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
      obj <- Inf
      initial_layouts <- vector("list", n_restarts)
      bnd <- sqrt(sum(r^2*pi))

      i <- 1L
      while (obj > small && i <= n_restarts) {
        initial_layouts[[i]] <- stats::nlm(
          f = optim_init,
          p = stats::runif(n*2, 0, bnd),
          d = distances,
          disjoint = disjoint,
          subset = subset,
          iterlim = 1000L,
          check.analyticals = FALSE
        )
        obj <- initial_layouts[[i]]$minimum
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

      orig[!empty_subsets] <- areas_disjoint

      # Try to find a solution using nlm() first (faster)
      # TODO: Allow user options here?
      nlm_solution <- stats::nlm(f = optim_final_loss,
                                 p = pars,
                                 areas = areas_disjoint,
                                 circle = circle,
                                 loss = loss,
                                 iterlim = 1e6)$estimate

      tpar <- as.data.frame(matrix(
        data = nlm_solution,
        ncol = if (circle) 3L else 5L,
        dimnames = list(setnames[!empty_sets],
                        if (circle)
                          c("h", "k", "r")
                        else
                          c("h", "k", "a", "b", "phi")),
        byrow = TRUE
      ), stringsAsFactors = TRUE)
      if (circle)
        tpar <- cbind(tpar, tpar[, 3L], 0)

      # Normalize layout
      nlm_fit <- as.vector(intersect_ellipses(nlm_solution, circle))

      nlm_pars <- compress_layout(normalize_pars(tpar), id, nlm_fit)

      nlm_diagError <- diagError(nlm_fit, orig[!empty_subsets])

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

        last_ditch_effort <- GenSA::GenSA(
          par = as.vector(newpars),
          fn = optim_final_loss,
          lower = constraints$lwr,
          upper = constraints$upr,
          circle = circle,
          areas = areas_disjoint,
          loss = loss,
          control = utils::modifyList(
            list(threshold.stop = sqrt(.Machine$double.eps),
                 max.call = 1e7,
                 trace.mat = FALSE),
            control$extraopt_control
          )
        )$par

        last_ditch_fit <- as.vector(intersect_ellipses(last_ditch_effort,
                                                       circle))
        last_ditch_diagError <- diagError(last_ditch_fit, orig)

        # Check for the best solution
        if (last_ditch_diagError < nlm_diagError) {
          final_par <- last_ditch_effort
          fit[!empty_subsets] <- last_ditch_fit
        } else {
          final_par <- nlm_solution
          fit[!empty_subsets] <- nlm_fit
        }
      } else {
        final_par <- nlm_solution
        fit[!empty_subsets] <- nlm_fit
      }

      # names(orig) <- names(fit) <-
      #   apply(id, 1L, function(x) paste0(setnames[x], collapse = "&"))

      regionError <- regionError(fit, orig)
      diagError <- diagError(regionError = regionError)
      stress <- stress(orig, fit)

      temp <- matrix(data = final_par,
                     ncol = if (circle) 3L else 5L,
                     byrow = TRUE)

      if (circle)
        temp <- cbind(temp, temp[, 3L], 0)

      # Normalize semiaxes and rotation
      temp <- normalize_pars(temp)

      # Find disjoint clusters and compress the layout
      temp <- compress_layout(temp, id, fit[!empty_subsets])

      # Center the solution on the coordinate plane
      fpar[!empty_sets, ] <- center_layout(temp)
    }
  } else {
    # One set
    if (length(areas) == 0) {
      fpar[] <- 0
    } else {
      fpar[!empty_sets, ] <- c(0, 0, sqrt(areas/pi), sqrt(areas/pi), 0)
      orig[!empty_subsets] <- fit[!empty_subsets] <- areas
    }
    regionError <- diagError <- stress <- 0
  }

  # Return eulerr structure
  structure(list(ellipses = fpar,
                 original.values = orig,
                 fitted.values = fit,
                 residuals = orig - fit,
                 regionError = regionError,
                 diagError = diagError,
                 stress = stress),
            class = c("euler", "list"))
}
