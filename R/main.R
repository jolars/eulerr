#' Euler diagrams
#'
#' @param sets
#' @return A list object of class 'vennr'.
#' @examples
#'
#' @export

eulerr <- function(sets) {
  # Collect names of all strings
  all_names <- lapply(strsplit(names(sets), split = "&", fixed = T), sort)
  names(sets) <- vapply(all_names, paste, collapse = "&", FUN.VALUE = character(1))

  # Reformat names and order them alphabetically
  all_areas <- vector("list", length = sum(lengths(all_names) == 1))
  for (i in lengths(all_names)) all_areas[[i]] <- sets[lengths(all_names) == i]

  oneset_names <- sort(names(all_areas[[1]]))

  for (i in seq_along(all_areas)) {
    cand_names <- utils::combn(oneset_names, i, paste, collapse = "&")
    cand_names <- cand_names[!cand_names %in% names(all_areas[[i]])]
    cand_values <- double(length(cand_names))
    names(cand_values) <- cand_names
    all_areas[[i]] <- c(all_areas[[i]], cand_values)
  }

  all_areas <- lapply(all_areas, function(x) x[order(names(x))])

  oneset_areas <- all_areas[[1]]
  twoset_areas <- all_areas[[2]]
  twoset_names <- names(twoset_areas)
  radiuses <- sqrt(oneset_areas / pi)
  all_areas <- unlist(all_areas)

  disjoint <- twoset_areas == 0

  twoset_distances <-
    vapply(twoset_names,
           function(x) {
             s <- unlist(strsplit(x, split = "&", fixed = T))
             overlap <- twoset_areas[x]
             separate_two_discs(r1 = radiuses[s[1]],
                                r2 = radiuses[s[2]],
                                overlap = overlap)
           }, FUN.VALUE = double(1))

  # Establish identities of disjoint and contained sets

  contained <- vapply(twoset_names,
                      function (x) {
                        s <- unlist(strsplit(x, split = "&", fixed = T))
                        ifelse(any(twoset_areas[x] == oneset_areas[s[1]],
                                   twoset_areas[x] == oneset_areas[s[2]]),
                               T, F)
                      }, FUN.VALUE = logical(1))

  # Compute an initial layout
  initial_layout <- stats::optim(
    par = stats::runif(length(oneset_areas) * 2, 0, min(radiuses)),
    fn = initial_layout_optimizer,
    gr = initial_layout_gradient,
    distances = twoset_distances,
    disjoint = disjoint,
    contained = contained,
    lower = rep(0, times = length(oneset_areas) * 2),
    upper = rep(sum(radiuses) * 2 - min(radiuses) *  2, times = length(oneset_areas) * 2),
    method = c("L-BFGS-B")
  )

  final_layout <- stats::optim(
    par = c(initial_layout$par, radiuses),
    fn = final_layout_optimizer,
    all_areas = all_areas,
    oneset_names = oneset_names,
    twoset_names = twoset_names,
    method = c("BFGS")
  )

  # final_layout <- nlminb(
  #   start = c(initial_layout$par, radiuses),
  #   objective = final_layout_optimizer,
  #   all_areas = all_areas,
  #   oneset_names = oneset_names,
  #   lower = rep(0, times = length(oneset_areas) * 3),
  #   upper = c(rep(sum(radiuses) * 2 - min(radiuses) *  2, times = length(oneset_areas) * 2), max(radiuses) * 2),
  #   twoset_names = twoset_names
  # )

  fpar <- matrix(final_layout$par, ncol = 3)
  x <- fpar[, 1]
  y <- fpar[, 2]
  r <- fpar[, 3]
  names(x) <- names(y) <- names(r) <- oneset_names

  structure(
    list(
      x = x,
      y = y,
      r = r,
      TSSE = final_layout$value
    ),
    class = c("eulerr", "list"))
}

# Utils -----------------------------------------------------------------

is_equal <- function(x, y, tol = .Machine$double.eps ^ 0.5) {
  abs(x - y) < tol
}