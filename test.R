power_set <- function(x) {
  sets <- lapply(seq_along(x), function(i) combn(x, i, simplify = FALSE))
  unlist(sets, recursive = FALSE)
}

in_list <- function(a, l) {
  for (i in seq_along(l)) {
    if (setequal(a, l[[i]])) {
      return(TRUE)
    }
  }
  FALSE
}

where_in_list <- function(a, l) {
  for (i in seq_along(l)) {
    if (setequal(a, l[[i]])) {
      return(i)
    }
  }
  0
}

combinations <- c("A" = 4, "B" = 1, "B&A" = 0.3, "C&B" = 0.1)
# combinations <- areas

n_restarts <- 10L # should this be made an argument?
small <- sqrt(.Machine$double.eps)

input <- "disjoint"

combinations <- combinations[combinations != 0] # drop all zeros

combos <- strsplit(names(combinations), split = "&", fixed = TRUE)
combos <- lapply(combos, sort)
combos <- combos[order(lengths(combos))]
sets <- unique(unlist(combos, use.names = FALSE))

combos_disjoint <- combos

# extend
if (input == "disjoint") {
  for (i in seq_along(combos)) {
    perms <- power_set(combos[[i]])
    for (j in seq_along(perms)) {
      if (!in_list(perms[[j]], combos_disjoint)) {
        combos <- c(combos, perms[[j]])
      }
    }
  }
}

if (input == "union") {
  areas <- combinations
  areas_disjoint <- double(length(combos_disjoint))
  for (i in seq_along(combos)) {
    for (k in seq_along(combos)) {
      if (i != k) {
        if (length(setdiff(combos[[i]], combos[[k]])) == 0) {
          areas_disjoint[i] <- areas_disjoint[i] - areas[k]
        }
      }
    }
  }
} else {
  areas_disjoint <- combinations
  areas <- double(length(combos))
  for (i in seq_along(combos)) {
    for (k in seq_along(combos_disjoint)) {
      if (length(setdiff(combos[[i]], combos_disjoint[[k]])) == 0) {
        areas[i] <- areas[i] + areas_disjoint[k]
      }
    }
  }
}

# setup preliminary return pars

n <- length(sets)
# id <- bit_indexr(n)

# # print(id)

# # N <- NROW(id)
N <- 2^n - 1

ones <- lengths(combos) == 1
twos <- lengths(combos) == 2

n_two <- choose(n, 2)

one_areas <- areas[ones]
names(one_areas) <- sets
two_areas <- double(n_two)
two_areas_nonempty <- areas[twos]
two_areas[seq_along(two_areas_nonempty)] <- two_areas_nonempty
two_combos <- combos[twos]

if (any(areas_disjoint == 0)) {
  stop("Check your set configuration. Some intersections are zero.")
}
if (any(areas_disjoint < 0)) {
  stop("Check your set configuration. Some intersections are negative.")
}

orig <- areas_disjoint

fpar <- as.data.frame(matrix(
  NA,
  ncol = 5,
  nrow = n,
  dimnames = list(setnames, c("h", "k", "a", "b", "phi"))
), stringsAsFactors = TRUE)


two <- eulerr:::choose_two(1:n)
r <- sqrt(areas[ones] / pi)

# Establish identities of disjoint and subset sets
subset <- disjoint <- matrix(FALSE, ncol = n, nrow = n)
distances <- area_mat <- matrix(0, ncol = n, nrow = n)

subset <- logical(n_two)
disjoint <- logical(n_two)
distances <- double(n_two)

for (i in 1:(n - 1)) {
  l <- n - i
  for (j in (i + 1):n) {
    it <- (i - 1) * l + j - 1
    # print(it)
    ind <- where_in_list(c(sets[i], sets[j]), combos)

    curr_area <- if (ind == 0) 0 else areas[ind]
    distances[it] <- eulerr:::separate_two_discs(r[i], r[j], curr_area)
  }
}

obj <- Inf
initial_layouts <- vector("list", n_restarts)
bnd <- sqrt(sum(r^2 * pi))

i <- 1L
while (obj > small && i <= n_restarts) {
  initial_layouts[[i]] <- stats::nlm(
    f = eulerr:::optim_init,
    p = stats::runif(n * 2, 0, bnd),
    d = distances,
    disjoint = disjoint,
    subset = subset,
    iterlim = 1000L,
    check.analyticals = FALSE
  )
  obj <- initial_layouts[[i]]$minimum
  i <- i + 1L
}
