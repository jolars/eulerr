library(eulerr)
context("Reproduce euler diagrams from r, x, y")

test_that("eulerr can reproduce random layout", {
  r <- runif(3, .1, .5)
  x <- runif(3)
  y <- runif(3)
  n_one <- c("A", "B", "C")
  n_two <- c("A&B", "A&C", "B&C")
  n_three <- c("A&B&C")
  a_all <- double(7)
  names(a_all) <- c(n_one, n_two, n_three)

  names(x) <- names(y) <- names(r) <- n_one

  return_intersections(par, areas, names, id) {

  }



})

sets <- c(SE = 13, Treat = 28, AntiCCP = 101, DAS28 = 91, "SE&Treat" = 1,
                 "SE&DAS28" = 14, "Treat&AntiCCP" = 6, "SE&AntiCCP&DAS28" = 1)

sets <- c("B&C" = 2, B = 6, A = 4, C = 3, D = 2, E = 7, F = 3,
                 "A&B" = 2, "F&A" = 2, "B&D" = 1, "D&F" = 0, "B&E" = 0,
                 "B&F" = 2, "C&D" = 1, "D&E" = 1, "E&F" = 1,
                 "A&B&F" = 1, "C&B&D" = 1)

# Uniform intersections
sets <- c("A" = 10, "B" = 10, "C" = 10, "A&B" = 8, "A&C" = 8, "B&C" = 8, "A&B&C" = 3)

# Completely disjoint
sets <- c("A" = 10, "B" = 10, "C" = 10)

# One completely contained
sets <- c("A" = 10, "B" = 10, "C" = 3, "A&B" = 6, "A&C" = 3, "B&C" = 3, "A&B&C" = 3)

# Two sets interacting inside a third

sets <- c("A" = 25, "B" = 5, "C" = 5, "A&B" = 5, "A&C" = 5, "B&C" = 3, "A&B&C" = 3)

# One set contained, all other interacting

sets <- c("A" = 15, "B" = 15, "C" = 5, "A&B" = 10, "A&C" = 5, "B&C" = 3, "A&B&C" = 3)

# Russian doll

sets <- c("A" = 15, "B" = 10, C = 5, "A&B" = 10, "A&C" = 5, "B&C" = 5, "A&B&C" = 5)

# Unequal overlaps

sets <- c("A" = 10, B = 9, C = 4, "A&B" = 2, "A&C" = 3, "B&C" = 3, "A&B&C" = 2)
