context("accuracy")

test_that("diagrams that can be perfectly fit are so", {
  set.seed(1)

  sets <- list()

  sets[[1]] <- c(A = 4, B = 6, C = 3, D = 2, E = 7, F = 3,
                 "A&B" = 2, "A&F" = 2, "B&C" = 2, "B&D" = 1,
                 "B&F" = 2, "C&D" = 1, "D&E" = 1, "E&F" = 1,
                 "A&B&F" = 1, "B&C&D" = 1)
  sets[[2]] <- c("A" = 30,
                 "A&B" = 3, "A&C" = 3, "A&D" = 3,
                 "A&B&C" = 2, "A&B&D" = 2, "A&C&D" = 2,
                 "A&B&C&D" = 1)

  for (set in sets) {
    x <- euler(set, shape = "ellipse")
    expect_lte(x$diagError, 1e-5)
  }
})
