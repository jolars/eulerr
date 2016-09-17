context("Test if diagrams can be reproduced")

test_that("eulerr can reproduce its own solution given the areas", {
  fit1 <- eulerr(c("A" = 10, "B" = 10, "C" = 10, "A&B" = 8, "A&C" = 8,
                  "B&C" = 8, "A&B&C" = 3))
  fit2 <- eulerr(fit1$fitted_areas)
  expect_equal(fit1$fitted_areas, fit2$fitted_areas, tolerance = 1e-02)
})

test_that("expect no errors for a variety of predefined sets", {
  # Uniform intersections
  s1 <- c("A" = 10, "B" = 10, "C" = 10, "A&B" = 8, "A&C" = 8, "B&C" = 8,
          "A&B&C" = 3)
  # Completely disjoint
  s2 <- c(A = 10, B = 10, C = 8)
  # One completely contained
  s3 <- c("A" = 10, "B" = 10, "C" = 3, "A&B" = 6, "A&C" = 3, "B&C" = 3,
          "A&B&C" = 3)
  # Two sets interacting inside a third
  s4 <- c("A" = 25, "B" = 5, "C" = 5, "A&B" = 5, "A&C" = 5, "B&C" = 3,
          "A&B&C" = 3)
  # One set contained, all other interacting
  s5 <- c("A" = 15, "B" = 15, "C" = 5, "A&B" = 10, "A&C" = 5, "B&C" = 3,
            "A&B&C" = 3)
  # Russian doll
  s6 <- c("A" = 15, "B" = 10, C = 5, "A&B" = 10, "A&C" = 5, "B&C" = 5,
          "A&B&C" = 5)
  # Unequal overlaps
  s7 <- c("A" = 10, B = 9, C = 4, "A&B" = 2, "A&C" = 3, "B&C" = 3, "A&B&C" = 2)
  # Only two sets
  s8 <- c(A = 10, B = 9)

  # Difficult set (From Wilkinsons article)
  s9 <-  c(A = 4, B = 6, C = 3, D = 2, E = 7, F = 3,
           "A&B" = 2, "A&F" = 2, "B&C" = 2, "B&D" = 1,
           "B&F" = 2, "C&D" = 1, "D&E" = 1, "E&F" = 1,
           "A&B&F" = 1, "B&C&D" = 1)

  expect_error(eulerr(s1), NA)
  expect_error(eulerr(s2), NA)
  expect_error(eulerr(s3), NA)
  expect_error(eulerr(s4), NA)
  expect_error(eulerr(s5), NA)
  expect_error(eulerr(s6), NA)
  expect_error(eulerr(s7), NA)
  expect_error(eulerr(s8), NA)
  expect_error(eulerr(s9), NA)
})

test_that("degenerative cases are fit properly", {
  # Test two circles completely on top of one another
  s10 <- c("A" = 10, "B" = 10, "A&B" = 10)
  expect_error(eulerr(s10), NA)
  fit <- eulerr(s10)
  expect_true(all(fit$residuals < 10e-3))
})
