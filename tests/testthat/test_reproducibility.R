context("Test if diagrams can be reproduced")

test_that("expect no errors for a variety of predefined sets", {
  # Uniform intersections
  s1 <- c("A" = 10, "B" = 10, "C" = 10,
          "A&B" = 4, "A&C" = 4, "B&C" = 4,
          "A&B&C" = 1)
  # Completely disjoint
  s2 <- c(A = 1, B = 1, C = 1)
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
  s9 <-  c(A = 4, B = 6, C = 3, D = 2, E = 7, F = 4,
           "A&B" = 2, "A&F" = 2, "B&C" = 2, "B&D" = 1,
           "B&F" = 2, "C&D" = 1, "D&E" = 1, "E&F" = 1,
           "A&B&F" = 1, "B&C&D" = 1)
  # From venneuler article
  s10 <- c("SE" = 30, "Treat" = 35, "Anti-CCP" = 107, "DAS28" = 106,
           "SE&Treat" = 4, "SE&DAS28" = 14, "Treat&Anti-CCP" = 6,
           "SE&Anti-CCP" = 3, "Anti-CCP&DAS28" = 2,
           "SE&Anti-CCP&DAS28" = 1)
  # Three sets intersecting inside a fourth
  s11 <- c("A" = 30, "B" = 7, "C" = 7, "D" = 7,
           "A&B" = 7, "A&C" = 7, "A&D" = 7, "B&C" = 2, "B&D" = 2, "C&D" = 2,
           "A&B&C" = 2, "A&B&D" = 2, "A&C&D" = 2, "B&C&D" = 1,
           "A&B&C&D" = 1)
  # From eulerAPE article
  s12 <- c("a" = 3491, "b" = 3409, "c" = 3503,
           "a&b" = 120, "a&c" = 114, "b&c" = 132,
           "a&b&c" = 50)
  # Sets with 0-intersections.
  s13 <- c("A" = 1, "B" = 0.4, "C" = 3,
           "A&B" = 0.2, "A&C" = 0, "B&C" = 0,
           "A&B&C" = 0)
  # Four uniform interactions
  s13 <- c("A" = 20, "B" = 20, "C" = 20, "D" = 20,
           "A&B" = 6, "A&C" = 6, "A&D" = 3, "B&C" = 3, "B&D" = 6, "C&D" = 6,
           "A&B&C" = 2, "A&B&D" = 2, "A&C&D" = 2, "B&C&D" = 2,
           "A&B&C&D" = 1)

  expect_error(eulerr(s1), NA)
  expect_error(eulerr(s2), NA)
  expect_error(eulerr(s3), NA)
  expect_error(eulerr(s4), NA)
  expect_error(eulerr(s5), NA)
  expect_error(eulerr(s6), NA)
  expect_error(eulerr(s7), NA)
  expect_error(eulerr(s8), NA)
  expect_error(eulerr(s9), NA)
  expect_error(eulerr(s10), NA)
  expect_error(eulerr(s11), NA)
  expect_error(eulerr(s12), NA)
  expect_error(eulerr(s13), NA)
})

test_that("degenerative cases are fit properly", {
  # Test two circles completely on top of one another
  sA <- c("A" = 10, "B" = 10, "A&B" = 10)

  expect_error(eulerr(sA), NA)
  expect_true(all(resid(eulerr(sA)) < 10e-3))
})
