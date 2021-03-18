context("Reproducibility of diagrams")

test_that("a variety of sets can be reproduced", {
  set.seed(1)

  s <- list()

  # Uniform intersections
  s[[1]] <- c("A" = 10, "B" = 10, "C" = 10,
              "A&B" = 4, "A&C" = 4, "B&C" = 4,
              "A&B&C" = 2)

  # Three completely disjoint sets
  s[[2]] <- c(A = 1, B = 1, C = 1)

  # One completely contained
  s[[3]] <- c("A" = 10, "B" = 10, "C" = 0, "A&B" = 4, "A&C" = 0, "B&C" = 0,
              "A&B&C" = 3)

  # Two sets interacting inside a third
  s[[4]] <- c("A" = 15, "B" = 0, "C" = 0, "A&B" = 3, "A&C" = 3, "B&C" = 0,
              "A&B&C" = 2)

  # One set contained, all other interacting
  s[[5]] <- c("A" = 15, "B" = 15, "C" = 0, "A&B" = 3, "A&C" = 0, "B&C" = 0,
              "A&B&C" = 3)

  # Russian doll
  s[[6]] <- c("A" = 15, "B" = 0, C = 0, "A&B" = 10, "A&C" = 0, "B&C" = 0,
              "A&B&C" = 5)

  # Unequal overlaps
  s[[7]] <- c("A" = 7, B = 6, C = 0, "A&B" = 0, "A&C" = 1, "B&C" = 1,
              "A&B&C" = 2)

  # Two disjoint sets
  s[[8]] <- c(A = 10, B = 9)

  # Difficult set (From Wilkinson's article)
  s[[9]] <-  c(A = 4, B = 6, C = 3, D = 2, E = 7, F = 3,
               "A&B" = 2, "A&F" = 2, "B&C" = 2, "B&D" = 1,
               "B&F" = 2, "C&D" = 1, "D&E" = 1, "E&F" = 1,
               "A&B&F" = 1, "B&C&D" = 1)

  # Gene sets (From Wilkinson's article)
  s[[10]] <- c("SE" = 13, "Treat" = 28, "Anti-CCP" = 101, "DAS28" = 91,
               "SE&Treat" = 1, "SE&DAS28" = 14, "Treat&Anti-CCP" = 6,
               "SE&Anti-CCP&DAS28" = 1)

  # Three sets intersecting inside a fourth
  s[[11]] <- c("A" = 30,
               "A&B" = 3, "A&C" = 3, "A&D" = 3,
               "A&B&C" = 2, "A&B&D" = 2, "A&C&D" = 2,
               "A&B&C&D" = 1)

  # From eulerAPE article
  s[[12]] <- c("a" = 3491, "b" = 3409, "c" = 3503,
               "a&b" = 120, "a&c" = 114, "b&c" = 132,
               "a&b&c" = 126)

  # One set disjoint, two intersecting
  s[[13]] <- c("A" = 1, "B" = 0.4, "C" = 3,
               "A&B" = 0.2, "A&C" = 0, "B&C" = 0,
               "A&B&C" = 0)

  # Four uniform interactions
  s[[14]] <- c("A" = 10, "B" = 10, "C" = 10, "D" = 10,
               "A&B" = 3, "A&C" = 3, "A&D" = 0, "B&C" = 0, "B&D" = 3, "C&D" = 3,
               "A&B&C" = 1, "A&B&D" = 1, "A&C&D" = 1, "B&C&D" = 1,
               "A&B&C&D" = 1)

  # Two circles intersecting completely
  s[[15]] <- c("A" = 0, "B" = 0, "A&B" = 10)

  # A single set
  s[[16]] <- c("A" = 1)

  # random diagram
  s[[17]] <- runif(15)
  names(s[[17]]) <- c("A", "B", "C", "E",
                      "A&B", "A&C", "A&E", "B&C", "B&E", "C&E",
                      "A&B&C", "A&B&E", "A&C&E", "B&C&E",
                      "A&B&C&E")

  for (shape in c("circle", "ellipse")) {
    for (i in seq_along(s)) {
      x <- euler(s[[i]],
                 shape = shape,
                 control = list(extraopt_control = list(max.call = 100)))
      expect_is(x, "euler")
      y <- expect_error(dont_print(x), NA)
    }
  }
})
