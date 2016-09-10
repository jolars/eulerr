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

  r_combos <- utils::combn(r, 2)
  r1 <- r_combos[1, ]
  r2 <- r_combos[2, ]

  d <- as.vector(dist(cbind(x, y)))

  disjoint  <- d > r1 + r2 | d == abs(r1 + r2)
  contained <- d < abs(r1 - r2)
  intersecting <- !(disjoint | contained)

  a_two <- double(length(n_two))
  names(a_two) <- n_two
  a_two[contained]    <- min(r1, r2) ^ 2 * pi
  a_two[disjoint]     <- 0
  a_two[intersecting] <- eulerr:::intersect_two_discs(d = d[intersecting],
                                             r1 = r1[intersecting],
                                             r2 = r2[intersecting])
  a_one <- r ^2 * pi

  layout <- final_layout_test(par = c(x, y, r),
                    all_areas = a_all,
                    oneset_areas = a_one,
                    setnames = n_one,
                    twoset_areas = a_two,
                    twoset_names = n_two)
  fit1 <- structure(
    list(
      x = x,
      y = y,
      r = r
    ),
    class = c("eulerr", "list"))

  fit2 <- eulerr(layout)

})