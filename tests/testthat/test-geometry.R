test_that("check that disc separation optimization works", {
  set.seed(1)

  r1 <- 5
  r2 <- 5
  tot <- r1^2*pi + r2^2*pi
  tol <- 1e-6

  expect_equal(eulerr:::separate_two_discs(r1, r2, 0), 10, tolerance = tol)
  expect_equal(eulerr:::separate_two_discs(r1, r2, tot), 0, tolerance = tol)
  expect_equal(eulerr:::separate_two_discs(r1, 0, 0), 0)
})

test_that("bounding box computations work as expected", {
  f <- eulerr:::get_bounding_box(0, 0, 1, 1, 0)
  expect_equivalent(unlist(f), c(-1, 1, -1, 1))
  expect_equal(eulerr:::get_bounding_box(0, 1, 1),
               eulerr:::get_bounding_box(0, 1, 1, 1, pi))

  expect_equal(eulerr:::get_bounding_box(0, 1, 1, 2, pi),
               eulerr:::get_bounding_box(0, 1, 1, 2, 2*pi))

  expect_equivalent(unlist(eulerr:::get_bounding_box(0, 0, 0)), rep(0, 4))

  expect_equal(eulerr:::get_bounding_box(0, 0, 1),
               eulerr:::get_bounding_box(c(0, 0), c(0, 0), c(1, 0.5)))
})

test_that("polygon clipping matches our expectations", {
  empty <- list()
  triangle <- list(x = c(-1, 0, 1),
                   y = c(0, 1, 0))
  square <- list(x = c(-1, -1, 0, 0),
                 y = c(0, 1, 1, 0))

  expect_equal(eulerr:::poly_clip(empty, square, "intersection"), list())
  expect_equal(eulerr:::poly_clip(empty, square, "union"), square)
  expect_equal(eulerr:::poly_clip(square, empty, "union"), square)
  expect_equal(eulerr:::poly_clip(square, square, "union"), square)
  expect_is(eulerr:::poly_clip(square, triangle, "intersection"), "list")
  expect_equal(eulerr:::poly_clip(empty, empty, "minus"), empty)
})
