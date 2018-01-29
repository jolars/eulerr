context("Utlity functions and other stuff")
#
test_that("getting options works", {
  expect_type(eulerr_options(), "list")
})

test_that("setting options works", {
  expect_error(eulerr_options(pointsize = 10), NA)
  expect_silent(eulerr_options(edges = list(col = "blue")))
  expect_silent(eulerr_options(list(fills = list(alpha = 0.5))))
  expect_null(eulerr_options(list("a"))[[1]])
  expect_silent(eulerr_options(eulerr_default_options()))
})

test_that("loading and unloading the package works", {
  expect_error(library(eulerr), NA)
  expect_silent(detach("package:eulerr", character.only = TRUE))
  expect_error(library(eulerr), NA)
})
