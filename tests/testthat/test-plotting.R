context("Plotting")

test_that("normal plotting works without errors", {
  f1 <- euler(c("A" = 10, "B" = 5, "A&B" = 2))

  expect_silent(dont_plot(f1,
                          fills = list(fill = c("black", "blue"), alpha = 0.3),
                          labels = list(font = 4)))

  expect_silent(dont_plot(f1))
  expect_silent(dont_plot(f1, legend = TRUE, quantities = TRUE))
  expect_silent(dont_plot(f1, legend = FALSE, quantities = TRUE))
  expect_silent(dont_plot(f1, legend = TRUE, labels = FALSE))
  expect_silent(dont_plot(f1, labels = c("asdf", "qwer")))

  grid <- expand.grid(labels = c(TRUE, FALSE),
                      quantities = c(TRUE, FALSE),
                      legend = c(TRUE, FALSE),
                      fills = c(TRUE, FALSE),
                      edges = c(TRUE, FALSE))

  for (i in seq_len(nrow(grid))) {
    expect_silent(dont_plot(f1,
                            labels = grid$labels[i],
                            quantities = grid$quantities[i],
                            legend = grid$legend[i],
                            fills = grid$fills[i],
                            edges = grid$edges[i]))
  }


  dat <- data.frame(
    Liberal = sample(c(TRUE, FALSE), size = 100, replace = TRUE),
    Conservative = sample(c(TRUE, TRUE, FALSE), size = 100, replace = TRUE),
    Gender = sample(c("Men", "Women"), size = 100, replace = TRUE),
    Nation = sample(c("Sweden", "Denmark"), size = 100, replace = TRUE)
  )

  f2 <- euler(dat[, 1:3], by = list(Gender))
  f3 <- euler(dat, by = list(Gender, Nation))

  expect_silent(dont_plot(f2,
                          fills = list(fill = "transparent",
                                       lty = c(1, 2),
                                       lwd = c(1, 2))))

  grid <- expand.grid(side = c(NA, "right", "left", "top", "bottom"),
                      main = c("Title", FALSE))

  for (i in seq_len(nrow(grid))) {
    if (is.na(grid$side[i])) {
      legend <- FALSE
    } else {
      legend <- list(side = grid$side[i])
    }
    main <- grid$main[i]

    expect_silent(dont_plot(f1, legend = legend, main = main))
    expect_silent(dont_plot(f2, legend = legend, main = main))
    expect_silent(dont_plot(f3, legend = legend, main = main))
  }

  expect_error(euler(dat, by = list(Gender, Nation, Gender)))
})

test_that("plotting zero-fits works", {
  s <- c(a = 0, b = 0)
  expect_is(plot(euler(s)), "gTree")
})

test_that("error_plot functions normally", {
  f <- euler(organisms)

  expect_silent(dont_plot(f, .f = error_plot))
  expect_silent(dont_plot(f,
                          pal = grDevices::colorRampPalette(c("red", "blue")),
                          .f = error_plot))
  expect_silent(dont_plot(f, quantities = FALSE, .f = error_plot))
  expect_silent(dont_plot(f, edges = FALSE, .f = error_plot))
})

