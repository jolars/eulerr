test_that("optimizer knob accepts all eunoia variants", {
  combo <- c(A = 3, B = 2, "A&B" = 1)
  optimizers <- c(
    "auto",
    "levenberg_marquardt",
    "lbfgs",
    "nelder_mead",
    "mads",
    "cma_es",
    "cma_es_lm",
    "trf",
    "cma_es_trf"
  )
  for (opt in optimizers) {
    fit <- euler(combo, control = list(optimizer = opt))
    expect_is(fit, "euler")
    expect_true(is.finite(fit$diagError), info = opt)
  }
})

test_that("invalid optimizer name errors", {
  expect_error(
    euler(c(A = 1, B = 1, "A&B" = 1), control = list(optimizer = "bogus"))
  )
})

test_that("n_restarts is honored and validated", {
  combo <- c(A = 3, B = 2, "A&B" = 1)
  expect_is(euler(combo, control = list(n_restarts = 1)), "euler")
  expect_is(euler(combo, control = list(n_restarts = 8)), "euler")
  expect_error(euler(combo, control = list(n_restarts = 0)))
  expect_error(euler(combo, control = list(n_restarts = 2.5)))
  expect_error(euler(combo, control = list(n_restarts = c(1, 2))))
})

test_that("new loss functions run", {
  combo <- c(A = 3, B = 2, "A&B" = 1)
  new_losses <- c(
    "log_sum_absolute",
    "smooth_sum_absolute",
    "smooth_max_absolute",
    "smooth_diag_error",
    "smooth_log_sum_absolute"
  )
  for (loss in new_losses) {
    fit <- euler(combo, loss = loss)
    expect_is(fit, "euler")
    expect_true(is.finite(fit$diagError), info = loss)
  }
})

test_that("loss_eps is validated", {
  combo <- c(A = 3, B = 2, "A&B" = 1)
  expect_is(
    euler(combo, loss = "smooth_sum_absolute", control = list(loss_eps = 0.05)),
    "euler"
  )
  expect_error(
    euler(combo, control = list(loss_eps = 0))
  )
  expect_error(
    euler(combo, control = list(loss_eps = -1))
  )
})
