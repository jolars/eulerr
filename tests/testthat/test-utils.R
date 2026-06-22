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

test_that("default_n_threads() respects the check environment and overrides", {
  envvars <- c(
    "_R_CHECK_LIMIT_CORES_",
    "OMP_THREAD_LIMIT",
    "EULERR_NUM_THREADS",
    "MC_CORES"
  )

  # Save and restore the inputs the heuristic reads.
  old_env <- Sys.getenv(envvars, unset = NA)
  old_opts <- options(eulerr.n_threads = NULL, mc.cores = NULL)
  on.exit(
    {
      for (nm in names(old_env)) {
        if (is.na(old_env[[nm]])) {
          Sys.unsetenv(nm)
        } else {
          do.call(Sys.setenv, stats::setNames(list(old_env[[nm]]), nm))
        }
      }
      options(old_opts)
    },
    add = TRUE
  )

  clear <- function() {
    Sys.unsetenv(envvars)
    options(eulerr.n_threads = NULL, mc.cores = NULL)
  }

  # Under R CMD check we must stay single-threaded.
  clear()
  Sys.setenv("_R_CHECK_LIMIT_CORES_" = "TRUE")
  expect_identical(default_n_threads(), 1L)

  clear()
  Sys.setenv("OMP_THREAD_LIMIT" = "2")
  expect_identical(default_n_threads(), 1L)

  # The option wins over everything, even under check.
  clear()
  Sys.setenv("_R_CHECK_LIMIT_CORES_" = "TRUE")
  options(eulerr.n_threads = 8L)
  expect_identical(default_n_threads(), 8L)

  # The environment variable is honored when no option is set.
  clear()
  Sys.setenv("EULERR_NUM_THREADS" = "3")
  expect_identical(default_n_threads(), 3L)

  # The `mc.cores` convention is honored as an exact request, bounded by the
  # available cores.
  clear()
  options(mc.cores = 1L)
  expect_identical(default_n_threads(), 1L)

  clear()
  Sys.setenv("MC_CORES" = "1")
  expect_identical(default_n_threads(), 1L)

  clear()
  options(mc.cores = 1000L)
  expect_identical(default_n_threads(), detect_available_cores())

  # Outside check, the default is a positive integer not exceeding the cores.
  clear()
  n <- default_n_threads()
  expect_true(is.integer(n) && length(n) == 1L && n >= 1L)
  n_cores <- parallel::detectCores(logical = TRUE)
  if (!is.na(n_cores)) {
    expect_lte(n, n_cores)
  }
})
