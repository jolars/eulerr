# Contributing to eulerr

Thanks for your interest in contributing to **eulerr**! This document outlines
how to file issues, propose changes, and set up a local development environment.

By participating in this project you agree to abide by its [Code of
Conduct](CONDUCT.md).

## Filing issues

Before opening an issue, please search the [existing
issues](https://github.com/jolars/eulerr/issues) to see whether your problem or
suggestion has already been raised.

When reporting a bug, please include:

- A minimal [reprex](https://reprex.tidyverse.org) that reproduces the problem.
- The output of `sessionInfo()` (or `utils::sessionInfo()`).
- The version of eulerr you are using (`packageVersion("eulerr")`).

For feature requests, describe the use case and, where possible, sketch the API
you would like to see.

## Pull requests

1. Fork the repository and create a topic branch from `main`.
2. Make your changes, keeping commits focused and self-contained.
3. Add or update tests in `tests/testthat/` for any user-visible change.
4. Update documentation as needed (see [Documentation](#documentation) below).
5. Run the checks listed in [Checks before
   submitting](#checks-before-submitting).
6. Open a pull request against `main` and describe the motivation for the
   change.

Small fixes (typos, doc tweaks) are welcome without prior discussion. For larger
changes, please open an issue first so we can agree on the approach before you
invest significant time.

## Development setup

eulerr has an R orchestration layer and a Rust computational core (via
[extendr-api](https://extendr.github.io/extendr/)), so you will need both
toolchains:

- R (>= 4.2) with the development tools listed in `DESCRIPTION` `Suggests`
  (notably `devtools`, `testthat`, `roxygen2`, and `rextendr`).
- A Rust toolchain matching the MSRV declared in `DESCRIPTION`
  `SystemRequirements` (currently `rustc >= 1.81.0`).

Common workflows from R:

- Install the package locally: `devtools::install()`
- Run the test suite: `devtools::test()`
- Run `R CMD check`: `devtools::check()`
- Regenerate roxygen docs and `NAMESPACE`: `devtools::document()`

For development builds with a debug profile and permissive Cargo flags, set
`DEBUG=1` (or `NOT_CRAN=1`) in the environment before installing.

If you prefer a task runner, the repository also ships a `Taskfile.yml` with
shortcuts for these commands (e.g. `task install`, `task test`, `task check`,
`task document`). Using [Task](https://taskfile.dev) is entirely optional.

### Working on the Rust side

The Rust crate under `src/rust/` is a thin extendr shim (`src/rust/src/lib.rs`)
that converts R inputs into a [`eunoia`](https://github.com/jolars/eunoia)
`DiagramSpec`, runs the fitter, and returns geometry back to R. **The actual
numerical engine --- layout optimization, geometry, and goodness-of-fit
computation --- lives in the separate
[`eunoia`](https://github.com/jolars/eunoia) Rust crate**, not in this
repository.

This means:

- Changes to the core fitting algorithms, geometry, or optimization should be
  made in the `eunoia` repository, not here. Once released, bump the `eunoia`
  dependency in `src/rust/Cargo.toml` and re-vendor (see below).
- Changes that belong in this repository are typically limited to the R/Rust
  interface (the `#[extendr]` functions), input parsing, plotting, and the
  R-side API.

Workflow:

- Edit the `#[extendr]` functions in `src/rust/src/lib.rs` and regenerate the
  R-side wrappers with `rextendr::document()` (or `task extendr-document`). Do
  not hand-edit `R/extendr-wrappers.R` or the `extendr_module!` block.
- After bumping any dependency in `src/rust/Cargo.toml` (including `eunoia`),
  re-vendor the Rust dependencies (see `task vendor-pkgs` in `Taskfile.yml` for
  the exact steps).

## Documentation

- R documentation is managed with roxygen2. Edit the roxygen comments in `R/*.R`
  and run `devtools::document()` to regenerate `man/*.Rd` and `NAMESPACE`.
- `README.md` is generated from `README.Rmd`; edit the `.Rmd` and re-knit.
- Long-form documentation lives in `vignettes/`.

## Style

- R code follows `air.toml`: 80-character line width, 2-space indent. Format
  with `air format` (or the air LSP integration in your editor).
- Combination names use `&` as the delimiter (e.g., `"A&B&C"`) consistently
  across the R and Rust layers.
- Plotting is implemented with grid grobs (`eulergram`); preserve the
  `setup_geometry()` → `setup_grobs()` separation when changing plotting
  behavior.

## Checks before submitting

Before opening a pull request, please make sure:

- `devtools::test()` passes.
- `devtools::check()` passes with no new errors, warnings, or notes.
- `devtools::document()` has been run if you changed roxygen comments or extendr
  functions, and any regenerated files are committed.
- New user-facing changes have a corresponding entry in `NEWS.md`.

## License

By contributing, you agree that your contributions will be licensed under the
[GPL-3](LICENSE) license that covers the project.
