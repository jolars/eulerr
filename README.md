
<!-- README.md is generated from README.Rmd. Please edit that file -->

# eulerr <a href="https://jolars.github.io/eulerr/"><img src='man/figures/logo.svg' align="right" height="139" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/jolars/eulerr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jolars/eulerr/actions/workflows/R-CMD-check.yaml)
[![Coverage
Status](https://codecov.io/gh/jolars/eulerr/branch/master/graph/badge.svg)](https://app.codecov.io/gh/jolars/eulerr)
[![CRAN
Badge](http://www.r-pkg.org/badges/version/eulerr)](https://cran.r-project.org/package=eulerr)
<!-- badges: end -->

**eulerr** generates area-proportional Euler diagrams that display set
relationships (intersections, unions, and disjoints) with circles or
ellipses. [Euler diagrams](https://en.wikipedia.org/wiki/Euler_diagram)
are Venn diagrams without the requirement that all set interactions be
present (whether they are empty or not), which means that, depending on
input, **eulerr** sometimes produces Venn diagrams and sometimes not.

With three or more sets intersecting, exact Euler diagrams are often
impossible. For such cases **eulerr** attempts to provide a good
approximation by numerically tuning the parameters of the ellipses or
circles to minimize the error in the resulting diagram. Residuals and
goodness of fit statistics are provided to assess whether the resulting
diagram can be trusted.

## Installation

Building **eulerr** from source requires a Rust toolchain. The minimum
supported `rustc` version is listed in the `SystemRequirements` field of
`DESCRIPTION` (currently `rustc >= 1.88.0`). If your system Rust is
older than that, install or update via [rustup](https://rustup.rs)
before installing the package from source. CRAN binary builds do not
need a local Rust toolchain.

### CRAN version

``` r
install.packages("eulerr")
```

### Development Version

``` r
devtools::install_github("jolars/eulerr")
```

### Web App

eulerr is also available as a web app at <https://eunoia.bz/app>.

## Usage

``` r
library(eulerr)

set.seed(26)

# From Wilkinson 2012
fit <- euler(
  c(
    "A" = 4,
    "B" = 6,
    "C" = 3,
    "D" = 2,
    "E" = 7,
    "F" = 3,
    "A&B" = 2,
    "A&F" = 2,
    "B&C" = 2,
    "B&D" = 1,
    "B&F" = 2,
    "C&D" = 1,
    "D&E" = 1,
    "E&F" = 1,
    "A&B&F" = 1,
    "B&C&D" = 1
  ),
  shape = "ellipse"
)
```

We can inspect the goodness-of-fit metrics *diagError* and *stress* for
the solution,

``` r
fit$stress
#> [1] 1.280328e-19
fit$diagError
#> [1] 6.773921e-11
```

and plot it

``` r
plot(fit)
```

![](man/figures/README-plot_method-1.png)<!-- -->

Please see [the introductory
vignette](https://CRAN.R-project.org/package=eulerr/vignettes/introduction.html)
for a brief introduction or [*eulerr under the
hood*](https://CRAN.R-project.org/package=eulerr/vignettes/under-the-hood.html)
for details.

## Eunoia

eulerr is based on [Eunoia](https://eunoia.bz), which is a Rust library
for fitting and visualizing Euler and Venn diagrams. The pure-Rust core
powers bindings in several languages, all backed by the same fitting
engine:

| Language | Package | Install |
|----|----|----|
| **R** | [`eulerr`](https://CRAN.R-project.org/package=eulerr) | `install.packages("eulerr")` |
| **Rust** | [`eunoia`](https://crates.io/crates/eunoia) ([repo](https://github.com/jolars/eunoia)) | `cargo add eunoia` |
| **Python** | [`eunoia`](https://pypi.org/project/eunoia/) ([repo](https://github.com/jolars/eunoia-py)) | `pip install eunoia` |
| **JavaScript** | [`@jolars/eunoia`](https://www.npmjs.com/package/@jolars/eunoia) ([repo](https://github.com/jolars/eunoia)) | `npm install @jolars/eunoia` |

## License

eulerr is open source software, licensed under the
[MIT](https://github.com/jolars/eulerr/blob/master/LICENSE.md) license.

## Versioning

eulerr uses [semantic versioning](https://semver.org).

## Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/jolars/eulerr/blob/master/CONDUCT.md). By
participating in this project you agree to abide by its terms.

## Acknowledgements

eulerr would not be possible without Ben Frederickson’s work on
[venn.js](http://www.benfrederickson.com) or Leland Wilkinson’s
[venneuler](https://cran.r-project.org/package=venneuler).
