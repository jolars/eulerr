
<!-- README.md is generated from README.Rmd. Please edit that file -->
eulerr
======

[![Travis-CI Build Status](https://travis-ci.org/jolars/eulerr.svg?branch=master)](https://travis-ci.org/jolars/eulerr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/jolars/eulerr?branch=master&svg=true)](https://ci.appveyor.com/project/jolars/eulerr) [![codecov](https://codecov.io/gh/jolars/eulerr/branch/master/graph/badge.svg)](https://codecov.io/gh/jolars/eulerr) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/eulerr)](https://cran.r-project.org/package=eulerr)

**eulerr** generates area-proportional euler diagrams that display set relationships (intersections, unions, and disjoints) with circles. [Euler diagrams](https://en.wikipedia.org/wiki/Euler_diagram) are Venn diagrams without the requirement that all set interactions be present (whether they are empty or not). That is, depending on input, eulerr will sometimes produce Venn diagrams but sometimes not.

With three or more sets intersecting, exact euler diagrams are often impossible. For such cases eulerr attempts to provide the best approximation possible by numerically tuning the circles' positions and radiuses so that the sum of squared errors is minimized.

When solutions are approximate, residuals and error statistics are provided to assess whether the resulting diagram can be trusted.

Installation
------------

The CRAN version can be installed by running

``` r
install.packages("eulerr")
```

The development version can be installed by running

``` r
devtools::install_github("jolars/eulerr")
```

Usage
-----

``` r
library(eulerr)
sets <- c("A" = 10, "B" = 9, "C" = 4,
          "A&B" = 2, "A&C" = 3, "B&C" = 3,
          "A&B&C" = 2)
set.seed(1)
fit <- eulerr(sets)
```

We can inspect the solution

``` r
fit
#>       original fitted residuals region_error
#> A           10  9.999     0.001        0.000
#> B            9  8.999     0.001        0.000
#> C            4  4.013    -0.013        0.000
#> A&B          2  2.026    -0.026        0.001
#> A&C          3  2.994     0.006        0.000
#> B&C          3  2.994     0.006        0.000
#> A&B&C        2  1.975     0.025        0.001
#> 
#>  
#> diagError:  0.001 
#> stress:     0
```

and plot it using `plot`.

``` r
plot(fit)
```

Please see the introductory [vignette](https://cran.r-project.org/web/packages/eulerr/vignettes/introduction.html) for usage details.

License
-------

eulerr is open source software, licensed under [GPL-3](LICENSE).

Code of conduct
---------------

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

Thanks
------

eulerr would not be possible without Ben Fredrickson's work on [venn.js](http://www.benfrederickson.com) or Leland Wilkinson's [venneuler](https://cran.r-project.org/package=venneuler).
