
<!-- README.md is generated from README.Rmd. Please edit that file -->
eulerr
======

[![Travis-CI Build Status](https://travis-ci.org/jolars/eulerr.svg?branch=master)](https://travis-ci.org/jolars/eulerr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/jolars/eulerr?branch=master&svg=true)](https://ci.appveyor.com/project/jolars/eulerr) [![codecov](https://codecov.io/gh/jolars/eulerr/branch/master/graph/badge.svg)](https://codecov.io/gh/jolars/eulerr)

**eulerr** generates area-proportional euler diagrams that display set relationships (intersections, unions, and disjoints) with circles. [Euler diagrams](https://en.wikipedia.org/wiki/Euler_diagram) are Venn diagrams without the requirement that all set interactions be present (whether they are empty or not). That is, depending on input, eulerr will sometimes produce Venn diagrams but sometimes not.

With three or more sets intersecting, exact euler diagrams are often impossible. For such cases eulerr attempts to provide the best approximation possible by numerically tuning the circles positions and radiuses so that the sum of absolute errors is minimized.

When solutions are approximate, eulerr helpfully provides statistics that allow the user decide if the approximation can be trusted.

Installation
------------

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
fit <- eulerr(sets)
#> Warning in eulerr.default(sets): ... arguments are currently ignored.
```

We look at the solution,

``` r
fit
#> $coefficients
#>            x        y         r
#> A  0.9348876 6.648058 10.248898
#> B 13.1813117 1.137312  9.722915
#> C  7.3187757 3.775446  6.490142
#> 
#> $original.values
#>     A     B     C   A&B   A&C   B&C A&B&C 
#>    10     9     4     2     3     3     2 
#> 
#> $fitted.values
#>        A        B        C      A&B      A&C      B&C    A&B&C 
#> 9.999775 8.999718 4.010000 2.027310 2.992819 2.992914 1.975733 
#> 
#> $residuals
#>             A             B             C           A&B           A&C 
#>  0.0002245617  0.0002822668 -0.0099999700 -0.0273101157  0.0071813602 
#>           B&C         A&B&C 
#>  0.0070858477  0.0242671779 
#> 
#> $stress
#> [1] 6.890803e-06
#> 
#> attr(,"class")
#> [1] "eulerr" "list"
```

and plot it using `plot`.

``` r
plot(fit)
```

License
-------

eulerr is open source software, licensed under GPL-3.

Thanks
------

eulerr would not be possible without Ben Fredrickson's work on [venn.js](http://www.benfrederickson.com) or Leland Wilkinson's [venneuler](https://cran.r-project.org/package=venneuler).
