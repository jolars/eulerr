
<!-- README.md is generated from README.Rmd. Please edit that file -->
eulerr
======

[![Travis-CI Build Status](https://travis-ci.org/jolars/eulerr.svg?branch=master)](https://travis-ci.org/jolars/eulerr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/jolars/eulerr?branch=master&svg=true)](https://ci.appveyor.com/project/jolars/eulerr) [![codecov](https://codecov.io/gh/jolars/eulerr/branch/master/graph/badge.svg)](https://codecov.io/gh/jolars/eulerr)

eulerr generates area-proportional euler diagrams that display set relationships (intersections, unions, and disjoints) using circles. [Euler diagrams](https://en.wikipedia.org/wiki/Euler_diagram) are Venn diagrams without the requirement that all set interactions be present (whether they are empty or not). Thus, depending on input, eulerr will sometimes produce Venn diagrams but other times not.

With three or more sets interacting, exact euler diagrams are frequently impossible. In these cases, eulerr will provide the best approximation possible by numerically tuning the circles positions and radiuses so that the sum of absolute errors is minimized.

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
```

We look at the solution,

``` r
fit
#> $circles
#>           x          y         r
#> A 0.5804283 10.8909681 10.249013
#> B 9.0546894  0.5566362  9.723067
#> C 5.0193810  5.5200165  6.482045
#> 
#> $original_areas
#>     A     B     C   A&B   A&C   B&C A&B&C 
#>    10     9     4     2     3     3     2 
#> 
#> $fitted_areas
#>         A         B         C       A&B       A&C       B&C     A&B&C 
#> 10.000001  9.000000  4.000000  2.056433  2.999999  2.999999  1.999998 
#> 
#> $residuals
#>             A             B             C           A&B           A&C 
#> -9.572706e-07  4.920243e-07  1.504690e-07 -5.643329e-02  9.951197e-07 
#>           B&C         A&B&C 
#>  8.535960e-07  1.698247e-06 
#> 
#> $stress
#> [1] 0.001710256
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

eulerr would not be possible without Ben Fredrickson's work on [venn.js](http://www.benfrederickson.com) or Leland Wilkinson's [venneuler](https://cran.r-project.org/web/packages/venneuler/index.html).
