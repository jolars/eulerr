
<!-- README.md is generated from README.Rmd. Please edit that file -->
eulerr
======

[![Travis-CI Build Status](https://travis-ci.org/jolars/eulerr.svg?branch=master)](https://travis-ci.org/jolars/eulerr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/jolars/eulerr?branch=master&svg=true)](https://ci.appveyor.com/project/jolars/eulerr) [![codecov](https://codecov.io/gh/jolars/eulerr/branch/master/graph/badge.svg)](https://codecov.io/gh/jolars/eulerr)

**eulerr** generates area-proportional euler diagrams that display set relationships (intersections, unions, and disjoints) with circles. [Euler diagrams](https://en.wikipedia.org/wiki/Euler_diagram) are Venn diagrams without the requirement that all set interactions be present (whether they are empty or not). That is, depending on input, eulerr will sometimes produce Venn diagrams but sometimes not.

With three or more sets intersecting, exact euler diagrams are often impossible. For such cases eulerr attempts to provide the best approximation possible by numerically tuning the circles positions and radiuses so that the sum of absolute errors is minimized.

When solutions are approximate, eulerr helpfully provides residuals and a stress statistic that allow the user decide if the approximation can be trusted.

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
#> $coefficients
#>          x         y         r
#> A 0.636158 11.266144 10.248850
#> B 8.183286  0.158440  9.722880
#> C 4.564672  5.472092  6.490124
#> 
#> $original.values
#>     A     B     C   A&B   A&C   B&C A&B&C 
#>    10     9     4     2     3     3     2 
#> 
#> $fitted.values
#>        A        B        C      A&B      A&C      B&C    A&B&C 
#> 9.999683 8.999654 4.009978 2.027319 2.992833 2.992882 1.975737 
#> 
#> $residuals
#>             A             B             C           A&B           A&C 
#>  0.0003166278  0.0003460540 -0.0099779647 -0.0273190236  0.0071670734 
#>           B&C         A&B&C 
#>  0.0071179365  0.0242629746 
#> 
#> $stress
#> [1] 9.38027e-06
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

eulerr is open source software, licensed under [GPL-3](LICENSE).

Code of conduct
---------------

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

Thanks
------

eulerr would not be possible without Ben Fredrickson's work on [venn.js](http://www.benfrederickson.com) or Leland Wilkinson's [venneuler](https://cran.r-project.org/package=venneuler).
