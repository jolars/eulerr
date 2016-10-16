
<!-- README.md is generated from README.Rmd. Please edit that file -->
eulerr
======

[![Travis-CI Build Status](https://travis-ci.org/jolars/eulerr.svg?branch=master)](https://travis-ci.org/jolars/eulerr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/jolars/eulerr?branch=master&svg=true)](https://ci.appveyor.com/project/jolars/eulerr) [![codecov](https://codecov.io/gh/jolars/eulerr/branch/master/graph/badge.svg)](https://codecov.io/gh/jolars/eulerr) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/eulerr)](https://cran.r-project.org/package=eulerr)

**eulerr** generates area-proportional euler diagrams that display set relationships (intersections, unions, and disjoints) with circles. [Euler diagrams](https://en.wikipedia.org/wiki/Euler_diagram) are Venn diagrams without the requirement that all set interactions be present (whether they are empty or not). That is, depending on input, eulerr will sometimes produce Venn diagrams but sometimes not.

With three or more sets intersecting, exact euler diagrams are often impossible. For such cases eulerr attempts to provide the best approximation possible by numerically tuning the circles positions and radiuses so that the sum of absolute errors is minimized.

When solutions are approximate, eulerr helpfully provides residuals and a stress statistic that allow the user decide if the approximation can be trusted.

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
fit <- eulerr(sets)
```

We look at the solution,

``` r
fit
#> $coefficients
#>            x         y         r
#> A 10.1294247 0.5053975 10.248901
#> B  0.1843481 9.5297602  9.722922
#> C  4.9451699 5.2096055  6.490149
#> 
#> $original.values
#>     A     B     C   A&B   A&C   B&C A&B&C 
#>    10     9     4     2     3     3     2 
#> 
#> $fitted.values
#>        A        B        C      A&B      A&C      B&C    A&B&C 
#> 9.999781 8.999730 4.010008 2.027308 2.992820 2.992919 1.975731 
#> 
#> $residuals
#>             A             B             C           A&B           A&C 
#>  0.0002186451  0.0002695094 -0.0100081709 -0.0273078240  0.0071795796 
#>           B&C         A&B&C 
#>  0.0070808247  0.0242685752 
#> 
#> $stress
#> [1] 9.37915e-06
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
