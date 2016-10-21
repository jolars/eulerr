
<!-- README.md is generated from README.Rmd. Please edit that file -->
eulerr
======

[![Travis-CI Build Status](https://travis-ci.org/jolars/eulerr.svg?branch=master)](https://travis-ci.org/jolars/eulerr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/jolars/eulerr?branch=master&svg=true)](https://ci.appveyor.com/project/jolars/eulerr) [![codecov](https://codecov.io/gh/jolars/eulerr/branch/master/graph/badge.svg)](https://codecov.io/gh/jolars/eulerr) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/eulerr)](https://cran.r-project.org/package=eulerr)

**eulerr** generates area-proportional euler diagrams that display set relationships (intersections, unions, and disjoints) with circles. [Euler diagrams](https://en.wikipedia.org/wiki/Euler_diagram) are Venn diagrams without the requirement that all set interactions be present (whether they are empty or not). That is, depending on input, eulerr will sometimes produce Venn diagrams but sometimes not.

With three or more sets intersecting, exact euler diagrams are often impossible. For such cases eulerr attempts to provide the best approximation possible by numerically tuning the circles' positions and radiuses so that the sum of squared errors is minimized.

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
set.seed(1)
fit <- eulerr(sets)
```

We can inspect the solution

``` r
fit
#> $coefficients
#>          x          y         r
#> A 1.414019 13.5875821 10.248902
#> B 4.370636  0.4878819  9.722912
#> C 2.955592  6.7589832  6.490137
#> 
#> $original.values
#>     A     B     C   A&B   A&C   B&C A&B&C 
#>    10     9     4     2     3     3     2 
#> 
#> $fitted.values
#>        A        B        C      A&B      A&C      B&C    A&B&C 
#> 9.999785 8.999713 4.009993 2.027303 2.992809 2.992910 1.975726 
#> 
#> $residuals
#>             A             B             C           A&B           A&C 
#>  0.0002154188  0.0002869387 -0.0099933937 -0.0273028295  0.0071909144 
#>           B&C         A&B&C 
#>  0.0070898292  0.0242741373 
#> 
#> $stress
#> [1] 9.379153e-06
#> 
#> attr(,"class")
#> [1] "eulerr" "list"
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
