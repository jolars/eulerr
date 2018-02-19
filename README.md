
<!-- README.md is generated from README.Rmd. Please edit that file -->
eulerr
======

[![Travis-CI Build Status](https://travis-ci.org/jolars/eulerr.svg?branch=master)](https://travis-ci.org/jolars/eulerr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/jolars/eulerr?branch=master&svg=true)](https://ci.appveyor.com/project/jolars/eulerr) [![Coverage Status](https://coveralls.io/repos/github/jolars/eulerr/badge.svg?branch=master)](https://coveralls.io/github/jolars/eulerr?branch=master) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/eulerr)](https://cran.r-project.org/package=eulerr) [![Research software impact](http://depsy.org/api/package/cran/eulerr/badge.svg)](http://depsy.org/package/r/eulerr)

**eulerr** generates area-proportional Euler diagrams that display set relationships (intersections, unions, and disjoints) with circles or ellipses. [Euler diagrams](https://en.wikipedia.org/wiki/Euler_diagram) are Venn diagrams without the requirement that all set interactions be present (whether they are empty or not), which means that, depending on input, **eulerr** sometimes produces Venn diagrams and sometimes not.

With three or more sets intersecting, exact Euler diagrams are often impossible. For such cases **eulerr** attempts to provide a good approximation by numerically tuning the parameters of the ellipses or circles to minimize the error in the resulting diagram. Residuals and goodness of fit statistics are provided to assess whether the resulting diagram can be trusted.

Installation
------------

### CRAN version

``` r
install.packages("eulerr")
```

### Development version

``` r
devtools::install_github("jolars/eulerr")
```

### Shiny app

eulerr is also available as a shiny app hosted at [eulerr.co](http://eulerr.co).

Usage
-----

``` r
library(eulerr)
# From Wilkinson 2012
fit <- euler(c("A" = 4, "B" = 6, "C" = 3, "D" = 2, "E" = 7, "F" = 3,
               "A&B" = 2, "A&F" = 2, "B&C" = 2, "B&D" = 1,
               "B&F" = 2, "C&D" = 1, "D&E" = 1, "E&F" = 1,
               "A&B&F" = 1, "B&C&D" = 1),
             shape = "ellipse")
```

We can inspect the goodness-of-fit metrics *diagError* and *stress* for the solution,

``` r
fit$stress
#> [1] 2.055708e-12
fit$diagError
#> [1] 2.183156e-07
```

and plot it

``` r
plot(fit)
```

![](tools/README-plot_method-1.png)

Please see [the introductory vignette](https://CRAN.R-project.org/package=eulerr/vignettes/introduction.pdf) for a brief introduction or [*eulerr under the hood*](https://CRAN.R-project.org/package=eulerr/vignettes/under-the-hood.pdf) for details.

License
-------

eulerr is open source software, licensed under [GPL-3](LICENSE).

Versioning
----------

eulerr uses [semantic versioning](http://semver.org).

Code of conduct
---------------

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

Acknowledgements
----------------

eulerr would not be possible without Ben Frederickson's work on [venn.js](http://www.benfrederickson.com) or Leland Wilkinson's [venneuler](https://cran.r-project.org/package=venneuler).
