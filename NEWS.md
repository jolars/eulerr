# eulerr 1.0.0

## New features
* Switched to the cost function used in EulerAPE for the default optimization
target but added the posibility to choose cost function via a `cost` argument
(currently `eulerape` and `venneuler`).
* Optimization routines have been completely rewritten in C++ using Rcpp and
RcppArmadillo to boost perfomance.
* Added the option to produce conditional eulerr plots via a `by` argument to
`eulerr`. The result is a list of euler diagrams that can be plotted
in a grid arrangement via a new plot method.
* Improved label placement by using a two-dimensional kernel density estimation
instead of means to calculate label centers.

## Bug fixes and minor improvements
* Cleaned up typos and grammar errors in the _Introduction to eulerr_ vignette.
* Added `mar` argument to `plot.eulerr` with a default that produces
symmetric margins.
* Switched to Vogel's sampling method to sample points for label placement.
* Minor clean up and performance fixes all around.
* Added a print.eulerr method.

# eulerr 0.1.0
* The first release.