# eulerr 2.0.0
## Major changes
* `eulerr()` has been replaced with `euler()` (see update 1.1.0) and made
defunct.
* There are two new methods for `euler`:
    - `euler.list()` produces diagrams from a list of sample spaces.
    - `euler.table()` produces diagrams from a `table` object, as long as there
      are no dimensions with values greater than 2.
* `plot.euler()` has been rewritten (again) from the ground up to better match
other high-level functions from **lattice**. This change is intended to be
as smooth as possible and should not make much of a difference to *most* users.
* Arguments `polygon_args`, `mar`, and `text_args` to `plot.euler()` have been
made defunct.

## Minor changes
* `plot.euler()` handles conflicting arguments better.
* c++ routines in `eulerr` now use registration.
* `euler()` now allows single sets (#9).
* Labels in `plot.euler()` now use a bold fontface by default in order to
distinguish them from the typeface used for counts.
* Argument `key` in `plot.euler()` has been deprecated and replaced with 
`auto.key`. Notice that using `key` does not throw a warning since the 
argument is used in `lattice::xyplot()` (which `plot.euler()` relies on).
* Argument `fill_opacity` is softly deprecated and has been replaced with 
`fill_alpha` for consistency with other lattice functions.

## Bug fixes
* `border` argument in `plot.euler()` works again (#7).

# eulerr 1.1.0
## Major changes
* `eulerr()` and its related methods been deprecated and are being replaced by
`euler()`, which takes slightly different input. Notably, the default is
now to provide input in the form of disjoint class combinations, rather
than unions. This is partly to make the function a drop-in replacement for
`venneuler::venneuler`.
* `plot.euler()` has been completely revamped, now interfacing `xyplot()` from
lattice. As a result, arguments `polygon_args`, `mar`, and `text_args` have been
deprecated.

## Minor changes
* Added a `counts` argument to `plot.eulerr`, which intersections and
complements with counts from the original set specificiation (#6).
* Added a `key` argument to `plot.eulerr` that prints a legend next to the
diagram.
* Switched to `atan2()` from RcppArmadillo.
* Added version requirement for RcppArmadillo.
* Dropped dependency on MASS for computing label placement, replacing it
with a faster, geometric algorithm.
* Dropped the cost function argument `cost` and now forces the function to
use sums of squares, which is more or less equivalent to the cost function
from `venneuler`.
* Color palettes in `plot.euler()` now chooses colors adapted to color vision
deficiency (deuteranopia). With increasingly large numbers of sets, this 
adaptation is relaxed to make sure that colors are kept visually distinct.
* `euler()` now uses `nlm()` instead of `optim(method = "Nelder-Mead")` for
its final optimization.

## Bug fixes
* The previous algorithm incorrectly computed loss from unions of sets. It now
computes loss from disjoint class combinations.
* Added missing row breaks in `print.eulerr`.

# eulerr 1.0.0

## New features
* Final optimization routines have been completely rewritten in C++ using Rcpp
and RcppArmadillo.
* Switched to the cost function from EulerAPE for the default optimization
target but added the posibility to choose cost function via a `cost` argument
(currently `eulerAPE` or `venneuler`).
* Added the option to produce conditional eulerr plots via a `by` argument to
`eulerr`. The result is a list of euler diagrams that can be plotted
in a grid arrangement via a new plot method.
* Improved label placement by using a two-dimensional kernel density estimation
instead of means to calculate label centers.

## Bug fixes and minor improvements
* Cleaned up typos and grammar errors in the _Introduction to eulerr_ vignette.
* Added `mar` argument to `plot.eulerr` with a default that produces
symmetric margins.
* Corrected the implementation of the `stress` statistic from venneuler.
* Switched to Vogel sampling to generate points to choose label positions from.
* Minor clean up and performance fixes all around.
* Added a `print.eulerr` method.
* Updated vignette to cover new features and changes.

# eulerr 0.1.0
* The first release.
