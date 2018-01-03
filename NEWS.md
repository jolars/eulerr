# eulerr 3.1.0.9000

# eulerr 3.1.0

## Major changes
* The last-ditch optimizer switched from `GenSA::GenSA()` to
`RcppDE::DEoptim()`.
* The optimizer used in all the remaining cases, including all circular
diagrams and initial layouts, was switched back to `stats::nlm()` again.
* In final optimization, we now use *stress* instead of residual sums
of squares as a target for our optimizer.

## Minor changes
* `label` is now a proper generic with an appropriate method (`label.euler()`).
* The **eulerr under the hood** vignette has received a substantial update.

## Bug fixes
* Fixed warnings resulting from the deprecated `counts` argument in one
of the vignettes.
* Fixed memcheck errors in the final optimizer.
* Corrected erroneous labeling when `auto.key = TRUE` and labels were
*not* in alphabetic order. (#15)

# eulerr 3.0.1
## Bug fixes
* Added the missing %\\VignetteEngine{knitr::knitr} to both vignettes. It had 
mistakenly been left out, which had mangled the resulting vignettes.

# eulerr 3.0.0
## Major changes
* Ellipses are now supported by setting the new argument `shape = "ellipse"`
in `euler()`. This functionality accompanies an overhaul of the
innards of the function.
* Initial optimization function and gradient have been ported to C++.
* The initial optimizer has been switched from
`stats::optim(..., method = "L-BFGS-B")` to `stats::nlminb()`.
* The final optimizer now falls back to `GenSA::GenSA()` when the fit from
`nlminb()` isn't good enough, by default for 3 sets and ellipses, but 
this behavior can be controlled via a new argument `control`.
* A packing algorithm has been introduced to arrange disjoint clusters of
ellipses/circles.
* The label placement algorithm has been rewritten to handle ellipses and
been ported to C++. It now uses numerical optimization, which should
provide slightly more accurate locations.
* The initial optimizer now uses an analytical Hessian in addition to
gradient.

## Minor changes
* The initial optimizer now restarts up to 10 times and picks the best
fit (unless it is perfect somewhere along the way).
* The default palette has been changed to a fixed palette, still adapted
to color deficiency, but with some manual adjustments to, among other things,
avoid unnecessary use of color.
* The names of the `diagError` and `regionError` metrics have been changed from
`diag_error` and `region_error` to reflect the original names.
* The coordinates for the centers are now called *h* and *k* instead of
*x* and *y*, respectively.
* A new `label()` function has been added to extract locations for the overlaps
for third party plotting (#10).
* The `counts` argument to `plot.euler()` and `panel.euler.labels()` have
been deprecated in favor of the more appropriate `quantities`.
* Argument `fill_opacity` in `plot.euler()` that was deprecated in 
[v2.0.0](https://github.com/jolars/eulerr/releases/tag/v2.0.0) has been made
defunct.

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
