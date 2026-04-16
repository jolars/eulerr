# Changelog

## eulerr (development version)

## eulerr 7.0.4

CRAN release: 2025-09-24

### Bug Fixes

- Ignore `air.toml` in `Rbuildignore` to fix R CMD check warning.

## eulerr 7.0.3

### Bug Fixes

- Fix deprecated use of `arma::is_finite()` in C++ code.
- Fix URL link to eulerAPE in documentation.
- Fix URL link to workshop paper in documentation and citation.

## eulerr 7.0.2

CRAN release: 2024-03-28

### Bug Fixes

- Fix order and layout of strips when grouping
  ([\#108](https://github.com/jolars/eulerr/issues/108), by
  [@altairwei](https://github.com/altairwei))

## eulerr 7.0.1

CRAN release: 2024-02-16

### Minor Changes

- Fixed some incorrect documentation of internal functions.
- Corrected an url for eulerAPE.

## eulerr 7.0.0

CRAN release: 2022-12-09

### New Features

- It is now possible to set the loss function to be used when trying to
  optimize the Euler diagram layout via `loss` and `loss_aggregator`.
  There is a new vignette that showcases this new feature.

### Minor Changes

- C++14 is now required for the package.

### Bug Fixes

- Label repelling via `adjust_labels` in
  [`plot.euler()`](https://jolars.github.io/eulerr/dev/reference/plot.euler.md)
  has been deprecated and removed to fix sanitizer warnings.

## eulerr 6.1.1

CRAN release: 2021-09-06

### Minor changes

- citation to conference paper added to `inst/CITATION`
- error messages for erroneous input have been improved in several
  places
- switched `PI` to `M_PI` to support `STRICT_R_HEADERS` in C++ code
  ([\#82](https://github.com/jolars/eulerr/issues/82), thanks
  [@eddelbuettel](https://github.com/eddelbuettel))

### Bug fixes

- error in documentation for list method has been fixed, thanks
  [@gprezza](https://github.com/gprezza) (##77)

## eulerr 6.1.0

CRAN release: 2020-03-09

### Minor changes

- Label repelling (activated by calling
  [`euler()`](https://jolars.github.io/eulerr/dev/reference/euler.md)
  with `adjust_labels = TRUE`) no longer repels text labels away from
  the edges of the shapes in the diagram.

### Bug fixes

- Rectify sanitizer error from clang-ASAN test environment.

## eulerr 6.0.2

CRAN release: 2020-03-04

### Bug fixes

- Set `stringsAsFactors = TRUE` inside all relevant functions in
  [`euler()`](https://jolars.github.io/eulerr/dev/reference/euler.md) to
  avoid errors in upcoming R version.
- Fix broken link in *eulerr under the hood* vignette.

## eulerr 6.0.1

### Minor changes

- Throw an error message when the number of sets in
  [`venn()`](https://jolars.github.io/eulerr/dev/reference/venn.md)
  exceeds 5 (##65)
- Performance improved when large lists are used as input to
  [`euler()`](https://jolars.github.io/eulerr/dev/reference/euler.md)
  and [`venn()`](https://jolars.github.io/eulerr/dev/reference/venn.md)
  (##64, [@privefl](https://github.com/privefl))

### Bug fixes

- Correctly handle `data.frame` inputs to
  [`euler()`](https://jolars.github.io/eulerr/dev/reference/euler.md)
  when categorical variables are character vectors and not factors.

## eulerr 6.0.0

CRAN release: 2019-09-27

### New features

- In
  [`plot.euler()`](https://jolars.github.io/eulerr/dev/reference/plot.euler.md),
  percentages can be added to the plot in addition to or instead of
  counts by providing a `list` to the `quantities` argument with an item
  `type` that can take any combination of `counts` and `percent`. This
  change also comes with a redesign of the grid graphics implementation
  for labels.
- [`eulerr_options()`](https://jolars.github.io/eulerr/dev/reference/eulerr_options.md)
  gains a new argument `padding` which controls the amount of padding
  between labels and quantities. (##48)
- [`plot.euler()`](https://jolars.github.io/eulerr/dev/reference/plot.euler.md)
  now uses code from the **ggrepel** package to prevent labels from
  overlapping or escaping the plot area if `adjust_labels` is set to
  `TRUE`.
- A new vignette featuring a gallery of plots from the package has been
  added.

### Minor changes

- The default `cex` for quantity labels has changed from 1.0 to 0.9.
- Labels for sets that overlap are now merged (partly fixes \##45)
- The fill colors for sets which are completely contained within another
  set are now once again composed of a mix of the color of the subset
  and the superset.
- Plotting data has been exposed in a `data` slot in the object created
  by calling to
  [`plot.euler()`](https://jolars.github.io/eulerr/dev/reference/plot.euler.md)
  (##57)

### Bug fixes

- An error in layout normalization that occurred sometimes with ellipses
  has been fixed.

### eulerr 5.1.0

### New features

- [`venn()`](https://jolars.github.io/eulerr/dev/reference/venn.md) is a
  new function that produces Venn diagrams for up to 5 sets. The
  interface is almost identical to
  [`euler()`](https://jolars.github.io/eulerr/dev/reference/euler.md)
  except that a single integer can also be provided. A new vignette,
  *Venn diagrams with eulerr*, exemplifies its use.

### Minor changes

- Calculations for the strips in
  [`plot.euler()`](https://jolars.github.io/eulerr/dev/reference/plot.euler.md)
  when a list of Euler diagrams is given has been improved. Setting
  `fontsize` or `cex` now results in appropriately sized strips as one
  would expect.
- Tiny overlaps (where the fraction of the area is less than one
  thousandth of the largest overlap) in the final diagram are no longer
  plotted.
- `eulergram()` objects from
  [`plot.euler()`](https://jolars.github.io/eulerr/dev/reference/plot.euler.md)
  now have a proper grob name for the canvas grob, so that extracting
  information from them is easier.

### Bug fixes

- Return value documentation for
  [`euler()`](https://jolars.github.io/eulerr/dev/reference/euler.md)
  now correctly says “ellipses” and not “coefficients”.
- `data.frame` or `matrix` inputs now work properly when values are
  numerical. (##42)
- Fixed some spelling errors in news and vignettes.

## eulerr 5.0.0

CRAN release: 2018-11-05

### New features

- [`error_plot()`](https://jolars.github.io/eulerr/dev/reference/error_plot.md)
  is a new function that offers diagnostic plots of fits from
  [`euler()`](https://jolars.github.io/eulerr/dev/reference/euler.md),
  letting the user visualize the error in the resulting Euler diagram.

### Major changes

- [`euler()`](https://jolars.github.io/eulerr/dev/reference/euler.md)
  once again uses the residual sums of squares, rather than the stress
  metric, as optimization objective, which means that output is always
  scaled appropriately to input (##28).
- [`plot.euler()`](https://jolars.github.io/eulerr/dev/reference/plot.euler.md)
  now uses the
  [polylabelr](https://CRAN.R-project.org/package=polylabelr) package to
  position labels for the overlaps of the ellipses, which has improved
  performance in plotting complicated diagrams considerably and reduced
  the amount of code in this package greatly.
- The c++ internals have been rewritten using more memory-efficient,
  performant and expressive code.

### Minor changes

- The
  [`euler.data.frame()`](https://jolars.github.io/eulerr/dev/reference/euler.md)
  method (and by proxy the
  [`euler.matrix()`](https://jolars.github.io/eulerr/dev/reference/euler.md)
  method) can now take matrices with factors in addition to the
  previously supported logical and integer (binary) input. The function
  will dummy code the variables for the user.
- A few performance fixes.
- Additional unit tests.
- Previously deprecated arguments to
  [`plot.euler()`](https://jolars.github.io/eulerr/dev/reference/plot.euler.md)
  have been made defunct.
- Added a data set, `plants`, to exemplify the list method for
  [`euler()`](https://jolars.github.io/eulerr/dev/reference/euler.md).
- Added a data set, `fruits`, to exemplify the data.frame method for
  [`euler()`](https://jolars.github.io/eulerr/dev/reference/euler.md).
- [`euler.data.frame()`](https://jolars.github.io/eulerr/dev/reference/euler.md)
  gains an argument `sep`, which is a character vector used to separate
  dummy-coded factors if there are factors or characters in the input.
- Added a data set, `organisms`, to exemplify the matrix method for
  [`euler()`](https://jolars.github.io/eulerr/dev/reference/euler.md).
- Added a data set, `pain`, to exemplify the table method for
  [`euler()`](https://jolars.github.io/eulerr/dev/reference/euler.md).
- [`euler.table()`](https://jolars.github.io/eulerr/dev/reference/euler.md)
  gains an argument, `factor_names`, for specifying whether the factor
  names should be included when generating dummy-coded variables in case
  the input is a data.frame with character or factor vectors or if the
  input is a table with more than two columns or rows.
- Parts of the *eulerr under the hood* vignette has been branched off
  into a new vignette regarding visualization.

### Bug fixes

- Empty combinations can now be provided and will be plotted (generating
  completely blank plots).
- [`euler.list()`](https://jolars.github.io/eulerr/dev/reference/euler.md)
  now passes its ellipsis argument along properly. (##33, thanks,
  [@banfai](https://github.com/banfai))
- Several spelling and grammar mistakes were corrected in vignettes and
  documentation.

## eulerr 4.1.0

CRAN release: 2018-04-20

### Minor changes

- [`plot.euler()`](https://jolars.github.io/eulerr/dev/reference/plot.euler.md)
  now returns a `gTree` object. All of the plotting mechanisms are now
  also found in this function and
  [`plot.eulergram()`](https://jolars.github.io/eulerr/dev/reference/plot.eulergram.md)
  and
  [`print.eulergram()`](https://jolars.github.io/eulerr/dev/reference/plot.eulergram.md)
  basically just call
  [`grid::grid.draw()`](https://rdrr.io/r/grid/grid.draw.html) on the
  result of
  [`plot.euler()`](https://jolars.github.io/eulerr/dev/reference/plot.euler.md).
  This change means that functions such as `gridExtra::grid.arrange()`
  now work as one would intuit on the objects produced by
  [`plot.euler()`](https://jolars.github.io/eulerr/dev/reference/plot.euler.md).
- Fitting and plotting Euler diagrams with empty sets is now allowed
  (##23). Empty sets in the input will be returned as `NA` in the
  resulting `data.frame` of ellipses.
- The last-ditch optimizer has been switched back to
  [`GenSA::GenSA()`](https://rdrr.io/pkg/GenSA/man/GenSA.html) from
  `RcppDE::DEoptim()`.

### Bug fixes

- The **grid** parameters available for *edges* are now correctly
  specified in the manual for
  [`plot.euler()`](https://jolars.github.io/eulerr/dev/reference/plot.euler.md).
- [`euler.data.frame()`](https://jolars.github.io/eulerr/dev/reference/euler.md)
  now works as expected for tibbles (from the **tibble** package) when
  argument `by` is used.

## eulerr 4.0.0

CRAN release: 2018-02-05

### Major changes

- [`plot.euler()`](https://jolars.github.io/eulerr/dev/reference/plot.euler.md)
  has been rewritten completely from scratch, now using a custom
  **grid**-based implementation rather than **lattice**. As a result,
  all `panel.*()` functions and `label()` have been deprecated as well
  as arguments `fill_alpha`, `auto.key`, `fontface`, `par.settings`,
  `default.prepanel`, `default.scales`, and `panel`. The method for
  plotting diagrams has also changed—rather than overlaying shapes on
  top of each other, the diagram is now split into separate polygons
  using the **polyclip** package. Instead of relying on semi-transparent
  fills, the colors of the fills are now blended in the CIELab color
  space (##16).
- The default color palette has been redesigned from scratch to suit the
  new plot method.
- A new function
  [`eulerr_options()`](https://jolars.github.io/eulerr/dev/reference/eulerr_options.md)
  have been provided in order to set default graphical parameters for
  the diagrams.

### Minor changes

- Arguments `counts` and `outer_strips` to
  [`plot.euler()`](https://jolars.github.io/eulerr/dev/reference/plot.euler.md)
  are now defunct.
- [`euler()`](https://jolars.github.io/eulerr/dev/reference/euler.md)
  now always returns ellipse-based parameters with columns `h`, `k`,
  `a`, `b`, and `phi`, regardless of which shape is used. This item was
  previously named “coefficients”, but it now called “ellipses” instead
  and a custom
  [`coef.euler()`](https://jolars.github.io/eulerr/dev/reference/coef.euler.md)
  method has been added to make cure that
  [`coef()`](https://rdrr.io/r/stats/coef.html) still works.
- Layouts are now partially normalized so that diagrams will look
  approximately the same even with different random seeds.

### Bug fixes

- Providing custom labels to `quantities` and `labels` arguments of
  [`plot.euler()`](https://jolars.github.io/eulerr/dev/reference/plot.euler.md)
  now works correctly (##20).

## eulerr 3.1.0

CRAN release: 2018-01-02

### Major changes

- The last-ditch optimizer switched from
  [`GenSA::GenSA()`](https://rdrr.io/pkg/GenSA/man/GenSA.html) to
  `RcppDE::DEoptim()`.
- The optimizer used in all the remaining cases, including all circular
  diagrams and initial layouts, was switched back to
  [`stats::nlm()`](https://rdrr.io/r/stats/nlm.html) again.
- In final optimization, we now use *stress* instead of residual sums of
  squares as a target for our optimizer.

### Minor changes

- `label` is now a proper generic with an appropriate method
  (`label.euler()`).
- The **eulerr under the hood** vignette has received a substantial
  update.

### Bug fixes

- Fixed warnings resulting from the deprecated `counts` argument in one
  of the vignettes.
- Fixed memcheck errors in the final optimizer.
- Corrected erroneous labeling when `auto.key = TRUE` and labels were
  *not* in alphabetic order. (##15)

## eulerr 3.0.1

CRAN release: 2017-11-15

### Bug fixes

- Added the missing %\VignetteEngine{knitr::knitr} to both vignettes. It
  had mistakenly been left out, which had mangled the resulting
  vignettes.

## eulerr 3.0.0

CRAN release: 2017-11-15

### Major changes

- Ellipses are now supported by setting the new argument
  `shape = "ellipse"` in
  [`euler()`](https://jolars.github.io/eulerr/dev/reference/euler.md).
  This functionality accompanies an overhaul of the innards of the
  function.
- Initial optimization function and gradient have been ported to C++.
- The initial optimizer has been switched from
  `stats::optim(..., method = "L-BFGS-B")` to
  [`stats::nlminb()`](https://rdrr.io/r/stats/nlminb.html).
- The final optimizer now falls back to
  [`GenSA::GenSA()`](https://rdrr.io/pkg/GenSA/man/GenSA.html) when the
  fit from [`nlminb()`](https://rdrr.io/r/stats/nlminb.html) isn’t good
  enough, by default for 3 sets and ellipses, but this behavior can be
  controlled via a new argument `control`.
- A packing algorithm has been introduced to arrange disjoint clusters
  of ellipses/circles.
- The label placement algorithm has been rewritten to handle ellipses
  and been ported to C++. It now uses numerical optimization, which
  should provide slightly more accurate locations.
- The initial optimizer now uses an analytical Hessian in addition to
  gradient.

### Minor changes

- The initial optimizer now restarts up to 10 times and picks the best
  fit (unless it is perfect somewhere along the way).
- The default palette has been changed to a fixed palette, still adapted
  to color deficiency, but with some manual adjustments to, among other
  things, avoid unnecessary use of color.
- The names of the `diagError` and `regionError` metrics have been
  changed from `diag_error` and `region_error` to reflect the original
  names.
- The coordinates for the centers are now called *h* and *k* instead of
  *x* and *y*, respectively.
- A new `label()` function has been added to extract locations for the
  overlaps for third party plotting (##10).
- The `counts` argument to
  [`plot.euler()`](https://jolars.github.io/eulerr/dev/reference/plot.euler.md)
  and `panel.euler.labels()` have been deprecated in favor of the more
  appropriate `quantities`.
- Argument `fill_opacity` in
  [`plot.euler()`](https://jolars.github.io/eulerr/dev/reference/plot.euler.md)
  that was deprecated in
  [v2.0.0](https://github.com/jolars/eulerr/releases/tag/v2.0.0) has
  been made defunct.

## eulerr 2.0.0

CRAN release: 2017-06-20

### Major changes

- [`eulerr()`](https://jolars.github.io/eulerr/dev/reference/eulerr-package.md)
  has been replaced with
  [`euler()`](https://jolars.github.io/eulerr/dev/reference/euler.md)
  (see update 1.1.0) and made defunct.
- There are two new methods for `euler`:
  - [`euler.list()`](https://jolars.github.io/eulerr/dev/reference/euler.md)
    produces diagrams from a list of sample spaces.
  - [`euler.table()`](https://jolars.github.io/eulerr/dev/reference/euler.md)
    produces diagrams from a `table` object, as long as there are no
    dimensions with values greater than 2.
- [`plot.euler()`](https://jolars.github.io/eulerr/dev/reference/plot.euler.md)
  has been rewritten (again) from the ground up to better match other
  high-level functions from **lattice**. This change is intended to be
  as smooth as possible and should not make much of a difference to
  *most* users.
- Arguments `polygon_args`, `mar`, and `text_args` to
  [`plot.euler()`](https://jolars.github.io/eulerr/dev/reference/plot.euler.md)
  have been made defunct.

### Minor changes

- [`plot.euler()`](https://jolars.github.io/eulerr/dev/reference/plot.euler.md)
  handles conflicting arguments better.
- c++ routines in `eulerr` now use registration.
- [`euler()`](https://jolars.github.io/eulerr/dev/reference/euler.md)
  now allows single sets (##9).
- Labels in
  [`plot.euler()`](https://jolars.github.io/eulerr/dev/reference/plot.euler.md)
  now use a bold font face by default in order to distinguish them from
  the typeface used for counts.
- Argument `key` in
  [`plot.euler()`](https://jolars.github.io/eulerr/dev/reference/plot.euler.md)
  has been deprecated and replaced with `auto.key`. Notice that using
  `key` does not throw a warning since the argument is used in
  [`lattice::xyplot()`](https://rdrr.io/pkg/lattice/man/xyplot.html)
  (which
  [`plot.euler()`](https://jolars.github.io/eulerr/dev/reference/plot.euler.md)
  relies on).
- Argument `fill_opacity` is softly deprecated and has been replaced
  with `fill_alpha` for consistency with other lattice functions.

### Bug fixes

- `border` argument in
  [`plot.euler()`](https://jolars.github.io/eulerr/dev/reference/plot.euler.md)
  works again (##7).

## eulerr 1.1.0

CRAN release: 2017-02-05

### Major changes

- [`eulerr()`](https://jolars.github.io/eulerr/dev/reference/eulerr-package.md)
  and its related methods been deprecated and are being replaced by
  [`euler()`](https://jolars.github.io/eulerr/dev/reference/euler.md),
  which takes slightly different input. Notably, the default is now to
  provide input in the form of disjoint class combinations, rather than
  unions. This is partly to make the function a drop-in replacement for
  `venneuler::venneuler`.
- [`plot.euler()`](https://jolars.github.io/eulerr/dev/reference/plot.euler.md)
  has been completely revamped, now interfacing `xyplot()` from lattice.
  As a result, arguments `polygon_args`, `mar`, and `text_args` have
  been deprecated.

### Minor changes

- Added a `counts` argument to `plot.eulerr`, which intersections and
  complements with counts from the original set specification (##6).
- Added a `key` argument to `plot.eulerr` that prints a legend next to
  the diagram.
- Switched to [`atan2()`](https://rdrr.io/r/base/Trig.html) from
  RcppArmadillo.
- Added version requirement for RcppArmadillo.
- Dropped dependency on MASS for computing label placement, replacing it
  with a faster, geometric algorithm.
- Dropped the cost function argument `cost` and now forces the function
  to use sums of squares, which is more or less equivalent to the cost
  function from `venneuler`.
- Color palettes in
  [`plot.euler()`](https://jolars.github.io/eulerr/dev/reference/plot.euler.md)
  now chooses colors adapted to color vision deficiency (deuteranopia).
  With increasingly large numbers of sets, this adaptation is relaxed to
  make sure that colors are kept visually distinct.
- [`euler()`](https://jolars.github.io/eulerr/dev/reference/euler.md)
  now uses [`nlm()`](https://rdrr.io/r/stats/nlm.html) instead of
  `optim(method = "Nelder-Mead")` for its final optimization.

### Bug fixes

- The previous algorithm incorrectly computed loss from unions of sets.
  It now computes loss from disjoint class combinations.
- Added missing row breaks in `print.eulerr`.

## eulerr 1.0.0

CRAN release: 2016-12-10

### New features

- Final optimization routines have been completely rewritten in C++
  using Rcpp and RcppArmadillo.
- Switched to the cost function from EulerAPE for the default
  optimization target but added the possibility to choose cost function
  via a `cost` argument (currently `eulerAPE` or `venneuler`).
- Added the option to produce conditional eulerr plots via a `by`
  argument to `eulerr`. The result is a list of Euler diagrams that can
  be plotted in a grid arrangement via a new plot method.
- Improved label placement by using a two-dimensional kernel density
  estimation instead of means to calculate label centers.

### Bug fixes and minor improvements

- Cleaned up typos and grammar errors in the *Introduction to eulerr*
  vignette.
- Added `mar` argument to `plot.eulerr` with a default that produces
  symmetric margins.
- Corrected the implementation of the `stress` statistic from venneuler.
- Switched to Vogel sampling to generate points to choose label
  positions from.
- Minor clean up and performance fixes all around.
- Added a `print.eulerr` method.
- Updated vignette to cover new features and changes.

## eulerr 0.1.0

CRAN release: 2016-10-16

- The first release.
