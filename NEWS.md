# eulerr 0.2.0

## New features
* Added the option to produce conditional eulerr plots via a `by` argument to
`eulerr`. The result is a list of euler diagrams. These diagrams can be plotted
in a grid arrangement via a new plot method.
* Better label placement using a two-dimensional kernel density estimation.

## Bug fixes and minor improvements
* Cleaned up typos and grammar errors in the _Introduction to eulerr_ vignette.
* Added `mar` argument to `plot.eulerr` with default that produces
symmetric diagrams.
* No longer uses *randtoolbox* to sample points on the discs for labael
placement.

# eulerr 0.1.0
* The first release.