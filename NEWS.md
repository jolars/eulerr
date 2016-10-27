# eulerr 0.2.0

## New features
* Added the option to produce conditional eulerr plots via a `by` argument to
`eulerr`. The result is a list of euler diagrams. These diagrams can be plotted
in a grid arrangement via a new plot method.
* Improved label placement by using a two-dimensional kernel density estimation
instead of means to calculate label centers.

## Bug fixes and minor improvements
* Cleaned up typos and grammar errors in the _Introduction to eulerr_ vignette.
* Added `mar` argument to `plot.eulerr` with default that produces
symmetric diagrams.
* Uses Vogel's method instead of *randtoolbox* to sample points on the disks for
labael placement.
* Vectorized polygon area computations to improve speed
* Minor clean up and performance fixes all around.

# eulerr 0.1.0
* The first release.