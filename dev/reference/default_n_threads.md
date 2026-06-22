# Default number of threads for the fitter's restart loop

Mirrors the approach taken by **data.table**: use half the available
cores at runtime, but stay single-threaded under `R CMD check` to honor
CRAN's two-core policy. eunoia parallelizes through `rayon`, which
ignores the `OMP_THREAD_LIMIT` signal that CRAN's check farm sets, so we
throttle here instead. Overrides are honored in order: the
`eulerr.n_threads` option, the `EULERR_NUM_THREADS` environment
variable, and then R's conventional `mc.cores` option (or `MC_CORES`
environment variable), the last of which is treated as an exact request
bounded by the available cores.

## Usage

``` r
default_n_threads()
```

## Value

A positive integer scalar (or whatever the `eulerr.n_threads` option is
set to, validated downstream).
