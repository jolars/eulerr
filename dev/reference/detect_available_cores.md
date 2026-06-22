# Number of CPU cores eulerr may use

Collects the core-count limits we trust and returns the smallest, never
less than one. This mirrors the (much more elaborate) min-of-signals
design of `parallelly::availableCores()`, but only the durable,
non-platform-specific signals: the detected core count, `R CMD check`'s
`_R_CHECK_LIMIT_CORES_` (capped at two), and `OMP_THREAD_LIMIT`. We
deliberately do not parse cgroup quotas or HPC scheduler variables.

## Usage

``` r
detect_available_cores()
```

## Value

A positive integer scalar.
