# Default placement options used when the caller doesn't supply any.

Mirrors `eunoia::plotting::PlacementStrategy::default()`: raycast
exterior solver, POI tether, proportional margin. Per-strategy knobs
`iterations` (force-directed) and `min_gap` (elbow) live alongside in
the flat shape that the Rust FFI expects; the user-facing sublists in
`eulerr_options(labels = list(force_directed = ..., elbow = ...))` are
flattened into this shape by
[`plot.euler()`](https://jolars.github.io/eulerr/dev/reference/plot.euler.md)
before placement runs.

## Usage

``` r
default_placement_opts()
```
