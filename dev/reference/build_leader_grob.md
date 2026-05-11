# Build the polyline leader for an exterior tag, or [`grid::nullGrob()`](https://rdrr.io/r/grid/grid.null.html) for interior / missing-tether placements.

Drawn before the text grobs so the text visually covers the leader's
anchor end — a cheap substitute for proper AABB edge clipping.

## Usage

``` r
build_leader_grob(ax, ay, kind, tx, ty, leader_gp_list, fallback_gp, name)
```
