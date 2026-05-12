# Build the polyline leader for an exterior tag, or [`grid::nullGrob()`](https://rdrr.io/r/grid/grid.null.html) for interior / missing-tether placements.

Terminates at `(lend_x, lend_y)` — the point on the label box AABB edge
supplied by eunoia (`LabelPlacement::leader_end`). Falls back to the
anchor when the leader endpoint isn't finite so older / partial
placement results still draw something sensible.

## Usage

``` r
build_leader_grob(
  ax,
  ay,
  kind,
  tx,
  ty,
  lend_x,
  lend_y,
  leader_gp_list,
  fallback_gp,
  name
)
```
