# Build the polyline leader for an exterior tag, or [`grid::nullGrob()`](https://rdrr.io/r/grid/grid.null.html) for interior / missing-tether placements.

Draws the polyline `tether → waypoints[1..] → leader_end`. For straight
leaders (raycast / force-directed) `waypoints_*` are empty, so the
polyline collapses to the single `tether → leader_end` segment. For
elbow leaders eunoia emits one knee waypoint, producing the orthogonal
`tether → knee → leader_end` bend.

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
  waypoints_x = numeric(0),
  waypoints_y = numeric(0),
  leader_gp_list,
  fallback_gp,
  name
)
```

## Details

Terminates at `(lend_x, lend_y)` — the point on the label box AABB edge
supplied by eunoia (`LabelPlacement::leader_end`). Falls back to the
anchor when the leader endpoint isn't finite so older / partial
placement results still draw something sensible.
