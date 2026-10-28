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

## Arguments

- ax, ay:

  the tag anchor, in native units

- kind:

  the placement kind eunoia returned; anything other than `"interior"`
  gets a leader

- tx, ty:

  the tether point on the shape, in native units

- lend_x, lend_y:

  the leader endpoint on the tag bbox, in native units

- waypoints_x, waypoints_y:

  intermediate leader vertices, in native units

- leader_gp_list:

  [`grid::gpar()`](https://rdrr.io/r/grid/gpar.html) for the leader
  line, or `NULL`

- fallback_gp:

  [`grid::gpar()`](https://rdrr.io/r/grid/gpar.html) used when
  `leader_gp_list` is `NULL`

- name:

  name to give the returned grob

## Details

Terminates at `(lend_x, lend_y)` — the point on the label box AABB edge
supplied by eunoia (`LabelPlacement::leader_end`). Falls back to the
anchor when the leader endpoint isn't finite so older / partial
placement results still draw something sensible.
