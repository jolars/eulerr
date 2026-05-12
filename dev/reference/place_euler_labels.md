# Place per-region labels using eunoia's `place_labels` API.

Inputs mirror `euler_plot_data` for shape geometry and add per-region
label sizes plus placement-strategy options. Returns, parallel to
`label_combos`:

## Usage

``` r
place_euler_labels(
  set_names,
  h,
  k,
  a,
  b,
  phi,
  container_h,
  container_k,
  container_width,
  container_height,
  n_vertices,
  label_combos,
  label_widths,
  label_heights,
  placement,
  placement_margin,
  placement_iterations,
  placement_tether,
  placement_leader_gap,
  label_precision
)
```

## Details

- `anchor_x` / `anchor_y` — placed label anchor (NA on miss);

- `kind` — one of `"interior"`, `"exterior_raycast"`,
  `"exterior_force_directed"`; `""` if no placement was produced;

- `tether_x` / `tether_y` — tether point for the leader line (NA for
  interior placements / misses).

Plus a canvas bbox (`canvas_bbox_h/k/width/height`) from eunoia's
`placements_bbox` — NaN when no placements were produced — for the R
side to grow `xlim/ylim` so exterior labels are never clipped.

The complement region is requested with `""` in `label_combos`; when
`container_*` are non-NULL the spec is built with `.complement(1.0)` so
eunoia emits the empty `Combination` from `decompose_regions`.
