# Build the leader / label / quantity / annotation gList for one tag.

Shared between
[`setup_tag()`](https://jolars.github.io/eulerr/reference/setup_tag.md)
(initial construction in
[`setup_grobs()`](https://jolars.github.io/eulerr/reference/setup_grobs.md))
and
[`makeContent.EulerTags()`](https://jolars.github.io/eulerr/reference/makeContent.EulerTags.md)
(draw-time re-placement on resize). Pure factory — no measurement; takes
anchor + tether already in native units, plus the stashed text / gpar
bundle. The label / quantity / annotation stack is centered vertically
on `(ax, ay)` so the bbox center matches the anchor eunoia placed and
leader endpoints land on the actual bbox edge.

## Usage

``` r
build_tag_grobs(
  ax,
  ay,
  kind,
  tx,
  ty,
  lend_x,
  lend_y,
  label_text,
  quantity_text,
  annotation_text,
  has_label,
  has_quantity,
  has_annotation,
  label_gp,
  quantity_gp,
  annotation_gp,
  label_rot,
  quantity_rot,
  annotation_rot,
  number,
  leader_gp_list,
  padding,
  waypoints_x = numeric(0),
  waypoints_y = numeric(0),
  name_prefix = "tag"
)
```

## Arguments

- ax, ay:

  the tag anchor, in native units

- kind:

  the placement kind eunoia returned (interior, exterior, ...)

- tx, ty:

  the tether point on the shape, in native units

- lend_x, lend_y:

  the leader endpoint on the tag bbox, in native units

- label_text, quantity_text, annotation_text:

  the three text components

- has_label, has_quantity, has_annotation:

  whether each component is drawn

- label_gp, quantity_gp, annotation_gp:

  per-component [`grid::gpar()`](https://rdrr.io/r/grid/gpar.html)

- label_rot, quantity_rot, annotation_rot:

  per-component rotation, in degrees

- number:

  the diagram's region index, used to name the grobs

- leader_gp_list:

  [`grid::gpar()`](https://rdrr.io/r/grid/gpar.html) for the leader
  line, or `NULL` for no leader

- padding:

  vertical separation between components, as a
  [`grid::unit()`](https://rdrr.io/r/grid/unit.html)

- waypoints_x, waypoints_y:

  intermediate leader vertices, in native units

- name_prefix:

  prefix for the generated grob names
