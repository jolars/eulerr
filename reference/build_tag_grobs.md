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
