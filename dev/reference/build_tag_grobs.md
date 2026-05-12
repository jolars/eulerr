# Build the leader / label / quantity gList for one tag.

Shared between
[`setup_tag()`](https://jolars.github.io/eulerr/dev/reference/setup_tag.md)
(initial construction in
[`setup_grobs()`](https://jolars.github.io/eulerr/dev/reference/setup_grobs.md))
and
[`makeContent.EulerTags()`](https://jolars.github.io/eulerr/dev/reference/makeContent.EulerTags.md)
(draw-time re-placement on resize). Pure factory — no measurement; takes
anchor + tether already in native units, plus the stashed text / gpar
bundle.

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
  has_label,
  has_quantity,
  label_gp,
  quantity_gp,
  label_rot,
  quantity_rot,
  number,
  leader_gp_list,
  padding,
  name_prefix = "tag"
)
```
