# Pad an axis range by `pt_pad` points, converted to native units against a measurement viewport with the supplied scale. Returns the original range unchanged if the conversion isn't finite (e.g. zero range, no device).

Pad an axis range by `pt_pad` points, converted to native units against
a measurement viewport with the supplied scale. Returns the original
range unchanged if the conversion isn't finite (e.g. zero range, no
device).

## Usage

``` r
pad_axis_native(
  lim,
  pt_pad,
  axis = c("x", "y"),
  layout_pos_row = NULL,
  layout_pos_col = NULL
)
```
