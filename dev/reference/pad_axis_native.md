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

## Arguments

- lim:

  the axis range to pad

- pt_pad:

  padding in points

- axis:

  which axis `lim` belongs to

- layout_pos_row, layout_pos_col:

  layout cell of the measurement viewport, so the conversion sees the
  panel's real size
