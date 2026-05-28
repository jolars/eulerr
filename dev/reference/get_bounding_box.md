# Get the bounding box for a row-wise collection of fitted shapes.

Dispatches on the shape kind. Ellipse/circle use the rotated-ellipse
bounding-box formula; rectangle/square use width/height (or side)
directly since both are axis-aligned in eunoia.

## Usage

``` r
get_bounding_box(shapes, k = NULL, a = NULL, b = NULL, phi = NULL)
```

## Arguments

- shapes:

  the `$shapes` data frame of a fitted euler object (after dropping NA
  rows for empty sets), or — for legacy callers — the numeric `h`
  vector. When `h` is numeric the function falls back to the legacy
  ellipse signature for back-compat with external code.

- k, a, b, phi:

  legacy ellipse parameters; only used when `shapes` is numeric (the
  legacy signature).

## Value

The bounding box as a list with `xlim` and `ylim`.
