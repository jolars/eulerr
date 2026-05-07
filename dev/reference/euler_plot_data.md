# Compute polygon geometry and label anchors for plotting a fitted Euler diagram, including the optional complement region inside a fitted container.

Inputs are the fitted shape parameters for the **non-empty** sets only,
in the order eulerr stores them (`x$ellipses` rows after dropping rows
with NA). When `container_*` are non-NULL they describe the fitted
universe-box rectangle; in that case the result also carries the
complement region geometry (the area inside the rectangle outside every
shape) and a label anchor for it. Eunoia's `decompose_regions` emits
this region under the empty `Combination` whenever the spec carries a
complement and a container is supplied — so eulerr no longer needs a
hand-rolled rectangle-minus-shapes pass.

## Usage

``` r
euler_plot_data(
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
  label_precision
)
```
