# Compute polygon geometry and label anchors for plotting a fitted Euler diagram.

Inputs are the fitted shape parameters for the **non-empty** sets only,
in the order eulerr stores them (`x$ellipses` rows after dropping rows
with NA). The output is a sparse, parallel set of vectors keyed by
region label (set names joined by `&` in input order). Lengths equal the
number of populated regions returned by `decompose_regions`.

## Usage

``` r
euler_plot_data(set_names, h, k, a, b, phi, n_vertices, label_precision)
```
