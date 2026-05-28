# Project the wide `$shapes` schema back into the legacy 5-column (h, k, a, b, phi) ellipse data frame for circle/ellipse fits. Preserves the row order and row names so back-compat consumers see exactly the shape they used to.

Project the wide `$shapes` schema back into the legacy 5-column (h, k,
a, b, phi) ellipse data frame for circle/ellipse fits. Preserves the row
order and row names so back-compat consumers see exactly the shape they
used to.

## Usage

``` r
shapes_to_ellipse_frame(shapes)
```
