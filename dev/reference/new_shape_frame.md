# Allocate a fresh `$shapes` data frame for `n_all` sets. Rows for empty sets keep NA in every column so downstream plotting can detect them via `is.na(shapes$h)` regardless of shape kind.

Allocate a fresh `$shapes` data frame for `n_all` sets. Rows for empty
sets keep NA in every column so downstream plotting can detect them via
`is.na(shapes$h)` regardless of shape kind.

## Usage

``` r
new_shape_frame(shape, n_all, row_names)
```
