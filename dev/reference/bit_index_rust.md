# Bit-index matrix: (2^n - 1) × n integer matrix where row i's column j indicates whether set j is included in subset i. Rows are in legacy cardinality-first order (see `legacy_subset_masks`) so that downstream positional code (`fills.grob.<i>`, plotter region indexing) keeps matching the legacy Rcpp/C++ behavior.

Bit-index matrix: (2^n - 1) × n integer matrix where row i's column j
indicates whether set j is included in subset i. Rows are in legacy
cardinality-first order (see `legacy_subset_masks`) so that downstream
positional code (`fills.grob.<i>`, plotter region indexing) keeps
matching the legacy Rcpp/C++ behavior.

## Usage

``` r
bit_index_rust(n)
```
