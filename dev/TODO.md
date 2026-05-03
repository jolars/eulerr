# TODO

## Eliminate eager 2^n bit-index materialization

`bit_indexr()` (R/utils.R) builds a `2^n × n` logical matrix used by 6
call sites to enumerate region combinations:

- `R/utils.R:tally_combinations` — building tallies for
  matrix/data.frame input
- `R/parse_input.R` — input list parsing
- `R/venn.R` — generating combo names for
  [`venn()`](https://jolars.github.io/eulerr/dev/reference/venn.md)
- `R/fit_diagram.R` — venn lookup table indexing
- `R/plot.euler.R` — region indices for plotting
- `R/setup_grobs.R` — region labeling for grobs

The eunoia rewrite was motivated in part by avoiding 2^n materialization
on the hot path. The R wrapper still does this for n up to ~7. Each call
site iterates rows of the matrix to either:

1.  Build “&”-joined region labels — already produced in canonical order
    by the Rust `fit_euler_diagram` as `result$combo_labels`. Plot/print
    code could read those directly instead of recomputing.
2.  Match input combos to subsets — could be done by direct string
    parsing of names without a matrix.

Refactor each site to enumerate combinations lazily (or reuse the
already-computed `combo_labels` from the fit result) and delete
`bit_indexr` entirely.
