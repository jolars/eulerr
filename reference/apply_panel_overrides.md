# Overlay per-panel overrides onto a styling parameter

Used to apply `by_group` overrides to `fills`, `patterns`, `edges`,
`labels`, and `quantities` for a single panel produced by
`euler(..., by = ...)`. The override list contains gpar-level fields
(and optionally `rot`), which are overlaid onto `param$gp` (and
`param$set_gp` for shape-mode patterns). Structural fields are not
overlaid here — they must be filtered upstream.

## Usage

``` r
apply_panel_overrides(param, override)
```

## Arguments

- param:

  a list with `$gp` (a [`grid::gpar`](https://rdrr.io/r/grid/gpar.html))
  and optionally `$rot` and/or `$set_gp`.

- override:

  a flat named list of fields to overlay.

## Value

The param list with overlaid fields.
