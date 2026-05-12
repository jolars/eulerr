# Resolve a `gap` option to a numeric in native units inside the current measurement viewport. `NULL` → falls back to `padding_native` so the visible leader-tip gap matches the spacing between label and quantity. A `grid::unit` value converts to native; a bare numeric is interpreted as `lines` (same convention as `eulerr_options()$padding`).

Resolve a `gap` option to a numeric in native units inside the current
measurement viewport. `NULL` → falls back to `padding_native` so the
visible leader-tip gap matches the spacing between label and quantity. A
[`grid::unit`](https://rdrr.io/r/grid/unit.html) value converts to
native; a bare numeric is interpreted as `lines` (same convention as
`eulerr_options()$padding`).

## Usage

``` r
resolve_gap_native(gap, padding_native)
```
