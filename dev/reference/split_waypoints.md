# Split a placement's flat `leader_waypoints_x` / `_y` / `_lengths` return triple into a per-label list of `list(x = ..., y = ...)` coordinate pairs. Each list element has length-`lengths[i]` `x`/`y` vectors (often `0` — straight leaders carry no waypoints).

Split a placement's flat `leader_waypoints_x` / `_y` / `_lengths` return
triple into a per-label list of `list(x = ..., y = ...)` coordinate
pairs. Each list element has length-`lengths[i]` `x`/`y` vectors (often
`0` — straight leaders carry no waypoints).

## Usage

``` r
split_waypoints(placements)
```
