# Open a temporary grid measurement device + viewport.

Returns an idempotent closer thunk that pops the viewport and closes the
null PDF device. Used so we can call
`grid::convertWidth(grobWidth(...), "native", ...)` at setup time — i.e.
before
[`plot.eulergram()`](https://jolars.github.io/eulerr/dev/reference/plot.eulergram.md)
ever opens a real device — to size the label boxes that drive label
placement.

## Usage

``` r
open_measurement_viewport(xlim, ylim)
```

## Details

Always opens its own off-screen PDF rather than reusing the caller's
active device. Pushing a viewport onto the caller's device adds an entry
to its display list, which knitr's plot capture treats as visible change
and emits as an extra blank plot before the real
[`plot.eulergram()`](https://jolars.github.io/eulerr/dev/reference/plot.eulergram.md)
draws. The off-screen PDF keeps measurement entirely out of the user's
display list.
