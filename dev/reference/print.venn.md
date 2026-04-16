# Print a summary of a Venn diagram

This function is responsible for printing objects from from
[`venn()`](https://jolars.github.io/eulerr/dev/reference/venn.md) and
provides a simple description of the number of sets and the
specifications for the ellipses of the Venn diagram.

## Usage

``` r
# S3 method for class 'venn'
print(x, round = 3, vsep = strrep("-", 0.75 * getOption("width")), ...)
```

## Arguments

- x:

  an object of class `'venn'`

- round:

  number of digits to round the ellipse specification to

- vsep:

  character string to paste in between `euler` objects when `x` is a
  nested `euler` object

- ...:

  arguments passed to
  [`base::print.data.frame()`](https://rdrr.io/r/base/print.dataframe.html)

## Value

Summary statistics of the fitted Venn diagram are printed to screen.

## See also

[`venn()`](https://jolars.github.io/eulerr/dev/reference/venn.md),
[`base::print.data.frame()`](https://rdrr.io/r/base/print.dataframe.html)

## Examples

``` r
venn(organisms)
#> 5 set Venn diagram 
#> 
#>             h      k a   b   phi
#> animal  0.176  0.096 1 0.6 0.000
#> mammal -0.037  0.197 1 0.6 1.257
#> plant  -0.198  0.026 1 0.6 2.513
#> sea    -0.086 -0.181 1 0.6 3.770
#> spiny   0.145 -0.137 1 0.6 5.027
```
