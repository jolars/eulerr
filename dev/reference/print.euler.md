# Print a summary of an Euler diagram

This function is responsible for printing fits from
[`euler()`](https://jolars.github.io/eulerr/dev/reference/euler.md) and
provides a summary of the fit. Prints a data frame of the original set
relationships and the fitted values as well as `diagError` and `stress`
statistics.

## Usage

``` r
# S3 method for class 'euler'
print(x, round = 3, vsep = strrep("-", 0.75 * getOption("width")), ...)
```

## Arguments

- x:

  `'euler'` object from
  [`euler()`](https://jolars.github.io/eulerr/dev/reference/euler.md)

- round:

  number of decimal places to round to

- vsep:

  character string to paste in between `euler` objects when `x` is a
  nested `euler` object

- ...:

  arguments passed to
  [`base::print.data.frame()`](https://rdrr.io/r/base/print.dataframe.html)

## Value

Summary statistics of the fitted Euler diagram are printed to screen.

## See also

[`euler()`](https://jolars.github.io/eulerr/dev/reference/euler.md),
[`base::print.data.frame()`](https://rdrr.io/r/base/print.dataframe.html)

## Examples

``` r
euler(organisms)
#>                         original fitted residuals regionError
#> animal                         0  0.576    -0.576       0.086
#> mammal                         0  0.297    -0.297       0.044
#> plant                          0  0.209    -0.209       0.031
#> sea                            0  0.428    -0.428       0.064
#> spiny                          0  0.160    -0.160       0.024
#> animal&mammal                  2  1.814     0.186       0.014
#> animal&sea                     1  0.599     0.401       0.053
#> animal&spiny                   0  0.211    -0.211       0.032
#> mammal&sea                     1  0.000     1.000       0.143
#> plant&sea                      1  0.862     0.138       0.014
#> plant&spiny                    1  0.000     1.000       0.143
#> sea&spiny                      0  0.172    -0.172       0.026
#> animal&mammal&sea              0  0.265    -0.265       0.040
#> animal&mammal&spiny            0  0.059    -0.059       0.009
#> animal&plant&sea               0  0.114    -0.114       0.017
#> animal&sea&spiny               1  0.692     0.308       0.039
#> plant&sea&spiny                0  0.016    -0.016       0.002
#> animal&mammal&sea&spiny        0  0.168    -0.168       0.025
#> animal&plant&sea&spiny         0  0.039    -0.039       0.006
#> 
#> diagError: 0.143 
#> stress:    0.352 
```
