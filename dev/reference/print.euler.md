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
#>                   original fitted residuals regionError
#> animal                   0  0.218    -0.218       0.035
#> mammal                   0  0.466    -0.466       0.075
#> plant                    0  0.370    -0.370       0.060
#> sea                      0  0.332    -0.332       0.054
#> spiny                    0  0.176    -0.176       0.028
#> animal&mammal            2  1.905     0.095       0.021
#> animal&sea               1  0.000     1.000       0.143
#> mammal&sea               1  0.576     0.424       0.050
#> plant&sea                1  0.805     0.195       0.013
#> plant&spiny              1  0.869     0.131       0.003
#> animal&mammal&sea        0  0.251    -0.251       0.041
#> animal&sea&spiny         1  0.000     1.000       0.143
#> mammal&plant&sea         0  0.035    -0.035       0.006
#> plant&sea&spiny          0  0.200    -0.200       0.032
#> 
#> diagError: 0.143 
#> stress:    0.321 
```
