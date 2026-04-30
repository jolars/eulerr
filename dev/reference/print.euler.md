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
#>                               original fitted residuals regionError
#> animal                               0  0.472    -0.472       0.075
#> mammal                               0  0.222    -0.222       0.035
#> plant                                0  0.373    -0.373       0.059
#> sea                                  0  0.337    -0.337       0.054
#> spiny                                0  0.178    -0.178       0.028
#> animal&mammal                        2  1.911     0.089       0.019
#> animal&plant                         0  0.000     0.000       0.000
#> mammal&plant                         0  0.000     0.000       0.000
#> animal&sea                           1  0.590     0.410       0.049
#> mammal&sea                           1  0.000     1.000       0.143
#> plant&sea                            1  0.815     0.185       0.013
#> animal&spiny                         0  0.000     0.000       0.000
#> mammal&spiny                         0  0.000     0.000       0.000
#> plant&spiny                          1  0.881     0.119       0.002
#> sea&spiny                            0  0.000     0.000       0.000
#> animal&mammal&plant                  0  0.000     0.000       0.000
#> animal&mammal&sea                    0  0.257    -0.257       0.041
#> animal&plant&sea                     0  0.037    -0.037       0.006
#> mammal&plant&sea                     0  0.000     0.000       0.000
#> animal&mammal&spiny                  0  0.000     0.000       0.000
#> animal&plant&spiny                   0  0.000     0.000       0.000
#> mammal&plant&spiny                   0  0.000     0.000       0.000
#> animal&sea&spiny                     1  0.000     1.000       0.143
#> mammal&sea&spiny                     0  0.000     0.000       0.000
#> plant&sea&spiny                      0  0.206    -0.206       0.033
#> animal&mammal&plant&sea              0  0.000     0.000       0.000
#> animal&mammal&plant&spiny            0  0.000     0.000       0.000
#> animal&mammal&sea&spiny              0  0.000     0.000       0.000
#> animal&plant&sea&spiny               0  0.000     0.000       0.000
#> mammal&plant&sea&spiny               0  0.000     0.000       0.000
#> animal&mammal&plant&sea&spiny        0  0.000     0.000       0.000
#> 
#> diagError: 0.143 
#> stress:    0.321 
```
