# Rounded mean

Mean rounded to the specified number of digits

## Usage

``` r
mean_round(x, ...)
```

## Arguments

- x:

  a number

- ...:

  other arguments passed to
  [f_round](https://achetverikov.github.io/APAstats/reference/f_round.md)

## Value

A character string containing the mean rounded to the specified number
of decimal places.

## Examples

``` r
mean_round(c(10, 99))
#> [1] "54.50"
mean_round(c(10, 99, NA))
#> [1] "54.50"
mean_round(c(10, 99), 2)
#> [1] "54.50"
```
