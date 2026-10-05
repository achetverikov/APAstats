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

Mean rounded to the specified number of digits (string)

## Examples

``` r
mean_round(c(10, 99))
#> [1] "54.50"
mean_round(c(10, 99, NA))
#> [1] "54.50"
mean_round(c(10, 99), 2)
#> [1] "54.50"
```
