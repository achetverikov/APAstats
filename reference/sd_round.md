# Rounded SD

SD rounded to the specified number of digits

## Usage

``` r
sd_round(x, ...)
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
sd_round(c(10, 99))
#> [1] "62.93"
sd_round(c(10, 99, NA))
#> [1] "62.93"
sd_round(c(10, 99), 2)
#> [1] "62.93"
```
