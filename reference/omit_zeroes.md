# Omit leading zero from number

Omit leading zero from number

## Usage

``` r
omit_zeroes(x, digits = 2)
```

## Arguments

- x:

  A number

- digits:

  Number of decimal digits to keep

## Value

A number without leading zero

## Examples

``` r
omit_zeroes(0.2312)
#> [1] ".23"
omit_zeroes(0.2312, digits = 3)
#> [1] ".231"
omit_zeroes("000.2312", digits = 1)
#> [1] ".2"
```
