# Sum with na.rm=TRUE

Sum with na.rm=TRUE

## Usage

``` r
sum_nn(x, ...)
```

## Arguments

- x:

  a vector of numbers

- ...:

  other arguments passed to sum

## Value

A numeric or complex scalar giving the sum of `x`, with missing values
removed.

## Examples

``` r
x <- c(NA, 10, 90)
sum(x)
#> [1] NA
sum_nn(x)
#> [1] 100
```
