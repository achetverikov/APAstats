# Sum with na.rm=T

Sum with na.rm=T

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

sum of x with NA removed

## Examples

``` r
x <- c(NA, 10, 90)
sum(x)
#> [1] NA
sum_nn(x)
#> [1] 100
```
