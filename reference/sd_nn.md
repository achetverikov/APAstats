# SD with na.rm=TRUE

SD with na.rm=TRUE

## Usage

``` r
sd_nn(x, ...)
```

## Arguments

- x:

  a vector of numbers

- ...:

  other arguments passed to sd

## Value

A numeric scalar giving the standard deviation of `x`, with missing
values removed.

## Examples

``` r
x <- c(NA, 10, 90)
sd(x)
#> [1] NA
sd_nn(x)
#> [1] 56.56854
```
