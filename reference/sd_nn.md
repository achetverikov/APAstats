# SD with na.rm=T

SD with na.rm=T

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

sd of x with NA removed

## Examples

``` r
x <- c(NA, 10, 90)
sd(x)
#> [1] NA
sd_nn(x)
#> [1] 56.56854
```
