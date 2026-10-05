# Mean with na.rm=T

Mean with na.rm=T

## Usage

``` r
mean_nn(x, ...)
```

## Arguments

- x:

  a vector of numbers

- ...:

  other arguments passed to mean

## Value

mean of x with NA removed

## Examples

``` r
x <- c(NA, 10, 90)
mean(x)
#> [1] NA
mean_nn(x)
#> [1] 50
```
