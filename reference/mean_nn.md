# Mean with na.rm=TRUE

Mean with na.rm=TRUE

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

A numeric or complex scalar giving the mean of `x`, with missing values
removed.

## Examples

``` r
x <- c(NA, 10, 90)
mean(x)
#> [1] NA
mean_nn(x)
#> [1] 50
```
