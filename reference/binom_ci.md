# Binomial confidence interval as a vector

Binomial confidence interval as a vector

## Usage

``` r
binom_ci(x)
```

## Arguments

- x:

  a vector of 0 and 1

## Value

a vector of mean, lower CI, upper CI, and length of x

## Examples

``` r
binom_ci(rbinom(500, 1, prob = 0.7))
#>           y        ymin        ymax         len 
#>   0.6860000   0.6440316   0.7251321 500.0000000 
```
