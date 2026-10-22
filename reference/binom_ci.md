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

A named numeric vector: `y` is the estimated proportion, `ymin` and
`ymax` are its 95% confidence limits, and `len` is the sample size.

## Examples

``` r
binom_ci(rbinom(500, 1, prob = 0.7))
#>           y        ymin        ymax         len 
#>   0.6860000   0.6440316   0.7251321 500.0000000 
```
