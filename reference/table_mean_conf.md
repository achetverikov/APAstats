# Get a list with means and confidence intervals

Get a list with means and confidence intervals

## Usage

``` r
table_mean_conf(x, digits = 2, binom = FALSE, ...)
```

## Arguments

- x:

  numeric vector to compute the mean for

- digits:

  number of digits in results (default: 2)

- binom:

  compute binomial CI instead of the usual ones

- ...:

  other parameters passed to
  [format_results](https://achetverikov.github.io/APAstats/reference/format_results.md)

## Value

a list with a mean and CI as formatted strings

## Examples

``` r
# For continuous data
x <- rnorm(100, mean = 50, sd = 10)
table_mean_conf(x)
#> [1] "48.77"          "[46.94, 50.70]"
table_mean_conf(x, digits = 3)
#> [1] "48.767"           "[46.887, 50.668]"

# For binary data
y <- rbinom(100, 1, prob = 0.7)
table_mean_conf(y, binom = TRUE)
#> [1] "0.64"         "[0.54, 0.73]"
```
