# Format t-test results in APA style

Format t-test results in APA style

## Usage

``` r
# S3 method for class 'htest'
apa(obj, show.mean = FALSE, abs = FALSE, ...)
```

## Arguments

- obj:

  An object from [stats::t.test](https://rdrr.io/r/stats/t.test.html)

- show.mean:

  Include mean value in results (useful for one-sample test)

- abs:

  Show the absolute value of t-statistic

- ...:

  Other arguments passed to
  [format_results](https://achetverikov.github.io/APAstats/reference/format_results.md)

## Value

A string with t-test value, degrees of freedom, p-value, and optionally
the mean with CI

## Examples

``` r
# One-sample t-test
t_res <- t.test(rnorm(20, mean = -10, sd = 2))
apa(t_res)
#> [1] "_t_(19.0) = -29.19, _p_ < .001"
apa(t_res, show.mean = TRUE)
#> [1] "_M_ = -10.37 [-11.11, -9.62], _t_(19.0) = -29.19, _p_ < .001"

# Two-sample t-test
t_two <- t.test(rnorm(20), rnorm(20, mean = 0.8))
apa(t_two)
#> [1] "_t_(34.8) = -1.52, _p_ = .137"
```
