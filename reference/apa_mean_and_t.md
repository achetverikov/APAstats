# Compare means with t-tests and format in APA style

Compare means with t-tests and format in APA style

## Usage

``` r
apa_mean_and_t(
  x,
  by,
  which.mean = 1,
  digits = 2,
  paired = FALSE,
  eff.size = FALSE,
  abs = FALSE,
  aggregate_by = NULL,
  transform.means = NULL,
  ...
)
```

## Arguments

- x:

  Dependent variable (numeric vector)

- by:

  Independent variable (factor with 2 levels)

- which.mean:

  Which means to show (0=none, 1=first, 2=second, 3=both)

- digits:

  Number of digits in results (default: 2)

- paired:

  Should it be a paired test (default: FALSE)

- eff.size:

  Should we include effect size (default: FALSE)

- abs:

  Should we show the absolute value if the t-test (T) or keep its sign
  (FALSE, default)

- aggregate_by:

  Do the aggregation by the third variable(s): either NULL (default), a
  single vector variable, or a list of variables to aggregate by.

- transform.means:

  A function to transform means and confidence intervals (default: NULL)

- ...:

  Additional arguments passed to
  [format_results](https://achetverikov.github.io/APAstats/reference/format_results.md)

## Value

A formatted string with t-test results in APA style

## Examples

``` r
# Using simulated data
rt <- rnorm(100)
gr <- factor(rep(c("A", "B"), each = 50))
apa_mean_and_t(rt, gr)
#> [1] "_M_ = 0.25 [0.01, 0.51], _t_(97.2) = 1.25, _p_ = .214"
apa_mean_and_t(rt, gr, which.mean = 3)
#> [1] "_M_ = 0.25 [-0.01, 0.50] vs. _M_ = 0.00 [-0.28, 0.28], _t_(97.2) = 1.25, _p_ = .214"
apa_mean_and_t(rt, gr, eff.size = TRUE)
#> [1] "_M_ = 0.25 [-0.02, 0.53], _t_(97.2) = 1.25, _p_ = .214, _d_ = 0.25"
```
