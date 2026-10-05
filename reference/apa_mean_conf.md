# Format means and confidence intervals in APA style

Format means and confidence intervals in APA style

## Usage

``` r
apa_mean_conf(
  x,
  bootCI = TRUE,
  addCI = FALSE,
  digits = 2,
  transform.means = NULL,
  ...
)
```

## Arguments

- x:

  A numeric vector

- bootCI:

  Use bootstrapped CI (TRUE) or normal approximation (FALSE)

- addCI:

  Add "95% CI =" prefix

- digits:

  Number of digits to use

- transform.means:

  An optional function to transform the means and CI to another scale

- ...:

  Other arguments passed to
  [format_results](https://achetverikov.github.io/APAstats/reference/format_results.md)

## Value

A string with a mean followed by confidence intervals in square brackets

## Examples

``` r
x <- runif(100, 0, 50)
apa_mean_conf(x)
#> [1] "_M_ = 25.40 [22.66, 28.16]"
apa_mean_conf(x, bootCI = FALSE)  # Normal approximation instead of bootstrap
#> [1] "_M_ = 25.40 [22.43, 28.36]"
```
