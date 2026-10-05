# Format binomial proportions and confidence intervals in APA style

Format binomial proportions and confidence intervals in APA style

## Usage

``` r
apa_binom_mean_conf(x, digits = 2, ...)
```

## Arguments

- x:

  A vector of zeros and ones

- digits:

  Number of digits in results

- ...:

  Additional arguments passed to
  [format_results](https://achetverikov.github.io/APAstats/reference/format_results.md)

## Value

A string with the mean and confidence interval in square brackets

## Examples

``` r
# Generate binomial data
set.seed(123)
x <- rbinom(500, 1, prob = 0.7)

# Format results in APA style
apa_binom_mean_conf(x)
#> [1] "_M_ = 0.71 [0.67, 0.75]"
```
