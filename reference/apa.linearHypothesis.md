# Describe linearHypothesis test results

Describe linearHypothesis test results

## Usage

``` r
# S3 method for class 'linearHypothesis'
apa(obj, ...)
```

## Arguments

- obj:

  hypothesis from
  [car::linearHypothesis](https://rdrr.io/pkg/car/man/linearHypothesis.html)

- ...:

  additional parameters passed to
  [format_results](https://achetverikov.github.io/APAstats/reference/format_results.md)

## Value

A character result reporting the chi-squared or F statistic, degrees of
freedom, and p-value for the linear-hypothesis test.

## Examples

``` r
if (requireNamespace("car", quietly = TRUE) &&
    requireNamespace("carData", quietly = TRUE)) {
  # Create a linear model
  mod.davis <- lm(weight ~ repwt, data = carData::Davis)
  
  # Test hypotheses
  res <- car::linearHypothesis(mod.davis, c("(Intercept) = 0", "repwt = 1"))
  print(apa(res))
  
  # Test using Chi-square
  res.chi <- car::linearHypothesis(mod.davis, c("(Intercept) = 0", "repwt = 1"), test = "Chisq")
  print(apa(res.chi))
}
#> [1] "_F_(2, 181) = 1.74, _p_ = .179"
#> [1] "$\\chi^2$(2) = 3.47, _p_ = .176"
```
