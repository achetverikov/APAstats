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

results of \\\chi^2\\ or *F* test

## Examples

``` r
if (requireNamespace("car", quietly = TRUE) &&
    requireNamespace("carData", quietly = TRUE)) {
  # Create a linear model
  mod.davis <- lm(weight ~ repwt, data = carData::Davis)
  
  # Test hypotheses
  res <- car::linearHypothesis(mod.davis, c("(Intercept) = 0", "repwt = 1"))
  apa(res)
  
  # Test using Chi-square
  res.chi <- car::linearHypothesis(mod.davis, c("(Intercept) = 0", "repwt = 1"), test = "Chisq")
  apa(res.chi)
}
#> [1] "$\\chi^2$(2) = 3.47, _p_ = .176"
```
