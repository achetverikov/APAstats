# Format chi-square test results in APA style

Format chi-square test results in APA style

## Usage

``` r
# S3 method for class 'chisq.test'
apa(obj, v = TRUE, addN = TRUE, ...)
```

## Arguments

- obj:

  A result from
  [stats::chisq.test](https://rdrr.io/r/stats/chisq.test.html)

- v:

  Add Cramer's V (default: TRUE)

- addN:

  Add N (default: TRUE)

- ...:

  Other parameters passed to
  [format_results](https://achetverikov.github.io/APAstats/reference/format_results.md)

## Value

Formatted results string

## Examples

``` r
# Create a contingency table
M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
dimnames(M) <- list(
  gender = c("F", "M"),
  party = c("Democrat", "Independent", "Republican")
)
chi_result <- chisq.test(M)
apa(chi_result)
#> [1] "$\\chi^2$(2, _N_ = 2757) = 30.07, _p_ < .001, _V_ = .10"
```
