# Describe [BayesFactor](https://rdrr.io/pkg/BayesFactor/man/BayesFactor-package.html) results

Describe
[BayesFactor](https://rdrr.io/pkg/BayesFactor/man/BayesFactor-package.html)
results

## Usage

``` r
# S3 method for class 'BFBayesFactor'
apa(obj, digits = 2, top_limit = 10000, convert_to_power = TRUE, ...)
```

## Arguments

- obj:

  an object of
  [BayesFactor](https://rdrr.io/pkg/BayesFactor/man/BayesFactor-package.html)
  class `BFBayesFactor`

- digits:

  number of digits to use

- top_limit:

  numbers above that limit (or below the digits limit) will be converted
  to exponential notation (if convert_to_power is TRUE)

- convert_to_power:

  enable or disable converting of very small or very large numbers to
  exponential notation

- ...:

  other parameters passed to
  [format_results](https://achetverikov.github.io/APAstats/reference/format_results.md)

## Value

A character result reporting the Bayes factor, optionally in scientific
notation for very large or small values.

## Note

Code for converting to exponential notation is based on
https://dankelley.github.io/r/2015/03/22/scinot.html

## Examples

``` r
if (requireNamespace("BayesFactor", quietly = TRUE)) {
  bf <- BayesFactor::ttestBF(x = c(0.8, 1.1, 0.9, 1.5, 1.2, 0.7))
  print(apa(bf))
  print(apa(bf, top_limit = 10))
}
#> [1] "_BF_ = 90.10"
#> [1] "_BF_ = $9.01\\times 10^{1}$"
```
