# Describe BayesFactor results

Describe BayesFactor results

## Usage

``` r
# S3 method for class 'BFBayesFactor'
apa(obj, digits = 2, top_limit = 10000, convert_to_power = TRUE, ...)
```

## Arguments

- obj:

  an object of
  [BayesFactor::BayesFactor](https://rdrr.io/pkg/BayesFactor/man/BayesFactor-package.html)
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

string describing the result

## Note

Code for converting to exponential notation is based on
https://dankelley.github.io/r/2015/03/22/scinot.html

## Examples

``` r
if (requireNamespace("BayesFactor", quietly = TRUE)) {
  # Load the puzzle data from BayesFactor package
  data(puzzles, package = "BayesFactor")
  
  # Run Bayesian ANOVA
  bfs <- BayesFactor::anovaBF(RT ~ shape * color + ID, data = puzzles, progress = FALSE)
  apa(bfs[1])
  apa(bfs[2] / bfs[14])
}
#> [1] "_BF_ = $2.21\\times 10^{-6}$"
```
