# Describe Hartigans' dip test results

Describe Hartigans' dip test results

## Usage

``` r
# S3 method for class 'dip.test'
apa(obj, ...)
```

## Arguments

- obj:

  a result from
  [diptest::dip.test](https://rdrr.io/pkg/diptest/man/dip.test.html)

- ...:

  other parameters passed to
  [format_results](https://achetverikov.github.io/APAstats/reference/format_results.md)

## Value

formatted results string

## Examples

``` r
if (requireNamespace("diptest", quietly = TRUE)) {
  # Generate bimodal data
  bimodal_data <- c(rnorm(100, -2, 1), rnorm(100, 2, 1))
  
  # Perform dip test
  dip_result <- diptest::dip.test(bimodal_data)
  
  # Format results in APA style
  apa(dip_result)
}
#> [1] "_D_ = 0.05, _p_ < .001"
```
