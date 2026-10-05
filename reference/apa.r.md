# Format correlation test results in APA style

Format correlation test results in APA style

## Usage

``` r
# S3 method for class 'r'
apa(obj, ...)
```

## Arguments

- obj:

  An object from
  [stats::cor.test](https://rdrr.io/r/stats/cor.test.html)

- ...:

  Other arguments passed to
  [format_results](https://achetverikov.github.io/APAstats/reference/format_results.md)

## Value

A string with correlation coefficient, degrees of freedom, and p-value

## Examples

``` r
# Pearson correlation
x <- rnorm(40)
y <- x * 0.6 + rnorm(40, 0, 0.8)
rc <- cor.test(x, y)
apa(rc)
#> [1] "_r_(38) = 0.80, _p_ < .001"
```
