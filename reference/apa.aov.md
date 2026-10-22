# Describe [stats::aov](https://rdrr.io/r/stats/aov.html) results

[`stats::aov()`](https://rdrr.io/r/stats/aov.html) models containing
`Error()` return an `aovlist` object, which cannot be passed to
[`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html). This method
extracts the ANOVA tables from each error stratum and reports effects
using the residual degrees of freedom from the same stratum.

## Usage

``` r
# S3 method for class 'aov'
apa(obj, term, sstype = 2, ...)

# S3 method for class 'aovlist'
apa(obj, term = NULL, f.digits = 2, ...)
```

## Arguments

- obj:

  fitted [stats::aov](https://rdrr.io/r/stats/aov.html) model with an
  `Error()` term

- term:

  model term to describe. A character term name or sequential number can
  be supplied; if `NULL`, all testable effects are returned.

- sstype:

  anova SS type (e.g., 2 or 3)

- ...:

  other parameters passed to
  [format_results](https://achetverikov.github.io/APAstats/reference/format_results.md)

- f.digits:

  number of digits for the F value

## Value

formatted string with F(df_numerator, df_denominator) = F_value, p =/\<
p_value

one or more formatted strings with F, numerator and denominator df, and
p values

## Examples

``` r
if (requireNamespace("car", quietly = TRUE)) {
  # Using the mtcars dataset
  fit <- aov(mpg ~ cyl * am, data = mtcars)
  print(apa(fit, 'cyl'))
  print(apa(fit, 'am'))
  print(apa(fit, 'cyl:am'))

  # Using a different SS type
  print(apa(fit, 'cyl', sstype = 3))
}
#> [1] "_F_(1, 28) = 52.03, _p_ < .001"
#> [1] "_F_(1, 28) = 4.28, _p_ = .048"
#> [1] "_F_(1, 28) = 3.41, _p_ = .076"
#> [1] "_F_(1, 28) = 19.40, _p_ < .001"
fit_rm <- aov(yield ~ N * P * K + Error(block), data = npk)
apa(fit_rm, "N")
#>                                N 
#> "_F_(1, 12) = 12.26, _p_ = .004" 
apa(fit_rm)
#>                            N:P:K                                N 
#>   "_F_(1, 4) = 0.48, _p_ = .525" "_F_(1, 12) = 12.26, _p_ = .004" 
#>                                P                                K 
#>  "_F_(1, 12) = 0.54, _p_ = .475"  "_F_(1, 12) = 6.17, _p_ = .029" 
#>                              N:P                              N:K 
#>  "_F_(1, 12) = 1.38, _p_ = .263"  "_F_(1, 12) = 2.15, _p_ = .169" 
#>                              P:K 
#>  "_F_(1, 12) = 0.03, _p_ = .863" 
```
