# Describe [stats::aov](https://rdrr.io/r/stats/aov.html) results

Describe [stats::aov](https://rdrr.io/r/stats/aov.html) results,
including repeated-measures models fitted with an `Error()` term
(`aovlist` objects).

## Usage

``` r
# S3 method for class 'aov'
apa(obj, term, sstype = 2, ...)

# S3 method for class 'aovlist'
apa(obj, term = NULL, f.digits = 2, ...)
```

## Arguments

- obj:

  fitted [stats::aov](https://rdrr.io/r/stats/aov.html) model

- term:

  model term to describe (a string with the term name or its sequential
  number). For `aovlist` objects, `NULL` returns all testable effects.

- sstype:

  anova SS type (e.g., 2 or 3)

- f.digits:

  number of digits for the F value

- ...:

  other parameters passed to
  [apa.anova](https://achetverikov.github.io/APAstats/reference/apa.anova.md)

## Value

formatted string with F(df_numerator, df_denominator) = F_value, p =/\<
p_value

## Examples

``` r
if (requireNamespace("car", quietly = TRUE)) {
  # Using the mtcars dataset
  fit <- aov(mpg ~ cyl * am, data = mtcars)
  apa(fit, 'cyl')
  apa(fit, 'am')
  apa(fit, 'cyl:am')

  # Using a different SS type
  apa(fit, 'cyl', sstype = 3)
}
#> [1] "_F_(1, 28) = 19.40, _p_ < .001"

# Repeated-measures aov with an Error() term
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
