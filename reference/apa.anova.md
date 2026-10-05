# Describe the results of ANOVA models or model comparisons

Describe the results of ANOVA models or model comparisons

## Usage

``` r
# S3 method for class 'anova'
apa(obj, term = 2, f.digits = 2, ...)
```

## Arguments

- obj:

  ANOVA results from
  [car::Anova](https://rdrr.io/pkg/car/man/Anova.html) or
  [stats::anova](https://rdrr.io/r/stats/anova.html)

- term:

  model term to describe (a string with the term name or its sequential
  number, default: 2)

- f.digits:

  number of digits in the results (default: 2)

- ...:

  other parameters passed to
  [format_results](https://achetverikov.github.io/APAstats/reference/format_results.md)

## Value

Formatted string with F (or chi2), df, and p

## Details

When using model comparison version, `term` can only be a number.

## Examples

``` r
if (requireNamespace("carData", quietly = TRUE)) {
  # Model comparison version
  mod1 <- lm(conformity ~ 1, data = carData::Moore)
  mod2 <- lm(conformity ~ fcategory, data = carData::Moore,
             contrasts = list(fcategory = contr.sum))
  mod3 <- lm(conformity ~ fcategory * partner.status, data = carData::Moore,
             contrasts = list(fcategory = contr.sum, partner.status = contr.sum))
  anova_obj <- anova(mod1, mod2, mod3)

  apa(anova_obj)
  apa(anova_obj, 3)

  # car::Anova version
  if (requireNamespace("car", quietly = TRUE)) {
    mod <- lm(conformity ~ fcategory * partner.status, data = carData::Moore,
              contrasts = list(fcategory = contr.sum, partner.status = contr.sum))
    afit <- car::Anova(mod)

    apa(afit, "fcategory")
    apa(afit, 2, 4)
  }
}
#> [1] "_F_(1, 39) = 10.1207, _p_ = .003"
```
