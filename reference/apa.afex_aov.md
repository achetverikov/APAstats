# Describe [afex](https://rdrr.io/pkg/afex/man/afex-package.html) ANOVA results

Provides formatted string like *F*(num.Df, den.Df) = ..., *p* ..., eta2
= ... based on
[afex::aov_ez](https://rdrr.io/pkg/afex/man/aov_car.html),
[afex::aov_car](https://rdrr.io/pkg/afex/man/aov_car.html), or
[afex::aov_4](https://rdrr.io/pkg/afex/man/aov_car.html) results.

## Usage

``` r
# S3 method for class 'afex_aov'
apa(
  obj,
  term,
  include_eta = TRUE,
  eta_digits = 2,
  f_digits = 2,
  df_digits = 0,
  append_to_table = FALSE,
  ...
)
```

## Arguments

- obj:

  [afex](https://rdrr.io/pkg/afex/man/afex-package.html) ANOVA object

- term:

  model term to describe (a string with the term name or its sequential
  number)

- include_eta:

  add generalized eta^2 for the model (default: TRUE)

- eta_digits:

  number of digits to use for eta^2 (default: 2)

- f_digits:

  number of digits to use for F (default: 2)

- df_digits:

  number of digits to use for df (default: 0)

- append_to_table:

  should the results be added to the
  [afex](https://rdrr.io/pkg/afex/man/afex-package.html) ANOVA table
  (default: FALSE)

- ...:

  other parameters passed to
  [format_results](https://achetverikov.github.io/APAstats/reference/format_results.md)

## Value

A character vector reporting F statistics, degrees of freedom, p-values,
and optional generalized eta-squared. With `append_to_table = TRUE`, a
`data.frame` of selected ANOVA rows with an added `res` column
containing the formatted results.

## Examples

``` r
if (requireNamespace("afex", quietly = TRUE)) {
  data(stroop, package = "afex")

  afex_res <- afex::aov_ez(
    id = "pno",
    dv = "rt",
    within = "condition",
    data = stroop[!is.na(stroop$rt), ],
    fun_aggregate = mean
  )

  print(apa(afex_res, "condition"))
  print(apa(afex_res, "condition", eta_digits = 3))
  print(apa(afex_res, "condition", include_eta = FALSE))
}
#> [1] "_F_(1, 684) = 3.24, _p_ = .072, $\\eta$^2^~G~ < .01"
#> [1] "_F_(1, 684) = 3.24, _p_ = .072, $\\eta$^2^~G~ < .001"
#> [1] "_F_(1, 684) = 3.24, _p_ = .072"
```
