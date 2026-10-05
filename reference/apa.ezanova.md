# Describe ezANOVA results

Provides formatted string like *F*(DFn, DFd) = ..., *p* ..., eta2 = ...
based on ezANOVA results

## Usage

``` r
# S3 method for class 'ezANOVA'
apa(
  obj,
  term,
  include_eta = TRUE,
  spher_corr = TRUE,
  eta_digits = 2,
  f_digits = 2,
  df_digits = 0,
  append_to_table = FALSE,
  ...
)
```

## Arguments

- obj:

  ezANOVA object from
  [ez::ezANOVA](https://rdrr.io/pkg/ez/man/ezANOVA.html)

- term:

  model term to describe (a string with the term name or its sequential
  number)

- include_eta:

  add eta^2 for the model (default: TRUE)

- spher_corr:

  use sphericity corrections (default: TRUE)

- eta_digits:

  number of digits to use for eta^2 (default: 2)

- f_digits:

  number of digits to use for F (default: 2)

- df_digits:

  number of digits to use for df (default: 0)

- append_to_table:

  should the results be added to the original ezANOVA table (default:
  FALSE)

- ...:

  other parameters passed to
  [format_results](https://achetverikov.github.io/APAstats/reference/format_results.md)

## Value

string with formatted results

## Examples

``` r
if (requireNamespace("ez", quietly = TRUE) && requireNamespace("afex", quietly = TRUE)) {
  # Using the Stroop dataset from afex package
  data(stroop, package = "afex")
  
  # Run ezANOVA
  ez_res <- ez::ezANOVA(
    data = stroop[!is.na(stroop$rt),],
    dv = rt,
    wid = pno,
    within = condition,
    detailed = TRUE
  )
  
  # Format results in APA style
  apa(ez_res, "condition")
  apa(ez_res, "condition", eta_digits = 3)
  apa(ez_res, "condition", include_eta = FALSE)
}
#> Warning: Collapsing data to cell means. *IF* the requested effects are a subset of the full design, you must use the "within_full" argument, else results may be inaccurate.
#> [1] "_F_(1, 684) = 3.24, _p_ = .072"
```
