# Describe lmer in-text

A shortcut for apa.glm(..., dtype=4)

## Usage

``` r
ins_lmer(obj, term = NULL, digits = 2, adj.digits = TRUE)
```

## Arguments

- obj:

  LMER model from [lme4::lmer](https://rdrr.io/pkg/lme4/man/lmer.html)

- term:

  model term to describe (a string with the term name or its sequential
  number)

- digits:

  number of digits for B and SD

- adj.digits:

  automatically adjusts digits so that B would not show up as "0.00"

## Value

A character result for a selected `term`, or a `data.frame` of
coefficient summaries when `term = NULL`. The table has the columns
documented in
[apa.glm](https://achetverikov.github.io/APAstats/reference/apa.glm.md);
`str` contains estimates, standard errors, and test statistics formatted
for in-text reporting.

## Examples

``` r
if (requireNamespace("lme4", quietly = TRUE)) {
  # Fit a mixed-effects model
  fm <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  
  # Concise in-text reporting
  print(ins_lmer(fm, "Days"))
  print(ins_lmer(fm, "(Intercept)", digits = 1))
}
#> [1] "_B_ = 10.47 (1.55), _t_ =  6.77"
#> [1] "_B_ = 251.4 (6.8), _t_ = 36.84"
```
