# Get nice matrix of fixed effects from lmer

Get nice matrix of fixed effects from lmer

## Usage

``` r
lmer_fixef(fit.lmer)
```

## Arguments

- fit.lmer:

  fitted [`lme4::lmer`](https://rdrr.io/pkg/lme4/man/lmer.html) model
  object

## Value

A `data.frame` with one row per fixed-effect coefficient and columns
`Estimate` (coefficient), `Std.Err` (standard error), and `t` (the ratio
of the estimate to its standard error). Row names identify model terms.
