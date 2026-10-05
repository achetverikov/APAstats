# Describe regression model (GLM, GLMer, lm, lm.circular, ...)

Describe regression model (GLM, GLMer, lm, lm.circular, ...)

## Usage

``` r
# S3 method for class 'glm'
apa(
  obj,
  term = NULL,
  dtype = 1,
  b.digits = 2,
  t.digits = 2,
  test.df = FALSE,
  p.as.number = FALSE,
  term.pattern = NULL,
  eff.size = FALSE,
  adj.digits = FALSE,
  ...
)

# S3 method for class 'lm'
apa(
  obj,
  term = NULL,
  dtype = 1,
  b.digits = 2,
  t.digits = 2,
  test.df = FALSE,
  p.as.number = FALSE,
  term.pattern = NULL,
  eff.size = FALSE,
  adj.digits = FALSE,
  ...
)

# S3 method for class 'glmerMod'
apa(
  obj,
  term = NULL,
  dtype = 1,
  b.digits = 2,
  t.digits = 2,
  test.df = FALSE,
  p.as.number = FALSE,
  term.pattern = NULL,
  eff.size = FALSE,
  adj.digits = FALSE,
  ...
)

# S3 method for class 'lmerModLmerTest'
apa(
  obj,
  term = NULL,
  dtype = 1,
  b.digits = 2,
  t.digits = 2,
  test.df = FALSE,
  p.as.number = FALSE,
  term.pattern = NULL,
  eff.size = FALSE,
  adj.digits = FALSE,
  ...
)
```

## Arguments

- obj:

  model object from [stats::glm](https://rdrr.io/r/stats/glm.html),
  [stats::lm](https://rdrr.io/r/stats/lm.html),
  [lme4::lmer](https://rdrr.io/pkg/lme4/man/lmer.html),
  [lme4::glmer](https://rdrr.io/pkg/lme4/man/glmer.html),
  [lmerTest::lmer](https://rdrr.io/pkg/lmerTest/man/lmer.html), etc.

- term:

  model term to describe (a string with the term name or its sequential
  number); if `NULL`, returns a formatted summary for all coefficients

- dtype:

  description type (1: t, p; 2: B(SE), p; 3: B, SE, t, p; or other: B
  (SE), t)

- b.digits:

  how many digits to use for *B* and *SE*

- t.digits:

  how many digits to use for *t*

- test.df:

  should we include degrees of freedom in description?

- p.as.number:

  should the p-values be transformed to numbers (T) or shown as strings
  (F)?

- term.pattern:

  return only the model terms matching the regex pattern (grepl is used)

- eff.size:

  should we include effect size (currently implemented only for simple
  regression)?

- adj.digits:

  automatically adjusts digits so that B or SE would not show up as
  "0.00"

- ...:

  other parameters passed to
  [format_results](https://achetverikov.github.io/APAstats/reference/format_results.md)

## Value

result

## Examples

``` r
# Using the Animals dataset from MASS package
Animals <- MASS::Animals
Animals$body <- log(Animals$body)
Animals$brain <- log(Animals$brain)

# Fit a linear model
fit <- lm(brain ~ body, Animals)

# Format results in different styles
apa(fit, "body")
#> [1] "_t_ = 6.35, _p_ < .001"
apa(fit, "(Intercept)")
#> [1] "_t_ = 6.18, _p_ < .001"
apa(fit, "body", 2)   # B(SE), p format
#> [1] "_B_ = 0.50 (0.08), _p_ < .001"
apa(fit, "body", 3)   # B, SE, t, p format
#> [1] "_B_ = 0.50, _SE_ = 0.08, _t_ = 6.35, _p_ < .001"
apa(fit, "body", 4)   # B(SE), t format
#> [1] "_B_ = 0.50 (0.08), _t_ = 6.35"
apa(fit, "body", 3, test.df = TRUE)  # Include df in the output
#> [1] "_B_ = 0.50, _SE_ = 0.08, _t_(26) = 6.35, _p_ < .001"

# Full model summary for all coefficients
apa(fit)
#>                B   SE Stat      p         eff                    str
#> (Intercept) 2.55 0.41 6.18 < .001 (Intercept) _t_ = 6.18, _p_ < .001
#> body        0.50 0.08 6.35 < .001        body _t_ = 6.35, _p_ < .001

# With effect size (requires rockchalk package)
if (requireNamespace("rockchalk", quietly = TRUE)) {
  apa(fit, "body", 4, eff.size = TRUE)
}
#> The deltaR-square values: the change in the R-square
#>       observed when a single term is removed.
#> Same as the square of the 'semi-partial correlation coefficient'
#>      deltaRsquare
#> body    0.6076101
#> [1] "_B_ = 0.50 (0.08), _t_ = 6.35, _R__{part}^2= .61"

if (requireNamespace("lmerTest", quietly = TRUE)) {
  fm <- lmerTest::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  apa(fm, "Days", test.df = TRUE)
}
#> [1] "_t_(17.00) =  6.77, _p_ < .001"

if (requireNamespace("lme4", quietly = TRUE)) {
  gm <- lme4::glmer(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    data = lme4::cbpp,
    family = binomial
  )
  apa(gm, "period2")
}
#> [1] "_Z_ = -3.27, _p_ = .001"
```
