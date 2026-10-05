# Describe lmerTest results

Note that this function uses *summary* object from lmerTest::lmer model
and not the model itself (see examples). Otherwise p-values will be
computed during the call and everything will be very slow.

## Usage

``` r
# S3 method for class 'summary.merMod'
apa(obj, factor, dtype = "t", ...)

# S3 method for class 'lmert'
apa(obj, factor, dtype = "t", ...)
```

## Arguments

- obj:

  *summary* object from
  [lmerTest::lmer](https://rdrr.io/pkg/lmerTest/man/lmer.html) model

- factor:

  name or number of the factor that needs to be described

- dtype:

  description type ("B"/"t")

- ...:

  other parameters passed to
  [format_results](https://achetverikov.github.io/APAstats/reference/format_results.md)

## Value

Formatted string

## Examples

``` r
if (requireNamespace("lmerTest", quietly = TRUE)) {
  # Fit a mixed-effects model with p-values
  fm <- lmerTest::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  fms <- summary(fm)
  
  # Format results in APA style
  apa(fms, "Days")
  apa(fms, "(Intercept)")
  apa(fms, 2)
  apa(fms, "Days", "B")
}
#> [1] "_B_ = 10.47 (1.55), _p_ < .001"
```
