# Describe lmerTest anova results

Describe lmerTest anova results

## Usage

``` r
# S3 method for class 'anova.merMod'
apa(obj, term, f.digits = 2, ...)
```

## Arguments

- obj:

  lmerTest anova results

- term:

  model term to describe (a string with the term name or its sequential
  number)

- f.digits:

  decimal digits for F value

- ...:

  other parameters passed to
  [format_results](https://achetverikov.github.io/APAstats/reference/format_results.md)

## Value

formatted string describing the results of anova

## Examples

``` r
if (requireNamespace("lmerTest", quietly = TRUE)) {
  # Sample data
  data(sleepstudy, package = "lme4")
  
  # Fit a mixed-effects model
  fit <- lmerTest::lmer(Reaction ~ Days + (1 + Days | Subject), sleepstudy)
  
  # ANOVA table
  afit <- anova(fit)
  
  # Format results in APA style
  apa(afit, "Days")
  apa(afit, "Days", f.digits = 3)
}
#> character(0)
```
