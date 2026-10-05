# Format statistical results in APA style

The `apa` function provides a convenient way to format statistical
results according to American Psychological Association (APA) style
guidelines. It dispatches to various S3 methods depending on the class
of the input object.

## Usage

``` r
apa(obj, ...)
```

## Arguments

- obj:

  An object to format in APA style

- ...:

  Additional arguments passed to specific methods

## Value

A formatted string in APA style

## Details

This function supports various statistical objects directly:

- [apa.ttest](https://achetverikov.github.io/APAstats/reference/apa.ttest.md)
  for [stats::t.test](https://rdrr.io/r/stats/t.test.html) and
  [apa.r](https://achetverikov.github.io/APAstats/reference/apa.r.md)
  for [stats::cor.test](https://rdrr.io/r/stats/cor.test.html) (both
  with class `htest`)

- [apa.aov](https://achetverikov.github.io/APAstats/reference/apa.aov.md)
  for [stats::aov](https://rdrr.io/r/stats/aov.html),
  [apa.anova](https://achetverikov.github.io/APAstats/reference/apa.anova.md)
  for [car::Anova](https://rdrr.io/pkg/car/man/Anova.html), and
  [apa.afex_aov](https://achetverikov.github.io/APAstats/reference/apa.afex_aov.md)
  for [afex::aov_ez](https://rdrr.io/pkg/afex/man/aov_car.html),
  [afex::aov_car](https://rdrr.io/pkg/afex/man/aov_car.html), or
  [afex::aov_4](https://rdrr.io/pkg/afex/man/aov_car.html)

- [apa.glm](https://achetverikov.github.io/APAstats/reference/apa.glm.md)
  for [stats::glm](https://rdrr.io/r/stats/glm.html),
  [stats::lm](https://rdrr.io/r/stats/lm.html),
  [lme4::lmer](https://rdrr.io/pkg/lme4/man/lmer.html),
  [lme4::glmer](https://rdrr.io/pkg/lme4/man/glmer.html), and
  [lmerTest::lmer](https://rdrr.io/pkg/lmerTest/man/lmer.html);
  [apa.lmert](https://achetverikov.github.io/APAstats/reference/apa.summary.merMod.md)
  remains available for summary objects from
  [lmerTest::lmer](https://rdrr.io/pkg/lmerTest/man/lmer.html)

- [apa.brmsfit](https://achetverikov.github.io/APAstats/reference/apa.brmsfit.md)
  for [brms::brm](https://paulbuerkner.com/brms/reference/brm.html) and
  [apa.BFBayesFactor](https://achetverikov.github.io/APAstats/reference/apa.BFBayesFactor.md)
  for
  [BayesFactor::anovaBF](https://rdrr.io/pkg/BayesFactor/man/anovaBF.html)

- [apa.chisq.test](https://achetverikov.github.io/APAstats/reference/apa.chisq.test.md)
  for [stats::chisq.test](https://rdrr.io/r/stats/chisq.test.html)

- [apa.dip.test](https://achetverikov.github.io/APAstats/reference/apa.dip.test.md)
  for [diptest::dip.test](https://rdrr.io/pkg/diptest/man/dip.test.html)

- [apa.roc.test](https://achetverikov.github.io/APAstats/reference/apa.roc.test.md)
  for [pROC::roc.test](https://rdrr.io/pkg/pROC/man/roc.test.html)

In addition, there are standalone utility functions for formatting:

- [apa_mean_sd](https://achetverikov.github.io/APAstats/reference/apa_mean_sd.md):
  Format means and standard deviations

- [apa_mean_conf](https://achetverikov.github.io/APAstats/reference/apa_mean_conf.md):
  Format means and confidence intervals

- [apa_binom_mean_conf](https://achetverikov.github.io/APAstats/reference/apa_binom_mean_conf.md):
  Format binomial means and confidence intervals

- [apa_mean_and_t](https://achetverikov.github.io/APAstats/reference/apa_mean_and_t.md):
  Compare means with t-tests and format results

- [table_mean_conf](https://achetverikov.github.io/APAstats/reference/table_mean_conf.md):
  Get formatted means and CIs (for tables)

- [table_mean_conf_by](https://achetverikov.github.io/APAstats/reference/table_mean_conf_by.md):
  Get formatted means and CIs by group (for tables)

## Examples

``` r
# t-test example
t_res <- t.test(rnorm(30), rnorm(30, 0.5))
apa(t_res)
#> [1] "_t_(55.1) = -1.47, _p_ = .148"

# Correlation example
cor_res <- cor.test(mtcars$mpg, mtcars$wt)
apa(cor_res)
#> [1] "_r_(30) = -0.87, _p_ < .001"
```
