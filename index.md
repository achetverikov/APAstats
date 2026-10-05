# apastats2

Utilities for APA-style reporting of statistical results, plus helper
functions for confidence intervals, summaries, and plotting.

## Install

``` r

install.packages("remotes")
remotes::install_github("achetverikov/apastats")
```

## Transition from apastats to apastats2

If you are migrating old scripts:

1.  Install/load the new package:

``` r

remotes::install_github("achetverikov/apastats")
library(apastats2)
```

2.  Prefer
    [`apa()`](https://achetverikov.github.io/APAstats/reference/apa.md)
    and `apa_*` functions in new code.
3.  Legacy `describe.*` functions still work for now, but are deprecated
    and planned for removal.
4.  Legacy dot-named utility functions are also deprecated; use
    snake_case equivalents (see mapping below).

In most cases, replacing
[`library(apastats)`](https://rdrr.io/r/base/library.html) with
[`library(apastats2)`](https://achetverikov.github.io/APAstats/) and
then progressively updating deprecated calls is enough.

## Quick Example

``` r

library(apastats2)

t_res <- t.test(rnorm(20, mean = 10, sd = 2))
apa(t_res)
```

## Example Output

How the results look when the Markdown strings are rendered in R
Markdown or Quarto:

![apastats2 output for a t-test, correlation, chi-square test, ANOVA,
mixed model, Bayes factor, and mean with confidence interval, as
rendered text](reference/figures/readme-output.png)

apastats2 output for a t-test, correlation, chi-square test, ANOVA,
mixed model, Bayes factor, and mean with confidence interval, as
rendered text

Source:
[`example/readme_output.Rmd`](https://achetverikov.github.io/APAstats/example/readme_output.Rmd)
(built-in R datasets).

## Main Function Families

- [`apa()`](https://achetverikov.github.io/APAstats/reference/apa.md):
  formatted output for many model/test objects via S3 methods
- `apa_*`: APA-formatted means, confidence intervals, test statistics
- [`get_adjusted_ci()`](https://achetverikov.github.io/APAstats/reference/get_adjusted_ci.md):
  confidence intervals/SEs for between-subject, within-subject, and
  mixed designs
- [`plot_pointrange()`](https://achetverikov.github.io/APAstats/reference/plot_pointrange.md):
  point-range plotting using the same adjusted-interval engine
- misc utilities
  ([`round_p()`](https://achetverikov.github.io/APAstats/reference/round_p.md),
  [`f_round()`](https://achetverikov.github.io/APAstats/reference/f_round.md),
  [`drop_empty_cols()`](https://achetverikov.github.io/APAstats/reference/drop_empty_cols.md),
  etc.)

## Documentation

- Website: <https://achetverikov.github.io/APAstats/>
- Function help:
  [`?apa`](https://achetverikov.github.io/APAstats/reference/apa.md),
  [`?apa_mean_conf`](https://achetverikov.github.io/APAstats/reference/apa_mean_conf.md),
  etc.
- Vignette:
  [`vignette("apastats2-intro", package = "apastats2")`](https://achetverikov.github.io/APAstats/articles/apastats2-intro.md)

## Notes

Some advanced methods rely on suggested packages (for example `ez`,
`lme4`, and `emmeans`).

## Deprecated Mappings

Legacy `describe.*` wrappers are deprecated and will be removed in the
next release. Use the following replacements:

- [`describe.ttest()`](https://achetverikov.github.io/APAstats/reference/apastats-deprecated.md)
  -\>
  [`apa()`](https://achetverikov.github.io/APAstats/reference/apa.md)
  (for `t.test` objects)
- [`describe.r()`](https://achetverikov.github.io/APAstats/reference/apastats-deprecated.md)
  -\>
  [`apa()`](https://achetverikov.github.io/APAstats/reference/apa.md)
  (for `cor.test` objects)
- [`describe.chi()`](https://achetverikov.github.io/APAstats/reference/apastats-deprecated.md)
  -\>
  [`apa()`](https://achetverikov.github.io/APAstats/reference/apa.md)
  (for `chisq.test` objects)
- [`describe.glm()`](https://achetverikov.github.io/APAstats/reference/apastats-deprecated.md)
  -\>
  [`apa()`](https://achetverikov.github.io/APAstats/reference/apa.md)
  (for `glm`/`lm` objects)
- [`describe.mean.sd()`](https://achetverikov.github.io/APAstats/reference/apastats-deprecated.md)
  -\>
  [`apa_mean_sd()`](https://achetverikov.github.io/APAstats/reference/apa_mean_sd.md)
- [`describe.mean.conf()`](https://achetverikov.github.io/APAstats/reference/apastats-deprecated.md)
  -\>
  [`apa_mean_conf()`](https://achetverikov.github.io/APAstats/reference/apa_mean_conf.md)
- [`describe.binom.mean.conf()`](https://achetverikov.github.io/APAstats/reference/apastats-deprecated.md)
  -\>
  [`apa_binom_mean_conf()`](https://achetverikov.github.io/APAstats/reference/apa_binom_mean_conf.md)
- [`describe.mean.and.t()`](https://achetverikov.github.io/APAstats/reference/apastats-deprecated.md)
  -\>
  [`apa_mean_and_t()`](https://achetverikov.github.io/APAstats/reference/apa_mean_and_t.md)

Additional deprecated compatibility mapping:

- [`get_superb_ci()`](https://achetverikov.github.io/APAstats/reference/get_adjusted_ci.md)
  -\>
  [`get_adjusted_ci()`](https://achetverikov.github.io/APAstats/reference/get_adjusted_ci.md)

For utility helpers, snake_case aliases are now available and dot-named
forms are legacy:

- [`mean.nn()`](https://achetverikov.github.io/APAstats/reference/apastats-deprecated.md)
  -\>
  [`mean_nn()`](https://achetverikov.github.io/APAstats/reference/mean_nn.md)
- [`sd.nn()`](https://achetverikov.github.io/APAstats/reference/apastats-deprecated.md)
  -\>
  [`sd_nn()`](https://achetverikov.github.io/APAstats/reference/sd_nn.md)
- [`sum.nn()`](https://achetverikov.github.io/APAstats/reference/apastats-deprecated.md)
  -\>
  [`sum_nn()`](https://achetverikov.github.io/APAstats/reference/sum_nn.md)
- [`drop.empty.cols()`](https://achetverikov.github.io/APAstats/reference/apastats-deprecated.md)
  -\>
  [`drop_empty_cols()`](https://achetverikov.github.io/APAstats/reference/drop_empty_cols.md)
- [`binom.ci()`](https://achetverikov.github.io/APAstats/reference/apastats-deprecated.md)
  -\>
  [`binom_ci()`](https://achetverikov.github.io/APAstats/reference/binom_ci.md)
- [`f.round()`](https://achetverikov.github.io/APAstats/reference/apastats-deprecated.md)
  -\>
  [`f_round()`](https://achetverikov.github.io/APAstats/reference/f_round.md)
- [`load.libs()`](https://achetverikov.github.io/APAstats/reference/apastats-deprecated.md)
  -\>
  [`load_libs()`](https://achetverikov.github.io/APAstats/reference/load_libs.md)
- [`lmer.fixef()`](https://achetverikov.github.io/APAstats/reference/apastats-deprecated.md)
  -\>
  [`lmer_fixef()`](https://achetverikov.github.io/APAstats/reference/lmer_fixef.md)
- [`omit.zeroes()`](https://achetverikov.github.io/APAstats/reference/apastats-deprecated.md)
  -\>
  [`omit_zeroes()`](https://achetverikov.github.io/APAstats/reference/omit_zeroes.md)
- [`base.breaks()`](https://achetverikov.github.io/APAstats/reference/apastats-deprecated.md)
  -\>
  [`base_breaks()`](https://achetverikov.github.io/APAstats/reference/base_breaks.md)
- [`base.breaks.x()`](https://achetverikov.github.io/APAstats/reference/apastats-deprecated.md)
  -\>
  [`base_breaks_x()`](https://achetverikov.github.io/APAstats/reference/base_breaks.md)
- [`base.breaks.y()`](https://achetverikov.github.io/APAstats/reference/apastats-deprecated.md)
  -\>
  [`base_breaks_y()`](https://achetverikov.github.io/APAstats/reference/base_breaks.md)
- [`plot.pointrange()`](https://achetverikov.github.io/APAstats/reference/apastats-deprecated.md)
  -\>
  [`plot_pointrange()`](https://achetverikov.github.io/APAstats/reference/plot_pointrange.md)

## Install legacy apastats

If you need the pre-`apastats2` code, install from the legacy
branch/tag:

``` r

remotes::install_github("achetverikov/apastats@legacy-apastats")
```
