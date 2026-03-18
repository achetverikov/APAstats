# apastats2

Utilities for APA-style reporting of statistical results, plus helper functions for confidence intervals, summaries, and plotting.

## Install

<<<<<<< HEAD
## Moving to apastats2

I'm slowly transitioning the package to a new style, see the most up to date version here:
https://github.com/achetverikov/APAstats/tree/dev
This switch will create breaking changes. 

## Main functions
|Function|Description|
|--- | ---|
| describe.Anova | Describe Anova results|
| describe.aov | Describe aov results|
| describe.bf | Describe BayesFactor results|
| describe.bimod.test | Describe bimodality test results|
| describe.binom.mean.conf | Describe mean and confidence intervals for binomial variable|
| describe.chi | Describe $chi^2$ results|
| describe.dip.test | Describe Hartigans' dip test results|
| describe.ezanova | Describe ezANOVA results|
| describe.ezstats | Describe ezStats results|
| describe.glm | Describe regression model (GLM, GLMer, lm, lm.circular, ...)|
| describe.lht | Describe linearHypothesis test results|
| describe.lmer | Describe lmer results|
| describe.lmert | Describe lmerTest results|
| describe.lmtaov | Describe lmerTest anova results|
| describe.lsmeans | Describe contrasts created by lsmeans|
| describe.emmeans | Describe contrasts created by emmeans|
| describe.mean.and.t | Describe two-sample t-test with means and effect sizes|
| describe.mean.conf | Describe mean and confidence intervals|
| describe.mean.sd | Describe mean and SD|
| describe.r | Describe Pearson test results|
| describe.roc.diff | Describe differences between ROC curves|
| describe.ttest | Describe t-test results|



## How to install?

```
install.packages("devtools")
devtools::install_github('achetverikov/apastats')
library(apastats)
=======
```r
install.packages("remotes")
remotes::install_github("achetverikov/apastats2")
>>>>>>> dev
```

## Transition from apastats to apastats2

If you are migrating old scripts:

1. Install/load the new package:
```r
remotes::install_github("achetverikov/apastats2")
library(apastats2)
```
2. Prefer `apa()` and `apa_*` functions in new code.
3. Legacy `describe.*` functions still work for now, but are deprecated and planned for removal.
4. Legacy dot-named utility functions are also deprecated; use snake_case equivalents (see mapping below).

In most cases, replacing `library(apastats)` with `library(apastats2)` and then progressively updating deprecated calls is enough.

## Quick Example

```r
library(apastats2)

t_res <- t.test(rnorm(20, mean = 10, sd = 2))
apa(t_res)
```

## Main Function Families

- `apa()`: formatted output for many model/test objects via S3 methods
- `apa_*`: APA-formatted means, confidence intervals, test statistics
- `plot_pointrange()`: point-range plotting with optional within-subject intervals
- misc utilities (`round_p()`, `f_round()`, `drop_empty_cols()`, etc.)

## Documentation

- Function help: `?apa`, `?apa_mean_conf`, etc.
- Vignette: `vignette("apastats2-intro", package = "apastats2")`

## Notes

Some advanced methods rely on suggested packages (for example `ez`, `lme4`, `superb`, `emmeans`).

## Deprecated Mappings

Legacy `describe.*` wrappers are deprecated and will be removed in the next release.
Use the following replacements:

- `describe.ttest()` -> `apa()` (for `t.test` objects)
- `describe.r()` -> `apa()` (for `cor.test` objects)
- `describe.chi()` -> `apa()` (for `chisq.test` objects)
- `describe.glm()` -> `apa()` (for `glm`/`lm` objects)
- `describe.mean.sd()` -> `apa_mean_sd()`
- `describe.mean.conf()` -> `apa_mean_conf()`
- `describe.binom.mean.conf()` -> `apa_binom_mean_conf()`
- `describe.mean.and.t()` -> `apa_mean_and_t()`

For utility helpers, snake_case aliases are now available and dot-named forms are legacy:

- `mean.nn()` -> `mean_nn()`
- `sd.nn()` -> `sd_nn()`
- `sum.nn()` -> `sum_nn()`
- `drop.empty.cols()` -> `drop_empty_cols()`
- `binom.ci()` -> `binom_ci()`
- `f.round()` -> `f_round()`
- `load.libs()` -> `load_libs()`
- `lmer.fixef()` -> `lmer_fixef()`
- `omit.zeroes()` -> `omit_zeroes()`
- `base.breaks()` -> `base_breaks()`
- `base.breaks.x()` -> `base_breaks_x()`
- `base.breaks.y()` -> `base_breaks_y()`
- `plot.pointrange()` -> `plot_pointrange()`
