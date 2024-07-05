# R functions for formatting results in APA style and other stuff

## What is it?

Functions for APA-style formatting for statistical tests results in markdown or LaTeX and other miscellanous stuff.

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
```

## How to use?

Most of the functions have names like "describe.something" where something is a name of statistical test or R function.

They take results of statistical test as input and provide a formatted output. So, for example, if you use repeated measures ANOVA, you can easily get something like "_F_(3, 54) = 516.61, _p_ < .001" from _ezANOVA_ results.

Methods included:
- chi-squared
- Pearson r
- t-test
- ANOVA, rm ANOVA
- lm, glm, lmer

Additional functions:
- plotting - pointrange plots with optional within-subject CIs; breaks-computing functions for Tufte-like plots
- misc - _drop.empty.cols()_ to drop empty columns from df, _mean.nn()_ and _sd.nn()_ for means and SDs without NA, _lengthu()_ for `length(unique(x))`

Some examples can be found here: https://github.com/achetverikov/apastats/blob/master/example/example.md and in help files.

## Disclaimer

Everything is provided as is, contributions are welcome, authors of borrowed functions are mentioned in functions descriptions.
