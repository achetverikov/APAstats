# R functions for formatting results in APA style and other stuff

## What is it? 

Functions for APA-style formatting for statistical tests results in markdown or LaTeX and other miscellanous stuff.

## Main functions

<table>
 <tr>
  <td>
  describe.Anova
  </td>
  <td>
  Describe Anova results
  </td>
 </tr>
 <tr>
  <td>
  describe.aov
  </td>
  <td>
  Describe aov results
  </td>
 </tr>
 <tr>
  <td>
  describe.bimod.test
  </td>
  <td>
  Describe bimodality test results
  </td>
 </tr>
 <tr>
  <td>
  describe.binom.mean.conf
  </td>
  <td>
  Describe mean and confidence intervals for binomial
  variable
  </td>
 </tr>
 <tr>
  <td>
  describe.chi
  </td>
  <td>
  Describe $chi^2$ results
  </td>
 </tr>
 <tr>
  <td>
  describe.dip.test
  </td>
  <td>
  Describe Hartigans' dip test results
  </td>
 </tr>
 <tr>
  <td>
  describe.ezanova
  </td>
  <td>
  Describe ezANOVA results
  </td>
 </tr>
 <tr>
  <td>
  describe.glm
  </td>
  <td>
  Describe regression model (GLM, GLMer, lm, lm.circular,
  ...)
  </td>
 </tr>
 <tr>
  <td>
  describe.lmer
  </td>
  <td>
  Describe lmer results
  </td>
 </tr>
 <tr>
  <td>
  describe.lmert
  </td>
  <td>
  Describe lmerTest results
  </td>
 </tr>
 <tr>
  <td>
  describe.lmtaov
  </td>
  <td>
  Describe lmerTest anova results
  </td>
 </tr>
 <tr>
  <td>
  describe.lsmeans
  </td>
  <td>
  Describe contrasts created by lsmeans
  </td>
 </tr>
 <tr>
  <td>
  describe.mean.and.t
  </td>
  <td>
  Describe two-sample t-test with means and effect sizes
  </td>
 </tr>
 <tr>
  <td>
  describe.mean.conf
  </td>
  <td>
  Describe mean and confidence intervals
  </td>
 </tr>
 <tr>
  <td>
  describe.mean.sd
  </td>
  <td>
  Describe mean and SD
  </td>
 </tr>
 <tr>
  <td>
  describe.r
  </td>
  <td>
  Describe Pearson test results
  </td>
 </tr>
 <tr>
  <td>
  describe.roc.diff
  </td>
  <td>
  Describe differences between ROC curves
  </td>
 </tr>
 <tr>
  <td>
  describe.ttest
  </td>
  <td>
  Describe t-test results
  </td>
 </tr>
</table>



## How to install?

```
install.packages("devtools")
devtools::install_github('ralfer/apa_format_and_misc',subdir='apastats')
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
- misc - _drop.empty.cols()_ to drop empty columns from df, _mymean()_ and _mysd()_ for means and SDs without NA, _lengthu()_ for length(unique(x))

Some examples can be found here: https://github.com/ralfer/apa_format_and_misc/blob/master/example/example.md and in help files.

## Disclaimer

Everything is provided as is, contributions are welcome, authors of borrowed functions are mentioned in functions descriptions. 

