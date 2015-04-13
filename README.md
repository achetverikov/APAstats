# R functions for formatting results in APA style and other stuff

## What is it? 

Functions for APA-style formatting for statistical tests results in markdown or LaTeX and other miscellanous stuff.

## How to use? 

Most of the functions in functions.R file have names like "describe.something" where something is a name of statistical test or R function. 
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

## Disclaimer

Everything is provided as is, contributions are welcome, authors of borrowed functions are mentioned in functions descriptions. 

