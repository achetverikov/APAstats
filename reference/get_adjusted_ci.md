# Get adjusted confidence intervals or standard errors

Computes grouped summary statistics and precision intervals for
between-subject, within-subject, and mixed designs. With no
within-subject factors, ordinary independent-observation intervals are
computed. Supplying a subject identifier in a between-subject design
first aggregates repeated rows within subject-by-group cells.

## Usage

``` r
get_adjusted_ci(
  data,
  value_var,
  within = NULL,
  between = NULL,
  wid = NULL,
  adjustments = list(purpose = "single", decorrelation = NULL),
  errorbar = "CI",
  gamma = 0.95,
  drop_NA_subj = FALSE,
  drop_missing_levels = TRUE,
  aggr_fun = mean,
  debug = FALSE,
  ...
)

get_superb_ci(
  data,
  wid,
  within,
  value_var,
  between = NULL,
  adjustments = list(purpose = "single", decorrelation = "CM"),
  errorbar = "CI",
  drop_NA_subj = FALSE,
  drop_missing_levels = TRUE,
  aggr_fun = mean,
  debug = FALSE,
  ...
)
```

## Arguments

- data:

  dataframe to summarize

- value_var:

  dependent variable (string)

- within:

  within-subject variables (vector of strings; default: NULL). If
  supplied, `wid` must also be supplied.

- between:

  between-subject/grouping variables (vector of strings; default: NULL)

- wid:

  observational-unit/subject variable (string; default: NULL). Required
  for within-subject designs. For between-subject designs it is
  optional; if supplied, repeated rows are aggregated within
  subject/group cells before intervals are computed.

- adjustments:

  adjustment settings. Supports `purpose = "single"` or `"difference"`.
  `decorrelation` can be `"CM"` or `"none"` for within-subject designs
  and must be `"none"` for purely between-subject designs. If omitted,
  decorrelation defaults to `"CM"` when `within` is supplied and
  `"none"` otherwise.

- errorbar:

  `"CI"`, `"SE"`, `"none"`, or a precision function. A precision
  function receives the condition vector and returns either one
  error-bar width or two interval limits.

- gamma:

  confidence level for interval functions (default: 0.95)

- drop_NA_subj:

  should subjects with missing within-subject cells be dropped?
  (default: FALSE)

- drop_missing_levels:

  should unused factor levels in within/between variables be dropped?
  (default: TRUE)

- aggr_fun:

  scalar summary function. For within-subject designs, and for
  between-subject designs with `wid` supplied, it is also used to
  aggregate repeated observations within observational-unit cells.
  Built-in precision formulas mirror `superb` for `mean`, `median`,
  `var`, `sd`, `IQR`, and the superb-style `MAD`, `hmean`, `gmean`,
  `fisherskew`, `pearsonskew`, and `fisherkurtosis` names. For other
  named functions, define a matching `SE.<name>` or `CI.<name>`
  function, or supply `errorbar` as a function.

- debug:

  output additional debugging info (default: FALSE)

- ...:

  additional parameters passed to
  [apa_format_mean_conf](https://achetverikov.github.io/APAstats/reference/apa_format_mean_conf.md)

## Value

A dataframe with grouping variables, `center`, `lowerwidth`,
`upperwidth`, `lower_ci`, `upper_ci`, and `descr`.

## Details

The within-subject implementation and statistic-specific precision
conventions are adapted from the superb framework and its R
implementation (Cousineau, Goulet, & Harding, 2021). In particular, the
Cousineau-Morey path follows the two-step centering and bias-correction
algorithm used by `superb::twoStepTransform()`. The code is implemented
locally; apastats2 does not depend on or call superb.

`get_superb_ci()` is retained as a deprecated compatibility wrapper. New
code should use `get_adjusted_ci()`.

## References

Cousineau, D., Goulet, M.-A., & Harding, B. (2021). Summary plots with
adjusted error bars: The superb framework with an implementation in R.
*Advances in Methods and Practices in Psychological Science, 4*(3).
doi:10.1177/25152459211035109

Cousineau, D. (2005). Confidence intervals in within-subject designs: A
simpler solution to Loftus and Masson's method. *The Quantitative
Methods for Psychology, 1*(1), 42-45. doi:10.20982/tqmp.01.1.p042

Morey, R. D. (2008). Confidence intervals from normalized data: A
correction to Cousineau (2005). *The Quantitative Methods for
Psychology, 4*(2), 61-64. doi:10.20982/tqmp.04.2.p061

## Examples

``` r
data(memory_noise)

# Between-subject intervals: Exp. 1 vs Exp. 1 HV
target_higher <- memory_noise[
  memory_noise$relative_noise == "target more noisy",
]
get_adjusted_ci(
  target_higher,
  value_var = "bias_percent",
  between = "experiment",
  wid = "participant"
)
#>   experiment    center lowerwidth upperwidth   lower_ci   upper_ci
#> 1     Exp. 1  2.178637 -11.001061  11.001061  -8.822424 13.1796977
#> 2  Exp. 1 HV -9.331493  -9.775628   9.775628 -19.107121  0.4441344
#>                        descr
#> 1  _M_ = 2.18 [-8.82, 13.18]
#> 2 _M_ = -9.33 [-19.11, 0.44]

# Within-subject Cousineau-Morey intervals for relative noise
get_adjusted_ci(
  memory_noise[memory_noise$experiment == "Exp. 1", ],
  value_var = "bias_percent",
  within = "relative_noise",
  wid = "participant"
)
#>      relative_noise   center lowerwidth upperwidth  lower_ci upper_ci
#> 1 target less noisy 3.082070  -9.684937   9.684937 -6.602867 12.76701
#> 2 target more noisy 2.178637  -9.684937   9.684937 -7.506300 11.86357
#>                       descr
#> 1 _M_ = 3.08 [-6.60, 12.77]
#> 2 _M_ = 2.18 [-7.51, 11.86]

# Mixed design: experiment is between, relative noise is within
get_adjusted_ci(
  memory_noise,
  value_var = "bias_percent",
  within = "relative_noise",
  between = "experiment",
  wid = "participant"
)
#>      relative_noise experiment    center lowerwidth upperwidth   lower_ci
#> 1 target less noisy     Exp. 1  3.082070  -9.684937   9.684937  -6.602867
#> 2 target less noisy  Exp. 1 HV -1.768371  -8.239158   8.239158 -10.007529
#> 3 target more noisy     Exp. 1  2.178637  -9.684937   9.684937  -7.506300
#> 4 target more noisy  Exp. 1 HV -9.331493  -8.239158   8.239158 -17.570651
#>    upper_ci                       descr
#> 1 12.767006   _M_ = 3.08 [-6.60, 12.77]
#> 2  6.470786  _M_ = -1.77 [-10.01, 6.47]
#> 3 11.863574   _M_ = 2.18 [-7.51, 11.86]
#> 4 -1.092336 _M_ = -9.33 [-17.57, -1.09]
```
