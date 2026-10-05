# Deprecated functions in the apastats package

These functions are deprecated and will be removed in a future version.
Please use their replacements as indicated.

- `describe.ttest` -\>
  [apa](https://achetverikov.github.io/APAstats/reference/apa.md) (for
  t.test objects)

- `describe.r` -\>
  [apa](https://achetverikov.github.io/APAstats/reference/apa.md) (for
  cor.test objects)

- `describe.chi` -\>
  [apa](https://achetverikov.github.io/APAstats/reference/apa.md) (for
  chisq.test objects)

- `describe.mean.sd` -\>
  [apa_mean_sd](https://achetverikov.github.io/APAstats/reference/apa_mean_sd.md)

- `describe.mean.conf` -\>
  [apa_mean_conf](https://achetverikov.github.io/APAstats/reference/apa_mean_conf.md)

- `describe.binom.mean.conf` -\>
  [apa_binom_mean_conf](https://achetverikov.github.io/APAstats/reference/apa_binom_mean_conf.md)

- `describe.mean.and.t` -\>
  [apa_mean_and_t](https://achetverikov.github.io/APAstats/reference/apa_mean_and_t.md)

- `describe.Anova` -\>
  [apa](https://achetverikov.github.io/APAstats/reference/apa.md) (for
  Anova objects)

- `describe.aov` -\>
  [apa](https://achetverikov.github.io/APAstats/reference/apa.md) (for
  aov objects)

- `describe.anova` -\>
  [apa](https://achetverikov.github.io/APAstats/reference/apa.md) (for
  anova objects)

- `describe.afex` -\>
  [apa](https://achetverikov.github.io/APAstats/reference/apa.md) (for
  afex_aov objects)

- `describe.bf` -\>
  [apa](https://achetverikov.github.io/APAstats/reference/apa.md) (for
  BFBayesFactor objects)

- `describe.brm` -\>
  [apa](https://achetverikov.github.io/APAstats/reference/apa.md) (for
  brmsfit objects)

- `describe.dip.test` -\>
  [apa](https://achetverikov.github.io/APAstats/reference/apa.md) (for
  dip.test objects)

- `describe.emmeans` -\>
  [apa](https://achetverikov.github.io/APAstats/reference/apa.md) (for
  emmeans objects)

- `describe.ezanova` -\>
  [apa](https://achetverikov.github.io/APAstats/reference/apa.md) (for
  ezanova objects)

- `describe.ezstats` -\>
  [apa](https://achetverikov.github.io/APAstats/reference/apa.md) (for
  ezstats objects)

- `describe.glm` -\>
  [apa](https://achetverikov.github.io/APAstats/reference/apa.md) (for
  glm/lm objects)

- `describe.lht` -\>
  [apa](https://achetverikov.github.io/APAstats/reference/apa.md) (for
  linearHypothesis objects)

- `describe.lmer` -\>
  [apa](https://achetverikov.github.io/APAstats/reference/apa.md) (for
  lmer objects)

- `describe.lmert` -\>
  [apa](https://achetverikov.github.io/APAstats/reference/apa.md) (for
  lmerTest objects)

- `describe.lmtaov` -\>
  [apa](https://achetverikov.github.io/APAstats/reference/apa.md) (for
  lmerTest anova objects)

- `describe.lsmeans` -\>
  [apa](https://achetverikov.github.io/APAstats/reference/apa.md) (for
  lsmeans objects)

- `describe.roc.diff` -\>
  [apa](https://achetverikov.github.io/APAstats/reference/apa.md) (for
  ROC difference objects)

- `mean.nn` -\>
  [mean_nn](https://achetverikov.github.io/APAstats/reference/mean_nn.md)

- `sd.nn` -\>
  [sd_nn](https://achetverikov.github.io/APAstats/reference/sd_nn.md)

- `sum.nn` -\>
  [sum_nn](https://achetverikov.github.io/APAstats/reference/sum_nn.md)

- `drop.empty.cols` -\>
  [drop_empty_cols](https://achetverikov.github.io/APAstats/reference/drop_empty_cols.md)

- `binom.ci` -\>
  [binom_ci](https://achetverikov.github.io/APAstats/reference/binom_ci.md)

- `mean.round` -\>
  [mean_round](https://achetverikov.github.io/APAstats/reference/mean_round.md)

- `sd.round` -\>
  [sd_round](https://achetverikov.github.io/APAstats/reference/sd_round.md)

- `load.libs` -\>
  [load_libs](https://achetverikov.github.io/APAstats/reference/load_libs.md)

- `lmer.fixef` -\>
  [lmer_fixef](https://achetverikov.github.io/APAstats/reference/lmer_fixef.md)

- `omit.zeroes` -\>
  [omit_zeroes](https://achetverikov.github.io/APAstats/reference/omit_zeroes.md)

- `f.round` -\>
  [f_round](https://achetverikov.github.io/APAstats/reference/f_round.md)

- `base.breaks` -\>
  [base_breaks](https://achetverikov.github.io/APAstats/reference/base_breaks.md)

- `base.breaks.x` -\>
  [base_breaks_x](https://achetverikov.github.io/APAstats/reference/base_breaks.md)

- `base.breaks.y` -\>
  [base_breaks_y](https://achetverikov.github.io/APAstats/reference/base_breaks.md)

- `plot.pointrange` -\>
  [plot_pointrange](https://achetverikov.github.io/APAstats/reference/plot_pointrange.md)

- `mymean` -\>
  [mean_nn](https://achetverikov.github.io/APAstats/reference/mean_nn.md)

- `mysum` -\>
  [sum_nn](https://achetverikov.github.io/APAstats/reference/sum_nn.md)

- `mysd` -\>
  [sd_nn](https://achetverikov.github.io/APAstats/reference/sd_nn.md)

## Usage

``` r
describe.ttest(t, ...)

describe.r(rc, ...)

describe.chi(tbl, ...)

describe.Anova(afit, term, f.digits = 2, ...)

describe.aov(fit, term = NULL, sstype = 2, ...)

describe.anova(anova_res, rown = 2, f.digits = 2, ...)

describe.afex(
  afex_fit,
  term,
  include_eta = TRUE,
  eta_digits = 2,
  f_digits = 2,
  df_digits = 0,
  append_to_table = FALSE,
  ...
)

describe.bf(bf, digits = 2, top_limit = 10000, convert_to_power = TRUE, ...)

describe.brm(
  mod,
  term,
  trans = NULL,
  digits = 2,
  eff.size = FALSE,
  eff.size.type = "r",
  nsamples = 100,
  ci.type = "HPDI",
  ...
)

describe.dip.test(x, ...)

describe.emmeans(obj, term, dtype = "B", df = FALSE, ...)

describe.ezanova(
  ezfit,
  term,
  include_eta = TRUE,
  spher_corr = TRUE,
  eta_digits = 2,
  f_digits = 2,
  df_digits = 0,
  append_to_table = FALSE,
  ...
)

describe.ezstats(ezstats_res, term = 1, ...)

describe.glm(
  fit,
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

describe.lht(hyp, ...)

describe.lmer(
  fm,
  pv,
  digits = c(2, 2, 2),
  incl.rel = 0,
  dtype = "B",
  incl.p = TRUE
)

describe.lmert(sfit, factor, dtype = "t", ...)

describe.lmtaov(afit, term, f.digits = 2, ...)

describe.lsmeans(obj, term, dtype = "B", df = FALSE, ...)

describe.roc.diff(roc_diff)

describe.mean.sd(
  x = NULL,
  m = NULL,
  sd = NULL,
  digits = 2,
  dtype = "p",
  m_units = "",
  sd_units = "",
  ...
)

describe.mean.conf(
  x,
  bootCI = TRUE,
  addCI = FALSE,
  digits = 2,
  transform.means = NULL,
  ...
)

describe.binom.mean.conf(x, digits = 2, ...)

describe.mean.and.t(
  x,
  by,
  which.mean = 1,
  digits = 2,
  paired = FALSE,
  eff.size = FALSE,
  abs = FALSE,
  aggregate_by = NULL,
  transform.means = NULL,
  ...
)

mymean(...)

mysum(...)

mysd(...)

# S3 method for class 'nn'
mean(x, ...)

sd.nn(x, ...)

# S3 method for class 'nn'
sum(x, ...)

drop.empty.cols(df)

binom.ci(x)

# S3 method for class 'round'
mean(x, ...)

sd.round(x, ...)

load.libs(libs)

lmer.fixef(fit.lmer)

omit.zeroes(x, digits = 2)

f.round(x, digits = 2, strip.lead.zeros = FALSE)

base.breaks(x, scale = "x", addSegment = TRUE, ...)

base.breaks.x(x, addSegment = TRUE, ...)

base.breaks.y(x, addSegment = TRUE, ...)

# S3 method for class 'pointrange'
plot(
  data,
  mapping,
  pos = position_dodge(0.3),
  pointsize = I(3),
  linesize = I(1),
  pointfill = I("white"),
  pointshape = NULL,
  within_subj = FALSE,
  wid = "uid",
  bars = "ci",
  withinvars = NULL,
  betweenvars = NULL,
  x_as_numeric = FALSE,
  custom_geom_before = NULL,
  connecting_line = FALSE,
  pretty_breaks_y = FALSE,
  pretty_y_axis = FALSE,
  exp_y = FALSE,
  print_aggregated_data = FALSE,
  do_aggregate = FALSE,
  add_margin = FALSE,
  margin_label = "all",
  margin_x_vals = NULL,
  bars_instead_of_points = FALSE,
  geom_bar_params = list(),
  add_jitter = FALSE,
  individual_points_params = list(),
  drop_NA_subj = FALSE,
  design = "between",
  debug = FALSE
)
```
