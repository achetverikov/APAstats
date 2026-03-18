#' Deprecated functions in the apastats package
#'
#' @description
#' These functions are deprecated and will be removed in a future version.
#' Please use their replacements as indicated.
#'
#' \itemize{
#'   \item `describe.ttest` -> [apa] (for t.test objects)
#'   \item `describe.r` -> [apa] (for cor.test objects)
#'   \item `describe.chi` -> [apa] (for chisq.test objects) 
#'   \item `describe.mean.sd` -> [apa_mean_sd]
#'   \item `describe.mean.conf` -> [apa_mean_conf]
#'   \item `describe.binom.mean.conf` -> [apa_binom_mean_conf]
#'   \item `describe.mean.and.t` -> [apa_mean_and_t]
#'   \item `describe.Anova` -> [apa] (for Anova objects)
#'   \item `describe.aov` -> [apa] (for aov objects)
#'   \item `describe.anova` -> [apa] (for anova objects)
#'   \item `describe.bf` -> [apa] (for BFBayesFactor objects)
#'   \item `describe.brm` -> [apa] (for brmsfit objects)
#'   \item `describe.dip.test` -> [apa] (for dip.test objects)
#'   \item `describe.emmeans` -> [apa] (for emmeans objects)
#'   \item `describe.ezanova` -> [apa] (for ezanova objects)
#'   \item `describe.ezstats` -> [apa] (for ezstats objects)
#'   \item `describe.glm` -> [apa] (for glm/lm objects)
#'   \item `describe.lht` -> [apa] (for linearHypothesis objects)
#'   \item `describe.lmer` -> [apa] (for lmer objects)
#'   \item `describe.lmert` -> [apa] (for lmerTest objects)
#'   \item `describe.lmtaov` -> [apa] (for lmerTest anova objects)
#'   \item `describe.lsmeans` -> [apa] (for lsmeans objects)
#'   \item `describe.roc.diff` -> [apa] (for ROC difference objects)
#'   \item `mean.nn` -> [mean_nn]
#'   \item `sd.nn` -> [sd_nn]
#'   \item `sum.nn` -> [sum_nn]
#'   \item `drop.empty.cols` -> [drop_empty_cols]
#'   \item `binom.ci` -> [binom_ci]
#'   \item `mean.round` -> [mean_round]
#'   \item `sd.round` -> [sd_round]
#'   \item `load.libs` -> [load_libs]
#'   \item `lmer.fixef` -> [lmer_fixef]
#'   \item `omit.zeroes` -> [omit_zeroes]
#'   \item `f.round` -> [f_round]
#'   \item `base.breaks` -> [base_breaks]
#'   \item `base.breaks.x` -> [base_breaks_x]
#'   \item `base.breaks.y` -> [base_breaks_y]
#'   \item `plot.pointrange` -> [plot_pointrange]
#'   \item `mymean` -> [mean_nn]
#'   \item `mysum` -> [sum_nn]
#'   \item `mysd` -> [sd_nn]
#' }
#'
#' @name apastats-deprecated
#' @keywords internal
NULL

# Statistical test result formatting functions
#' @rdname apastats-deprecated
#' @export
describe.ttest <- function(t, ...) {
  .Deprecated("apa")
  apa.ttest(t, ...)
}

#' @rdname apastats-deprecated
#' @export
describe.r <- function(rc, ...) {
  .Deprecated("apa")
  apa.r(rc, ...)
}

#' @rdname apastats-deprecated
#' @export
describe.chi <- function(tbl, ...) {
  .Deprecated("apa")
  apa.chisq.test(tbl, ...)
}

#' @rdname apastats-deprecated
#' @export
describe.Anova <- function(afit, term, f.digits = 2, ...) {
  .Deprecated("apa")
  apa.anova(afit, term, f.digits, ...)
}

#' @rdname apastats-deprecated
#' @export
describe.aov <- function(fit, term, sstype = 2, ...) {
  .Deprecated("apa")
  apa.aov(fit, term, sstype, ...)
}

#' @rdname apastats-deprecated
#' @export
describe.anova <- function(anova_res, rown = 2, f.digits = 2, ...) {
  .Deprecated("apa")
  apa.anova(anova_res, rown, f.digits, ...)
}

#' @rdname apastats-deprecated
#' @export
describe.bf <- function(bf, digits = 2, top_limit = 10000, convert_to_power = TRUE, ...) {
  .Deprecated("apa")
  apa.BFBayesFactor(bf, digits, top_limit, convert_to_power, ...)
}

#' @rdname apastats-deprecated
#' @export
describe.brm <- function(mod, term, trans = NULL, digits = 2, eff.size = FALSE, eff.size.type = "r", nsamples = 100, ci.type = "HPDI", ...) {
  .Deprecated("apa")
  apa.brmsfit(mod, term, trans, digits, eff.size, eff.size.type, nsamples, ci.type, ...)
}

#' @rdname apastats-deprecated
#' @export
describe.dip.test <- function(x, ...) {
  .Deprecated("apa")
  apa.dip.test(x, ...)
}

#' @rdname apastats-deprecated
#' @export
describe.emmeans <- function(obj, term, dtype = "B", df = FALSE, ...) {
  .Deprecated("apa")
  apa.emmeans(obj, term, dtype, df, ...)
}

#' @rdname apastats-deprecated
#' @export
describe.ezanova <- function(ezfit, term, include_eta = TRUE, spher_corr = TRUE, eta_digits = 2, f_digits = 2, df_digits = 0, append_to_table = FALSE, ...) {
  .Deprecated("apa")
  apa.ezANOVA(ezfit, term, include_eta, spher_corr, eta_digits, f_digits, df_digits, append_to_table, ...)
}

#' @rdname apastats-deprecated
#' @export
describe.ezstats <- function(ezstats_res, term = 1, ...) {
  .Deprecated("apa")
  apa_ezStats(ezstats_res, term, ...)
}

#' @rdname apastats-deprecated
#' @export
describe.glm <- function(fit, term = NULL, dtype = 1, b.digits = 2, t.digits = 2, test.df = FALSE, p.as.number = FALSE, term.pattern = NULL, eff.size = FALSE, adj.digits = FALSE, ...) {
  .Deprecated("apa")
  apa.glm(fit, term, dtype, b.digits, t.digits, test.df, p.as.number, term.pattern, eff.size, adj.digits, ...)
}

#' @rdname apastats-deprecated
#' @export
describe.lht <- function(hyp, ...) {
  .Deprecated("apa")
  apa.linearHypothesis(hyp, ...)
}

#' @rdname apastats-deprecated
#' @export
describe.lmer <- function(fm, pv, digits = c(2, 2, 2), incl.rel = 0, dtype = "B", incl.p = TRUE) {
  .Deprecated("apa.glm")
  if (!isTRUE(all.equal(incl.rel, 0)) || !isTRUE(incl.p)) {
    warning("Arguments 'incl.rel' and 'incl.p' are ignored; use apa.glm()/apa() arguments instead.")
  }
  apa.glm(
    obj = fm,
    term = pv,
    dtype = if (identical(dtype, "B")) 2 else 1,
    b.digits = digits[1],
    t.digits = digits[3]
  )
}

#' @rdname apastats-deprecated
#' @export
describe.lmert <- function(sfit, factor, dtype = "t", ...) {
  .Deprecated("apa")
  apa.lmert(sfit, factor, dtype, ...)
}

#' @rdname apastats-deprecated
#' @export
describe.lmtaov <- function(afit, term, f.digits = 2, ...) {
  .Deprecated("apa")
  apa.anova.merMod(afit, term, f.digits, ...)
}

#' @rdname apastats-deprecated
#' @export
describe.lsmeans <- function(obj, term, dtype = "B", df = FALSE, ...) {
  .Deprecated("apa")
  apa.emmeans(obj, term, dtype, df, ...)
}

#' @rdname apastats-deprecated
#' @export
describe.roc.diff <- function(roc_diff) {
  .Deprecated("apa")
  apa.roc.diff(roc_diff)
}

# Utility functions
#' @rdname apastats-deprecated
#' @export
describe.mean.sd <- function(x = NULL, m = NULL, sd = NULL, digits = 2, dtype = "p", m_units = "", sd_units = "", ...) {
  .Deprecated("apa_mean_sd")
  apa_mean_sd(x, m, sd, digits, dtype, m_units, sd_units, ...)
}

#' @rdname apastats-deprecated
#' @export
describe.mean.conf <- function(x, bootCI = TRUE, addCI = FALSE, digits = 2, transform.means = NULL, ...) {
  .Deprecated("apa_mean_conf")
  apa_mean_conf(x, bootCI, addCI, digits, transform.means, ...)
}

#' @rdname apastats-deprecated
#' @export
describe.binom.mean.conf <- function(x, digits = 2, ...) {
  .Deprecated("apa_binom_mean_conf")
  apa_binom_mean_conf(x, digits, ...)
}

#' @rdname apastats-deprecated
#' @export
describe.mean.and.t <- function(x, by, which.mean = 1, digits = 2, paired = FALSE, eff.size = FALSE, abs = FALSE, aggregate_by = NULL, transform.means = NULL, ...) {
  .Deprecated("apa_mean_and_t")
  apa_mean_and_t(x, by, which.mean, digits, paired, eff.size, abs, aggregate_by, transform.means, ...)
}

# Helper functions with shortened names
#' @rdname apastats-deprecated
#' @export
mymean <- function(...) {
  .Deprecated("mean_nn", package = "apastats2")
  mean_nn(...)
}

#' @rdname apastats-deprecated
#' @export
mysum <- function(...) {
  .Deprecated("sum_nn", package = "apastats2")
  sum_nn(...)
}

#' @rdname apastats-deprecated
#' @export
mysd <- function(...) {
  .Deprecated("sd_nn", package = "apastats2")
  sd_nn(...)
}

#' @rdname apastats-deprecated
#' @export
mean.nn <- function(x, ...) {
  .Deprecated("mean_nn", package = "apastats2")
  mean_nn(x, ...)
}

#' @rdname apastats-deprecated
#' @export
sd.nn <- function(x, ...) {
  .Deprecated("sd_nn", package = "apastats2")
  sd_nn(x, ...)
}

#' @rdname apastats-deprecated
#' @export
sum.nn <- function(x, ...) {
  .Deprecated("sum_nn", package = "apastats2")
  sum_nn(x, ...)
}

#' @rdname apastats-deprecated
#' @export
drop.empty.cols <- function(df) {
  .Deprecated("drop_empty_cols", package = "apastats2")
  drop_empty_cols(df)
}

#' @rdname apastats-deprecated
#' @export
binom.ci <- function(x) {
  .Deprecated("binom_ci", package = "apastats2")
  binom_ci(x)
}

#' @rdname apastats-deprecated
#' @export
mean.round <- function(x, ...) {
  .Deprecated("mean_round", package = "apastats2")
  mean_round(x, ...)
}

#' @rdname apastats-deprecated
#' @export
sd.round <- function(x, ...) {
  .Deprecated("sd_round", package = "apastats2")
  sd_round(x, ...)
}

#' @rdname apastats-deprecated
#' @export
load.libs <- function(libs) {
  .Deprecated("load_libs", package = "apastats2")
  load_libs(libs)
}

#' @rdname apastats-deprecated
#' @export
lmer.fixef <- function(fit.lmer) {
  .Deprecated("lmer_fixef", package = "apastats2")
  lmer_fixef(fit.lmer)
}

#' @rdname apastats-deprecated
#' @export
omit.zeroes <- function(x, digits = 2) {
  .Deprecated("omit_zeroes", package = "apastats2")
  omit_zeroes(x, digits = digits)
}

#' @rdname apastats-deprecated
#' @export
f.round <- function(x, digits = 2, strip.lead.zeros = FALSE) {
  .Deprecated("f_round", package = "apastats2")
  f_round(x, digits = digits, strip.lead.zeros = strip.lead.zeros)
}

#' @rdname apastats-deprecated
#' @export
base.breaks <- function(x, scale = "x", addSegment = TRUE, ...) {
  .Deprecated("base_breaks", package = "apastats2")
  base_breaks(x, scale = scale, addSegment = addSegment, ...)
}

#' @rdname apastats-deprecated
#' @export
base.breaks.x <- function(x, addSegment = TRUE, ...) {
  .Deprecated("base_breaks_x", package = "apastats2")
  base_breaks_x(x, addSegment = addSegment, ...)
}

#' @rdname apastats-deprecated
#' @export
base.breaks.y <- function(x, addSegment = TRUE, ...) {
  .Deprecated("base_breaks_y", package = "apastats2")
  base_breaks_y(x, addSegment = addSegment, ...)
}

#' @rdname apastats-deprecated
#' @export
plot.pointrange <- function(data, mapping, pos = position_dodge(0.3), pointsize = I(3), linesize = I(1),
                            pointfill = I("white"), pointshape = NULL, within_subj = FALSE,
                            wid = "uid", bars = "ci", withinvars = NULL, betweenvars = NULL,
                            x_as_numeric = FALSE, custom_geom_before = NULL, connecting_line = FALSE,
                            pretty_breaks_y = FALSE, pretty_y_axis = FALSE, exp_y = FALSE, print_aggregated_data = FALSE,
                            do_aggregate = FALSE, add_margin = FALSE, margin_label = "all", margin_x_vals = NULL,
                            bars_instead_of_points = FALSE, geom_bar_params = list(), add_jitter = FALSE,
                            individual_points_params = list(), drop_NA_subj = FALSE, design = "between", debug = FALSE) {
  .Deprecated("plot_pointrange", package = "apastats2")
  plot_pointrange(
    data = data, mapping = mapping, pos = pos, pointsize = pointsize, linesize = linesize,
    pointfill = pointfill, pointshape = pointshape, within_subj = within_subj,
    wid = wid, bars = bars, withinvars = withinvars, betweenvars = betweenvars,
    x_as_numeric = x_as_numeric, custom_geom_before = custom_geom_before, connecting_line = connecting_line,
    pretty_breaks_y = pretty_breaks_y, pretty_y_axis = pretty_y_axis, exp_y = exp_y, print_aggregated_data = print_aggregated_data,
    do_aggregate = do_aggregate, add_margin = add_margin, margin_label = margin_label, margin_x_vals = margin_x_vals,
    bars_instead_of_points = bars_instead_of_points, geom_bar_params = geom_bar_params, add_jitter = add_jitter,
    individual_points_params = individual_points_params, drop_NA_subj = drop_NA_subj, design = design, debug = debug
  )
}
