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
#'   \item `mymean` -> [mean.nn]
#'   \item `mysum` -> [sum.nn]
#'   \item `mysd` -> [sd.nn]
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
  apa.chisq(tbl, ...)
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
  apa.ezanova(ezfit, term, include_eta, spher_corr, eta_digits, f_digits, df_digits, append_to_table, ...)
}

#' @rdname apastats-deprecated
#' @export
describe.ezstats <- function(ezstats_res, term = 1, ...) {
  .Deprecated("apa")
  apa.ezstats(ezstats_res, term, ...)
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
  apa.lht(hyp, ...)
}

#' @rdname apastats-deprecated
#' @export
describe.lmer <- function(fm, pv, digits = c(2, 2, 2), incl.rel = 0, dtype = "B", incl.p = TRUE) {
  .Deprecated("apa")
  apa.lmer(fm, pv, digits, incl.rel, dtype, incl.p)
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
  apa.lmtaov(afit, term, f.digits, ...)
}

#' @rdname apastats-deprecated
#' @export
describe.lsmeans <- function(obj, term, dtype = "B", df = FALSE, ...) {
  .Deprecated("apa")
  apa.lsmeans(obj, term, dtype, df, ...)
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
  .Deprecated("mean.nn", package = "apastats")
  mean.nn(...)
}

#' @rdname apastats-deprecated
#' @export
mysum <- function(...) {
  .Deprecated("sum.nn", package = "apastats")
  sum.nn(...)
}

#' @rdname apastats-deprecated
#' @export
mysd <- function(...) {
  .Deprecated("sd.nn", package = "apastats")
  sd.nn(...)
}