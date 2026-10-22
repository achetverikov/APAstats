#' Describe regression model (GLM, GLMer, lm, lm.circular, ...)
#'
#' @param obj model object from [stats::glm], [stats::lm], [lme4::lmer], [lme4::glmer], [lmerTest::lmer], etc.
#' @param term model term to describe (a string with the term name or its sequential number); if `NULL`, returns a formatted summary for all coefficients
#' @param dtype description type (1: t, p;  2: B(SE), p; 3: B, SE, t, p; or other: B (SE), t)
#' @param b.digits how many digits to use for _B_ and _SE_
#' @param t.digits how many digits to use for _t_
#' @param test.df should we include degrees of freedom in description?
#' @param eff.size should we include effect size (currently implemented only for simple regression)?
#' @param adj.digits automatically adjusts digits so that B or SE would not show up as "0.00"
#' @param p.as.number should the p-values be transformed to numbers (TRUE) or shown as strings (FALSE)?
#' @param term.pattern return only the model terms matching the regex pattern (grepl is used)
#' @param ... other parameters passed to [format_results]
#'
#' @return A character result for a selected `term`. Otherwise, a `data.frame`
#' with columns `B` (coefficient estimate), `SE` (standard error), `Stat`
#' (test statistic), `p` (p-value), `eff` (term name), and `str` (complete
#' APA-formatted result). The summary columns are character vectors except
#' that `p` is numeric when `p.as.number = TRUE`. `term.pattern` filters
#' the rows of this data frame.
#' @method apa glm
#' @export
#'
#' @examples
#' # Using the Animals dataset from MASS package
#' Animals <- MASS::Animals
#' Animals$body <- log(Animals$body)
#' Animals$brain <- log(Animals$brain)
#' 
#' # Fit a linear model
#' fit <- lm(brain ~ body, Animals)
#' 
#' # Format results in different styles
#' apa(fit, "body")
#' apa(fit, "(Intercept)")
#' apa(fit, "body", 2)   # B(SE), p format
#' apa(fit, "body", 3)   # B, SE, t, p format
#' apa(fit, "body", 4)   # B(SE), t format
#' apa(fit, "body", 3, test.df = TRUE)  # Include df in the output
#' 
#' # Full model summary for all coefficients
#' apa(fit)
#' 
#' # With effect size (requires rockchalk package)
#' if (requireNamespace("rockchalk", quietly = TRUE)) {
#'   print(apa(fit, "body", 4, eff.size = TRUE))
#' }
#'
#' if (requireNamespace("lmerTest", quietly = TRUE)) {
#'   fm <- lmerTest::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
#'   print(apa(fm, "Days", test.df = TRUE))
#' }
#'
#' if (requireNamespace("lme4", quietly = TRUE)) {
#'   gm <- lme4::glmer(
#'     cbind(incidence, size - incidence) ~ period + (1 | herd),
#'     data = lme4::cbpp,
#'     family = binomial
#'   )
#'   print(apa(gm, "period2"))
#' }
apa.glm <- function(obj, term = NULL, dtype = 1, b.digits = 2, t.digits = 2,
                    test.df = FALSE, p.as.number = FALSE, term.pattern = NULL,
                    eff.size = FALSE, adj.digits = FALSE, ...) {
  fit_class <- class(obj)[1]
  fit_family <- family(obj)[1]
  if (!is.numeric(dtype) | dtype < 0 | dtype > 4) dtype <- 3

  if (fit_class == "lm.circular.cl") {
    afit <- data.frame(obj$coefficients, obj$se.coef, obj$t.values, obj$p.values)
    t_z <- "t"
    if (test.df) {
      test.df <- FALSE
      warning("df for lm.circular are not implemented")
    }
  } else {
    model_summary <- summary(obj)
    afit <- data.frame(coef(model_summary))
    if (inherits(obj, "lmerModLmerTest")) {
      if (test.df) dfs <- afit[, 3]
      afit <- afit[, c(1, 2, 4, 5)]
    }
    t_z <- if (fit_family == "gaussian") "t" else "Z"
    if (eff.size) {
      if (!requireNamespace("rockchalk", quietly = TRUE)) {
        warning("Cannot compute effect size. The 'rockchalk' package is not available. Please install it using install.packages('rockchalk').")
      } else {
        ess <- rockchalk::getDeltaRsquare(obj)
      }
    }
  }
  if (!is.null(term) && !(term %in% row.names(afit))) {
    stop(sprintf("Term %s is absent from the model", term))
  }
  if (!is.null(term) && adj.digits) {
    while (plyr::round_any(min(abs(afit[term, 1]), abs(afit[term, 2])), 10^(-b.digits)) < (10^(-b.digits))) {
      b.digits <- b.digits + 1
    }
  } else if (adj.digits) {
    while (plyr::round_any(abs(afit[which.min(pmin(abs(afit[, 1]), abs(afit[, 2]))), 1]), 10^(-b.digits)) < (10^(-b.digits))) {
      b.digits <- b.digits + 1
    }
  }
  if (length(attr(terms(obj), "term.labels")) == (length(rownames(afit)) + 1)) {
    rownames(afit) <- c("Intercept", attr(terms(obj), "term.labels"))
  }
  if (inherits(obj, "lmerMod") && !inherits(obj, "lmerModLmerTest")) {
    if (dtype != 4) {
      warning("p-values for lmer are only a rough estimate from z-distribution, not suitable for the real use")
    }
    afit$pvals <- 2 * pnorm(-abs(afit[, 3]))
  }
  if (test.df) {
    if (!inherits(obj, "lmerModLmerTest")) dfs <- model_summary$df[2]
    dfs <- if (isTRUE(all.equal(dfs, as.integer(dfs)))) {
      as.character(round(dfs))
    } else f_round(dfs, t.digits)
    dfs <- paste0("(", dfs, ")")
  } else dfs <- ""

  p_str <- round_p(afit[, 4])
  res_df <- data.frame(B = f_round(afit[, 1], 2), SE = f_round(afit[, 2], 2),
                      Stat = f_round(afit[, 3], t.digits),
                      p = if (p.as.number) zapsmall(as.vector(afit[, 4]), 4) else p_str,
                      eff = row.names(afit), row.names = row.names(afit))
  test_str <- sprintf("\\emph{%s}%s %s", t_z, dfs,
    round_p(afit[, 3], digits = t.digits, strip.lead.zeros = FALSE,
            replace.very.small = if (dtype == 1) 0.001 else 0.01))
  coef_str <- sprintf("\\emph{B} = %.*f (%.*f)", b.digits, afit[, 1], b.digits, afit[, 2])
  if (dtype == 1) {
    res_df$str <- paste0(test_str, ", \\emph{p} ", p_str)
  } else if (dtype == 2) {
    res_df$str <- paste0(coef_str, ", \\emph{p} ", p_str)
  } else if (dtype == 3) {
    res_df$str <- sprintf("\\emph{B} = %.*f, \\emph{SE} = %.*f, %s, \\emph{p} %s",
                          b.digits, afit[, 1], b.digits, afit[, 2], test_str, p_str)
  } else if (dtype == 4) {
    res_df$str <- paste0(coef_str, ", ", test_str)
  }
  if (eff.size) {
    if (!exists("ess")) stop("Effect sizes are not implemented for THAT kind of models yet.")
    res_df$str <- paste0(res_df$str, ", \\emph{R}_{part}^2",
                         round_p(c(NA, ess), digits = ifelse(min(ess) < 0.01, 3, 2)))
  }
  res_df$str <- format_results(res_df$str, ...)
  if (!is.null(term)) {
    res_df[term, "str"]
  } else if (!is.null(term.pattern)) {
    res_df[grepl(term.pattern, res_df$eff), ]
  } else res_df
}

#' @rdname apa.glm
#' @method apa lm
#' @export
apa.lm <- apa.glm

#' @rdname apa.glm
#' @method apa glmerMod
#' @export
apa.glmerMod <- apa.glm

#' @rdname apa.glm
#' @method apa lmerModLmerTest
#' @export
apa.lmerModLmerTest <- apa.glm


#' Describe lmerTest results
#'
#' Note that this function uses *summary* object from lmerTest::lmer model and not the model itself (see examples). Otherwise p-values will be computed during the call and everything will be very slow.
#'
#' @param obj *summary* object from [lmerTest::lmer] model
#' @param factor name or number of the factor that needs to be described
#' @param dtype description type ("B"/"t")
#' @param ... other parameters passed to [format_results]
#'
#' @return A character vector reporting the selected fixed-effect estimate,
#' standard error, test statistic, degrees of freedom when available, and p-value.
#' @method apa summary.merMod
#' @export
#'
#' @examples
#' if (requireNamespace("lmerTest", quietly = TRUE)) {
#'   # Fit a mixed-effects model with p-values
#'   fm <- lmerTest::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
#'   fms <- summary(fm)
#'   
#'   # Format results in APA style
#'   print(apa(fms, "Days"))
#'   print(apa(fms, "(Intercept)"))
#'   print(apa(fms, 2))
#'   print(apa(fms, "Days", "B"))
#' }
apa.summary.merMod <- function(obj, factor, dtype = "t", ...) {
  coef <- obj$coefficients[factor, ]
  if (obj$objClass == "glmerMod") {
    test_name <- "z"
    test.df <- ""
    names(coef) <- stringr::str_replace(names(coef), "z", "t")
  } else {
    test_name <- "t"
    test.df <- paste0("(", round(coef["df"]), ")")
  }
  if (dtype == "t") {
    res_str <- sprintf("\\emph{%s}%s = %.2f, \\emph{p} %s", test_name, test.df, coef["t value"], round_p(coef["Pr(>|t|)"]))
  } else if (dtype == "B") {
    res_str <- sprintf("\\emph{B} = %.2f (%.2f), \\emph{p} %s", coef["Estimate"], coef["Std. Error"], round_p(coef["Pr(>|t|)"]))
  }
  format_results(res_str, ...)
}

#' @rdname apa.summary.merMod
#' @export
apa.lmert <- apa.summary.merMod

#' Describe [brms][brms::brms-package] model results
#'
#' @param obj model object from [brms::brm]
#' @param term model term to describe
#' @param trans an optional function to transform the results to another scale (default: NULL)
#' @param digits number of digits in the output
#' @param eff.size string describing how to compute an effect size (currently, either 'fe_to_all', 'part_fe', or 'part_fe_re')
#' @param eff.size.type type of the effect size ('r' or 'r2')
#' @param nsamples number of samples to use for the effect size computations
#' @param ci.type type of intervals to use (currently, all that is not HPDI is treated as ETI using `bayestestR::eti`)
#' @param ... other parameters passed to [format_results]
#'
#' @return A character result reporting the posterior mean coefficient, its
#' 95% interval, and optionally an effect-size estimate.
#' @importFrom data.table data.table
#' @method apa brmsfit
#' @export
#'
#' @examples
#' \donttest{
#' if (requireNamespace("brms", quietly = TRUE) &&
#'     requireNamespace("bayestestR", quietly = TRUE)) {
#'   # Generate sample data
#'   set.seed(42)
#'   x <- seq(-1, 1, length.out = 30)
#'   y <- 1 + 2 * x + rnorm(30, sd = 0.5)
#'   
#'   # Fit a Bayesian regression model
#'   fit <- brms::brm(y ~ x, data = data.frame(x, y), backend = "rstan",
#'                    chains = 1, cores = 1, iter = 500, seed = 42, refresh = 0)
#'   print(apa(fit, "x"))
#'   
#'   # Convert x to another scale for output
#'   print(apa(fit, "x", trans = function(val) val + 5))
#' }
#' }
apa.brmsfit <- function(obj, term, trans = NULL, digits = 2, eff.size = FALSE,
                        eff.size.type = "r", nsamples = 100, ci.type = "HPDI", ...) {
  if (!requireNamespace("brms", quietly = TRUE)) {
    stop("Package 'brms' is required for this function")
  }
  if (!requireNamespace("bayestestR", quietly = TRUE)) {
    stop("Package 'bayestestR' is required for this function")
  }
  post_samp <- brms::as_draws_matrix(obj, variable = paste0("b_", term), regex = FALSE)
  if (ncol(post_samp) > 1) warning(paste("More than one match using", term, "term"))
  post_samp <- post_samp[, 1]
  if (is.function(trans)) post_samp <- trans(post_samp)
  ci_fun <- if (ci.type == "HPDI") bayestestR::hdi else bayestestR::eti
  ci <- ci_fun(post_samp, ci = 0.95)

  es <- NULL
  if (eff.size != FALSE) {
    var_res <- var(resid(obj, nsamples = nsamples)[, 1])
    fixed_effects <- brms::fixef(obj)
    var_term <- var(obj$data[, term, with = FALSE] * fixed_effects[term, "Estimate"])
    denominator <- switch(as.character(eff.size),
      fe_to_all = var(fitted(obj, nsamples = nsamples)[, 1]) + var_res,
      part_fe = var_term + var_res,
      part_fe_re = {
        random_effects <- brms::ranef(obj)
        term_re <- data.table(random_effects[[1]][, , term], keep.rownames = TRUE)
        mod_data_preds <- merge(obj$data, term_re, by.x = names(random_effects), by.y = "rn")
        var_term <- var(mod_data_preds[, term, with = FALSE] *
          (fixed_effects[term, "Estimate"] + mod_data_preds[, "Estimate"]))
        var_term + var_res
      }
    )
    if (!is.null(denominator)) es <- var_term / denominator
  }
  res_str <- sprintf("\\emph{b} = %.*f, 95%% %s = [%.*f, %.*f]",
    digits, mean(post_samp), ci.type, digits, ci[, "CI_low"], digits, ci[, "CI_high"])
  if (!is.null(es)) {
    res_str <- if (eff.size.type == "r") {
      paste0("_r_ = ", f_round(sqrt(es), digits = digits), ", ", res_str)
    } else paste0(res_str, ", $R^2_part$ = ", f_round(es, digits = digits))
  }
  format_results(res_str, ...)
}

#' Describe [BayesFactor][BayesFactor::BayesFactor-package] results
#'
#' @param obj an object of [BayesFactor][BayesFactor::BayesFactor-package] class `BFBayesFactor`
#' @param digits number of digits to use
#' @param top_limit numbers above that limit (or below the digits limit) will be converted to exponential notation (if convert_to_power is TRUE)
#' @param convert_to_power enable or disable converting of very small or very large numbers to exponential notation
#' @param ... other parameters passed to [format_results]
#' @return A character result reporting the Bayes factor, optionally in
#' scientific notation for very large or small values.
#' @note Code for converting to exponential notation is based on https://dankelley.github.io/r/2015/03/22/scinot.html
#' @method apa BFBayesFactor
#' @export
#'
#' @examples
#' if (requireNamespace("BayesFactor", quietly = TRUE)) {
#'   bf <- BayesFactor::ttestBF(x = c(0.8, 1.1, 0.9, 1.5, 1.2, 0.7))
#'   print(apa(bf))
#'   print(apa(bf, top_limit = 10))
#' }
apa.BFBayesFactor <- function(obj, digits = 2, top_limit = 10000, convert_to_power = TRUE, ...) {
  bf_val <- exp(obj@bayesFactor[1])
  if ((bf_val < (10^(-digits)) | bf_val > top_limit) & convert_to_power) {
    exponent <- floor(log10(bf_val))
    bf_val <- round(bf_val / 10^exponent, digits = digits)
    format_results(paste0("\\emph{BF} = $", bf_val, "\\times 10^{", as.integer(exponent), "}$"), ...)
  } else {
    format_results(sprintf(paste0("\\emph{BF} = %.", digits, "f"), exp(obj@bayesFactor[1])), ...)
  }
}

#' Describe lmer in-text
#'
#' A shortcut for apa.glm(..., dtype=4)
#'
#' @param obj LMER model from [lme4::lmer]
#' @param term model term to describe (a string with the term name or its sequential number)
#' @param digits number of digits for B and SD
#' @param adj.digits automatically adjusts digits so that B would not show up as "0.00"
#' @return A character result for a selected `term`, or a `data.frame` of
#' coefficient summaries when `term = NULL`. The table has the columns
#' documented in [apa.glm]; `str` contains estimates, standard errors, and
#' test statistics formatted for in-text reporting.
#' @export
#'
#' @examples
#' if (requireNamespace("lme4", quietly = TRUE)) {
#'   # Fit a mixed-effects model
#'   fm <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
#'   
#'   # Concise in-text reporting
#'   print(ins_lmer(fm, "Days"))
#'   print(ins_lmer(fm, "(Intercept)", digits = 1))
#' }
ins_lmer <- function(obj, term = NULL, digits = 2, adj.digits = TRUE) {
  apa.glm(obj, term = term, b.digits = digits, adj.digits = adj.digits, dtype = 4)
}
