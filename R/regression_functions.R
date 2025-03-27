#' Describe regression model (GLM, GLMer, lm, lm.circular, ...)
#'
#' @param obj model object from [stats::glm], [stats::lm], [lme4::lmer], etc.
#' @param term model term to describe (a string with the term name or its sequential number)
#' @param dtype description type (1: t, p;  2: B(SE), p; 3: B, SE, t, p; or other: B (SE), t)
#' @param b.digits how many digits to use for _B_ and _SE_
#' @param t.digits how many digits to use for _t_
#' @param test.df should we include degrees of freedom in description?
#' @param eff.size should we include effect size (currently implemented only for simple regression)?
#' @param adj.digits automatically adjusts digits so that B or SE would not show up as "0.00"
#' @param p.as.number should the p-values be transformed to numbers (T) or shown as strings (F)?
#' @param term.pattern return only the model terms matching the regex pattern (grepl is used)
#' @param ... other parameters passed to [format.results]
#'
#' @return result
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
#' # Full model summary
#' apa(fit)
#' 
#' # With effect size (requires rockchalk package)
#' if (requireNamespace("rockchalk", quietly = TRUE)) {
#'   apa(fit, "body", 4, eff.size = TRUE)
#' }
apa.glm <- function(obj, term = NULL, dtype = 1, b.digits = 2, t.digits = 2, 
                    test.df = FALSE, p.as.number = FALSE, term.pattern = NULL, 
                    eff.size = FALSE, adj.digits = FALSE, ...) {
  fit_package <- attr(class(obj), "package")
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
    if (grepl("merModLmerTest", fit_class)) {
      afit <- data.frame(coef(summary(obj)))
      if (test.df) {
        dfs <- afit[, 3]
      }
      afit <- afit[, c(1, 2, 4, 5)]
    } else {
      afit <- data.frame(coef(summary(obj)))
    }
    if (fit_family == "gaussian") {
      t_z <- "t"
    } else {
      t_z <- "Z"
    }
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
  if (fit_class == "lmerMod") {
    if (dtype != 4) {
      warning("p-values for lmer are only a rough estimate from z-distribution, not suitable for the real use")
    }
    afit$pvals <- 2 * pnorm(-abs(afit[, 3]))
  }
  
  if (test.df) {
    if (!grepl("merModLmerTest", fit_class)) {
      dfs <- summary(obj)$df[2]
    }
    if (isTRUE(all.equal(dfs, as.integer(dfs)))) {
      dfs <- as.character(round(dfs))
    } else {
      dfs <- f.round(dfs, t.digits)
    }
    dfs <- paste0("(", dfs, ")")
  } else {
    dfs <- ""
  }
  
  res_df <- data.frame(B = f.round(afit[, 1], 2), SE = f.round(afit[, 2], 2), 
                       Stat = f.round(afit[, 3], t.digits), 
                       p = if (p.as.number) zapsmall(as.vector(afit[, 4]), 4) else round.p(afit[, 4]), 
                       eff = row.names(afit), row.names = row.names(afit))
  
  if (dtype == 1) {
    res_df$str <- sprintf(paste0("\\emph{", t_z, "}", dfs, " %s, \\emph{p} %s"), 
                          round.p(afit[, 3], digits = t.digits, strip.lead.zeros = FALSE), 
                          round.p(afit[, 4]))
  } else if (dtype == 2) {
    res_df$str <- sprintf(paste0("\\emph{B} = %.", b.digits, "f (%.", b.digits, "f), \\emph{p} %s"), 
                          afit[, 1], afit[, 2], round.p(afit[, 4]))
  } else if (dtype == 3) {
    res_df$str <- sprintf(paste0("\\emph{B} = %.", b.digits, "f, \\emph{SE} = %.", b.digits, 
                                 "f, \\emph{", t_z, "}", dfs, " %s, \\emph{p} %s"), 
                          afit[, 1], afit[, 2], 
                          round.p(afit[, 3], digits = t.digits, 
                                  strip.lead.zeros = FALSE, replace.very.small = 0.01), 
                          round.p(afit[, 4]))
  } else if (dtype == 4) {
    res_df$str <- sprintf(paste0("\\emph{B} = %.", b.digits, "f (%.", b.digits, "f), \\emph{", t_z, "}", dfs, " %s"), 
                          afit[, 1], afit[, 2], 
                          round.p(afit[, 3], digits = t.digits, 
                                  strip.lead.zeros = FALSE, replace.very.small = 0.01))
  }
  
  if (eff.size && !exists("ess")) {
    stop("Effect sizes are not implemented for THAT kind of models yet.")
  } else if (eff.size && exists("ess")) {
    res_df$str <- paste0(res_df$str, ", \\emph{R}_{part}^2", 
                         round.p(c(NA, ess), digits = ifelse(min(ess) < 0.01, 3, 2)))
  }
  
  res_df$str <- format.results(res_df$str, ...)
  if (!is.null(term)) {
    res_df[term, "str"]
  } else if (!is.null(term.pattern)) {
    res_df[grepl(term.pattern, res_df$eff), ]
  } else {
    res_df
  }
}

#' @rdname apa.glm
#' @method apa lm
#' @export
apa.lm <- apa.glm

#' Describe lmer results
#'
#' @param obj LMER model from [lme4::lmer]
#' @param pv p values table (from Anova)
#' @param digits number of digits in results (default: 2)
#' @param incl.rel include the relation sign for _p_
#' @param dtype description type (B or t)
#' @param incl.p include p values
#'
#' @return result
#' @method apa lmerMod
#' @export
#'
#' @examples
#' if (requireNamespace("lme4", quietly = TRUE)) {
#'   # Fit a mixed-effects model
#'   fm <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
#'   
#'   # Get approximate p-values
#'   if (requireNamespace("lmerTest", quietly = TRUE)) {
#'     pv <- list(fixed = data.frame("Pr(>|t|)" = c(0.001, 0.001)))
#'     rownames(pv$fixed) <- c("(Intercept)", "Days")
#'     
#'     # Format results
#'     apa(fm, pv)
#'     apa(fm, pv, digits = c(1, 1, 1))
#'     apa(fm, pv, dtype = "t")
#'   }
#' }
apa.lmerMod <- function(obj, pv, digits = c(2, 2, 2), incl.rel = 0, dtype = "B", incl.p = TRUE) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' is required for this function")
  }
  
  cc <- lme4::fixef(obj)
  ss <- sqrt(diag(as.matrix(vcov(obj))))
  data <- data.frame(
    Estimate = cc, Std.Err = ss, t = cc / ss,
    p = pv[["fixed"]][, "Pr(>|t|)"], row.names = names(cc)
  )
  for (i in c(1:3)) {
    data[, i] <- format(round(data[, i], digits[i]), nsmall = digits[i])
  }
  if (incl.p == FALSE) {
    data$str <- sprintf("\\emph{t} = %s, \\emph{p} %s", data$t, round.p(data[, 4], 1))
  }
  if (dtype == "t") {
    data$str <- sprintf("\\emph{B} = %s (%s), \\emph{t} = %s", data$Estimate, data$Std.Err, data$t)
  } else if (dtype == "B") {
    data$str <- sprintf("\\emph{B} = %s (%s), \\emph{p} %s", data$Estimate, data$Std.Err, round.p(data[, 4], 1))
  }
  data[, 4] <- round.p(data[, 4], incl.rel)
  data
}

#' @rdname apa.lmerMod
#' @export
apa.lmer <- apa.lmerMod

#' Describe lmerTest results
#'
#' Note that this function uses *summary* object from lmerTest::lmer model and not the model itself (see examples). Otherwise p-values will be computed during the call and everything will be very slow.
#'
#' @param obj *summary* object from [lmerTest::lmer] model
#' @param factor name or number of the factor that needs to be described
#' @param dtype description type ("B"/"t")
#' @param ... other parameters passed to [format.results]
#'
#' @return Formatted string
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
#'   apa(fms, "Days")
#'   apa(fms, "(Intercept)")
#'   apa(fms, 2)
#'   apa(fms, "Days", "B")
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
    res_str <- sprintf("\\emph{%s}%s = %.2f, \\emph{p} %s", test_name, test.df, coef["t value"], round.p(coef["Pr(>|t|)"]))
  } else if (dtype == "B") {
    res_str <- sprintf("\\emph{B} = %.2f (%.2f), \\emph{p} %s", coef["Estimate"], coef["Std. Error"], round.p(coef["Pr(>|t|)"]))
  }
  format.results(res_str, ...)
}

#' @rdname apa.summary.merMod
#' @export
apa.lmert <- apa.summary.merMod

#' Describe brms model results
#'
#' @param obj model object from [brms::brm]
#' @param term model term to describe
#' @param trans an optional function to transform the results to another scale (default: NULL)
#' @param digits number of digits in the output
#' @param eff.size string describing how to compute an effect size (currently, either 'fe_to_all', 'part_fe', or 'part_fe_re')
#' @param eff.size.type type of the effect size ('r' or 'r2')
#' @param nsamples number of samples to use for the effect size computations
#' @param ci.type type of intervals to use (currently, all that is not HPDI is treated as ETI using `bayestestR::eti`)
#' @param ... other parameters passed to [format.results]
#'
#' @return string describing the result
#' @importFrom data.table data.table
#' @method apa brmsfit
#' @export
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("brms", quietly = TRUE)) {
#'   # Generate sample data
#'   x <- rnorm(500, sd = 6)
#'   y <- 4 * x + rnorm(500)
#'   
#'   # Fit a Bayesian regression model
#'   fit <- brms::brm(y ~ x, data = data.frame(x, y), chains = 1, iter = 500)
#'   apa(fit, "x")
#'   
#'   # Convert x to another scale for output
#'   apa(fit, "x", trans = function(val) val + 5)
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
  
  post_samp <- brms::posterior_samples(obj, c(paste0("b_", term)), exact_match = TRUE)
  if (ncol(post_samp) > 1) {
    warning(paste("More than one match using", term, "term"))
  }
  post_samp <- post_samp[, 1]
  if (is.function(trans)) {
    post_samp <- trans(post_samp)
  }
  if (ci.type == "HPDI") {
    ci <- bayestestR::hdi(post_samp, ci = 0.95)
  } else {
    ci <- bayestestR::eti(post_samp, ci = 0.95)
  }
  # it is tricky to compute partial R2, this is just one of the approaches
  
  es <- NULL
  if (eff.size != FALSE) {
    var_res <- var(resid(obj, nsamples = nsamples)[, 1])
    var_term <- var(obj$data[, term, with = FALSE] * brms::fixef(obj)[term, "Estimate"])
  }
  if ("fe_to_all" == eff.size) {
    var_tot <- var(fitted(obj, nsamples = nsamples)[, 1]) + var_res
    es <- var_term / var_tot
  } else if ("part_fe" == eff.size) {
    # var_f = var(fitted(obj, re_formula = NA)[,1])
    es <- var_term / (var_term + var_res)
  } else if ("part_fe_re" == eff.size) {
    term_re <- data.table(brms::ranef(obj)[[1]][, , term], keep.rownames = TRUE)
    mod_data_preds <- merge(obj$data, term_re, by.x = names(brms::ranef(obj)), by.y = "rn")
    var_term <- var(mod_data_preds[, term, with = FALSE] * (brms::fixef(obj)[term, "Estimate"] + mod_data_preds[, "Estimate"]))
    var_tot <- var_term + var_res
    
    es <- var_term / var_tot
  }
  
  
  res_str <- sprintf(paste0("\\emph{b} = %.", digits, "f, 95%% %s = [%.", digits, "f, %.", digits, "f]"), mean(post_samp), ci.type, ci[, "CI_low"], ci[, "CI_high"])
  if (!is.null(es)) {
    if (eff.size.type == "r") {
      res_str <- paste0("_r_ = ", f.round(sqrt(es), digits = digits), ", ", res_str)
    } else {
      res_str <- paste0(res_str, ", $R^2_part$ = ", f.round(es, digits = digits))
    }
  }
  
  format.results(res_str, ...)
}

#' Describe BayesFactor results
#'
#' @param obj an object of [BayesFactor] class `BFBayesFactor`
#' @param digits number of digits to use
#' @param top_limit numbers above that limit (or below the digits limit) will be converted to exponential notation (if convert_to_power is TRUE)
#' @param convert_to_power enable or disable converting of very small or very large numbers to exponential notation
#' @param ... other parameters passed to [format.results]
#' @return string describing the result
#' @note Code for converting to exponential notation is based on http://dankelley.github.io/r/2015/03/22/scinot.html
#' @method apa BFBayesFactor
#' @export
#'
#' @examples
#' if (requireNamespace("BayesFactor", quietly = TRUE)) {
#'   # Load the puzzle data from BayesFactor package
#'   data(puzzles, package = "BayesFactor")
#'   
#'   # Run Bayesian ANOVA
#'   bfs <- BayesFactor::anovaBF(RT ~ shape * color + ID, data = puzzles, progress = FALSE)
#'   apa(bfs[1])
#'   apa(bfs[2] / bfs[14])
#' }
apa.BFBayesFactor <- function(obj, digits = 2, top_limit = 10000, convert_to_power = TRUE, ...) {
  bf_val <- exp(obj@bayesFactor[1])
  if ((bf_val < (10^(-digits)) | bf_val > top_limit) & convert_to_power) {
    exponent <- floor(log10(bf_val))
    bf_val <- round(bf_val / 10^exponent, digits = digits)
    format.results(paste0("\\emph{BF} = $", bf_val, "\\times 10^{", as.integer(exponent), "}$"), ...)
  } else {
    format.results(sprintf(paste0("\\emph{BF} = %.", digits, "f"), exp(obj@bayesFactor[1])), ...)
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
#' @return result
#' @export
#'
#' @examples
#' if (requireNamespace("lme4", quietly = TRUE)) {
#'   # Fit a mixed-effects model
#'   fm <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
#'   
#'   # Concise in-text reporting
#'   ins_lmer(fm, "Days")
#'   ins_lmer(fm, "(Intercept)", digits = 1)
#' }
ins_lmer <- function(obj, term = NULL, digits = 2, adj.digits = TRUE) {
  apa.glm(obj, term = term, b.digits = digits, adj.digits = adj.digits, dtype = 4)
}