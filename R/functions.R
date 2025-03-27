#' Describe an object in APA style
#'
#' @param obj an object to describe
#' @param ... other arguments passed to 
#'
#' @return description in APA (American Psychological Association) format
#' @export
#' 
apa <- function(obj, ...){
  UseMethod('apa')
}



#' Describe the results of [car::Anova] model or of a [stats::anova] model comparison 
#'
#' @param obj ANOVA results from [car::Anova] or [stats::anova]
#' @param term model term to describe (a string with the term name or its sequential number, default: 2)
#' @param f.digits number of digits in the results (default: 2)
#' @param ... other parameters passed to [apastats2::format.results]
#' 
#' @details
#' 
#' When using model comparison version, `term` can only be a number.
#'  
#' @return Formatted string with F (or chi2), df, and p
#' @method apa anova
#' @export
#'
#' @examples
#
#' # model comparison version
#' mod1 <- lm(conformity ~ 1, data = carData::Moore)
#' mod2 <- lm(conformity ~ fcategory, data = carData::Moore, contrasts = list(fcategory = contr.sum))
#' mod3 <- lm(conformity ~ fcategory * partner.status, data = carData::Moore, contrasts = list(fcategory = contr.sum, partner.status = contr.sum))
#' anova_obj <- anova(mod1, mod2, mod3)
#' 
#' anova_obj
#' apa(anova_obj)
#' apa(anova_obj, 3)
#' 
#' # car::Anova version
#' mod <- lm(conformity ~ fcategory * partner.status, data = carData::Moore, contrasts = list(fcategory = contr.sum, partner.status = contr.sum))
#' afit <- car::Anova(mod)
#' 
#' afit
#' apa(afit, "fcategory")
#' apa(afit, 2, 4)
#' 

apa.anova <- function(obj, term = 2, f.digits = 2, ...) {
  if ("Df.res" %in% colnames(obj)) {
    res_str <- sprintf(paste0("\\emph{F}(%i, %.", f.digits, "f) = %.", f.digits, "f, \\emph{p} %s"), obj[term, "Df"], obj[term, "Df.res"], obj[term, "F"], round.p(obj[term, "Pr(>F)"]))
  } else if ("F" %in% names(obj)) {
    res_str <- sprintf(paste0("\\emph{F}(%.0f, %.0f) = %.", f.digits, "f, \\emph{p} %s"), obj[term, "Df"], obj[term, "Res.Df"], obj[term, "F"], round.p(obj[term, "Pr(>F)"]))
  } else if ("Chisq" %in% names(obj)) {
    res_str <- sprintf(paste0("$\\chi^2$(%i) = %.", f.digits, "f, \\emph{p} %s"), obj[term, "Df"], obj[term, "Chisq"], round.p(obj[term, "Pr(>Chisq)"]))
  } else if ("F value" %in% names(obj)) {
    res_str <- sprintf(paste0("\\emph{F}(%i, %i) = %.", f.digits, "f, \\emph{p} %s"), obj[term, "Df"], obj["Residuals", "Df"], obj[term, "F value"], round.p(obj[term, "Pr(>F)"]))
  } else {
    stop('The object does not have one of the expected columns (F / F value / Chisq)')
  }
  
  format.results(res_str, ...)
}

#' Describe [stats::aov] results
#'
#' @param obj fitted [stats::aov] model
#' @param term model term to describe (a string with the term name or its sequential number)
#' @param sstype anova SS type (e.g., 2 or 3)
#' @param ... other parameters passed to apa.anova
#'
#' @return formatted string with F(df_numerator, df_denomiator) = F_value, p =/< p_value
#' @method apa aov
#' @export
#' 
#' @examples
#' fit <- aov(answerTime ~ stim_gender*user_gender, data = faces)
#' apa(fit, 'stim_gender')
#'

apa.aov <- function(obj, term = term, sstype = 2, ...) {
  afit <- car::Anova(obj, type = sstype)
  apa(afit, term = term, ...)
}

#' Describe BayesFactor results
#'
#' @param obj an object of [BayesFactor] class `BFBayesFactor`
#' @param digits number of digits to use
#' @param top_limit numbers above that limit (or below the digits limit) will be converted to exponential notation (if convert_to_power is TRUE)
#' @param convert_to_power enable or disable converting of very small or very large numbers to exponential notation
#' @param ... other parameters passed to [apastats2::format.results]
#' @return string describing the result
#' @note Code for converting to exponential notation is based on http://dankelley.github.io/r/2015/03/22/scinot.html
#' @method apa BFBayesFactor
#' @export apa.BFBayesFactor
#'
#' @examples
#'
#' require("BayesFactor")
#' data(puzzles)
#'
#' bfs <- anovaBF(RT ~ shape * color + ID, data = puzzles, progress = FALSE)
#' apa(bfs[1])
#' apa(bfs[2] / bfs[14])
#'
apa.BFBayesFactor <- function(obj, digits = 2, top_limit = 10000, convert_to_power = TRUE, ...) {
  bf_val <- exp(obj@bayesFactor[1])
  if ((bf_val < (10^(-digits)) | bf_val > top_limit) & convert_to_power) {
    exponent <- floor(log10(bf_val))
    bf_val <- round(bf_val / 10^exponent, digits = digits)
    format.results(paste0("\\emph{BF} = $", bf_val, "\\times 10^{", as.integer(exponent), "}$"), ...)
  } else {
    format.results(sprintf(paste0("\\emph{BF} = %.", digits, "f"), exp(bf@bayesFactor[1])), ...)
  }
}

#' Describe brms model results
#'
#' @param obj model object
#' @param term model term to describe
#' @param trans an optional function to transform the results to another scale (default: NULL)
#' @param digits number of digits in the output
#' @param eff.size string describing how to compute an effect size (currently, either 'fe_to_all', 'part_fe', or 'part_fe_re')
#' @param eff.size.type type of the effect size ('r' or 'r2')
#' @param nsamples number of samples to use for the effect size computations
#' @param ci.type type of intervals to use (currently, all that is not HPDI is treated as ETI using `bayestestR::eti`)
#' @param ... other parameters passed to \link{format.results}
#'
#' @return string describing the result
#' @importFrom data.table data.table
#' @method apa brmsfit
#' @export apa.brmsfit
#'
#' @examples
#' \dontrun{
#' require(brms)
#' x <- rnorm(500, sd = 6)
#' y <- 4 * x + rnorm(500)
#' fit <- brm(y ~ x, data = data.frame(x, y), chains = 1, iter = 500)
#' apa(fit, "x")
#' # convert x to another scale for output
#' apa(fit, "x", trans = function(val) val + 5)
#' }
apa.brmsfit <- function(obj, term, trans = NULL, digits = 2, eff.size = FALSE, eff.size.type = "r", nsamples = 100, ci.type = "HPDI", ...) {
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

#' Describe the results of $\chi^2$ or Hartigans' dip tests
#' @param obj a result from [stats::chisq.test] or [diptest::dip.test]
#' @param ... other parameters passed to \link{format.results}
#' @method apa htest
#' @return formatted results string
#' 
apa.htest <- function(obj, ...){
  if (obj$method=="Pearson's Chi-squared test"){
    apa.chisq(obj, ...)
  } else if (obj$method == "Hartigans' dip test for unimodality / multimodality"){
    apa.dip.test(obj, ...)
  } else {
    stop('The method from obj$method is not recognized')
  }
}

#'
#' @param v add Cramer's V (default: T)
#' @param addN add N (default: T)
#'
#' @describeIn apa.htest Describe Pearson $\\chi^2$ results
#' @export
#'
#' @examples
#'
#' ## From chisq.test help page
#' ## ---------------------
#' ## From Agresti(2007) p.39
#' M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
#' dimnames(M) <- list(
#'   gender = c("FALSE", "M"),
#'   party = c("Democrat", "Independent", "Republican")
#' )
#' (Xsq <- chisq.test(M)) # Prints test summary
#' ## ----------------------
#' apa(Xsq)
#'
#'
apa.chisq <- function(obj, v = TRUE, addN = TRUE, ...) {
  tbl <- obj$observed
  if (length(dim(tbl)) != 2 & !is.matrix(tbl)) tbl <- table(tbl)
  cv <- sqrt(obj$statistic / (sum(tbl) * min(dim(tbl) - 1)))
  n <- sum(tbl)
  res <- sprintf("$\\chi^2$(%i%s) = %.2f, \\emph{p} %s%s", obj$parameter, ifelse(addN, paste0(", \\emph{N} = ", sum(tbl)), ""), obj$statistic, round.p(obj$p.value), ifelse(v, paste0(", \\emph{V} = ", omit.zeroes(round(cv, 2)))))
  format.results(res, ...)
}



#' @describeIn apa.htest Describe Hartigans' dip test results
#' @export
#' 
#' @examples
#' requireNamespace('diptest')
#' data(statfaculty, package = 'diptest')
#' (d.t <- dip.test(statfaculty))
#' apa(d.t)
#' 
apa.dip.test <- function(obj, ...) {
  res <- sprintf("\\emph{D} = %.2f, \\emph{p} %s", obj$statistic, round.p(obj$p.value))
  format.results(res, ...)
}


#' Describe contrasts created by emmeans
#'
#' @param obj summary object from emmeans::contrast
#' @param term contrast number(s)
#' @param dtype description type, "t", "B", or any other letter
#' @param df include DF in t-test description (default: False)
#' @param ... other parameters passed to \link{format.results}
#'
#' @return string with formatted results
#' @export
#'
#' @examples
#'
#' require(emmeans)
#' warp.lm <- lm(breaks ~ wool * tension, data = warpbreaks)
#' warp.lsm <- emmeans::emmeans(warp.lm, ~ tension | wool)
#' (sum_contr <- summary(contrast(warp.lsm, "trt.vs.ctrl")))
#' apa(sum_contr, 1)
#' apa(sum_contr, 3)
#' apa(sum_contr, 3, dtype = "t")
#' apa(sum_contr, 3, dtype = "c")
#' apa(sum_contr, 3, dtype = "c", df = TRUE)
apa.emmeans <- function(obj, term, dtype = "B", df = FALSE, ...) {
  obj <- as.data.frame(obj[term, ])
  df_str <- ifelse(df, sprintf("(%i)", round(obj$df)), "")
  if (dtype == "t") {
    res_str <- sprintf("\\emph{%s}%s = %.2f, \\emph{p} %s", "t", df_str, obj$t.ratio, round.p(obj$p.value))
  } else if (dtype == "B") {
    res_str <- sprintf("\\emph{B} = %.2f (%.2f), \\emph{p} %s", obj$estimate, obj$SE, round.p(obj$p.value))
  } else {
    res_str <- sprintf("\\emph{B} = %.2f (%.2f), \\emph{%s}%s = %.2f, \\emph{p} %s", obj$estimate, obj$SE, "t", df_str, obj$t.ratio, round.p(obj$p.value))
  }
  format.results(res_str, ...)
}
#' Describe ezANOVA results
#' Provides formatted string like _F_(DFn, DFd) = ..., _p_ ..., eta2 = ... based on ezANOVA results
#'
#' @param ezfit ezANOVA object
#' @param term model term to describe (a string with the term name or its sequential number)
#' @param include_eta add eta^2 for the model (default: True)
#' @param spher_corr use sphericity corrections	(default: True)
#' @param eta_digits number of digits to use for eta^2 (default: 2)
#' @param f_digits number of digits to use for F (default: 2)
#' @param df_digits number of digits to use for df (default: 0)
#' @param append_to_table should the results be added to the original ezANOVA table (default: False)
#' @param ... other parameters passed to \link{format.results}
#'
#' @return string with formatted results
#' @export
#'
#' @examples
#' 
#' data(faces)
#' faces$uid <- factor(faces$uid)
#' ez_res <- ez::ezANOVA(faces[faces$correct == 1], dv = answerTime, 
#'                       wid = uid, within = stim_gender, between = user_gender)
#' apa(ez_res, "user_gender")
#' apa(ez_res, "user_gender", eta_digits = 3)
#' apa(ez_res, "user_gender:stim_gender", eta_digits = 3)
#' apa(ez_res, 3, eta_digits = 3)
apa.ezanova <- function(ezfit, term, include_eta = TRUE, spher_corr = TRUE, eta_digits = 2, f_digits = 2, df_digits = 0, append_to_table = FALSE, ...) {
  eza <- ezfit$ANOVA
  if (spher_corr & ("Sphericity Corrections" %in% names(ezfit))) {
    eza <- merge(eza, ezfit$`Sphericity Corrections`, by = "Effect", all.x = TRUE)
    eza[!is.na(eza$GGe), "p"] <- eza[!is.na(eza$GGe), ]$`p[GG]`
  }
  rownames(eza) <- eza$Effect
  
  suffix <- sprintf(", $\\eta$^2^~G~ %s", round.p(eza[term, "ges"],
                                                  digits = eta_digits,
                                                  replace.very.small = 10^(-eta_digits)
  ))
  if (include_eta == FALSE) {
    suffix <- rep("", length(suffix))
  }
  res <- format.results(sprintf("\\emph{F}(%.*f, %.*f) = %.*f, \\emph{p} %s%s", df_digits, eza[term, "DFn"], df_digits, eza[term, "DFd"], f_digits, eza[term, "F"], round.p(eza[term, "p"]), suffix), ...)
  if (append_to_table) {
    cbind(eza[term, ], res)
  } else {
    res
  }
}
#' Describe ezStats results
#'
#' Returns formatted string with mean and SD from ezStats object
#'
#' @param ezstats_res data.frame returned by ezStats
#' @param term model term to describe (a string with the term name or its sequential number)
#' @param ... other parameters passed to apa.mean.sd
#'
#' If a string is used for a term and there is more than one factor, "X:Y:Z" format is assumed (value in first column : value in the second, and so on). If no term is supplied, the first row is used. So if you need to select a term based on two or more variables, you can also just filter ezStats result beforehand (or use a row number as a term).
#'
#' @return string with formatted results
#' @export
#'
#' @examples
#' data(faces)
#' faces$uid <- factor(faces$uid)
#' faces$correct <- factor(faces$correct)
#' ezstats_res <- ez::ezStats(faces, dv = answerTime, wid = uid, within = .(stim_gender, correct))
#' ezstats_res
#' apa(ezstats_res, "M:0")
#' apa(ezstats_res, 2)
#' apa(ezstats_res, 2, digits = 1)
#'
apa.ezstats <- function(ezstats_res, term = 1, ...) {
  rownames(ezstats_res) <- apply(ezstats_res[, 1:(which(colnames(ezstats_res) == "N") - 1)], 1, paste, collapse = ":")
  with(ezstats_res[term, ], apa.mean.sd(m = Mean, sd = SD, ...))
}
#' Describe regression model (GLM, GLMer, lm, lm.circular, ...)
#'
#' @param fit model object
#' @param term model term to describe (a string with the term name or its sequential number)
#' @param dtype description type (1: t, p;  2: B(SE), p; 3: B, SE, t, p; or other: B (SE), t)
#' @param b.digits how many digits to use for _B_ and _SE_
#' @param t.digits how many digits to use for _t_
#' @param test.df should we include degrees of freedom in description?
#' @param eff.size should we include effect size (currently implemented only for simple regression)?
#' @param adj.digits automatically adjusts digits so that B or SE would not show up as "0.00"
#' @param p.as.number should the p-values be transformed to numbers (T) or shown as strings (F)?
#' @param term.pattern return only the model terms matching the regex pattern (grepl is used)
#' @param ... other parameters passed to \link{format.results}
#'
#' @return result
#' @export
#'
#' @examples
#'
#' Animals <- MASS::Animals
#' Animals$body <- log(Animals$body)
#' Animals$brain <- log(Animals$brain)
#' fit <- lm(brain ~ body, Animals)
#' apa(fit, "body")
#' apa(fit, "(Intercept)")
#' apa(fit, "body", 2)
#' apa(fit, "body", 3)
#' apa(fit, "body", 4)
#' apa(fit, "body", 3, test.df = 1)
#' apa(fit)
#' \dontrun{
#' require(rockchalk)
#' apa(fit, "body", 4, eff.size = TRUE)
#' }
apa.glm <- function(fit, term = NULL, dtype = 1, b.digits = 2, t.digits = 2, test.df = FALSE, p.as.number = FALSE, term.pattern = NULL, eff.size = FALSE, adj.digits = FALSE, ...) {
  fit_package <- attr(class(fit), "package")
  fit_class <- class(fit)[1]
  fit_family <- family(fit)[1]
  
  if (!is.numeric(dtype) | dtype < 0 | dtype > 4) dtype <- 3
  
  if (fit_class == "lm.circular.cl") {
    print(1)
    afit <- data.frame(fit$coefficients, fit$se.coef, fit$t.values, fit$p.values)
    t_z <- "t"
    if (test.df) {
      test.df <- FALSE
      warning("df for lm.circular are not implemented")
    }
  } else {
    if (grepl("merModLmerTest", fit_class)) {
      afit <- data.frame(coef(summary(fit)))
      if (test.df) {
        dfs <- afit[, 3]
      }
      afit <- afit[, c(1, 2, 4, 5)]
    } else {
      afit <- data.frame(coef(summary(fit)))
    }
    if (fit_family == "gaussian") {
      t_z <- "t"
    } else {
      t_z <- "Z"
    }
    if (eff.size) {
      if (!requireNamespace("rockchalk", quietly = TRUE)) {
        warning("Cannot compute effect size. The 'rockchalk' package is not available. Please install it using install.packages('rockchalk').")
      } 
      ess <- rockchalk::getDeltaRsquare(fit)
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
  
  if (length(attr(terms(fit), "term.labels")) == (length(rownames(afit)) + 1)) {
    rownames(afit) <- c("Intercept", attr(terms(fit), "term.labels"))
  }
  if (fit_class == "lmerMod") {
    if (dtype != 4) {
      warning("p-values for lmer are only a rough estimate from z-distribution, not suitable for the real use")
    }
    afit$pvals <- 2 * pnorm(-abs(afit[, 3]))
  }
  
  if (test.df) {
    if (!grepl("merModLmerTest", fit_class)) {
      dfs <- summary(fit)$df[2]
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
  
  res_df <- data.frame(B = f.round(afit[, 1], 2), SE = f.round(afit[, 2], 2), Stat = f.round(afit[, 3], t.digits), p = if (p.as.number) zapsmall(as.vector(afit[, 4]), 4) else round.p(afit[, 4]), eff = row.names(afit), row.names = row.names(afit))
  
  if (dtype == 1) {
    res_df$str <- sprintf(paste0("\\emph{", t_z, "}", dfs, " %s, \\emph{p} %s"), round.p(afit[, 3], digits = t.digits, strip.lead.zeros = FALSE), round.p(afit[, 4]))
  } else if (dtype == 2) {
    res_df$str <- sprintf(paste0("\\emph{B} = %.", b.digits, "f (%.", b.digits, "f), \\emph{p} %s"), afit[, 1], afit[, 2], round.p(afit[, 4]))
  } else if (dtype == 3) {
    res_df$str <- sprintf(paste0("\\emph{B} = %.", b.digits, "f, \\emph{SE} = %.", b.digits, "f, \\emph{", t_z, "}", dfs, " %s, \\emph{p} %s"), afit[, 1], afit[, 2], round.p(afit[, 3], digits = t.digits, strip.lead.zeros = FALSE, replace.very.small = 0.01), round.p(afit[, 4]))
  } else if (dtype == 4) {
    res_df$str <- sprintf(paste0("\\emph{B} = %.", b.digits, "f (%.", b.digits, "f), \\emph{", t_z, "}", dfs, " %s"), afit[, 1], afit[, 2], round.p(afit[, 3], digits = t.digits, strip.lead.zeros = FALSE, replace.very.small = 0.01))
  }
  
  if (eff.size & !exists("ess")) {
    stop("Effect sizes are not implemented for THAT kind of models yet.")
  } else if (eff.size) {
    res_df$str <- paste0(res_df$str, ", \\emph{R}_{part}^2", round.p(c(NA, ess), digits = ifelse(min(ess) < 0.01, 3, 2)))
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
#' Describe linearHypothesis test results
#'
#' @param hyp hypothesis from \link[car]{linearHypothesis}
#' @param ... additional papameters passed to \link{format.results}
#'
#' @return results of \eqn{\chi^2} or _F_ test
#' @export
#' @examples
#'
#' mod.davis <- lm(weight ~ repwt, data = carData::Davis)
#'
#' res <- car::linearHypothesis(mod.davis, c("(Intercept) = 0", "repwt = 1"))
#' apa(res)
#'
#' res.chi <- car::linearHypothesis(mod.davis, c("(Intercept) = 0", "repwt = 1"), test = "Chisq")
#' apa(res.chi)
apa.lht <- function(hyp, ...) {
  res <- hyp[2, ]
  
  if ("Chisq" %in% names(res)) {
    res <- res[c("Df", "Chisq", "Pr(>Chisq)")]
    res[3] <- round.p(res[3])
    format.results(do.call(sprintf, c(list("$\\chi^2$(%g) = %.2f, \\emph{p} %s"), res)), ...)
  } else {
    res[6] <- round.p(res[6])
    res <- res[c(3, 1, 5, 6)]
    format.results(do.call(sprintf, c(list("\\emph{F}(%g, %g) = %.2f, \\emph{p} %s"), res)), ...)
  }
}
#' Describe lmer results
#'
#' This function is deprecated in favor of \link{apa.glm}
#'
#' @param fm LMER model from lme4
#' @param pv p values table (from Anova)
#' @param digits number of digits in results (default: 2)
#' @param incl.rel include the relation sign for _p_
#' @param dtype description type (B or t)
#' @param incl.p include p values
#'
#' @return result
#' @export
#'

apa.lmer <- function(fm, pv, digits = c(2, 2, 2), incl.rel = 0, dtype = "B", incl.p = TRUE) {
  .Deprecated("apa.glm")
  cc <- lme4::fixef(fm)
  ss <- sqrt(diag(as.matrix(vcov(fm))))
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
#' Describe lmerTest results
#'
#' Note that this function uses *summary* object from lmerTest::lmer model and not the model itself (see the example). Otherwise p-values will be computed during the call and everything will be very slow.
#'
#' @param sfit *summary* object from lmerTest::lmer model
#' @param factor name or number of the factor that needs to be describe
#' @param dtype description type ("B"/"t")
#' @param ... other parameters passed to \link{format.results}
#'
#' @return Formatted string
#' @export
#'
#' @examples
#'
#' fm <- lmerTest::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
#' fms <- summary(fm)
#' apa(fms, "Days")
#' apa(fms, "(Intercept)")
#' apa(fms, 2)
#' apa(fms, "Days", "B")
#'
apa.lmert <- function(sfit, factor, dtype = "t", ...) {
  coef <- sfit$coefficients[factor, ]
  if (sfit$objClass == "glmerMod") {
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
#' Describe lmerTest anova results
#'
#' @param afit - lmerTest anova results
#' @param term model term to describe (a string with the term name or its sequential number)
#' @param f.digits - decimal digits for F value
#' @param ... other parameters passed to \link{format.results}
#'
#' @return formatted string describing the results of anova
#' @export
#'
#' @examples
#' data(faces)
#'
#' fit <- lmerTest::lmer(answerTime ~ correct * stim_gender + (1 + correct * stim_gender | uid), faces)
#' afit <- anova(fit)
#' apa(afit, "correct:stim_gender")
apa.lmtaov <- function(afit, term, f.digits = 2, ...) {
  afit <- data.frame(afit)
  res_str <- sprintf(paste0("\\emph{F}(%.0f, %.1f) = %.", f.digits, "f, \\emph{p} %s"), afit[term, "NumDF"], afit[term, "DenDF"], afit[term, "F.value"], round.p(afit[term, "Pr..F."]))
  format.results(res_str, ...)
}
#' Describe contrasts created by lsmeans
#'
#' @param obj summary object from lsmeans::contrast
#' @param term contrast number
#' @param dtype description type, "t", "B", or any other letter
#' @param df include DF in t-test description (default: False)
#' @param ... other parameters passed to \link{format.results}
#'
#' @return string with formatted results
#' @export
#'
#' @examples
#'
#' warp.lm <- lm(breaks ~ wool * tension, data = warpbreaks)
#' warp.lsm <- lsmeans::lsmeans(warp.lm, ~ tension | wool)
#' (sum_contr <- summary(lsmeans::contrast(warp.lsm, "trt.vs.ctrl")))
#' apa(sum_contr, 1)
#' apa(sum_contr, 3)
#' apa(sum_contr, 3, dtype = "t")
#' apa(sum_contr, 3, dtype = "c")
#' apa(sum_contr, 3, dtype = "c", df = TRUE)
apa.lsmeans <- function(obj, term, dtype = "B", df = FALSE, ...) {
  obj <- obj[term, ]
  if (dtype == "t") {
    res_str <- sprintf("\\emph{%s}%s = %.2f, \\emph{p} %s", "t", ifelse(df, paste0("(", round(obj["df"]), ")"), ""), obj["t.ratio"], round.p(obj["p.value"]))
  } else if (dtype == "B") {
    res_str <- sprintf("\\emph{B} = %.2f (%.2f), \\emph{p} %s", obj["estimate"], obj["SE"], round.p(obj["p.value"]))
  } else {
    res_str <- sprintf("\\emph{B} = %.2f (%.2f), \\emph{%s}%s = %.2f, \\emph{p} %s", obj["estimate"], obj["SE"], "t", ifelse(df, paste0("(", round(obj["df"]), ")"), ""), obj["t.ratio"], round.p(obj["p.value"]))
  }
  format.results(res_str, ...)
}
#' Describe two-sample t-test with means and effect sizes
#'
#' @param x dependent variable
#' @param by independent variable
#' @param which.mean which mean to show (0 - none, 1 - first group (default), 2 - second group, 3 - both)
#' @param digits number of digits in results (default: 2)
#' @param paired should it be a paired test (default: FALSE)
#' @param eff.size should we include effect size (default: FALSE)
#' @param abs should we show the absolute value if the t-test (T) or keep its sign (FALSE, default)
#' @param aggregate_by do the aggregation by the thrird variable(s): either NULL (default), a single vector variable, or a list of variables to aggregate by.
#' @param transform.means a function to transform means and confidence intervals (default: NULL)
#' @param ... other parameters passed to \link{format.results}
#'
#' @return result
#' 
#' @export
#'
#' @examples
#' data(faces)
#' rt <- faces$answerTime
#' gr <- faces$stim_gender
#' apa.mean.and.t(rt, gr)
#' apa.mean.and.t(rt, gr, which.mean = 3)
#' apa.mean.and.t(rt, gr, eff.size = TRUE)
#'
#' sid <- faces$sid
#' apa.mean.and.t(rt, gr, which.mean = 3, aggregate_by = sid)
#' log_rt <- log(rt * 1000)
#' apa.mean.and.t(log_rt, gr, which.mean = 3, aggregate_by = sid)
#' apa.mean.and.t(log_rt, gr, which.mean = 3, aggregate_by = sid, transform.means = exp)
apa.mean.and.t <- function(x, by, which.mean = 1, digits = 2, paired = FALSE, eff.size = FALSE, abs = FALSE, aggregate_by = NULL, transform.means = NULL, ...) {
  
  Lower <- Upper <- NULL  # due to NSE notes in R CMD check
  
  if (lengthu(by) != 2) {
    stop('"by" should have exactly two levels')
  }
  if (!is.null(aggregate_by)) {
    if (is.list(aggregate_by)) {
      aggregate_by <- append(aggregate_by, list(by = by))
    } else {
      aggregate_by <- list(aggregate_by = aggregate_by, by = by)
    }
    aggr_df <- Hmisc::summarize(x, aggregate_by, mean.nn)
    x <- as.numeric(aggr_df$x)
    by <- aggr_df$by
  }
  summaries <- Hmisc::summarize(x, by, Hmisc::smean.cl.boot)
  
  if (!is.null(transform.means)) {
    summaries[, c("x", "Lower", "Upper")] <- sapply(summaries[, c("x", "Lower", "Upper")], exp)
  }
  
  summaries <- transform(summaries, mean.descr = sprintf(paste0("\\emph{M} = %.", digits, "f [%.", digits, "f, %.", digits, "f]"), x, Lower, Upper))
  
  if (which.mean == 3) {
    means <- paste0(summaries[1, "mean.descr"], " vs. ", summaries[2, "mean.descr"], ", ")
  } else if (which.mean == 0) {
    means <- ""
  } else {
    means <- paste0(summaries[which.mean, "mean.descr"], ", ")
  }
  
  if (paired){
    if (length(x[by==unique(by)[1]])!=length(x[by==unique(by)[2]])){
      stop('For a paired t-test groups defined by `by` should have equal length.')
    }
    t_res <- t.test(x[by==unique(by)[1]],x[by==unique(by)[2]], paired = T)
  } else t_res <- t.test(x~by)
  res_str <- paste0(means, apa.ttest(t_res, abs = abs))
  if (eff.size) {
    if (!requireNamespace("lsr", quietly = TRUE)) {
      warning("Cannot compute effect size. The 'lsr' package is not available. Please install it using install.packages('lsr').")
      
    } else {
      eff_size <- lsr::cohensD(x ~ by, method = ifelse(paired, "paired", "unequal"))
      res_str <- paste0(res_str, ", \\emph{d} = ", f.round(eff_size, digits = digits))
    }
    
  }
  
  format.results(res_str, ...)
}

#' Describe mean and confidence intervals for binomial variable
#'
#' @param x a vector of values
#' @param digits number of digits in results (default: 2)
#'
#' @return a string with the mean and confidence interval in square brackets
#' @export apa.binom.mean.conf
#'
#' @examples
#'
#' apa.binom.mean.conf(faces$correct[1:100])
#' # note that it is slightly different from asymptotic CI
#' apa.mean.conf(faces$correct[1:100], bootCI = FALSE)
#' # although similar to the bootstrapped CI
#' apa.mean.conf(faces$correct[1:100], bootCI = TRUE)
apa.binom.mean.conf <- function(x, digits = 2) {
  format.results(with(data.frame(Hmisc::binconf(sum(x), length(x))), sprintf(paste0("\\emph{M} = %.", digits, "f [%.", digits, "f, %.", digits, "f]"), PointEst, Lower, Upper)))
}
#' Describe mean and confidence intervals
#'
#' @param x value that should be described
#' @param bootCI use bootstrapped (T, default) or Gaussian (F) confidence intervals
#' @param addCI adds "95% CI =" before confidence intervals (default: F)
#' @param digits number of digits to use
#' @param transform.means an optional function to transform the means and CI to another scale
#' @param ... other arguments passed to [apastats2::format.results]
#'
#' @return a string with a mean followed by confidence intervals in square brackets.
#'
#' @export
#'
#' @examples
#'
#' x <- runif(100, 0, 50)
#' apa.mean.conf(x)
#' apa.mean.conf(x, bootCI = FALSE)
#' apa.mean.conf(x, digits = 5)
#' apa.mean.conf(x, transform.means = function(val) val * 2)
#' 
apa.mean.conf <- function(x, bootCI = TRUE, addCI = FALSE, digits = 2, transform.means = NULL, ...) {
  if (bootCI) {
    res <- Hmisc::smean.cl.boot(x)
  } else {
    res <- Hmisc::smean.cl.normal(x)
  }
  if (!is.null(transform.means)) {
    res <- sapply(res, transform.means)
  }
  ci_str <- ifelse(addCI, ", 95%% \\emph{CI} =", "")
  format.results(with(as.list(res), sprintf(paste0(
    "\\emph{M} = %.",
    digits, "f", ci_str, " [%.", digits, "f, %.", digits,
    "f]"
  ), Mean, Lower, Upper)), ...)
}
#' Describe mean and SD
#'
#' Computes mean and SD for a vector (or just uses mean and sd if provided) and returns them as formatted string
#'
#' @param x vector of values
#' @param m mean (not used if x is provided)
#' @param sd SD (not used if x is provided)
#' @param digits number of digits used for description (default: 2)
#' @param dtype "p" for SD in parentheses (default), "c" for SD after comma
#' @param m_units a character to add units (e.g., "°" for degrees) to the mean
#' @param sd_units a character to add units (e.g., "°" for degrees) to SD
#' @param ... other arguments passed to \link{format.results}
#'
#' @return formatted string
#' @export
#'
#' @examples
#'
#' x <- rnorm(1000, 50, 25)
#' apa.mean.sd(x)
#' apa.mean.sd(x, 0)
#' apa.mean.sd(x, dtype = "c")
apa.mean.sd <- function(x = NULL, m = NULL, sd = NULL, digits = 2, dtype = "p", m_units = "", sd_units = "", ...) {
  if (dtype == "c") {
    s1 <- ", "
    s2 <- ""
  } else {
    s1 <- " ("
    s2 <- ")"
  }
  if (!is.null(x)) {
    m_sd <- as.list(Hmisc::smean.sd(x))
    m <- m_sd$Mean
    sd <- m_sd$SD
  }
  format.results(sprintf(
    "\\emph{M} = %.*f%s%s\\emph{SD} = %.*f%s%s",
    digits, m, m_units, s1, digits, sd, sd_units, s2
  ), ...)
}
#' Describe Pearson test results
#'
#' @param rc an object from \link[stats]{cor.test}
#' @param ... other arguments passed to \link{format.results}
#'
#' @return A string with correlation coefficient, sample size, and p-value.
#' @export
#'
#' @examples
#' x <- rnorm(40)
#' y <- x * 2 + rnorm(40)
#' rc <- cor.test(x, y)
#' apa(rc)
apa.r <- function(rc, ...) {
  format.results(sprintf("\\emph{r}(%.0f) = %.2f, \\emph{p} %s", rc$parameter, rc$estimate, round.p(rc$p.value)), ...)
}

#' Describe differences between ROC curves
#'
#' @param roc_diff a difference between the ROC curves
#'
#' @return result
#' @export
#'

apa.roc.diff <- function(roc_diff) {
  sprintf("\\emph{D} = %0.2f, \\emph{p} %s", roc_diff$statistic, round.p(roc_diff$p.value))
}

#' Describe htest class ([stats::t.test] or [stats::cor.test])
#'
#' @param obj object of htest class
#' @param ... 
#' 
#' Redirects to \link{apa.r} or \link{apa.ttest}
#' @export
#' 
apa.htest <- function(obj, ...){
  if (grepl('correl',obj$method)){
    apa.r(obj,...)
  } else apa.ttest(obj, ...)
}
#' Describe t-test results
#'
#' @param t an object from \link[stats]{t.test}
#' @param show.mean  include mean value in results (useful for one-sample test)
#' @param abs should we show the sign of t-test
#' @param ... other arguments passed to \link{format.results}
#'
#' @return A string with t-test value, degrees of freedom, p-value, and, optionally, a mean with 95% confidence interval in square brackets.
#' @export
#'
#' @examples
#' t_res <- t.test(rnorm(20, mean = -10, sd = 2))
#' apa(t_res)
#' apa(t_res, show.mean = TRUE)
#' apa(t_res, show.mean = TRUE, abs = TRUE)
apa.ttest <- function(t, show.mean = FALSE, abs = FALSE, ...) {
  if (abs) t$statistic <- abs(t$statistic)
  if (show.mean == TRUE) {
    res_str <- sprintf("\\emph{M} = %.2f [%.2f, %.2f], \\emph{t}(%.1f) = %.2f, \\emph{p} %s", t$estimate, t$conf.int[1], t$conf.int[2], t$parameter, t$statistic, round.p(t$p.value))
  } else {
    res_str <- sprintf("\\emph{t}(%.1f) = %.2f, \\emph{p} %s", t$parameter, t$statistic, round.p(t$p.value))
  }
  format.results(res_str, ...)
}
#' Describe lmer in-text
#'
#' A shortcut for apa.glm(..., short=4)
#'
#' @param fm LMER model from lme4
#' @param term model term to describe (a string with the term name or its sequential number)
#' @param  digits number of digits for B and SD
#' @param  adj.digits automatically adjusts digits so that B would not show up as "0.00"
#' @return result
#' @export
#'

ins.lmer <- function(fm, term = NULL, digits = 2, adj.digits = TRUE) {
  apa.glm(fm, term = term, b.digits = digits, adj.digits = adj.digits, dtype = 4)
}
#' Get a list with means and confidence intervals
#'
#' @param x value to compute the mean for
#' @param digits number of digits in results (default: 2)
#' @param binom compute binomial CI instead of the usual ones
#' @param ... other parameters passed to \link{format.results}
#'
#' @return a list with a mean and CI
#' @export
#'
#' @examples
#' table.mean.conf(faces$correct)
#'
table.mean.conf <- function(x, digits = 2, binom = FALSE, ...) {
  if (binom) {
    res <- data.frame(Hmisc::binconf(sum(x), length(x)))
    colnames(res) <- c("Mean", "Lower", "Upper")
  } else {
    res <- as.list(Hmisc::smean.cl.boot(x))
  }
  res <- with(res, c(f.round(Mean, digits), sprintf(paste0("[%.", digits, "f, %.", digits, "f]"), Lower, Upper)))
  res
}
#' Get a list of means and confidence intervals by group
#'
#' @param x value to compute the mean for
#' @param by group labels
#' @param digits number of digits in results (default: 2)
#' @param binom compute binomial CI instead of the usual ones
#'
#' @return a list of means and CI with keys corresponding to the group labels
#' @export
#'
#' @examples
#' table.mean.conf.by(faces$answerTime, faces$correct, 4)
#'
table.mean.conf.by <- function(x, by, digits = 2, binom = FALSE) {
  tapply(x, by, table.mean.conf, digits, binom)
}
