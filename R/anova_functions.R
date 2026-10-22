.format_anova_table <- function(table, term, cols, include_eta, eta_digits,
                                f_digits, df_digits, append_to_table, ...) {
  suffix <- if (include_eta) {
    sprintf(", $\\eta$^2^~G~ %s", round_p(table[term, "ges"], digits = eta_digits,
                                        replace.very.small = 10^(-eta_digits)))
  } else ""
  res <- format_results(sprintf("\\emph{F}(%.*f, %.*f) = %.*f, \\emph{p} %s%s",
    df_digits, table[term, cols[1]], df_digits, table[term, cols[2]],
    f_digits, table[term, "F"], round_p(table[term, cols[3]]), suffix), ...)
  if (append_to_table) cbind(table[term, , drop = FALSE], res) else res
}

#' Describe the results of ANOVA models or model comparisons 
#'
#' @param obj ANOVA results from [car::Anova] or [stats::anova]
#' @param term model term to describe (a string with the term name or its sequential number, default: 2)
#' @param f.digits number of digits in the results (default: 2)
#' @param ... other parameters passed to [format_results]
#' 
#' @details
#' When using model comparison version, `term` can only be a number.
#'  
#' @return Formatted string with F (or chi2), df, and p
#' @method apa anova
#' @export
#'
#' @examples
#' if (requireNamespace("carData", quietly = TRUE)) {
#'   # Model comparison version
#'   mod1 <- lm(conformity ~ 1, data = carData::Moore)
#'   mod2 <- lm(conformity ~ fcategory, data = carData::Moore,
#'              contrasts = list(fcategory = contr.sum))
#'   mod3 <- lm(conformity ~ fcategory * partner.status, data = carData::Moore,
#'              contrasts = list(fcategory = contr.sum, partner.status = contr.sum))
#'   anova_obj <- anova(mod1, mod2, mod3)
#'
#'   print(apa(anova_obj))
#'   print(apa(anova_obj, 3))
#'
#'   # car::Anova version
#'   if (requireNamespace("car", quietly = TRUE)) {
#'     mod <- lm(conformity ~ fcategory * partner.status, data = carData::Moore,
#'               contrasts = list(fcategory = contr.sum, partner.status = contr.sum))
#'     afit <- car::Anova(mod)
#'
#'     print(apa(afit, "fcategory"))
#'     print(apa(afit, 2, 4))
#'   }
#' }
apa.anova <- function(obj, term = 2, f.digits = 2, ...) {
  if ("Df.res" %in% colnames(obj)) {
    res_str <- sprintf(paste0("\\emph{F}(%i, %.", f.digits, "f) = %.", f.digits, "f, \\emph{p} %s"), 
                       obj[term, "Df"], obj[term, "Df.res"], obj[term, "F"], 
                       round_p(obj[term, "Pr(>F)"]))
  } else if ("F" %in% names(obj)) {
    res_str <- sprintf(paste0("\\emph{F}(%.0f, %.0f) = %.", f.digits, "f, \\emph{p} %s"), 
                       obj[term, "Df"], obj[term, "Res.Df"], obj[term, "F"], 
                       round_p(obj[term, "Pr(>F)"]))
  } else if ("Chisq" %in% names(obj)) {
    res_str <- sprintf(paste0("$\\chi^2$(%i) = %.", f.digits, "f, \\emph{p} %s"), 
                       obj[term, "Df"], obj[term, "Chisq"], 
                       round_p(obj[term, "Pr(>Chisq)"]))
  } else if ("F value" %in% names(obj)) {
    res_str <- sprintf(paste0("\\emph{F}(%i, %i) = %.", f.digits, "f, \\emph{p} %s"), 
                       obj[term, "Df"], obj["Residuals", "Df"], obj[term, "F value"], 
                       round_p(obj[term, "Pr(>F)"]))
  } else {
    stop('The object does not have one of the expected columns (F / F value / Chisq)')
  }
  
  format_results(res_str, ...)
}

#' Describe [stats::aov] results
#'
#' @param obj fitted [stats::aov] model
#' @param term model term to describe (a string with the term name or its sequential number)
#' @param sstype anova SS type (e.g., 2 or 3)
#' @param ... other parameters passed to [apa.anova]
#'
#' @return formatted string with F(df_numerator, df_denominator) = F_value, p =/< p_value
#' @method apa aov
#' @export
#' 
#' @examples
#' if (requireNamespace("car", quietly = TRUE)) {
#'   # Using the mtcars dataset
#'   fit <- aov(mpg ~ cyl * am, data = mtcars)
#'   print(apa(fit, 'cyl'))
#'   print(apa(fit, 'am'))
#'   print(apa(fit, 'cyl:am'))
#'
#'   # Using a different SS type
#'   print(apa(fit, 'cyl', sstype = 3))
#' }
apa.aov <- function(obj, term, sstype = 2, ...) {
  if (requireNamespace("car", quietly = TRUE)) {
    afit <- car::Anova(obj, type = sstype)
    apa(afit, term = term, ...)
  } else {
    stop("Package 'car' is required for this function")
  }
}

#' Describe repeated-measures `aov` results with an `Error()` term
#'
#' `stats::aov()` models containing `Error()` return an `aovlist` object,
#' which cannot be passed to `car::Anova()`. This method extracts the ANOVA
#' tables from each error stratum and reports effects using the residual degrees
#' of freedom from the same stratum.
#'
#' @param obj fitted [stats::aov] model with an `Error()` term
#' @param term model term to describe. A character term name or sequential
#'   number can be supplied; if `NULL`, all testable effects are returned.
#' @param f.digits number of digits for the F value
#' @param ... other parameters passed to [format_results]
#'
#' @return one or more formatted strings with F, numerator and denominator df,
#'   and p values
#' @rdname apa.aov
#' @method apa aovlist
#' @export
#'
#' @examples
#' fit_rm <- aov(yield ~ N * P * K + Error(block), data = npk)
#' apa(fit_rm, "N")
#' apa(fit_rm)
apa.aovlist <- function(obj, term = NULL, f.digits = 2, ...) {
  rows <- lapply(summary(obj), function(stratum) {
    tables <- if (is.data.frame(stratum)) list(stratum) else if (is.list(stratum)) stratum else list()
    lapply(tables, function(tab) {
      if (!is.data.frame(tab) ||
          !all(c("Df", "F value", "Pr(>F)") %in% colnames(tab))) return(NULL)
      term_names <- trimws(rownames(tab))
      residual_i <- which(term_names == "Residuals")
      effect_i <- which(term_names != "Residuals" & !is.na(tab[, "F value"]))
      if (!length(residual_i) || !length(effect_i)) return(NULL)
      data.frame(term = term_names[effect_i], Df = tab[effect_i, "Df"],
                 Df.res = tab[residual_i[1], "Df"], F_value = tab[effect_i, "F value"],
                 p = tab[effect_i, "Pr(>F)"], stringsAsFactors = FALSE)
    })
  })
  effects <- do.call(rbind, unlist(rows, recursive = FALSE))
  if (is.null(effects) || nrow(effects) == 0L) {
    stop("No testable effects were found in the aovlist object.")
  }

  if (!is.null(term)) {
    if (is.numeric(term)) {
      if (length(term) != 1 || is.na(term) || term < 1 || term > nrow(effects)) {
        stop(sprintf("term must be between 1 and %i.", nrow(effects)))
      }
      effects <- effects[term, , drop = FALSE]
    } else {
      keep <- effects$term %in% term
      if (!any(keep)) {
        stop(sprintf("Term '%s' was not found. Available terms are: %s.",
                     paste(term, collapse = "', '"),
                     paste(unique(effects$term), collapse = ", ")))
      }
      effects <- effects[keep, , drop = FALSE]
    }
  }
  res <- sprintf(paste0("\\emph{F}(%.0f, %.0f) = %.", f.digits, "f, \\emph{p} %s"),
                 effects$Df, effects$Df.res, effects$F_value, round_p(effects$p))
  names(res) <- effects$term
  format_results(res, ...)
}

#' Describe ezANOVA results
#' 
#' Provides formatted string like _F_(DFn, DFd) = ..., _p_ ..., eta2 = ... based on ezANOVA results
#'
#' @param obj ezANOVA object from [ez::ezANOVA]
#' @param term model term to describe (a string with the term name or its sequential number)
#' @param include_eta add eta^2 for the model (default: TRUE)
#' @param spher_corr use sphericity corrections	(default: TRUE)
#' @param eta_digits number of digits to use for eta^2 (default: 2)
#' @param f_digits number of digits to use for F (default: 2)
#' @param df_digits number of digits to use for df (default: 0)
#' @param append_to_table should the results be added to the original ezANOVA table (default: FALSE)
#' @param ... other parameters passed to [format_results]
#'
#' @return A character vector reporting F statistics, degrees of freedom,
#' p-values, and optional generalized eta-squared. With `append_to_table = TRUE`,
#' a `data.frame` of selected ANOVA rows with an added `res` column containing
#' the formatted results.
#' @method apa ezANOVA
#' @export
#'
#' @examples
#' if (requireNamespace("ez", quietly = TRUE) && requireNamespace("afex", quietly = TRUE)) {
#'   # Using the Stroop dataset from afex package
#'   data(stroop, package = "afex")
#'   
#'   # Run ezANOVA
#'   ez_res <- ez::ezANOVA(
#'     data = stroop[!is.na(stroop$rt),],
#'     dv = rt,
#'     wid = pno,
#'     within = condition,
#'     detailed = TRUE
#'   )
#'   
#'   # Format results in APA style
#'   print(apa(ez_res, "condition"))
#'   print(apa(ez_res, "condition", eta_digits = 3))
#'   print(apa(ez_res, "condition", include_eta = FALSE))
#' }

apa.ezANOVA <- function(obj, term, include_eta = TRUE, spher_corr = TRUE,
                        eta_digits = 2, f_digits = 2, df_digits = 0,
                        append_to_table = FALSE, ...) {
  eza <- obj$ANOVA
  if (spher_corr & ("Sphericity Corrections" %in% names(obj))) {
    eza <- merge(eza, obj$"Sphericity Corrections", by = "Effect", all.x = TRUE)
    eza[!is.na(eza$GGe), "p"] <- eza[!is.na(eza$GGe), ]$"p[GG]"
  }
  rownames(eza) <- eza$Effect
  .format_anova_table(eza, term, c("DFn", "DFd", "p"), include_eta, eta_digits,
                      f_digits, df_digits, append_to_table, ...)
}

#' Describe [afex][afex::afex-package] ANOVA results
#'
#' Provides formatted string like _F_(num.Df, den.Df) = ..., _p_ ..., eta2 = ... based on [afex::aov_ez], [afex::aov_car], or [afex::aov_4] results.
#'
#' @param obj [afex][afex::afex-package] ANOVA object
#' @param term model term to describe (a string with the term name or its sequential number)
#' @param include_eta add generalized eta^2 for the model (default: TRUE)
#' @param eta_digits number of digits to use for eta^2 (default: 2)
#' @param f_digits number of digits to use for F (default: 2)
#' @param df_digits number of digits to use for df (default: 0)
#' @param append_to_table should the results be added to the [afex][afex::afex-package] ANOVA table (default: FALSE)
#' @param ... other parameters passed to [format_results]
#'
#' @return A character vector reporting F statistics, degrees of freedom,
#' p-values, and optional generalized eta-squared. With `append_to_table = TRUE`,
#' a `data.frame` of selected ANOVA rows with an added `res` column containing
#' the formatted results.
#' @method apa afex_aov
#' @export
#'
#' @examples
#' if (requireNamespace("afex", quietly = TRUE)) {
#'   data(stroop, package = "afex")
#'
#'   afex_res <- afex::aov_ez(
#'     id = "pno",
#'     dv = "rt",
#'     within = "condition",
#'     data = stroop[!is.na(stroop$rt), ],
#'     fun_aggregate = mean
#'   )
#'
#'   print(apa(afex_res, "condition"))
#'   print(apa(afex_res, "condition", eta_digits = 3))
#'   print(apa(afex_res, "condition", include_eta = FALSE))
#' }
apa.afex_aov <- function(obj, term, include_eta = TRUE, eta_digits = 2,
                          f_digits = 2, df_digits = 0, append_to_table = FALSE, ...) {
  eza <- data.frame(obj$anova_table)
  eza$effect <- row.names(eza)
  .format_anova_table(eza, term, c("num.Df", "den.Df", "Pr..F."), include_eta,
                      eta_digits, f_digits, df_digits, append_to_table, ...)
}

#' Describe ezStats results
#'
#' Returns formatted string with mean and SD from ezStats object
#'
#' @param obj data.frame returned by [ez::ezStats]
#' @param term model term to describe (a string with the term name or its sequential number)
#' @param ... other parameters passed to [apa_mean_sd]
#'
#' If a string is used for a term and there is more than one factor, "X:Y:Z" format is assumed (value in first column : value in the second, and so on). If no term is supplied, the first row is used. So if you need to select a term based on two or more variables, you can also just filter ezStats result beforehand (or use a row number as a term).
#'
#' @return string with formatted results
#' @export
#'
#' @examples
#' if (requireNamespace("ez", quietly = TRUE) && requireNamespace("afex", quietly = TRUE)) {
#'   # Using the Stroop dataset from afex package
#'   data(stroop, package = "afex")
#'   # Get descriptive statistics
#'
#'   ezstats_res <- ez::ezStats(
#'     data = stroop[!is.na(stroop$rt),],
#'     dv = rt,
#'     wid = pno,
#'     within = .(congruency)
#'   )
#'   
#'   # Format results in APA style
#'   print(apa_ezStats(ezstats_res, 1))
#'   print(apa_ezStats(ezstats_res, "incongruent"))
#'   print(apa_ezStats(ezstats_res, 1, digits = 3))
#' }
apa_ezStats <- function(obj, term = 1, ...) {
  if (which(colnames(obj) == "N")>2){
    rownames(obj) <- apply(obj[, 0:(which(colnames(obj) == "N") - 1)], 1, paste, collapse = ":")
  } else {
    rownames(obj) <- obj[,1]
  }
  with(obj[term, ], apa_mean_sd(m = Mean, sd = SD, ...))
}

#' Describe contrasts created by [emmeans][emmeans::emmeans-package]
#'
#' @param obj summary object from [emmeans::contrast]
#' @param term contrast number(s)
#' @param dtype description type, "t", "B", or any other letter
#' @param df include DF in t-test description (default: False)
#' @param ... other parameters passed to [format_results]
#'
#' @return string with formatted results
#' @method apa summary_emm
#' @export
#'
#' @examples
#' if (requireNamespace("emmeans", quietly = TRUE)) {
#'   # Create a model
#'   warp.lm <- lm(breaks ~ wool * tension, data = warpbreaks)
#'   
#'   # Create estimated marginal means
#'   warp.emm <- emmeans::emmeans(warp.lm, ~ tension | wool)
#'   
#'   # Create contrasts
#'   sum_contr <- summary(emmeans::contrast(warp.emm, "trt.vs.ctrl"))
#'   
#'   # Format results in APA style
#'   print(apa(sum_contr, 1))
#'   print(apa(sum_contr, 3))
#'   print(apa(sum_contr, 3, dtype = "t"))
#'   print(apa(sum_contr, 3, dtype = "c"))
#'   print(apa(sum_contr, 3, dtype = "c", df = TRUE))
#' }
apa.summary_emm <- function(obj, term, dtype = "B", df = FALSE, ...) {
  obj <- as.data.frame(obj[term, ])
  df_str <- ifelse(df, sprintf("(%i)", round(obj$df)), "")
  if (dtype == "t") {
    res_str <- sprintf("\\emph{%s}%s = %.2f, \\emph{p} %s", 
                       "t", df_str, obj$t.ratio, round_p(obj$p.value))
  } else if (dtype == "B") {
    res_str <- sprintf("\\emph{B} = %.2f (%.2f), \\emph{p} %s", 
                       obj$estimate, obj$SE, round_p(obj$p.value))
  } else {
    res_str <- sprintf("\\emph{B} = %.2f (%.2f), \\emph{%s}%s = %.2f, \\emph{p} %s", 
                       obj$estimate, obj$SE, "t", df_str, obj$t.ratio, round_p(obj$p.value))
  }
  format_results(res_str, ...)
}

#' @rdname apa.summary_emm
#' @method apa emmeans
#' @export
apa.emmeans <- apa.summary_emm

#' Describe lmerTest anova results
#'
#' @param obj lmerTest anova results
#' @param term model term to describe (a string with the term name or its sequential number)
#' @param f.digits decimal digits for F value
#' @param ... other parameters passed to [format_results]
#'
#' @return formatted string describing the results of anova
#' @method apa anova.merMod
#' @export
#'
#' @examples
#' if (requireNamespace("lmerTest", quietly = TRUE)) {
#'   # Sample data
#'   data(sleepstudy, package = "lme4")
#'   
#'   # Fit a mixed-effects model
#'   fit <- lmerTest::lmer(Reaction ~ Days + (1 + Days | Subject), sleepstudy)
#'   
#'   # ANOVA table
#'   afit <- anova(fit)
#'   
#'   # Format results in APA style
#'   print(apa(afit, "Days"))
#'   print(apa(afit, "Days", f.digits = 3))
#' }
apa.anova.merMod <- function(obj, term, f.digits = 2, ...) {
  obj <- data.frame(obj)
  res_str <- sprintf(paste0("\\emph{F}(%.0f, %.1f) = %.", f.digits, "f, \\emph{p} %s"), 
                     obj[term, "NumDF"], obj[term, "DenDF"], 
                     obj[term, "F.value"], round_p(obj[term, "Pr..F."]))
  format_results(res_str, ...)
}

#' Describe linearHypothesis test results
#'
#' @param obj hypothesis from [car::linearHypothesis]
#' @param ... additional parameters passed to [format_results]
#'
#' @return A character result reporting the chi-squared or F statistic,
#' degrees of freedom, and p-value for the linear-hypothesis test.
#' @method apa linearHypothesis
#' @export
#'
#' @examples
#' if (requireNamespace("car", quietly = TRUE) &&
#'     requireNamespace("carData", quietly = TRUE)) {
#'   # Create a linear model
#'   mod.davis <- lm(weight ~ repwt, data = carData::Davis)
#'   
#'   # Test hypotheses
#'   res <- car::linearHypothesis(mod.davis, c("(Intercept) = 0", "repwt = 1"))
#'   print(apa(res))
#'   
#'   # Test using Chi-square
#'   res.chi <- car::linearHypothesis(mod.davis, c("(Intercept) = 0", "repwt = 1"), test = "Chisq")
#'   print(apa(res.chi))
#' }
apa.linearHypothesis <- function(obj, ...) {
  res <- obj[2, ]
  
  if ("Chisq" %in% names(res)) {
    res <- res[c("Df", "Chisq", "Pr(>Chisq)")]
    res[3] <- round_p(res[3])
    format_results(do.call(sprintf, c(list("$\\chi^2$(%g) = %.2f, \\emph{p} %s"), res)), ...)
  } else {
    res[6] <- round_p(res[6])
    res <- res[c(3, 1, 5, 6)]
    format_results(do.call(sprintf, c(list("\\emph{F}(%g, %g) = %.2f, \\emph{p} %s"), res)), ...)
  }
}
