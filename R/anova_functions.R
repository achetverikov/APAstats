#' Describe the results of ANOVA models or model comparisons 
#'
#' @param obj ANOVA results from [car::Anova] or [stats::anova]
#' @param term model term to describe (a string with the term name or its sequential number, default: 2)
#' @param f.digits number of digits in the results (default: 2)
#' @param ... other parameters passed to [format.results]
#' 
#' @details
#' When using model comparison version, `term` can only be a number.
#'  
#' @return Formatted string with F (or chi2), df, and p
#' @method apa anova
#' @export
#'
#' @examples
#' # Model comparison version
#' mod1 <- lm(conformity ~ 1, data = carData::Moore)
#' mod2 <- lm(conformity ~ fcategory, data = carData::Moore, 
#'            contrasts = list(fcategory = contr.sum))
#' mod3 <- lm(conformity ~ fcategory * partner.status, data = carData::Moore, 
#'            contrasts = list(fcategory = contr.sum, partner.status = contr.sum))
#' anova_obj <- anova(mod1, mod2, mod3)
#' 
#' apa(anova_obj)
#' apa(anova_obj, 3)
#' 
#' # car::Anova version
#' if (requireNamespace("car", quietly = TRUE)) {
#'   mod <- lm(conformity ~ fcategory * partner.status, data = carData::Moore, 
#'             contrasts = list(fcategory = contr.sum, partner.status = contr.sum))
#'   afit <- car::Anova(mod)
#'   
#'   apa(afit, "fcategory")
#'   apa(afit, 2, 4)
#' }
apa.anova <- function(obj, term = 2, f.digits = 2, ...) {
  if ("Df.res" %in% colnames(obj)) {
    res_str <- sprintf(paste0("\\emph{F}(%i, %.", f.digits, "f) = %.", f.digits, "f, \\emph{p} %s"), 
                       obj[term, "Df"], obj[term, "Df.res"], obj[term, "F"], 
                       round.p(obj[term, "Pr(>F)"]))
  } else if ("F" %in% names(obj)) {
    res_str <- sprintf(paste0("\\emph{F}(%.0f, %.0f) = %.", f.digits, "f, \\emph{p} %s"), 
                       obj[term, "Df"], obj[term, "Res.Df"], obj[term, "F"], 
                       round.p(obj[term, "Pr(>F)"]))
  } else if ("Chisq" %in% names(obj)) {
    res_str <- sprintf(paste0("$\\chi^2$(%i) = %.", f.digits, "f, \\emph{p} %s"), 
                       obj[term, "Df"], obj[term, "Chisq"], 
                       round.p(obj[term, "Pr(>Chisq)"]))
  } else if ("F value" %in% names(obj)) {
    res_str <- sprintf(paste0("\\emph{F}(%i, %i) = %.", f.digits, "f, \\emph{p} %s"), 
                       obj[term, "Df"], obj["Residuals", "Df"], obj[term, "F value"], 
                       round.p(obj[term, "Pr(>F)"]))
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
#' @param ... other parameters passed to [apa.anova]
#'
#' @return formatted string with F(df_numerator, df_denomiator) = F_value, p =/< p_value
#' @method apa aov
#' @export
#' 
#' @examples
#' # Using the mtcars dataset
#' fit <- aov(mpg ~ cyl * am, data = mtcars)
#' apa(fit, 'cyl')
#' apa(fit, 'am')
#' apa(fit, 'cyl:am')
#' 
#' # Using a different SS type
#' apa(fit, 'cyl', sstype = 3)
apa.aov <- function(obj, term, sstype = 2, ...) {
  if (requireNamespace("car", quietly = TRUE)) {
    afit <- car::Anova(obj, type = sstype)
    apa(afit, term = term, ...)
  } else {
    stop("Package 'car' is required for this function")
  }
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
#' @param ... other parameters passed to [format.results]
#'
#' @return string with formatted results
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
#'   apa(ez_res, "condition")
#'   apa(ez_res, "condition", eta_digits = 3)
#'   apa(ez_res, "condition", include_eta = FALSE)
#' }
apa.ezANOVA <- function(obj, term, include_eta = TRUE, spher_corr = TRUE, 
                        eta_digits = 2, f_digits = 2, df_digits = 0, 
                        append_to_table = FALSE, ...) {
  eza <- obj$ANOVA
  if (spher_corr & ("Sphericity Corrections" %in% names(obj))) {
    eza <- merge(eza, obj$`Sphericity Corrections`, by = "Effect", all.x = TRUE)
    eza[!is.na(eza$GGe), "p"] <- eza[!is.na(eza$GGe), ]$`p[GG]`
  }
  rownames(eza) <- eza$Effect
  
  suffix <- sprintf(", $\\eta$^2^~G~ %s", 
                    round.p(eza[term, "ges"],
                            digits = eta_digits,
                            replace.very.small = 10^(-eta_digits)))
  if (include_eta == FALSE) {
    suffix <- ""
  }
  res <- format.results(sprintf("\\emph{F}(%.*f, %.*f) = %.*f, \\emph{p} %s%s", 
                                df_digits, eza[term, "DFn"], 
                                df_digits, eza[term, "DFd"], 
                                f_digits, eza[term, "F"], 
                                round.p(eza[term, "p"]), suffix), ...)
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
#'   apa_ezStats(ezstats_res, 1)
#'   apa_ezStats(ezstats_res, "incongruent")
#'   apa_ezStats(ezstats_res, 1, digits = 3)
#' }
apa_ezStats <- function(obj, term = 1, ...) {
  if (which(colnames(obj) == "N")>2){
    rownames(obj) <- apply(obj[, 0:(which(colnames(obj) == "N") - 1)], 1, paste, collapse = ":")
  } else {
    rownames(obj) <- obj[,1]
  }
  with(obj[term, ], apa_mean_sd(m = Mean, sd = SD, ...))
}

#' Describe contrasts created by emmeans
#'
#' @param obj summary object from [emmeans::contrast]
#' @param term contrast number(s)
#' @param dtype description type, "t", "B", or any other letter
#' @param df include DF in t-test description (default: False)
#' @param ... other parameters passed to [format.results]
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
#'   apa(sum_contr, 1)
#'   apa(sum_contr, 3)
#'   apa(sum_contr, 3, dtype = "t")
#'   apa(sum_contr, 3, dtype = "c")
#'   apa(sum_contr, 3, dtype = "c", df = TRUE)
#' }
apa.summary_emm <- function(obj, term, dtype = "B", df = FALSE, ...) {
  obj <- as.data.frame(obj[term, ])
  df_str <- ifelse(df, sprintf("(%i)", round(obj$df)), "")
  if (dtype == "t") {
    res_str <- sprintf("\\emph{%s}%s = %.2f, \\emph{p} %s", 
                       "t", df_str, obj$t.ratio, round.p(obj$p.value))
  } else if (dtype == "B") {
    res_str <- sprintf("\\emph{B} = %.2f (%.2f), \\emph{p} %s", 
                       obj$estimate, obj$SE, round.p(obj$p.value))
  } else {
    res_str <- sprintf("\\emph{B} = %.2f (%.2f), \\emph{%s}%s = %.2f, \\emph{p} %s", 
                       obj$estimate, obj$SE, "t", df_str, obj$t.ratio, round.p(obj$p.value))
  }
  format.results(res_str, ...)
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
#' @param ... other parameters passed to [format.results]
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
#'   apa(afit, "Days")
#'   apa(afit, "Days", f.digits = 3)
#' }
apa.anova.merMod <- function(obj, term, f.digits = 2, ...) {
  obj <- data.frame(obj)
  res_str <- sprintf(paste0("\\emph{F}(%.0f, %.1f) = %.", f.digits, "f, \\emph{p} %s"), 
                     obj[term, "NumDF"], obj[term, "DenDF"], 
                     obj[term, "F.value"], round.p(obj[term, "Pr..F."]))
  format.results(res_str, ...)
}

#' Describe linearHypothesis test results
#'
#' @param obj hypothesis from [car::linearHypothesis]
#' @param ... additional parameters passed to [format.results]
#'
#' @return results of \eqn{\chi^2} or _F_ test
#' @method apa linearHypothesis
#' @export
#'
#' @examples
#' if (requireNamespace("car", quietly = TRUE)) {
#'   # Create a linear model
#'   mod.davis <- lm(weight ~ repwt, data = carData::Davis)
#'   
#'   # Test hypotheses
#'   res <- car::linearHypothesis(mod.davis, c("(Intercept) = 0", "repwt = 1"))
#'   apa(res)
#'   
#'   # Test using Chi-square
#'   res.chi <- car::linearHypothesis(mod.davis, c("(Intercept) = 0", "repwt = 1"), test = "Chisq")
#'   apa(res.chi)
#' }
apa.linearHypothesis <- function(obj, ...) {
  res <- obj[2, ]
  
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