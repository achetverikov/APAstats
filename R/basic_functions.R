#' Format statistical results in APA style
#'
#' @description
#' The `apa` function provides a convenient way to format statistical results
#' according to American Psychological Association (APA) style guidelines.
#' It dispatches to various S3 methods depending on the class of the input object.
#'
#' @details
#' This function supports various statistical objects directly:
#' \itemize{
#'   \item [apa.ttest] for [stats::t.test] and [apa.r] for [stats::cor.test] (both with class \code{htest})
#'   \item [apa.aov] for [stats::aov] and [apa.anova] for [car::Anova]
#'   \item [apa.glm] for [stats::glm] and [stats::lm]
#'   \item [apa.lmer] for [lme4::lmer] and [apa.lmert] for [lmerTest::lmer]
#'   \item [apa.brmsfit] for [brms::brm] and [apa.BFBayesFactor] for [BayesFactor::anovaBF]
#'   \item [apa.chisq.test] for [stats::chisq.test]
#'   \item [apa.dip.test] for [diptest::dip.test]
#'   \item [apa.roc.test] for [pROC::roc.test]
#' }
#'
#' In addition, there are standalone utility functions for formatting:
#' \itemize{
#'   \item [apa_mean_sd]: Format means and standard deviations
#'   \item [apa_mean_conf]: Format means and confidence intervals
#'   \item [apa_binom_mean_conf]: Format binomial means and confidence intervals
#'   \item [apa_mean_and_t]: Compare means with t-tests and format results
#'   \item [table_mean_conf]: Get formatted means and CIs (for tables)
#'   \item [table_mean_conf_by]: Get formatted means and CIs by group (for tables)
#' }
#'
#' @param obj An object to format in APA style
#' @param ... Additional arguments passed to specific methods
#'
#' @return A formatted string in APA style
#' @export
#' 
#' @examples
#' # t-test example
#' t_res <- t.test(rnorm(30), rnorm(30, 0.5))
#' apa(t_res)
#' 
#' # Correlation example
#' cor_res <- cor.test(mtcars$mpg, mtcars$wt)
#' apa(cor_res)
apa <- function(obj, ...) {
  UseMethod("apa")
}

#' @export
apa.default <- function(obj, ...) {
  stop(paste("No apa method available for objects of class", 
             paste(class(obj), collapse=", "), 
             "\nSee ?apa for supported object types."))
}

#' Format t-test results in APA style
#'
#' @param obj An object from [stats::t.test]
#' @param show.mean Include mean value in results (useful for one-sample test)
#' @param abs Show the absolute value of t-statistic
#' @param ... Other arguments passed to [format.results]
#'
#' @return A string with t-test value, degrees of freedom, p-value, and optionally the mean with CI
#' @method apa htest
#' @export
#'
#' @examples
#' # One-sample t-test
#' t_res <- t.test(rnorm(20, mean = -10, sd = 2))
#' apa(t_res)
#' apa(t_res, show.mean = TRUE)
#' 
#' # Two-sample t-test
#' t_two <- t.test(rnorm(20), rnorm(20, mean = 0.8))
#' apa(t_two)
apa.htest <- function(obj, show.mean = FALSE, abs = FALSE, ...) {
  # This is a generic method for htest objects
  # Different implementations for different test types
  if (grepl('correl', obj$method)) {
    return(apa.r(obj, ...))
  } else if (grepl('Chi', obj$method)) {
    return(apa.chisq.test(obj, ...))
  } else if (grepl('dip', obj$method)) {
    return(apa.dip.test(obj, ...))
  } else {
    # Default to t-test
    return(apa.ttest(obj, show.mean, abs, ...))
  }
}

#' Format t-test results in APA style
#'
#' @param obj An object from [stats::t.test]
#' @param show.mean Include mean value in results (useful for one-sample test)
#' @param abs Show the absolute value of t-statistic
#' @param ... Other arguments passed to [format.results]
#'
#' @return A string with t-test value, degrees of freedom, p-value, and optionally the mean with CI
#' @export
#'
#' @examples
#' # One-sample t-test
#' t_res <- t.test(rnorm(20, mean = -10, sd = 2))
#' apa(t_res)
#' apa(t_res, show.mean = TRUE)
#' apa(t_res, show.mean = TRUE, abs = TRUE)
#' 
#' # Two-sample t-test
#' t_two <- t.test(rnorm(20), rnorm(20, mean = 0.8))
#' apa(t_two)
apa.ttest <- function(obj, show.mean = FALSE, abs = FALSE, ...) {
  if (abs) obj$statistic <- abs(obj$statistic)
  if (show.mean == TRUE) {
    res_str <- sprintf("\\emph{M} = %.2f [%.2f, %.2f], \\emph{t}(%.1f) = %.2f, \\emph{p} %s", 
                       obj$estimate, obj$conf.int[1], obj$conf.int[2], 
                       obj$parameter, obj$statistic, round.p(obj$p.value))
  } else {
    res_str <- sprintf("\\emph{t}(%.1f) = %.2f, \\emph{p} %s", 
                       obj$parameter, obj$statistic, round.p(obj$p.value))
  }
  format.results(res_str, ...)
}

#' Format correlation test results in APA style
#'
#' @param obj An object from [stats::cor.test]
#' @param ... Other arguments passed to [format.results]
#'
#' @return A string with correlation coefficient, degrees of freedom, and p-value
#' @export
#'
#' @examples
#' # Pearson correlation
#' x <- rnorm(40)
#' y <- x * 0.6 + rnorm(40, 0, 0.8)
#' rc <- cor.test(x, y)
#' apa(rc)
apa.r <- function(obj, ...) {
  format.results(sprintf("\\emph{r}(%.0f) = %.2f, \\emph{p} %s", 
                         obj$parameter, obj$estimate, round.p(obj$p.value)), ...)
}

#' Format chi-square test results in APA style
#'
#' @param obj A result from [stats::chisq.test]
#' @param v Add Cramer's V (default: TRUE)
#' @param addN Add N (default: TRUE)
#' @param ... Other parameters passed to [format.results]
#' @method apa chisq.test
#' @return Formatted results string
#' @export
#' 
#' @examples
#' # Create a contingency table
#' M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
#' dimnames(M) <- list(
#'   gender = c("F", "M"),
#'   party = c("Democrat", "Independent", "Republican")
#' )
#' chi_result <- chisq.test(M)
#' apa(chi_result)
apa.chisq.test <- function(obj, v = TRUE, addN = TRUE, ...) {
  tbl <- obj$observed
  if (length(dim(tbl)) != 2 & !is.matrix(tbl)) tbl <- table(tbl)
  cv <- sqrt(obj$statistic / (sum(tbl) * min(dim(tbl) - 1)))
  n <- sum(tbl)
  res <- sprintf("$\\chi^2$(%i%s) = %.2f, \\emph{p} %s%s", 
                 obj$parameter, 
                 ifelse(addN, paste0(", \\emph{N} = ", sum(tbl)), ""), 
                 obj$statistic, 
                 round.p(obj$p.value), 
                 ifelse(v, paste0(", \\emph{V} = ", omit.zeroes(round(cv, 2))), ""))
  format.results(res, ...)
}

#' Describe Hartigans' dip test results
#'
#' @param obj a result from [diptest::dip.test]
#' @param ... other parameters passed to [format.results]
#' @method apa dip.test
#' @return formatted results string
#' @export
#' 
#' @examples
#' if (requireNamespace("diptest", quietly = TRUE)) {
#'   # Generate bimodal data
#'   bimodal_data <- c(rnorm(100, -2, 1), rnorm(100, 2, 1))
#'   
#'   # Perform dip test
#'   dip_result <- diptest::dip.test(bimodal_data)
#'   
#'   # Format results in APA style
#'   apa(dip_result)
#' }
apa.dip.test <- function(obj, ...) {
  res <- sprintf("\\emph{D} = %.2f, \\emph{p} %s", obj$statistic, round.p(obj$p.value))
  format.results(res, ...)
}

#' Describe differences between ROC curves
#'
#' @param obj a difference between the ROC curves from [pROC::roc.test]
#' @return result
#' @method apa roc.test
#' @export
#'
#' @examples
#' if (requireNamespace("pROC", quietly = TRUE)) {
#'   # Create sample data
#'   set.seed(42)
#'   n <- 100
#'   group <- factor(sample(c(0, 1), n, replace = TRUE))
#'   test1 <- rnorm(n, mean = 1 * (as.numeric(group) - 1))
#'   test2 <- rnorm(n, mean = 0.7 * (as.numeric(group) - 1))
#'   
#'   # Create ROC curves
#'   roc1 <- pROC::roc(group, test1)
#'   roc2 <- pROC::roc(group, test2)
#'   
#'   # Test difference between ROC curves
#'   roc_diff <- pROC::roc.test(roc1, roc2)
#'   
#'   # Format results in APA style
#'   apa(roc_diff)
#' }
apa.roc.test <- function(obj) {
  sprintf("\\emph{D} = %0.2f, \\emph{p} %s", obj$statistic, round.p(obj$p.value))
}

#' @rdname apa.roc.test
#' @method apa roc.diff
#' @export
apa.roc.diff <- apa.roc.test

#' Format means and standard deviations in APA style
#'
#' @param x A numeric vector
#' @param m Pre-computed mean (optional)
#' @param sd Pre-computed standard deviation (optional)
#' @param digits Number of digits for rounding
#' @param dtype Format type: "p" for parentheses or "c" for comma
#' @param m_units Units for mean (e.g., "°" for degrees)
#' @param sd_units Units for standard deviation
#' @param ... Additional arguments passed to [format.results]
#'
#' @return A formatted string with mean and standard deviation in APA style
#' @export
#'
#' @examples
#' x <- rnorm(1000, 50, 25)
#' apa_mean_sd(x)
#' apa_mean_sd(m = 50.2, sd = 9.8)  # With pre-computed values
#' apa_mean_sd(x, dtype = "c")  # Comma format instead of parentheses
apa_mean_sd <- function(x = NULL, m = NULL, sd = NULL, digits = 2, 
                        dtype = "p", m_units = "", sd_units = "", ...) {
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
#' Format mean and confidence interval values into APA-style string
#'
#' @param mean_val The mean value
#' @param lower_ci Lower confidence interval bound
#' @param upper_ci Upper confidence interval bound
#' @param addCI Add "95% CI =" prefix
#' @param digits Number of digits to use
#' @param ... Other arguments passed to [format.results]
#'
#' @return A formatted string with mean and confidence intervals
#' @export
#' @examples
#' # Example usage
#' mean_val <- 5.67
#' lower_ci <- 4.56
#' upper_ci <- 6.78
#' # Format mean and CI in APA style
#' apa_format_mean_conf(mean_val, lower_ci, upper_ci)
#' # Format mean and CI with additional CI prefix
#' apa_format_mean_conf(mean_val, lower_ci, upper_ci, addCI = TRUE)
#' # Format mean and CI with custom number of digits
#' apa_format_mean_conf(mean_val, lower_ci, upper_ci, digits = 3)
#' 
apa_format_mean_conf <- function(mean_val, lower_ci, upper_ci, addCI = FALSE, digits = 2, ...) {
  ci_str <- ifelse(addCI, ", 95%% \\emph{CI} =", "")
  format.results(sprintf(paste0(
    "\\emph{M} = %.",
    digits, "f", ci_str, " [%.", digits, "f, %.", digits,
    "f]"
  ), mean_val, lower_ci, upper_ci), ...)
}

#' Format means and confidence intervals in APA style
#'
#' @param x A numeric vector
#' @param bootCI Use bootstrapped CI (TRUE) or normal approximation (FALSE)
#' @param addCI Add "95% CI =" prefix
#' @param digits Number of digits to use
#' @param transform.means An optional function to transform the means and CI to another scale
#' @param ... Other arguments passed to [format.results]
#'
#' @return A string with a mean followed by confidence intervals in square brackets
#' @export
#'
#' @examples
#' x <- runif(100, 0, 50)
#' apa_mean_conf(x)
#' apa_mean_conf(x, bootCI = FALSE)  # Normal approximation instead of bootstrap
apa_mean_conf <- function(x, bootCI = TRUE, addCI = FALSE, digits = 2, 
                          transform.means = NULL, ...) {
  if (bootCI) {
    res <- Hmisc::smean.cl.boot(x)
  } else {
    res <- Hmisc::smean.cl.normal(x)
  }
  if (!is.null(transform.means)) {
    res <- sapply(res, transform.means)
  }
  res <- as.list(res)
  apa_format_mean_conf(res$Mean, res$Lower, res$Upper, addCI = addCI, digits = digits, ...)
}

#' Format binomial proportions and confidence intervals in APA style
#'
#' @param x A vector of zeros and ones
#' @param digits Number of digits in results
#' @param ... Additional arguments passed to [format.results]
#'
#' @return A string with the mean and confidence interval in square brackets
#' @export
#'
#' @examples
#' # Generate binomial data
#' set.seed(123)
#' x <- rbinom(500, 1, prob = 0.7)
#' 
#' # Format results in APA style
#' apa_binom_mean_conf(x)
apa_binom_mean_conf <- function(x, digits = 2, ...) {
  format.results(with(data.frame(Hmisc::binconf(sum(x), length(x))), 
                      sprintf(paste0("\\emph{M} = %.", digits, "f [%.", digits, "f, %.", digits, "f]"), 
                              PointEst, Lower, Upper)), ...)
}

#' Compare means with t-tests and format in APA style
#'
#' @param x Dependent variable (numeric vector)
#' @param by Independent variable (factor with 2 levels)
#' @param which.mean Which means to show (0=none, 1=first, 2=second, 3=both)
#' @param digits Number of digits in results (default: 2)
#' @param paired Should it be a paired test (default: FALSE)
#' @param eff.size Should we include effect size (default: FALSE)
#' @param abs Should we show the absolute value if the t-test (T) or keep its sign (FALSE, default)
#' @param aggregate_by Do the aggregation by the third variable(s): either NULL (default), a single vector variable, or a list of variables to aggregate by.
#' @param transform.means A function to transform means and confidence intervals (default: NULL)
#' @param ... Additional arguments passed to [format.results]
#'
#' @return A formatted string with t-test results in APA style
#' @export
#'
#' @examples
#' # Using simulated data
#' rt <- rnorm(100)
#' gr <- factor(rep(c("A", "B"), each = 50))
#' apa_mean_and_t(rt, gr)
#' apa_mean_and_t(rt, gr, which.mean = 3)
#' apa_mean_and_t(rt, gr, eff.size = TRUE)
apa_mean_and_t <- function(x, by, which.mean = 1, digits = 2, paired = FALSE, 
                           eff.size = FALSE, abs = FALSE, aggregate_by = NULL, 
                           transform.means = NULL, ...) {
  
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
    summaries[, c("x", "Lower", "Upper")] <- sapply(summaries[, c("x", "Lower", "Upper")], transform.means)
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
    t_res <- t.test(x[by==unique(by)[1]],x[by==unique(by)[2]], paired = TRUE)
  } else {
    t_res <- t.test(x~by)
  }
  
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

#' Get a list with means and confidence intervals
#'
#' @param x numeric vector to compute the mean for
#' @param digits number of digits in results (default: 2)
#' @param binom compute binomial CI instead of the usual ones
#' @param ... other parameters passed to [format.results]
#'
#' @return a list with a mean and CI as formatted strings
#' @export
#'
#' @examples
#' # For continuous data
#' x <- rnorm(100, mean = 50, sd = 10)
#' table_mean_conf(x)
#' table_mean_conf(x, digits = 3)
#' 
#' # For binary data
#' y <- rbinom(100, 1, prob = 0.7)
#' table_mean_conf(y, binom = TRUE)
table_mean_conf <- function(x, digits = 2, binom = FALSE, ...) {
  if (binom) {
    res <- data.frame(Hmisc::binconf(sum(x), length(x)))
    colnames(res) <- c("Mean", "Lower", "Upper")
  } else {
    res <- as.list(Hmisc::smean.cl.boot(x))
  }
  res <- with(res, c(f.round(Mean, digits), 
                     sprintf(paste0("[%.", digits, "f, %.", digits, "f]"), 
                             Lower, Upper)))
  res
}

#' Get a list of means and confidence intervals by group
#'
#' @param x numeric vector to compute the mean for
#' @param by grouping variable
#' @param digits number of digits in results (default: 2)
#' @param binom compute binomial CI instead of the usual ones
#'
#' @return a list of means and CI with keys corresponding to the group labels
#' @export
#'
#' @examples
#' # Group means for continuous data
#' x <- rnorm(100, mean = 50, sd = 10)
#' groups <- rep(c("A", "B", "C"), length.out = 100)
#' table_mean_conf_by(x, groups)
#' 
#' # Group means for binary data 
#' y <- rbinom(100, 1, prob = 0.7)
#' table_mean_conf_by(y, groups, binom = TRUE)
table_mean_conf_by <- function(x, by, digits = 2, binom = FALSE) {
  tapply(x, by, table_mean_conf, digits, binom)
}


#' Format results from list objects in APA style
#'
#' This function detects the type of statistical results stored in a list object
#' and redirects to the appropriate APA formatting method. Currently supports
#' [ezANOVA] results ([apa.ezANOVA]).
#'
#' @param obj A list object containing statistical results
#' @param ... Additional arguments passed to the appropriate method
#'
#' @return A formatted string with statistical results in APA style
#' @method apa list
#' @export
#'
apa.list <- function(obj, ...) {
  # Check for different types of list results and dispatch accordingly
  
  # Check if this looks like an ezANOVA result
  if ("ANOVA" %in% names(obj)) {
    return(apa.ezANOVA(obj, ...))
  }
  

  # If no matching type is found
  stop("Unrecognized list format. This list doesn't match any supported statistical result types.")
}