#' Mean with na.rm=T
#'
#' @param x a vector of numbers
#' @param ... other arguments passed to mean
#'
#' @return mean of x with NA removed
#' @export mean_nn
#'
#' @examples
#' x <- c(NA, 10, 90)
#' mean(x)
#' mean_nn(x)
#'

mean_nn <- function(x, ...) {
  mean(x, na.rm = TRUE, ...)
}

#' SD with na.rm=T
#'
#' @param x a vector of numbers
#' @param ... other arguments passed to sd
#'
#' @return sd of x with NA removed
#' @export sd_nn
#'
#' @examples
#' x <- c(NA, 10, 90)
#' sd(x)
#' sd_nn(x)
sd_nn <- function(x, ...) {
  sd(x, na.rm = TRUE, ...)
}

#' Sum with na.rm=T
#'
#' @param x a vector of numbers
#' @param ... other arguments passed to sum
#'
#' @return sum of x with NA removed
#' @export sum_nn
#'
#' @examples
#' x <- c(NA, 10, 90)
#' sum(x)
#' sum_nn(x)
sum_nn <- function(x, ...) {
  sum(x, na.rm = TRUE, ...)
}

#' Length of unique values
#'
#' Counts unique values
#'
#' @param x a vector
#'
#' @return number of unique values in x
#' @export
#'
#' @examples
#' x <- c(5, 7, 8, 9, 5, 7)
#' length(x)
#' lengthu(x)
lengthu <- function(x) {
  length(unique(x))
}

#' Drop empty columns from a data.frame
#'
#' Drop empty (consisting of NA only) columns from a data.frame. Based on https://stackoverflow.com/a/2644009/1344028
#'
#' @param df a data.frame
#'
#' @return data.frame without empty columns
#' @export drop_empty_cols
#'
#' @examples
#' df <- data.frame(x = rnorm(20), y = rep("A", 20), z = rep(NA, 20))
#' str(df)
#' df <- drop_empty_cols(df)
#' str(df)
#'
drop_empty_cols <- function(df) {
  Filter(function(x) !all(is.na(x)), df)
}

#' Binomial confidence interval as a vector
#'
#' @param x a vector of 0 and 1
#'
#' @return a vector of mean, lower CI, upper CI, and length of x
#' @export binom_ci
#'
#' @examples
#' binom_ci(rbinom(500, 1, prob = 0.7))
binom_ci <- function(x) {
  ci <- Hmisc::binconf(sum(x), length(x))
  c(y = ci[1], ymin = ci[2], ymax = ci[3], len = length(x))
}

#' Rounded mean
#'
#' Mean rounded to the specified number of digits
#'
#' @param x a number
#' @param ... other arguments passed to [apastats2::f_round]
#' 
#' @return Mean rounded to the specified number of digits (string)
#' @export mean_round
#'
#' @examples
#' mean_round(c(10, 99))
#' mean_round(c(10, 99, NA))
#' mean_round(c(10, 99), 2)
mean_round <- function(x, ...) {
  f_round(mean_nn(x), ...)
}

#' Rounded SD
#'
#' SD rounded to the specified number of digits
#'
#' @param x a number
#' @param ... other arguments passed to [apastats2::f_round]
#'
#' @return Mean rounded to the specified number of digits (string)
#' @export
#'
#' @examples
#' sd_round(c(10, 99))
#' sd_round(c(10, 99, NA))
#' sd_round(c(10, 99), 2)
sd_round <- function(x, ...) {
  f_round(sd_nn(x), ...)
}

#' Quietly load libraries
#'
#' @param libs a vector of libraries names
#'
#' @return None
#' @export
#'
#' @examples
#' load_libs(c("ggplot2", "apastats2", "Hmisc"))
load_libs <- function(libs) {
  suppressMessages(invisible(lapply(libs, require, character.only = TRUE)))
}


#' Cut a numeric variable into groups (bins) with advanced options
#'
#' @param x vector of numeric values to cut into groups
#' @param ncuts number of cuts (default: NULL)
#' @param eq_groups should the groups be equal (default: FALSE)
#' @param cuts where to put the cuts (default: NULL), not used if ncuts is used
#' @param num_labels should the labels be transformed into numbers (default: FALSE)
#' @param labels a vector of labels to use for the groups (default: NULL)
#' @param include_oob include values outside of the boundaries provided in `cuts` (default: TRUE)
#' @param labels_at_means should labels be created as means between cuts (T) or as pairs of cuts (F)
#' @param label_pairs_format formatting string to use when labels are generated from pairs of cuts (default: \[%.2f, %.2f\])
#' @param ... other parameters passed to [base::cut]
#'
#' If `ncuts` is used, then the variable is cut into N cuts either of equal group size (eq_groups = TRUE) or equally distant from each other (eq_groups = FALSE). If `labels` are not provided, they are generated as  means between cuts if labels_at_means is T.
#'
#' @return a vector of group labels the same length as the original value vector
#' @export
#'
#' @examples
#' set.seed(1)
#' x <- sample(1:100, 20)
#' sort(x)
#'
#' adv_cut(x, ncuts = 5)
#' adv_cut(x, ncuts = 5, eq_groups = TRUE)
#' adv_cut(x, ncuts = 5, eq_groups = TRUE, num_labels = TRUE)
#' adv_cut(x, ncuts = 5, eq_groups = TRUE, labels_at_means = FALSE)
#' adv_cut(x, cuts = seq(0, 100, by = 20))
#' adv_cut(x, cuts = seq(0, 100, by = 20), labels_at_means = FALSE)
#' adv_cut(x, cuts = seq(0, 100, by = 20), 
#'            labels_at_means = FALSE, label_pairs_format = "[%i, %i]")
#' 
adv_cut <- function(x, ncuts = NULL, eq_groups = FALSE, cuts = NULL, num_labels = FALSE,
                    labels = NULL, include_oob = TRUE, labels_at_means = TRUE, label_pairs_format = "[%.2f, %.2f]", ...) {
  if (!is.null(ncuts)) {
    if (eq_groups) {
      cuts <- Hmisc::cut2(x, g = ncuts, onlycuts = TRUE)
    } else {
      cuts <- seq(min(x), max(x), length.out = ncuts + 1)
    }
  }

  if (include_oob) {
    x[x > max(cuts)] <- max(cuts)
    x[x <= min(cuts)] <- min(cuts) + 1e-12
  }

  s <- cut(x, breaks = cuts, ...)

  if (is.null(labels)) {
    if (labels_at_means) {
      labels <- seq_mean(cuts)
    } else {
      labels <- sapply(1:(length(cuts) - 1), \(i) sprintf(label_pairs_format, cuts[i], cuts[i + 1]))
    }
  }

  levels(s) <- labels

  if (num_labels) {
    s <- as.numeric(as.character(s))
  }
  s
}

#' Get the means between the points of a sequence
#'
#' @param x a vector of numeric values
#'
#' @return means between consecutive values
#' @export
#'
#' @examples
#'
#' seq_mean(c(1, 3, 5, 7, 10))
#'
seq_mean <- function(x) {
  x[1:(length(x) - 1)] + diff(x) / 2
}

#' Get nice matrix of fixed effects from lmer
#'
#' @param fit.lmer fitted `lme4::lmer` model object
#'
#' @return result
#' @export
#'

lmer_fixef <- function(fit.lmer) {
  ss <- sqrt(diag(as.matrix(vcov(fit.lmer))))
  cc <- lme4::fixef(fit.lmer)
  data.frame(Estimate = cc, Std.Err = ss, t = cc / ss)
}


#' Omit leading zero from number
#'
#' @param x A number
#' @param digits Number of decimal digits to keep
#'
#' @return A number without leading zero
#' @export
#'
#' @examples
#' omit_zeroes(0.2312)
#' omit_zeroes(0.2312, digits = 3)
#' omit_zeroes("000.2312", digits = 1)
omit_zeroes <- function(x, digits = 2) {
  sub("^.", "", f_round(x, digits))
}

#' Formatted rounding
#'
#' @param x A number
#' @param digits Number of decimal digits to keep
#' @param strip.lead.zeros remove zero before decimal point (default is false)
#'
#' @return Value A number rounded to the specified number of digits
#' @export
#'
#' @examples
#' f_round(5.8242)
#' f_round(5.8251)
#' f_round(5.82999, digits = 3)
#' f_round(5.82999, digits = 4)
f_round <- function(x, digits = 2, strip.lead.zeros = FALSE) {
  values_string <- stringr::str_trim(format(round(as.numeric(x), digits), nsmall = digits))
  if (strip.lead.zeros) {
    values_string <- sub("^0", "", values_string)
    values_string <- sub("^-0", "-", values_string)
  }
  return(values_string)
}

#' Round *p*-value
#'
#' If p-value is <= 0.001, returns ".001" else returns p-value rounded to the specified number of digits, optionally including relation sign ("<" or "=").
#'
#' @param values a vector of p-values
#' @param include.rel include relation sign
#' @param digits a number of decimal digits
#' @param strip.lead.zeros remove zero before decimal point
#' @param replace.very.small replace values lower than this criteria (NULL to keep values as is)
#' @return Formatted p-value
#' @export
#'
#' @examples
#' p_values <- c(0.025, 0.0001, 0.001, 0.568)
#' round_p(p_values)
#' round_p(p_values, digits = 2)
#' round_p(p_values, include.rel = FALSE)
#' round_p(p_values, include.rel = FALSE, strip.lead.zeros = FALSE)
#' round_p(p_values, include.rel = FALSE, strip.lead.zeros = FALSE, replace.very.small = 0.01)
#'
round_p <- function(values, include.rel = 1, digits = 3, strip.lead.zeros = TRUE, replace.very.small = 0.001) {
  values <- as.numeric(values)
  rel <- ifelse(include.rel, "= ", "")
  values_string <- format(round(values, digits = digits), nsmall = digits)
  if (strip.lead.zeros) {
    values_string <- sub("^0", "", values_string)
  }
  values_string <- paste(rel, values_string, sep = "")

  if (!is.null(replace.very.small)) {
    values_string[abs(values) <= replace.very.small] <- paste0("< ", ifelse(strip.lead.zeros, sub("^0", "", replace.very.small), replace.very.small))
  }

  values_string
}
#' Format results
#'
#' Internal function used to convert latex-formatted results to pandoc style.
#'
#' @param res_str text
#' @param type 'pandoc', 'latex', or 'plotmath' (the latter is very poorly implemented)
#'
#' @return \code{res_str} with latex 'emph' tags replaced with pandoc '_'
#' @export

format_results <- function(res_str, type = "pandoc") {
  if (type == "latex") {
    res_str
  } else if (type == "pandoc") {
    stringr::str_replace_all(res_str, "\\\\emph\\{(.*?)\\}", "_\\1_")
  } else if (type == "plotmath") {
    res_str <- stringr::str_replace_all(res_str, "\\\\emph\\{(.*?)\\}", "italic(\\1)")
    res_str <- stringr::str_replace_all(res_str, "=", "==")
    res_str <- stringr::str_replace_all(res_str, "_([^_=^ ]*)", "[\\1]")
    if (any(grepl(",", res_str))) {
      res_str <- paste0("list(", res_str, ")")
    }
  }
}


#' Double aggregation
#'
#' Aggregates value twice providing mean of means, SD of SDs, etc.
#'
#' @param x value to aggregate
#' @param by vector to aggregate by (e.g., ID of participant)
#' @param fun function to apply
#' @param ... additional parameters passed to fun
#'
#' @return value aggregated first by specified vector and then aggregated again
#' @export
#'
#' @examples
#'
#' x <- rnorm(100)
#' id <- rep(1:10, each = 10)
#'
#' aggregate(x ~ id, FUN = mean)
#' aggr2(x, id, mean)
#'
aggr2 <- function(x, by, fun, ...) {
  if (!is.list(by)) by <- list(by)
  fun(aggregate(x, by, FUN = fun, ...)$x)
}

#' Paste several strings, add 'and' before last
#'
#' @param x vector of strings
#' @param sep separator (is not used for only two groups)
#' @param suffix suffix to append to each value before the separator
#'
#' @return a string iterating the values in x
#' @export
#'
#' @examples
#' data(iris)
#' # get mean petal width and SD by group
#' res <- as.vector(by(iris$Sepal.Width, iris$Species, apa_mean_sd))
#' res
#' paste_and(res)
#' paste_and(res, sep = ";")
#'
#' data(memory_noise)
#' # get bias and SD in the two experiment samples
#' res <- as.vector(by(
#'   memory_noise$bias_percent,
#'   memory_noise$experiment,
#'   apa_mean_sd
#' ))
#' res
#' # no comma with two groups
#' paste_and(res)
#' paste_and(res, suffix = " percentage points")
#'
paste_and <- function(x, sep = ", ", suffix = "") {
  collapse <- paste0(suffix, sep)
  last_sep <- ifelse(length(x) > 2, paste0(collapse, "and "), paste0(suffix, " and "))
  paste0(paste0(x[1:(length(x) - 1)], collapse = collapse), last_sep, x[length(x)], suffix)
}


.get_interval_stat_name <- function(expr, fun) {
  known <- list(
    mean = base::mean,
    median = stats::median,
    var = stats::var,
    sd = stats::sd,
    IQR = stats::IQR
  )
  for (nm in names(known)) {
    if (identical(fun, known[[nm]])) {
      return(nm)
    }
  }

  if (is.character(expr) && length(expr) == 1L) {
    return(expr)
  }
  if (is.symbol(expr)) {
    return(as.character(expr))
  }
  if (is.call(expr) &&
      as.character(expr[[1]]) %in% c("::", ":::") &&
      length(expr) >= 3L) {
    return(as.character(expr[[3]]))
  }
  NULL
}

.get_interval_precision_fun <- function(stat_name, stat_fun, errorbar, caller_env) {
  if (is.function(errorbar)) {
    return(errorbar)
  }

  if (!is.character(errorbar) || length(errorbar) != 1L) {
    stop("errorbar must be 'CI', 'SE', 'none', or a precision function.")
  }

  errorbar_type <- toupper(errorbar)
  if (!(errorbar_type %in% c("CI", "SE", "NONE"))) {
    stop("errorbar must be 'CI', 'SE', 'none', or a precision function.")
  }
  if (errorbar_type == "NONE") {
    return(function(x, gamma = 0.95) 0)
  }

  precision_builtin <- switch(
    stat_name,
    mean = if (errorbar_type == "SE") {
      function(x, gamma = 0.95) stats::sd(x) / sqrt(length(x))
    } else {
      function(x, gamma = 0.95) {
        se <- stats::sd(x) / sqrt(length(x))
        base::mean(x) + se * stats::qt(
          c(0.5 - gamma / 2, 0.5 + gamma / 2),
          df = length(x) - 1
        )
      }
    },
    median = if (errorbar_type == "SE") {
      function(x, gamma = 0.95) {
        sqrt(pi / 2) * stats::sd(x) / sqrt(length(x))
      }
    } else {
      function(x, gamma = 0.95) {
        se <- sqrt(pi / 2) * stats::sd(x) / sqrt(length(x))
        stats::median(x) + se * stats::qt(
          c(0.5 - gamma / 2, 0.5 + gamma / 2),
          df = length(x) - 1
        )
      }
    },
    var = if (errorbar_type == "SE") {
      function(x, gamma = 0.95) {
        stats::var(x) * sqrt(2 / (length(x) - 1))
      }
    } else {
      function(x, gamma = 0.95) {
        vx <- stats::var(x)
        q <- stats::qchisq(
          c(0.5 + gamma / 2, 0.5 - gamma / 2),
          df = length(x) - 1
        )
        vx * (length(x) - 1) / q
      }
    },
    sd = if (errorbar_type == "SE") {
      function(x, gamma = 0.95) {
        stats::sd(x) / sqrt(2 * (length(x) - 1))
      }
    } else {
      function(x, gamma = 0.95) {
        q <- sqrt(stats::qchisq(
          c(0.5 + gamma / 2, 0.5 - gamma / 2),
          df = length(x) - 1
        ))
        stats::sd(x) * sqrt(length(x) - 1) / q
      }
    },
    MAD = if (errorbar_type == "SE") {
      function(x, gamma = 0.95) {
        sqrt(2 / pi) * stats::sd(x) / sqrt(length(x))
      }
    } else {
      function(x, gamma = 0.95) {
        se <- sqrt(2 / pi) * stats::sd(x) / sqrt(length(x))
        stat_fun(x) + se * stats::qt(
          c(0.5 - gamma / 2, 0.5 + gamma / 2),
          df = length(x) - 1
        )
      }
    },
    IQR = if (errorbar_type == "SE") {
      function(x, gamma = 0.95) {
        q <- stats::dnorm(stats::qnorm(0.25))
        stats::sd(x) / (2 * sqrt(length(x)) * q)
      }
    } else {
      function(x, gamma = 0.95) {
        q <- stats::dnorm(stats::qnorm(0.25))
        se <- stats::sd(x) / (2 * sqrt(length(x)) * q)
        stats::IQR(x) + se * stats::qt(
          c(0.5 - gamma / 2, 0.5 + gamma / 2),
          df = length(x) - 1
        )
      }
    },
    hmean = if (errorbar_type == "SE") {
      function(x, gamma = 0.95) {
        stat_fun(x)^2 * stats::sd(1 / x) / sqrt(length(x) - 1)
      }
    } else {
      function(x, gamma = 0.95) {
        se <- stat_fun(x)^2 * stats::sd(1 / x) / sqrt(length(x) - 1)
        stat_fun(x) + se * stats::qt(
          c(0.5 - gamma / 2, 0.5 + gamma / 2),
          df = length(x) - 1
        )
      }
    },
    gmean = if (errorbar_type == "SE") {
      function(x, gamma = 0.95) {
        stat_fun(x) * stats::sd(log(x)) / sqrt(length(x) - 1)
      }
    } else {
      function(x, gamma = 0.95) {
        se <- stat_fun(x) * stats::sd(log(x)) / sqrt(length(x) - 1)
        stat_fun(x) + se * stats::qt(
          c(0.5 - gamma / 2, 0.5 + gamma / 2),
          df = length(x) - 1
        )
      }
    },
    fisherskew = if (errorbar_type == "SE") {
      function(x, gamma = 0.95) {
        n <- length(x)
        sqrt((6 * n * (n - 1)) / ((n - 2) * (n + 1) * (n + 3)))
      }
    } else {
      function(x, gamma = 0.95) {
        n <- length(x)
        se <- sqrt((6 * n * (n - 1)) / ((n - 2) * (n + 1) * (n + 3)))
        stat_fun(x) + se * stats::qnorm(
          c(0.5 - gamma / 2, 0.5 + gamma / 2)
        )
      }
    },
    pearsonskew = if (errorbar_type == "SE") {
      function(x, gamma = 0.95) {
        sqrt((pi / 2 - 1) / length(x))
      }
    } else {
      function(x, gamma = 0.95) {
        se <- sqrt((pi / 2 - 1) / length(x))
        stat_fun(x) + se * stats::qt(
          c(0.5 - gamma / 2, 0.5 + gamma / 2),
          df = length(x) - 1
        )
      }
    },
    fisherkurtosis = if (errorbar_type == "SE") {
      function(x, gamma = 0.95) {
        n <- length(x)
        se_skew <- sqrt((6 * n * (n - 1)) / ((n - 2) * (n + 1) * (n + 3)))
        2 * se_skew * sqrt((n^2 - 1) / ((n - 3) * (n + 5)))
      }
    } else {
      function(x, gamma = 0.95) {
        n <- length(x)
        se_skew <- sqrt((6 * n * (n - 1)) / ((n - 2) * (n + 1) * (n + 3)))
        se <- 2 * se_skew * sqrt((n^2 - 1) / ((n - 3) * (n + 5)))
        minbx <- 2 * (n - 1) / (n - 3)
        lnc <- stats::qlnorm(c(0.5 - gamma / 2, 0.5 + gamma / 2))
        stat_fun(x) + 2 * lnc^(se / 2) - minbx
      }
    },
    NULL
  )

  if (!is.null(precision_builtin)) {
    return(precision_builtin)
  }

  if (is.null(stat_name) || !nzchar(stat_name)) {
    stop(
      "For an anonymous summary function, supply errorbar as a precision ",
      "function returning one width or two interval limits."
    )
  }

  companion_name <- paste0(errorbar_type, ".", stat_name)
  candidate_envs <- list(environment(stat_fun), caller_env, globalenv())
  for (env in candidate_envs) {
    if (!is.null(env) &&
        exists(companion_name, envir = env, mode = "function", inherits = TRUE)) {
      return(get(companion_name, envir = env, mode = "function", inherits = TRUE))
    }
  }

  stop(
    sprintf(
      "No %s precision function is known for summary function '%s'. ",
      errorbar_type, stat_name
    ),
    sprintf(
      "Define %s(x%s) or pass errorbar as a function.",
      companion_name,
      if (errorbar_type == "CI") ", gamma = 0.95" else ""
    )
  )
}

.call_interval_precision <- function(fun, x, gamma = 0.95) {
  fml <- tryCatch(names(formals(fun)), error = function(e) character())
  use_gamma <- "gamma" %in% fml || "..." %in% fml
  out <- if (use_gamma) fun(x, gamma = gamma) else fun(x)

  if (!is.numeric(out) || !(length(out) %in% c(1L, 2L))) {
    stop(
      "The precision function must return one numeric width or two numeric interval limits."
    )
  }
  as.numeric(out)
}

#' Get adjusted confidence intervals or standard errors
#'
#' Computes grouped summary statistics and precision intervals for between-
#' subject, within-subject, and mixed designs. With no within-subject factors,
#' ordinary independent-observation intervals are computed. Supplying a subject
#' identifier in a between-subject design first aggregates repeated rows within
#' subject-by-group cells.
#'
#' The within-subject implementation and statistic-specific precision
#' conventions are adapted from the superb framework and its R implementation
#' (Cousineau, Goulet, & Harding, 2021). In particular, the Cousineau-Morey
#' path follows the two-step centering and bias-correction algorithm used by
#' superb. The code is implemented locally; apastats2 does not depend on or
#' call superb.
#'
#' @param data dataframe to summarize
#' @param value_var dependent variable (string)
#' @param within within-subject variables (vector of strings; default: NULL).
#'   If supplied, wid must also be supplied.
#' @param between between-subject/grouping variables (vector of strings;
#'   default: NULL)
#' @param wid observational-unit/subject variable (string; default: NULL).
#'   Required for within-subject designs. For between-subject designs it is
#'   optional; if supplied, repeated rows are aggregated within subject/group
#'   cells before intervals are computed.
#' @param adjustments adjustment settings. Supports purpose = "single" or
#'   "difference". decorrelation can be "CM" or "none" for within-subject
#'   designs and must be "none" for purely between-subject designs. If omitted,
#'   decorrelation defaults to "CM" when within is supplied and "none"
#'   otherwise.
#' @param errorbar "CI", "SE", "none", or a precision function. A precision
#'   function receives the condition vector and returns either one error-bar
#'   width or two interval limits.
#' @param gamma confidence level for interval functions (default: 0.95)
#' @param drop_NA_subj should subjects with missing within-subject cells be
#'   dropped? (default: FALSE)
#' @param drop_missing_levels should unused factor levels in within/between
#'   variables be dropped? (default: TRUE)
#' @param aggr_fun scalar summary function. For within-subject designs, and for
#'   between-subject designs with wid supplied, it is also used to aggregate
#'   repeated observations within observational-unit cells. Built-in precision
#'   formulas mirror superb for mean, median, var, sd, IQR, and the superb-style
#'   MAD, hmean, gmean, fisherskew, pearsonskew, and fisherkurtosis names.
#'   For other named functions, define a matching SE.<name> or CI.<name>
#'   function, or supply errorbar as a function.
#' @param debug output additional debugging info (default: FALSE)
#' @param ... additional parameters passed to [apa_format_mean_conf]
#'
#' @return dataframe with grouping variables, center, lowerwidth, upperwidth,
#'   lower_ci, upper_ci, and descr
#'
#' @references
#' Cousineau, D., Goulet, M.-A., & Harding, B. (2021). Summary plots with
#' adjusted error bars: The superb framework with an implementation in R.
#' Advances in Methods and Practices in Psychological Science, 4(3).
#' doi:10.1177/25152459211035109
#'
#' Cousineau, D. (2005). Confidence intervals in within-subject designs:
#' A simpler solution to Loftus and Masson's method. The Quantitative Methods
#' for Psychology, 1(1), 42-45. doi:10.20982/tqmp.01.1.p042
#'
#' Morey, R. D. (2008). Confidence intervals from normalized data:
#' A correction to Cousineau (2005). The Quantitative Methods for Psychology,
#' 4(2), 61-64. doi:10.20982/tqmp.04.2.p061
#'
#' @export
#'
#' @examples
#' data(memory_noise)
#'
#' # Between-subject intervals: Exp. 1 vs Exp. 1 HV
#' target_higher <- memory_noise[
#'   memory_noise$relative_noise == "target more noisy",
#' ]
#' get_adjusted_ci(
#'   target_higher,
#'   value_var = "bias_percent",
#'   between = "experiment",
#'   wid = "participant"
#' )
#'
#' # Within-subject Cousineau-Morey intervals for relative noise
#' get_adjusted_ci(
#'   memory_noise[memory_noise$experiment == "Exp. 1", ],
#'   value_var = "bias_percent",
#'   within = "relative_noise",
#'   wid = "participant"
#' )
#'
#' # Mixed design: experiment is between, relative noise is within
#' get_adjusted_ci(
#'   memory_noise,
#'   value_var = "bias_percent",
#'   within = "relative_noise",
#'   between = "experiment",
#'   wid = "participant"
#' )
#'
get_adjusted_ci <- function(data, value_var, within = NULL, between = NULL,
                            wid = NULL,
                            adjustments = list(
                              purpose = "single",
                              decorrelation = NULL
                            ),
                            errorbar = "CI", gamma = 0.95,
                            drop_NA_subj = FALSE,
                            drop_missing_levels = TRUE,
                            aggr_fun = mean, debug = FALSE, ...) {
  within <- unique(as.character(within))
  within <- within[!is.na(within) & nzchar(within)]
  between <- unique(as.character(between))
  between <- between[!is.na(between) & nzchar(between)]
  has_within <- length(within) > 0L

  if (has_within && (is.null(wid) || length(wid) != 1L || !nzchar(wid))) {
    stop("wid must be supplied for within-subject designs.")
  }
  if (!is.null(wid) &&
      (length(wid) != 1L || is.na(wid) || !nzchar(wid))) {
    stop("wid must be NULL or a single variable name.")
  }
  if (!is.numeric(gamma) || length(gamma) != 1L ||
      is.na(gamma) || gamma <= 0 || gamma >= 1) {
    stop("gamma must be a single number between 0 and 1.")
  }

  aggr_expr <- substitute(aggr_fun)
  if (is.character(aggr_fun) && length(aggr_fun) == 1L) {
    aggr_fun <- match.fun(aggr_fun)
  }
  if (!is.function(aggr_fun)) {
    stop("aggr_fun must be a function or the name of a function.")
  }

  stat_name <- .get_interval_stat_name(aggr_expr, aggr_fun)
  caller_env <- parent.frame()
  precision_fun <- .get_interval_precision_fun(
    stat_name, aggr_fun, errorbar, caller_env
  )

  scalar_summary <- function(x) {
    out <- aggr_fun(x)
    if (!is.numeric(out) || length(out) != 1L) {
      stop("aggr_fun must return one numeric value.")
    }
    as.numeric(out)
  }

  all_vars <- unique(c(
    within,
    between,
    if (!is.null(wid)) wid else character(),
    value_var
  ))
  for (x in all_vars) {
    if (!(x %in% names(data))) {
      stop(sprintf(
        'Variable "%s" is not in the data. Check if it is present and its name is spelled correctly.',
        x
      ))
    }
  }

  data <- as.data.frame(data)

  # superb required factors for repeated-measures designs. Keep that
  # conversion only when it matters; between-only summaries preserve original
  # group types, while still dropping unused levels from existing factors.
  factor_vars <- unique(c(within, between))
  for (x in factor_vars) {
    if (!is.factor(data[[x]]) && has_within) {
      warning(paste0("Converting \"", x, "\" to a factor."))
      data[[x]] <- factor(data[[x]])
    } else if (is.factor(data[[x]]) && drop_missing_levels) {
      old_levels <- levels(data[[x]])
      data[[x]] <- droplevels(data[[x]])
      if (!identical(old_levels, levels(data[[x]]))) {
        message(paste0(
          "Some levels of \"", x, "\" seem to be absent in the data, excluding them."
        ))
        message(paste0(
          "New levels of \"", x, "\": ",
          paste(levels(data[[x]]), collapse = ", ")
        ))
      }
    }
  }

  na_check_vars <- unique(c(
    within,
    between,
    if (!is.null(wid)) wid else character(),
    value_var
  ))
  for (x in na_check_vars) {
    if (anyNA(data[[x]])) {
      stop(sprintf(
        'NAs present in "%s", they should be removed from all variables beforehand.',
        x
      ))
    }
  }

  default_decorrelation <- if (has_within) "CM" else "none"
  adjustments <- utils::modifyList(
    list(purpose = "single", decorrelation = NULL),
    adjustments
  )
  if (is.null(adjustments$decorrelation)) {
    adjustments$decorrelation <- default_decorrelation
  }

  purpose <- tolower(adjustments$purpose)
  decorrelation <- toupper(adjustments$decorrelation)

  if (!(purpose %in% c("single", "difference"))) {
    stop('Only adjustments$purpose = "single" or "difference" is supported.')
  }
  if (!(decorrelation %in% c("CM", "NONE"))) {
    stop('Only adjustments$decorrelation = "CM" or "none" is supported.')
  }
  if (!has_within && decorrelation != "NONE") {
    stop("Within-subject decorrelation cannot be used when within is NULL.")
  }
  if (!is.null(adjustments$popSize) && any(adjustments$popSize != Inf)) {
    stop("Finite-population adjustments are not supported by get_adjusted_ci.")
  }
  if (!is.null(adjustments$samplingDesign) &&
      !identical(toupper(adjustments$samplingDesign), "SRS")) {
    stop("Cluster-randomized sampling adjustments are not supported by get_adjusted_ci.")
  }

  row_key <- function(df, cols) {
    if (length(cols) == 0L) {
      return(rep(".all", nrow(df)))
    }
    do.call(
      paste,
      c(lapply(df[cols], as.character), list(sep = "\034"))
    )
  }

  if (has_within) {
    cell_vars <- c(wid, between, within)
    aggregated <- stats::aggregate(
      data[[value_var]],
      by = data[cell_vars],
      FUN = scalar_summary
    )
    names(aggregated)[ncol(aggregated)] <- ".value"

    units <- unique(aggregated[c(wid, between)])
    within_grid <- unique(data[within])
    expected <- merge(units, within_grid, by = NULL)
    joined <- merge(
      expected,
      aggregated,
      by = cell_vars,
      all.x = TRUE,
      sort = FALSE
    )

    missing <- joined[
      is.na(joined$.value),
      c(wid, between, within),
      drop = FALSE
    ]
    if (nrow(missing) > 0L) {
      missing_display <- missing[c(wid, between)]
      missing_display$missing_condition <- apply(
        missing[within, drop = FALSE],
        1,
        function(z) paste(paste0(within, "=", z), collapse = ", ")
      )
      print(unique(missing_display))

      unit_vars <- c(wid, between)
      bad_units <- unique(row_key(missing, unit_vars))
      if (drop_NA_subj) {
        keep <- !(row_key(joined, unit_vars) %in% bad_units)
        joined <- joined[keep, , drop = FALSE]
        warning(sprintf(
          "NAs present after aggregation, dropping %i rows",
          length(bad_units)
        ))
      } else {
        stop("NAs present after aggregation")
      }
    }

    if (nrow(joined) == 0L) {
      stop("No complete subject-condition data remain after aggregation.")
    }

    for (x in c(within, between)) {
      if (is.factor(data[[x]])) {
        joined[[x]] <- factor(
          as.character(joined[[x]]),
          levels = levels(data[[x]])
        )
      }
    }

    unit_vars <- c(wid, between)
    joined$.unit_key <- row_key(joined, unit_vars)
    joined$.between_key <- row_key(joined, between)
    joined$.condition_key <- row_key(joined, within)

    analysis_dt <- data.table::as.data.table(joined)

    if (decorrelation == "CM") {
      # Adapted from superb::twoStepTransform(): subject centering followed
      # by the Morey bias correction, separately within between-subject groups.
      analysis_dt[
        ,
        c(".n_conditions") := data.table::uniqueN(get(".condition_key")),
        by = ".between_key"
      ]
      if (any(analysis_dt$.n_conditions < 2)) {
        stop(
          "Cousineau-Morey adjustment requires at least two within-subject conditions."
        )
      }

      analysis_dt[
        ,
        c(".subject_mean") := mean(get(".value")),
        by = unit_vars
      ]
      analysis_dt[
        ,
        c(".grand_mean") := mean(get(".subject_mean")),
        by = ".between_key"
      ]
      analysis_dt[
        ,
        c(".cm_centered") :=
          get(".value") - get(".subject_mean") + get(".grand_mean")
      ]
      analysis_dt[
        ,
        c(".condition_mean") := mean(get(".cm_centered")),
        by = c(".between_key", ".condition_key")
      ]
      analysis_dt[
        ,
        c(".analysis_value") :=
          sqrt(get(".n_conditions") / (get(".n_conditions") - 1)) *
          (get(".cm_centered") - get(".condition_mean")) +
          get(".condition_mean")
      ]
    } else {
      analysis_dt[, c(".analysis_value") := get(".value")]
    }

    summary_vars <- c(within, between)
  } else {
    if (is.null(wid)) {
      analysis_dt <- data.table::as.data.table(
        data[c(between, value_var)]
      )
      data.table::setnames(analysis_dt, value_var, ".analysis_value")
      aggregated <- NULL
    } else {
      cell_vars <- c(wid, between)
      aggregated <- stats::aggregate(
        data[[value_var]],
        by = data[cell_vars],
        FUN = scalar_summary
      )
      names(aggregated)[ncol(aggregated)] <- ".analysis_value"
      analysis_dt <- data.table::as.data.table(aggregated)
    }
    summary_vars <- between
  }

  purpose_multiplier <- if (purpose == "difference") sqrt(2) else 1

  summarize_one <- function(analysis_value) {
    center <- scalar_summary(analysis_value)
    precision <- .call_interval_precision(
      precision_fun,
      analysis_value,
      gamma = gamma
    )

    if (length(precision) == 2L) {
      lowerwidth <- min(precision) - center
      upperwidth <- max(precision) - center
    } else {
      lowerwidth <- -precision
      upperwidth <- precision
    }

    list(
      center = center,
      lowerwidth = purpose_multiplier * lowerwidth,
      upperwidth = purpose_multiplier * upperwidth
    )
  }

  if (length(summary_vars) > 0L) {
    result <- analysis_dt[
      ,
      summarize_one(get(".analysis_value")),
      by = summary_vars
    ]
  } else {
    result <- analysis_dt[
      ,
      summarize_one(get(".analysis_value"))
    ]
  }
  result <- as.data.frame(result)

  for (x in summary_vars) {
    if (is.factor(data[[x]])) {
      result[[x]] <- factor(
        as.character(result[[x]]),
        levels = levels(data[[x]])
      )
    }
  }

  if (length(summary_vars) > 0L) {
    ord <- do.call(
      order,
      lapply(summary_vars, function(x) {
        if (is.factor(result[[x]])) {
          as.integer(result[[x]])
        } else {
          result[[x]]
        }
      })
    )
    result <- result[ord, , drop = FALSE]
  }
  rownames(result) <- NULL

  result$lower_ci <- result$center + result$lowerwidth
  result$upper_ci <- result$center + result$upperwidth
  result$descr <- apa_format_mean_conf(
    result$center,
    result$lower_ci,
    result$upper_ci,
    ...
  )

  if (debug) {
    if (!is.null(aggregated)) {
      message("Aggregated observational-unit data:")
      print(aggregated)
    }
    message("Data used for interval computation:")
    print(
      analysis_dt[
        ,
        c(summary_vars, ".analysis_value"),
        with = FALSE
      ]
    )
  }

  result
}

#' Deprecated name for adjusted confidence intervals
#'
#' get_superb_ci() is retained for backward compatibility. New code should use
#' [get_adjusted_ci()]. Its within-subject results are intended to remain
#' numerically compatible with the former superb-backed implementation.
#'
#' @rdname get_adjusted_ci
#' @export
get_superb_ci <- function(data, wid, within, value_var, between = NULL,
                          adjustments = list(
                            purpose = "single",
                            decorrelation = "CM"
                          ),
                          errorbar = "CI", drop_NA_subj = FALSE,
                          drop_missing_levels = TRUE, aggr_fun = mean,
                          debug = FALSE, ...) {
  .Deprecated("get_adjusted_ci", package = "apastats2")
  call <- match.call()
  call[[1L]] <- quote(get_adjusted_ci)
  eval(call, envir = parent.frame())
}
