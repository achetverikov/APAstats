#' Mean with na.rm=T
#'
#' @param x a vector of numbers
#' @param ... other arguments passed to mean
#'
#' @return mean of x with NA removed
#' @export mean.nn
#'
#' @examples
#' x <- c(NA, 10, 90)
#' mean(x)
#' mean.nn(x)
#'

mean.nn <- function(x, ...) {
  mean(x, na.rm = TRUE, ...)
}

#' SD with na.rm=T
#'
#' @param x a vector of numbers
#' @param ... other arguments passed to sd
#'
#' @return sd of x with NA removed
#' @export sd.nn
#'
#' @examples
#' x <- c(NA, 10, 90)
#' sd(x)
#' sd.nn(x)
sd.nn <- function(x, ...) {
  sd(x, na.rm = TRUE, ...)
}

#' Sum with na.rm=T
#'
#' @param x a vector of numbers
#' @param ... other arguments passed to sum
#'
#' @return sum of x with NA removed
#' @export sum.nn
#'
#' @examples
#' x <- c(NA, 10, 90)
#' sum(x)
#' sum.nn(x)
sum.nn <- function(x, ...) {
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
#' Drop empty (consisting of NA only) columns from a data.frame. Based on http://stackoverflow.com/a/2644009/1344028
#'
#' @param df a data.frame
#'
#' @return data.frame without empty columns
#' @export drop.empty.cols
#'
#' @examples
#' df <- data.frame(x = rnorm(20), y = rep("A", 20), z = rep(NA, 20))
#' str(df)
#' df <- drop.empty.cols(df)
#' str(df)
#'
drop.empty.cols <- function(df) {
  Filter(function(x) !all(is.na(x)), df)
}

#' Binomial confidence interval as a vector
#'
#' @param x a vector of 0 and 1
#'
#' @return a vector of mean, lower CI, upper CI, and length of x
#' @export binom.ci
#'
#' @examples
#' binom.ci(rbinom(500, 1, prob = 0.7))
binom.ci <- function(x) {
  ci <- Hmisc::binconf(sum(x), length(x))
  c(y = ci[1], ymin = ci[2], ymax = ci[3], len = length(x))
}

#' Rounded mean
#'
#' Mean rounded to the specified number of digits
#'
#' @param x a number
#' @param ... other arguments passed to [apastats2::f.round]
#' 
#' @return Mean rounded to the specified number of digits (string)
#' @export mean.round
#'
#' @examples
#' mean.round(c(10, 99))
#' mean.round(c(10, 99, NA))
#' mean.round(c(10, 99), 2)
mean.round <- function(x, ...) {
  f.round(mean.nn(x), ...)
}

#' Rounded SD
#'
#' SD rounded to the specified number of digits
#'
#' @param x a number
#' @param ... other arguments passed to [apastats2::f.round]
#'
#' @return Mean rounded to the specified number of digits (string)
#' @export
#'
#' @examples
#' sd.round(c(10, 99))
#' sd.round(c(10, 99, NA))
#' sd.round(c(10, 99), 2)
sd.round <- function(x, ...) {
  f.round(sd.nn(x), ...)
}

#' Quietly load libraries
#'
#' @param libs a vector of libraries names
#'
#' @return None
#' @export
#'
#' @examples
#' load.libs(c("ggplot2", "apastats2", "Hmisc"))
load.libs <- function(libs) {
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

lmer.fixef <- function(fit.lmer) {
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
#' omit.zeroes(0.2312)
#' omit.zeroes(0.2312, digits = 3)
#' omit.zeroes("000.2312", digits = 1)
omit.zeroes <- function(x, digits = 2) {
  sub("^.", "", f.round(x, digits))
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
#' f.round(5.8242)
#' f.round(5.8251)
#' f.round(5.82999, digits = 3)
#' f.round(5.82999, digits = 4)
f.round <- function(x, digits = 2, strip.lead.zeros = FALSE) {
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
#' @export round.p
#'
#' @examples
#' p_values <- c(0.025, 0.0001, 0.001, 0.568)
#' round.p(p_values)
#' round.p(p_values, digits = 2)
#' round.p(p_values, include.rel = FALSE)
#' round.p(p_values, include.rel = FALSE, strip.lead.zeros = FALSE)
#' round.p(p_values, include.rel = FALSE, strip.lead.zeros = FALSE, replace.very.small = 0.01)
#'
round.p <- function(values, include.rel = 1, digits = 3, strip.lead.zeros = TRUE, replace.very.small = 0.001) {
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
#' @export format.results

format.results <- function(res_str, type = "pandoc") {
  if (type == "latex") {
    res_str
  } else if (type == "pandoc") {
    stringr::str_replace_all(res_str, "\\\\emph\\{(.*?)\\}", "_\\1_")
  } else if (type == "plotmath") {
    res_str <- stringi::stri_replace_all(res_str,
      regex = c("\\\\emph\\{(.*?)\\}", "=", "_([^_=^ ]*)"),
      replacement = c("italic($1)", "==", "[$1]"), vectorize_all = FALSE
    )
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
#' res <- as.vector(by(iris$Sepal.Width, iris$Species, describe.mean.sd))
#' res
#' paste_and(res)
#' paste_and(res, sep = ";")
#'
#' data(faces)
#' # get mean response times (in ms) by response accuracy
#' res <- as.vector(by(faces$answerTime * 1000, faces$correct, describe.mean.sd))
#' res
#' # no comma with two groups
#' paste_and(res)
#' paste_and(res, suffix = " ms")
#'
paste_and <- function(x, sep = ", ", suffix = "") {
  collapse <- paste0(suffix, sep)
  last_sep <- ifelse(length(x) > 2, paste0(collapse, "and "), paste0(suffix, " and "))
  paste0(paste0(x[1:(length(x) - 1)], collapse = collapse), last_sep, x[length(x)], suffix)
}


#' Get within-subject CI using the superb package
#'
#' @param data dataframe to use
#' @param wid subject variable or another clustering variable (string)
#' @param within within-subject variables (vector of strings)
#' @param value_var dependent variable (string)
#' @param between between-subject variables (vector of strings; default: NULL)
#' @param adjustments adjustment settings as used for [superb::superbData] (default: single CI estimates, Cousineau-Morey adjustment)
#' @param errorbar type of error bars to use (CI/SE)
#' @param drop_NA_subj should subjects with NA values be dropped? (default: FALSE)
#' @param drop_missing_levels should the missing levels of the variables in within/between be dropped? (default: TRUE)
#' @param debug output additional debugging info (default: FALSE)
#' 
#' @return dataframe with computed CIs
#' @export
#'
#' @examples
#' data(faces)
#' get_superb_ci(faces, "uid", "stim_gender", "answerTime")
#'
get_superb_ci <- function(data, wid, within, value_var, between = NULL, adjustments = list(purpose = "single", decorrelation = "CM"), errorbar = "CI", drop_NA_subj = FALSE, drop_missing_levels = TRUE, debug = FALSE) {
  requireNamespace('superb')
  errorbar <- toupper(errorbar)
  for (x in c(within, between)) {
    if (!is.factor(data[[x]])) {
      warning(paste0("Converting \"", x, "\" to a factor."))
      data[[x]] <- factor(data[[x]])
    } else if (drop_missing_levels & length(levels(data[[x]])) != length(unique(data[[x]]))) {
      message(paste0("Some levels of \"", x, "\" seem to be absent in the data, excluding them."))
      data[[x]] <- factor(data[[x]])
      message(paste0("New levels of \"", x, "\": ", paste(levels(data[[x]]), collapse = ", ")))
    }
    if (any(is.na(data[[x]]))) {
      stop(sprintf("NAs present in \"%s\", they should be removed from all variables beforehand.", x))
    }
  }
  for (x in c(wid, value_var)) {
    if (any(is.na(data[[x]]))) {
      stop(sprintf("NAs present in \"%s\", they should be removed from all variables beforehand.", x))
    }
  }

  dcast_form <- paste0(paste0(c(wid, between), collapse = "+"), "~", paste0(within, collapse = "+"))
  wide_data <- reshape2::dcast(data, dcast_form, value.var = value_var, fun.aggregate = mean)
  if (anyNA(wide_data)) {
    print(wide_data[!complete.cases(wide_data), ])
    if (drop_NA_subj) {
      n_dropped <- sum(!complete.cases(wide_data))
      wide_data <- wide_data[complete.cases(wide_data), ]
      warning(sprintf("NAs present after aggregation, dropping %i rows", n_dropped))
    } else {
      stop("NAs present after aggregation")
    }
  }
  WSFactors <- sapply(within, \(x) paste0(x, "(", length(levels(data[[x]])), ")"))
  variables <- colnames(wide_data)[(2 + length(between)):length(colnames(wide_data))]

  WSDesign <- do.call(expand.grid, lapply(within, \(x) c(1:length(levels(data[[x]])))))
  if (length(within) > 1) {
    WSDesign <- WSDesign[do.call(order, WSDesign), ]
  }

  WSDesign <- apply(WSDesign, 1, as.vector, simplify = FALSE)

  if (debug == TRUE) {
    message("WSDesign: ")
    cat(WSDesign)
    message("wide_data: ")

    cat(wide_data)
  }
  colnames_wsd <- colnames(wide_data)


  if (!all(grepl("^[\\w. ]+$", colnames_wsd, perl = TRUE))) warning("Within- and between-subject factors levels should only contain letters, digits, underscores, dots, or spaces. If you experience errors, try removing special characters from factor levels.")

  cur_superb.feedback <- options('superb.feedback')
  options(superb.feedback = 'none')
  
  # suppressMessages({
    spp_data <- superb::superbData(wide_data,
      WSFactors = WSFactors,
      factorOrder = c(within, between),
      adjustments = adjustments,
      variables = variables,
      WSDesign = WSDesign,
      BSFactors = between,
      errorbar = errorbar
    )
  # })
  
  options(superb.feedback = cur_superb.feedback)
  spp_data <- spp_data$summaryStatistics
  for (x in within) {
    spp_data[[x]] <- factor(spp_data[[x]],
      levels = c(1:length(levels(data[[x]]))),
      labels = levels(data[[x]])
    )
  }
  spp_data
}
