test_that("incomplete-cell diagnostics are informative and suppressible", {
  data <- data.frame(
    id = c(1, 1, 2), group = factor(rep("control group", 3)),
    condition = factor(c("left response", "right response", "left response")),
    rt = c(1, 2, 3)
  )
  printed <- capture.output(error <- tryCatch(
    get_adjusted_ci(data, value_var = "rt", within = "condition",
                    between = "group", wid = "id"),
    error = identity
  ))
  expect_length(printed, 0)
  expect_s3_class(error, "error")
  diagnostic <- conditionMessage(error)
  expect_match(diagnostic, "NAs present after aggregation", fixed = TRUE)
  expect_match(diagnostic, "control group", fixed = TRUE)
  expect_match(diagnostic, "condition=right response", fixed = TRUE)
  expect_false(grepl("\\b[A-Z]{5}\\b", diagnostic))

  printed <- capture.output(result <- suppressWarnings(get_adjusted_ci(
    data, value_var = "rt", within = "condition", between = "group",
    wid = "id", drop_NA_subj = TRUE
  )))
  expect_length(printed, 0)
  expect_equal(nrow(result), 2)
})

test_that("get_adjusted_ci matches the Cousineau-Morey calculation", {
  data <- data.frame(
    id = factor(rep(1:3, each = 2)),
    condition = factor(
      rep(c("left response", "right/response"), 3),
      levels = c("left response", "right/response")
    ),
    rt = c(1, 3, 2, 5, 4, 6)
  )

  res <- get_adjusted_ci(
    data,
    value_var = "rt",
    within = "condition",
    wid = "id"
  )

  wide <- matrix(c(1, 3, 2, 5, 4, 6), nrow = 3, byrow = TRUE)
  c_count <- ncol(wide)
  centered <- wide - rowMeans(wide) + mean(rowMeans(wide))
  corrected <- sweep(centered, 2, colMeans(centered), "-")
  corrected <- corrected * sqrt(c_count / (c_count - 1))
  corrected <- sweep(corrected, 2, colMeans(centered), "+")
  expected_center <- colMeans(corrected)
  expected_se <- apply(corrected, 2, stats::sd) / sqrt(nrow(corrected))
  expected_width <- stats::qt(.975, df = nrow(corrected) - 1) * expected_se

  expect_equal(res$center, expected_center)
  expect_equal(res$lowerwidth, -expected_width)
  expect_equal(res$upperwidth, expected_width)
  expect_identical(
    levels(res$condition),
    c("left response", "right/response")
  )
})

test_that("get_adjusted_ci supports SE and difference-purpose intervals", {
  data <- data.frame(
    id = factor(rep(1:4, each = 2)),
    condition = factor(rep(c("A", "B"), 4)),
    y = c(1, 2, 2, 5, 3, 5, 5, 7)
  )

  single <- get_adjusted_ci(
    data,
    value_var = "y",
    within = "condition",
    wid = "id",
    errorbar = "SE"
  )
  difference <- get_adjusted_ci(
    data,
    value_var = "y",
    within = "condition",
    wid = "id",
    errorbar = "SE",
    adjustments = list(purpose = "difference", decorrelation = "CM")
  )

  expect_equal(difference$upperwidth, single$upperwidth * sqrt(2))
  expect_equal(difference$lowerwidth, single$lowerwidth * sqrt(2))
})

test_that("get_adjusted_ci can drop incomplete subject-condition units", {
  data <- data.frame(
    id = c(1, 1, 2),
    condition = factor(
      c("A", "B", "A"),
      levels = c("A", "B")
    ),
    y = c(1, 2, 3)
  )

  expect_warning(
    res <- withCallingHandlers(
      get_adjusted_ci(
        data,
        value_var = "y",
        within = "condition",
        wid = "id",
        drop_NA_subj = TRUE
      ),
      warning = function(w) {
        if (grepl("NaN", conditionMessage(w))) invokeRestart("muffleWarning")
      }
    ),
    "dropping 1 rows"
  )
  expect_equal(nrow(res), 2)
})

test_that("get_adjusted_ci rejects unsupported decorrelation options", {
  data <- data.frame(
    id = factor(rep(1:3, each = 2)),
    condition = factor(rep(c("A", "B"), 3)),
    y = 1:6
  )

  expect_error(
    get_adjusted_ci(
      data,
      value_var = "y",
      within = "condition",
      wid = "id",
      adjustments = list(purpose = "single", decorrelation = "unsupported")
    ),
    "decorrelation"
  )
  expect_error(
    get_adjusted_ci(
      data,
      value_var = "y",
      between = "condition",
      adjustments = list(purpose = "single", decorrelation = "CM")
    ),
    "cannot be used"
  )
})

test_that("get_adjusted_ci supports superb-compatible non-mean summaries", {
  data <- expand.grid(
    id = factor(1:8),
    condition = factor(c("A", "B")),
    trial = 1:3
  )
  data$y <- as.numeric(data$id) * 0.7 +
    as.numeric(data$condition) * 1.3 +
    data$trial^2 * 0.11 +
    as.numeric(data$id) * as.numeric(data$condition) * 0.03

  median_ci <- get_adjusted_ci(
    data,
    value_var = "y",
    within = "condition",
    wid = "id",
    aggr_fun = median,
    errorbar = "CI"
  )
  sd_se <- get_adjusted_ci(
    data,
    value_var = "y",
    within = "condition",
    wid = "id",
    aggr_fun = stats::sd,
    errorbar = "SE"
  )
  var_ci <- get_adjusted_ci(
    data,
    value_var = "y",
    within = "condition",
    wid = "id",
    aggr_fun = stats::var,
    errorbar = "CI"
  )

  expect_true(all(is.finite(median_ci$center)))
  expect_true(all(is.finite(median_ci$lower_ci)))
  expect_true(all(is.finite(sd_se$center)))
  expect_true(all(is.finite(sd_se$upperwidth)))
  expect_true(all(is.finite(var_ci$center)))
  expect_true(all(var_ci$lower_ci <= var_ci$center))
  expect_true(all(var_ci$upper_ci >= var_ci$center))
})

test_that("get_adjusted_ci supports custom summary and precision functions", {
  data <- expand.grid(
    id = factor(1:10),
    condition = factor(c("A", "B")),
    trial = 1:4
  )
  data$y <- as.numeric(data$id) +
    2 * as.numeric(data$condition) +
    c(-1, 0, 0.5, 2)[data$trial]

  trimmed_mean <- function(x) mean(x, trim = 0.2)
  SE.trimmed_mean <- function(x) stats::sd(x) / sqrt(length(x))

  named <- get_adjusted_ci(
    data,
    value_var = "y",
    within = "condition",
    wid = "id",
    aggr_fun = trimmed_mean,
    errorbar = "SE"
  )
  direct <- get_adjusted_ci(
    data,
    value_var = "y",
    within = "condition",
    wid = "id",
    aggr_fun = function(x) mean(x, trim = 0.2),
    errorbar = function(x) stats::sd(x) / sqrt(length(x))
  )

  expect_equal(named$center, direct$center)
  expect_equal(named$lowerwidth, direct$lowerwidth)
  expect_equal(named$upperwidth, direct$upperwidth)

  no_precision <- function(x) mean(x, trim = 0.1)
  expect_error(
    get_adjusted_ci(
      data,
      value_var = "y",
      within = "condition",
      wid = "id",
      aggr_fun = no_precision,
      errorbar = "CI"
    ),
    "No CI precision function"
  )
})

test_that("get_adjusted_ci supports no error bars", {
  data <- data.frame(
    id = factor(rep(1:4, each = 2)),
    condition = factor(rep(c("A", "B"), 4)),
    y = c(1, 2, 2, 5, 3, 5, 5, 7)
  )

  res <- get_adjusted_ci(
    data,
    value_var = "y",
    within = "condition",
    wid = "id",
    errorbar = "none"
  )

  expect_equal(res$lowerwidth, rep(0, nrow(res)))
  expect_equal(res$upperwidth, rep(0, nrow(res)))
  expect_equal(res$lower_ci, res$center)
  expect_equal(res$upper_ci, res$center)
})

test_that("get_adjusted_ci computes ordinary between-subject intervals", {
  data <- data.frame(
    group = factor(rep(c("A", "B"), each = 5)),
    y = c(1, 2, 4, 5, 8, 2, 3, 3, 7, 10)
  )

  res <- get_adjusted_ci(
    data,
    value_var = "y",
    between = "group"
  )

  split_y <- split(data$y, data$group)
  expected_center <- vapply(split_y, mean, numeric(1))
  expected_se <- vapply(split_y, stats::sd, numeric(1)) /
    sqrt(vapply(split_y, length, integer(1)))
  expected_width <- vapply(
    seq_along(split_y),
    function(i) {
      stats::qt(.975, df = length(split_y[[i]]) - 1) * expected_se[[i]]
    },
    numeric(1)
  )

  expect_equal(res$center, unname(expected_center))
  expect_equal(res$lowerwidth, -unname(expected_width))
  expect_equal(res$upperwidth, unname(expected_width))
})

test_that("get_adjusted_ci can aggregate repeated rows by subject for between designs", {
  data <- expand.grid(
    id = factor(1:6),
    trial = 1:3,
    KEEP.OUT.ATTRS = FALSE
  )
  data$group <- factor(ifelse(as.numeric(data$id) <= 3, "A", "B"))
  data$y <- as.numeric(data$id) + data$trial * c(0.1, 0.3, 0.7)[data$trial]

  res <- get_adjusted_ci(
    data,
    value_var = "y",
    between = "group",
    wid = "id"
  )

  subj <- stats::aggregate(y ~ id + group, data, mean)
  split_y <- split(subj$y, subj$group)
  expected_center <- vapply(split_y, mean, numeric(1))
  expected_se <- vapply(split_y, stats::sd, numeric(1)) /
    sqrt(vapply(split_y, length, integer(1)))
  expected_width <- vapply(
    seq_along(split_y),
    function(i) {
      stats::qt(.975, df = length(split_y[[i]]) - 1) * expected_se[[i]]
    },
    numeric(1)
  )

  expect_equal(res$center, unname(expected_center))
  expect_equal(res$lowerwidth, -unname(expected_width))
  expect_equal(res$upperwidth, unname(expected_width))
})

test_that("get_superb_ci remains a deprecated compatibility wrapper", {
  data <- data.frame(
    id = factor(rep(1:4, each = 2)),
    condition = factor(rep(c("A", "B"), 4)),
    y = c(1, 2, 2, 5, 3, 5, 5, 7)
  )

  current <- get_adjusted_ci(
    data,
    value_var = "y",
    within = "condition",
    wid = "id"
  )

  expect_warning(
    legacy <- get_superb_ci(data, "id", "condition", "y"),
    "deprecated"
  )

  expect_equal(legacy$center, current$center)
  expect_equal(legacy$lowerwidth, current$lowerwidth)
  expect_equal(legacy$upperwidth, current$upperwidth)
})
test_that("CA uses paired correlations within each mixed-design group", {
  first <- cbind(c(1, 4, 2, 8, 5, 3), c(3, 1, 6, 4, 8, 2),
                 c(2, 6, 5, 1, 4, 7), c(9, 3, 4, 8, 2, 6))
  second <- cbind(first[, 1] * 2, rev(first[, 2]) + 1,
                  first[, 3] / 2, rev(first[, 4]) * 3)
  matrices <- list(control = first, treatment = second)
  data <- do.call(rbind, lapply(names(matrices), function(group) {
    grid <- expand.grid(id = 1:6, a = c("left", "right"), b = c("low", "high"))
    grid$group <- group
    grid$y <- as.vector(matrices[[group]])
    grid
  }))
  # Repeated rows must be aggregated before computing paired correlations.
  data <- rbind(transform(data, y = y - .1), transform(data, y = y + .1))
  data <- data[nrow(data):1, ]
  data[c("a", "b", "group")] <- lapply(data[c("a", "b", "group")], factor)
  for (errorbar in c("CI", "SE")) {
    for (purpose in c("single", "difference")) {
      result <- get_adjusted_ci(
        data, "y", within = c("a", "b"), between = "group", wid = "id",
        errorbar = errorbar, gamma = .9,
        adjustments = list(decorrelation = "ca", purpose = purpose)
      )
      for (group in names(matrices)) {
        values <- matrices[[group]]
        correlations <- cor(values)
        width <- apply(values, 2, sd) / sqrt(nrow(values)) *
          sqrt(1 - mean(correlations[upper.tri(correlations)]))
        if (errorbar == "CI") width <- width * qt(.95, nrow(values) - 1)
        if (purpose == "difference") width <- width * sqrt(2)
        rows <- result[result$group == group, ]
        # Returned order is a, b; matrix columns follow expand.grid order.
        order <- c(1, 3, 2, 4)
        expect_equal(rows$center, colMeans(values)[order])
        expect_equal(rows$lowerwidth, -width[order])
        expect_equal(rows$upperwidth, width[order])
      }
    }
  }
})

test_that("CA preserves non-mean centers and asymmetric custom intervals", {
  data <- expand.grid(id = 1:5, condition = factor(c("A", "B", "C")))
  values <- cbind(c(1, 2, 8, 3, 5), c(3, 5, 1, 7, 2), c(5, 4, 2, 8, 3))
  data$y <- as.vector(values)
  asymmetric <- function(x) c(median(x) - 1, median(x) + 3)
  ca <- get_adjusted_ci(
    data, "y", within = "condition", wid = "id", aggr_fun = median,
    errorbar = asymmetric, adjustments = list(decorrelation = "CA")
  )
  correlations <- cor(values)
  multiplier <- sqrt(1 - mean(correlations[upper.tri(correlations)]))
  expect_equal(ca$center, apply(values, 2, median))
  expect_equal(ca$lowerwidth, rep(-multiplier, 3))
  expect_equal(ca$upperwidth, rep(3 * multiplier, 3))

  none <- get_adjusted_ci(
    data, "y", within = "condition", wid = "id", errorbar = "none",
    adjustments = list(decorrelation = "CA")
  )
  expect_equal(none$lower_ci, none$center)
  expect_equal(none$upper_ci, none$center)

  # An incomplete subject contributes neither to correlations nor summaries.
  incomplete <- rbind(data, data.frame(id = 6, condition = factor("A"), y = 100))
  expect_warning(dropped <- get_adjusted_ci(
    incomplete, "y", within = "condition", wid = "id", aggr_fun = median,
    errorbar = asymmetric, drop_NA_subj = TRUE,
    adjustments = list(decorrelation = "CA")
  ), "dropping")
  expect_equal(dropped, ca)
})

test_that("CA handles negative and perfect correlations and rejects undefined ones", {
  data <- expand.grid(id = 1:4, condition = factor(c("A", "B")))
  data$y <- c(1:4, 8:5)
  ca <- get_adjusted_ci(
    data, "y", within = "condition", wid = "id", errorbar = "SE",
    adjustments = list(decorrelation = "CA")
  )
  expect_equal(ca$upperwidth, rep(sd(1:4) / 2 * sqrt(2), 2))
  data$y <- c(1:4, 5:8)
  perfect <- get_adjusted_ci(
    data, "y", within = "condition", wid = "id",
    adjustments = list(decorrelation = "CA")
  )
  expect_equal(perfect$lower_ci, perfect$center)
  expect_equal(perfect$upper_ci, perfect$center)

  data$y[5:8] <- 1
  expect_error(get_adjusted_ci(
    data, "y", within = "condition", wid = "id",
    adjustments = list(decorrelation = "CA")
  ), "nonzero variance")
  for (rows in list(1:4, c(1, 5))) {
    expect_error(get_adjusted_ci(
      droplevels(data[rows, ]), "y", within = "condition", wid = "id",
      adjustments = list(decorrelation = "CA")
    ), "two complete subjects and two conditions")
  }
  expect_error(get_adjusted_ci(
    data, "y", between = "condition", adjustments = list(decorrelation = "CA")
  ), "cannot be used")
})
