test_that("get_adjusted_ci prints informative labels for incomplete cells", {
  data <- data.frame(
    id = c(1, 1, 2),
    group = factor(rep("control group", 3)),
    condition = factor(
      c("left response", "right response", "left response"),
      levels = c("left response", "right response")
    ),
    rt = c(1, 2, 3)
  )

  printed <- capture.output(
    expect_error(
      get_adjusted_ci(
        data,
        value_var = "rt",
        within = "condition",
        between = "group",
        wid = "id"
      ),
      "NAs present after aggregation"
    )
  )

  expect_true(any(grepl("control group", printed, fixed = TRUE)))
  expect_true(any(grepl("condition=right response", printed, fixed = TRUE)))
  expect_false(any(grepl("\\b[A-Z]{5}\\b", printed)))
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
    res <- get_adjusted_ci(
      data,
      value_var = "y",
      within = "condition",
      wid = "id",
      drop_NA_subj = TRUE
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
      adjustments = list(purpose = "single", decorrelation = "CA")
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
