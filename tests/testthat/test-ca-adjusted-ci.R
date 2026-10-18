ca_fixture <- function() {
  set.seed(713)
  a <- rnorm(18)
  b <- rnorm(18)
  c <- rnorm(18)
  wide <- data.frame(A = a, B = .6 * a + b, C = -.4 * a + .3 * b + c)
  list(wide = wide, long = data.frame(
    id = rep(1:18, 3), condition = factor(rep(names(wide), each = 18)),
    y = unlist(wide, use.names = FALSE)))
}

test_that("CA scales original intervals using all pairwise correlations", {
  x <- ca_fixture()
  adjustment <- sqrt(1 - mean(cor(x$wide)[upper.tri(cor(x$wide))]))
  for (errorbar in list("CI", "SE", "none", function(x) c(mean(x) - 2, mean(x) + 3))) {
    raw <- get_adjusted_ci(x$long, "y", within = "condition", wid = "id",
      errorbar = errorbar, gamma = .9, adjustments = list(decorrelation = "none"))
    ca <- get_adjusted_ci(x$long, "y", within = "condition", wid = "id",
      errorbar = errorbar, gamma = .9, adjustments = list(decorrelation = "CA"))
    difference <- get_adjusted_ci(x$long, "y", within = "condition", wid = "id",
      errorbar = errorbar, gamma = .9,
      adjustments = list(decorrelation = "CA", purpose = "difference"))
    expect_equal(ca$center, raw$center)
    expect_equal(ca$upperwidth, adjustment * raw$upperwidth)
    expect_equal(ca$lowerwidth, adjustment * raw$lowerwidth)
    expect_equal(difference$upperwidth, sqrt(2) * ca$upperwidth)
    expect_equal(difference$lowerwidth, sqrt(2) * ca$lowerwidth)
  }
  # Perfect negative correlation widens rather than shrinks the interval.
  negative <- data.frame(id = rep(1:4, 2), condition = factor(rep(c("A", "B"), each = 4)),
    y = c(1:4, 4:1))
  ca <- get_adjusted_ci(negative, "y", within = "condition", wid = "id",
    errorbar = "SE", adjustments = list(decorrelation = "CA"))
  expect_equal(ca$upperwidth, rep(sqrt(2) * sd(1:4) / 2, 2))
})

test_that("CA matches superb for mixed designs and multiple within factors", {
  skip_if_not_installed("superb")
  # superb resolves precision functions through the search path.
  was_attached <- "package:superb" %in% search()
  suppressPackageStartupMessages(library(superb))
  if (!was_attached) on.exit(detach("package:superb", character.only = TRUE))
  old <- options(superb.feedback = "none")
  on.exit(options(old), add = TRUE)
  set.seed(217)
  wide <- data.frame(group = factor(rep(c("one", "two"), each = 12)),
    A = rnorm(24), B = rnorm(24), C = rnorm(24), D = rnorm(24))
  wide$B <- wide$B + wide$A * rep(c(2, -1), each = 12)
  long <- data.frame(id = rep(1:24, 4), group = rep(wide$group, 4),
    first = factor(rep(c("a", "a", "b", "b"), each = 24)),
    second = factor(rep(c("x", "y", "x", "y"), each = 24)),
    y = unlist(wide[-1], use.names = FALSE))
  # Duplicate trials symmetrically: correlation must use subject-cell means.
  trials <- rbind(transform(long, y = y - .2), transform(long, y = y + .2))
  for (errorbar in c("CI", "SE")) {
    for (purpose in c("single", "difference")) {
      opts <- list(decorrelation = "CA", purpose = purpose)
      actual <- get_adjusted_ci(trials, "y", within = c("first", "second"),
        between = "group", wid = "id", errorbar = errorbar, adjustments = opts)
      reference <- suppressMessages(superb::superbData(wide,
        WSFactors = c("first(2)", "second(2)"),
        WSDesign = list(c(1, 1), c(1, 2), c(2, 1), c(2, 2)),
        factorOrder = c("first", "second", "group"), variables = c("A", "B", "C", "D"),
        BSFactors = "group", errorbar = errorbar, adjustments = opts))$summaryStatistics
      key <- paste(actual$first, actual$second, actual$group)
      ref_key <- paste(c("a", "b")[as.integer(reference$first)],
        c("x", "y")[as.integer(reference$second)], reference$group)
      reference <- reference[match(key, ref_key), ]
      expect_equal(actual$center, reference$center)
      expect_equal(actual$lowerwidth, reference$lowerwidth)
      expect_equal(actual$upperwidth, reference$upperwidth)
    }
  }
})

test_that("CA handles missing subjects and rejects undefined correlations", {
  x <- ca_fixture()$long
  incomplete <- x[-1, ]
  expect_error(get_adjusted_ci(incomplete, "y", within = "condition", wid = "id",
    adjustments = list(decorrelation = "CA")), "NAs present after aggregation")
  expect_warning(actual <- get_adjusted_ci(incomplete, "y", within = "condition", wid = "id",
    drop_NA_subj = TRUE, adjustments = list(decorrelation = "CA")), "dropping 1")
  expected <- get_adjusted_ci(subset(x, id != 1), "y", within = "condition", wid = "id",
    adjustments = list(decorrelation = "CA"))
  expect_equal(actual, expected)
  for (bad in list(subset(x, id == 1), subset(x, condition == "A"))) {
    expect_error(get_adjusted_ci(bad, "y", within = "condition", wid = "id",
      adjustments = list(decorrelation = "CA")), "at least two")
  }
  x$y[x$condition == "A"] <- 1
  expect_error(get_adjusted_ci(x, "y", within = "condition", wid = "id",
    adjustments = list(decorrelation = "CA")), "nonzero variance")
  expect_error(get_adjusted_ci(x, "y", between = "condition",
    adjustments = list(decorrelation = "CA")), "cannot be used")
})
