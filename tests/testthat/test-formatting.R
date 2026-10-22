test_that("regression styles preserve precision and return types", {
  fit <- lm(mpg ~ wt, mtcars)
  expected <- c(
    "_t_(30) = -9.56, _p_ < .001",
    "_B_ = -5.34 (0.56), _p_ < .001",
    "_B_ = -5.34, _SE_ = 0.56, _t_(30) = -9.56, _p_ < .001",
    "_B_ = -5.34 (0.56), _t_(30) = -9.56"
  )
  actual <- vapply(1:4, function(dtype) {
    apa(fit, "wt", dtype = dtype, test.df = TRUE)
  }, character(1))
  expect_identical(actual, expected)
  table <- apa(fit, p.as.number = TRUE, b.digits = 3)
  expect_s3_class(table, "data.frame")
  expect_named(table, c("B", "SE", "Stat", "p", "eff", "str"))
  expect_type(table$p, "double")
  expect_identical(apa(fit, term.pattern = "^wt"), apa(fit)["wt", , drop = FALSE])
  expect_identical(apa(fit, "wt", dtype = 2, b.digits = 3),
                   "_B_ = -5.344 (0.559), _p_ < .001")
})

test_that("regression styles retain their distinct small-statistic thresholds", {
  x <- seq(-1, 1, length.out = 10)
  noise <- resid(lm(sin(seq_along(x)) ~ x))
  slope_se <- sqrt(sum(noise^2) / (length(x) - 2) / sum((x - mean(x))^2))
  fit <- lm(I(noise + 0.005 * slope_se * x) ~ x)
  expect_match(apa(fit, "x", dtype = 1, t.digits = 3), "_t_ = 0.005", fixed = TRUE)
  expect_match(apa(fit, "x", dtype = 3, t.digits = 3), "_t_ < 0.01", fixed = TRUE)
})

test_that("ANOVA methods preserve corrections and optional table output", {
  ez <- list(ANOVA = data.frame(Effect = "condition", DFn = 1, DFd = 10,
                               F = 4.96, p = 0.05, ges = 0.15))
  afex <- structure(list(anova_table = data.frame(
    "num Df" = 1, "den Df" = 10, F = 4.96, "Pr(>F)" = 0.05, ges = 0.15,
    check.names = FALSE, row.names = "condition"
  )), class = "afex_aov")
  expected <- "_F_(1, 10) = 4.96, _p_ = .050"
  expect_identical(apa(ez, "condition", include_eta = FALSE), expected)
  expect_identical(apa(afex, "condition", include_eta = FALSE), expected)
  expect_identical(apa(ez, "condition"), apa(afex, "condition"))
  table <- apa(ez, "condition", append_to_table = TRUE)
  expect_s3_class(table, "data.frame")
  expect_identical(table$res, apa(ez, "condition"))
  expect_identical(table$ges, 0.15)

  ez[["Sphericity Corrections"]] <- data.frame(
    Effect = "condition", GGe = 0.75, "p[GG]" = 0.08, check.names = FALSE)
  expect_match(apa(ez, "condition"), "_p_ = .080", fixed = TRUE)
  expect_match(apa(ez, "condition", spher_corr = FALSE), "_p_ = .050", fixed = TRUE)
})

test_that("mean and binomial interval text keeps requested output formatting", {
  expect_identical(apa_binom_mean_conf(c(1, 0, 1, 1, 0)),
                   "_M_ = 0.60 [0.23, 0.88]")
  expect_identical(apa_format_mean_conf(1.5, 1, 2, addCI = TRUE),
                   "_M_ = 1.50, 95% _CI_ = [1.00, 2.00]")
  expect_identical(apa_format_mean_conf(1.5, 1, 2, type = "latex"),
                   "\\emph{M} = 1.50 [1.00, 2.00]")
  text <- apa_mean_and_t(c(1, 3, 4, 7, 8, 12), rep(1:2, each = 3),
                         which.mean = 3)
  expect_match(text, "^_M_ = .* vs[.] _M_ = .*_t_")
})

test_that("plotmath formatting returns text without a comma", {
  expect_identical(format_results("\\emph{x} = 1", type = "plotmath"),
                   "italic(x) == 1")
  expect_identical(format_results(c("x = 1", "y = 2"), type = "plotmath"),
                   c("x == 1", "y == 2"))
})

test_that("Bayesian formatting extracts an exact coefficient without deprecated APIs", {
  skip_if_not_installed("brms")
  skip_if_not_installed("bayestestR")
  local_mocked_bindings(as_draws_matrix = function(x, variable, regex, ...) {
    expect_identical(variable, "b_x")
    expect_false(regex)
    matrix(seq(0, 1, length.out = 101), ncol = 1,
           dimnames = list(NULL, "b_x"))
  }, .package = "brms")
  model <- structure(list(), class = "brmsfit")
  result <- expect_no_warning(apa(model, "x", ci.type = "ETI"))
  expect_match(result, "_b_ = 0.50, 95% ETI", fixed = TRUE)
  transformed <- apa(model, "x", ci.type = "ETI", trans = function(x) x + 5)
  expect_match(transformed, "_b_ = 5.50, 95% ETI", fixed = TRUE)
})
