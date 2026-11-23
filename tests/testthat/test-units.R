test_that("descriptive units appear once after the complete block", {
  expect_identical(apa_format_mean_conf(350, 320, 380, unit = " ms"),
    "_M_ = 350.00 [320.00, 380.00] ms")
  expect_identical(apa_format_mean_conf(350, 320, 380, addCI = TRUE, unit = " ms"),
    "_M_ = 350.00, 95% _CI_ = [320.00, 380.00] ms")
  expect_identical(apa_mean_sd(m = 350, sd = 30, unit = " ms"),
    "_M_ = 350.00 (_SD_ = 30.00) ms")
  expect_identical(apa_mean_sd(m = 350, sd = 30, dtype = "c", unit = " ms"),
    "_M_ = 350.00, _SD_ = 30.00 ms")
  expect_identical(apa_format_mean_conf(c(1, 2), c(0, 1), c(2, 3),
    unit = c(" ms", " s")), c("_M_ = 1.00 [0.00, 2.00] ms", "_M_ = 2.00 [1.00, 3.00] s"))
  expect_identical(apa_mean_conf(1:6, bootCI = FALSE, unit = " ms"),
    paste0(apa_mean_conf(1:6, bootCI = FALSE), " ms"))
  expect_identical(apa_mean_conf(1:6, bootCI = FALSE,
    transform.means = function(x) x * 1000, unit = " ms"),
    paste0(apa_mean_conf(1:6, bootCI = FALSE, transform.means = function(x) x * 1000), " ms"))
  expect_identical(apa_binom_mean_conf(c(1, 0, 1, 1, 0), unit = " proportion"),
    "_M_ = 0.60 [0.23, 0.88] proportion")
})

test_that("unit defaults and legacy suffixes remain compatible", {
  plain <- apa_format_mean_conf(1.5, 1, 2)
  expect_identical(apa_format_mean_conf(1.5, 1, 2, unit = NULL), plain)
  expect_identical(apa_format_mean_conf(1.5, 1, 2, unit = ""), plain)
  expect_identical(apa_format_mean_conf(1.5, 1, 2, FALSE, 2, " ms", type = "latex"),
    "\\emph{M} = 1.50 [1.00, 2.00] ms")
  expect_identical(apa_mean_sd(m = 1, sd = 2, m_units = " ms", sd_units = " ms"),
    "_M_ = 1.00 ms (_SD_ = 2.00 ms)")
  expect_error(apa_mean_sd(m = 1, sd = 2, unit = " ms", m_units = " ms"),
    "Use either unit")
})

test_that("unit suffix spacing and markup are entirely caller supplied", {
  expect_identical(apa_format_mean_conf(1, 0, 2, unit = "ms"),
    "_M_ = 1.00 [0.00, 2.00]ms")
  expect_identical(apa_format_mean_conf(1, 0, 2, unit = " / ms"),
    "_M_ = 1.00 [0.00, 2.00] / ms")
  expect_identical(apa_format_mean_conf(45, 40, 50, unit = "\u00b0"),
    "_M_ = 45.00 [40.00, 50.00]\u00b0")
  expect_identical(apa_format_mean_conf(1, 0, 2, unit = " \\textmu{}s", type = "latex"),
    "\\emph{M} = 1.00 [0.00, 2.00] \\textmu{}s")
  text <- apa_format_mean_conf(1, 0, 2, unit = '~"ms"', type = "plotmath")
  expect_identical(text, 'list(italic(M) == 1.00 [0.00, 2.00]~"ms")')
  expect_type(parse(text = text), "expression")
})

test_that("combined outputs put a single unit before test statistics", {
  x <- c(1, 3, 4, 7, 8, 12)
  group <- rep(1:2, each = 3)
  vapply(c(1, 3), function(which) {
    set.seed(42)
    plain <- apa_mean_and_t(x, group, which.mean = which)
    set.seed(42)
    labelled <- apa_mean_and_t(x, group, which.mean = which, unit = " ms")
    expect_identical(labelled, sub(", _t_", " ms, _t_", plain, fixed = TRUE))
    expect_equal(stringr::str_count(labelled, stringr::fixed("ms")), 1)
    TRUE
  }, logical(1))
  set.seed(42)
  plain <- apa_mean_and_t(x, group, which.mean = 0)
  set.seed(42)
  expect_identical(apa_mean_and_t(x, group, which.mean = 0, unit = " ms"), plain)
  fit <- t.test(x)
  expect_identical(apa(fit, show.mean = TRUE, unit = " ms"),
    sub(", _t_", " ms, _t_", apa(fit, show.mean = TRUE), fixed = TRUE))
  expect_identical(apa(fit, unit = " ms"), apa(fit))
})

test_that("table units appear only in the CI cell and adjusted-CI text", {
  set.seed(42)
  plain <- table_mean_conf(1:6)
  set.seed(42)
  labelled <- table_mean_conf(1:6, unit = " ms")
  expect_identical(labelled, c(plain[1], paste0(plain[2], " ms")))
  set.seed(42)
  plain <- table_mean_conf_by(1:8, rep(1:2, each = 4))
  set.seed(42)
  labelled <- table_mean_conf_by(1:8, rep(1:2, each = 4), unit = " ms")
  expect_identical(lapply(labelled, function(x) x[1]), lapply(plain, function(x) x[1]))
  expect_identical(lapply(labelled, function(x) x[2]),
    lapply(plain, function(x) paste0(x[2], " ms")))
  data <- data.frame(id = rep(1:3, each = 2), condition = factor(rep(c("a", "b"), 3)),
    rt = c(1, 3, 2, 5, 4, 6))
  plain <- get_adjusted_ci(data, "rt", within = "condition", wid = "id")
  labelled <- get_adjusted_ci(data, "rt", within = "condition", wid = "id", unit = " ms")
  expect_identical(labelled$descr, paste0(plain$descr, " ms"))
  expect_identical(labelled[names(labelled) != "descr"], plain[names(plain) != "descr"])
})

test_that("coefficient units precede tests and can vary by term", {
  fit <- lm(mpg ~ wt, mtcars)
  expect_identical(apa(fit, "wt", dtype = 2, unit = " mpg/1000 lb"),
    "_B_ = -5.34 (0.56) mpg/1000 lb, _p_ < .001")
  expect_identical(apa(fit, "wt", dtype = 3, test.df = TRUE, unit = " mpg/1000 lb"),
    "_B_ = -5.34, _SE_ = 0.56 mpg/1000 lb, _t_(30) = -9.56, _p_ < .001")
  expect_identical(apa(fit, "wt", dtype = 4, unit = " mpg/1000 lb"),
    "_B_ = -5.34 (0.56) mpg/1000 lb, _t_ = -9.56")
  expect_identical(apa(fit, "wt", dtype = 1, unit = " mpg/1000 lb"),
    apa(fit, "wt", dtype = 1))
  labelled <- apa(fit, dtype = 2, unit = c("(Intercept)" = " mpg", wt = " mpg/1000 lb"))
  expect_match(labelled$str[1], ") mpg, _p_", fixed = TRUE)
  expect_match(labelled$str[2], ") mpg/1000 lb, _p_", fixed = TRUE)
  partial <- apa(fit, dtype = 2, unit = c(wt = " mpg/1000 lb"))
  expect_identical(partial$str[1], apa(fit, "(Intercept)", dtype = 2))
  expect_error(apa(fit, unit = c(missing_term = "ms")), "matching model terms")
  expect_error(apa(fit, unit = c(wt = "ms", wt = "s")), "unique names")
})

test_that("estimated-contrast units stay with estimates", {
  skip_if_not_installed("emmeans")
  fit <- lm(mpg ~ factor(cyl), mtcars)
  contrasts <- summary(pairs(emmeans::emmeans(fit, ~ cyl)))
  plain <- apa.summary_emm(contrasts, 1, dtype = "all")
  expect_identical(apa.summary_emm(contrasts, 1, dtype = "all", unit = " mpg"),
    sub("), _t_", ") mpg, _t_", plain, fixed = TRUE))
  plain <- apa.summary_emm(contrasts, 1, dtype = "B")
  expect_identical(apa.summary_emm(contrasts, 1, dtype = "B", unit = " mpg"),
    sub("), _p_", ") mpg, _p_", plain, fixed = TRUE))
  expect_identical(apa.summary_emm(contrasts, 1, dtype = "t", unit = " mpg"),
    apa.summary_emm(contrasts, 1, dtype = "t"))
})

test_that("mixed-model summary units precede p-values", {
  skip_if_not_installed("lmerTest")
  fit <- lmerTest::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  model <- summary(fit)
  plain <- apa(model, "Days", dtype = "B")
  expect_identical(apa(model, "Days", dtype = "B", unit = " ms/day"),
    sub("), _p_", ") ms/day, _p_", plain, fixed = TRUE))
  expect_identical(apa(model, "Days", dtype = "t", unit = " ms/day"),
    apa(model, "Days", dtype = "t"))
})

test_that("Bayesian coefficient units follow the interval", {
  skip_if_not_installed("brms")
  skip_if_not_installed("bayestestR")
  local_mocked_bindings(as_draws_matrix = function(x, variable, regex, ...) {
    matrix(seq(0, 1, length.out = 101), ncol = 1, dimnames = list(NULL, "b_x"))
  }, .package = "brms")
  model <- structure(list(), class = "brmsfit")
  plain <- apa(model, "x", ci.type = "ETI")
  expect_identical(apa(model, "x", ci.type = "ETI", unit = " ms"), paste0(plain, " ms"))
})
