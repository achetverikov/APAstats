test_that("memory_noise has the intended mixed-design structure", {
  data(memory_noise, package = "apastats2")

  expect_equal(nrow(memory_noise), 70)
  expect_equal(length(unique(memory_noise$participant)), 35)
  exp_counts <- table(memory_noise$experiment)
  expect_identical(names(exp_counts), c("Exp. 1A", "Exp. 1B"))
  expect_equal(as.integer(exp_counts), c(36L, 34L))
  expect_equal(
    levels(memory_noise$relative_noise),
    c("target less noisy", "target more noisy")
  )
  expect_true(is.ordered(memory_noise$relative_noise))
  expect_true(all(table(memory_noise$participant) == 2))
  expect_true(all(memory_noise$n_trials > 0))
  expect_true(all(memory_noise$bias_percent >= -100))
  expect_true(all(memory_noise$bias_percent <= 100))
})

test_that("memory_noise reproduces the unequal-noise bias direction", {
  data(memory_noise, package = "apastats2")

  means <- aggregate(
    bias_percent ~ relative_noise,
    memory_noise,
    mean
  )
  less <- means$bias_percent[means$relative_noise == "target less noisy"]
  more <- means$bias_percent[means$relative_noise == "target more noisy"]

  expect_gt(less, 0)
  expect_lt(more, 0)
})

test_that("memory_noise supports between, within, and mixed adjusted intervals", {
  data(memory_noise, package = "apastats2")

  target_more <- memory_noise[
    memory_noise$relative_noise == "target more noisy",
  ]
  between <- get_adjusted_ci(
    target_more,
    value_var = "bias_percent",
    between = "experiment",
    wid = "participant"
  )
  expect_equal(nrow(between), 2)
  expect_true(all(is.finite(between$center)))

  within <- get_adjusted_ci(
    memory_noise[memory_noise$experiment == "Exp. 1A", ],
    value_var = "bias_percent",
    within = "relative_noise",
    wid = "participant"
  )
  expect_equal(nrow(within), 2)
  expect_true(all(is.finite(within$center)))

  mixed <- get_adjusted_ci(
    memory_noise,
    value_var = "bias_percent",
    within = "relative_noise",
    between = "experiment",
    wid = "participant"
  )
  expect_equal(nrow(mixed), 4)
  expect_true(all(is.finite(mixed$lower_ci)))
  expect_true(all(is.finite(mixed$upper_ci)))
})
