test_that("plot_pointrange adds a margin for between- and within-subject designs", {
  md <- memory_noise
  within_plot <- plot_pointrange(
    md,
    aes(x = relative_noise, color = experiment, y = bias_percent),
    wid = "participant", within_subj = TRUE, withinvars = "relative_noise",
    betweenvars = "experiment", add_margin = TRUE
  )
  between_plot <- plot_pointrange(
    md,
    aes(x = relative_noise, color = experiment, y = bias_percent),
    withinvars = "relative_noise", betweenvars = "experiment", add_margin = TRUE
  )
  for (p in list(within_plot, between_plot)) {
    expect_equal(sum(p$data$relative_noise == "all"), 2)
    expect_true(all(is.finite(c(p$data$ymin, p$data$ymax))))
  }
})

test_that("pointrange limits match ordinary t intervals and exponentiation", {
  d <- data.frame(group = rep(c("A", "B"), each = 4),
                  y = c(1, 2, 4, 7, 2, 5, 6, 9))
  samples <- split(d$y, d$group)
  centers <- vapply(samples, mean, numeric(1))
  widths <- qt(0.975, 3) * vapply(samples, sd, numeric(1)) / sqrt(4)
  p <- plot_pointrange(d, aes(x = group, y = y), betweenvars = "group")
  expect_equal(p$data$y, unname(centers))
  expect_equal(p$data$ymin, unname(centers - widths))
  expect_equal(p$data$ymax, unname(centers + widths))
  transformed <- plot_pointrange(d, aes(x = group, y = y),
                                 betweenvars = "group", exp_y = TRUE)
  expect_equal(transformed$data$ymin, exp(p$data$ymin))
  expect_equal(transformed$data$ymax, exp(p$data$ymax))
})
