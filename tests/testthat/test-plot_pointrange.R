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
