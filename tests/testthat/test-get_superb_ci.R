test_that("get_superb_ci prints informative labels for incomplete wide rows", {
  skip_if_not_installed("superb")
  skip_if_not_installed("reshape2")

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
      get_superb_ci(data, "id", "condition", "rt", between = "group"),
      "NAs present after aggregation"
    )
  )

  expect_true(any(grepl("control group", printed, fixed = TRUE)))
  expect_true(any(grepl("condition=right response", printed, fixed = TRUE)))
  expect_false(any(grepl("\\b[A-Z]{5}\\b", printed)))
})
