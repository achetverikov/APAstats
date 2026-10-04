test_that("apa supports aovlist models from Error()", {
  fit <- stats::aov(yield ~ N * P * K + Error(block), data = npk)

  expect_s3_class(fit, "aovlist")

  all_results <- apa(fit)
  expect_gt(length(all_results), 0)
  expect_true(all(grepl("^_F_\\(", all_results)))
  expect_false(any(grepl("NA", all_results, fixed = TRUE)))

  n_result <- apa(fit, "N")
  expect_length(n_result, 1)
  expect_match(n_result, "^_F_\\(")
  expect_match(n_result, "_p_")

  expect_warning(
    legacy_result <- describe.aov(fit, "N"),
    "deprecated",
    ignore.case = TRUE
  )
  expect_identical(unname(legacy_result), unname(n_result))
})

test_that("apa.aovlist reports unknown terms clearly", {
  fit <- stats::aov(yield ~ N * P * K + Error(block), data = npk)

  expect_error(
    apa(fit, "not_a_term"),
    "Available terms"
  )
})
