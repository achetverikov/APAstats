test_that("numbers2words converts common integer values", {
  expect_identical(apastats2:::numbers2words(0), "zero")
  expect_identical(apastats2:::numbers2words(19), "nineteen")
  expect_identical(apastats2:::numbers2words(42), "forty two")
  expect_identical(
    apastats2:::numbers2words(1005),
    "one thousand five"
  )
  expect_identical(
    apastats2:::numbers2words(c(-12, 1000000)),
    c("minus twelve", "one million")
  )
})

test_that("numbers2words preserves NA and rejects unsupported values", {
  expect_true(is.na(apastats2:::numbers2words(NA_real_)))
  expect_error(apastats2:::numbers2words(Inf), "finite")
  expect_error(apastats2:::numbers2words(1e15), "below 1 quadrillion")
})
