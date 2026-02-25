test_that("wave_overview has correct structure", {
  expect_s3_class(wave_overview, "data.frame")
  expect_equal(nrow(wave_overview), 20)
  expect_named(wave_overview, c("wave", "survey_round", "type", "questionnaire"))
  expect_true(all(c("w1", "w6_M", "w14_V") %in% wave_overview$wave))
})

test_that("codebook has correct structure", {
  expect_s3_class(codebook, "data.frame")
  expect_gt(nrow(codebook), 0)
  expect_true("wave" %in% names(codebook))
  expect_true("variable_name" %in% names(codebook))
  expect_true("variable_label" %in% names(codebook))
})

test_that(".normalise_wave handles case and missing underscore", {
  expect_equal(.normalise_wave("W6_M"), "w6_m")
  expect_equal(.normalise_wave("w6M"),  "w6_m")
  expect_equal(.normalise_wave("W1"),   "w1")
  expect_equal(.normalise_wave("w14V"), "w14_v")
})

test_that("get_wave rejects invalid wave identifiers", {
  expect_error(get_wave("w99"),   "not a valid wave")
  expect_error(get_wave("wave1"), "not a valid wave")
})

test_that("get_wave downloads and returns a data frame", {
  skip_on_cran()
  skip_if_offline()
  w1 <- get_wave("w1")
  expect_s3_class(w1, "data.frame")
  expect_gt(nrow(w1), 0)
  expect_gt(ncol(w1), 0)
})

test_that("get_codebook rejects invalid wave", {
  expect_error(get_codebook("w99"), "not a valid wave")
})

test_that("get_codebook downloads and returns a data frame", {
  skip_on_cran()
  skip_if_offline()
  cb <- get_codebook("w1")
  expect_s3_class(cb, "data.frame")
  expect_gt(nrow(cb), 0)
  expect_true("variable_name" %in% names(cb))
})

test_that("lookup_variable returns a data frame", {
  result <- lookup_variable("income")
  expect_s3_class(result, "data.frame")
  expect_named(result, c("wave", "variable_name", "variable_label",
                          "block", "question"))
})

test_that("lookup_variable is case-insensitive", {
  r1 <- lookup_variable("income")
  r2 <- lookup_variable("INCOME")
  expect_equal(nrow(r1), nrow(r2))
})

test_that("lookup_variable returns empty frame for no-match", {
  result <- lookup_variable("xyzzy_no_match_12345")
  expect_equal(nrow(result), 0)
})
