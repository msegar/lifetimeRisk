library(testthat)
source("helper-testdata.R")

context("calculate_age_specific_rates")

test_that("calculate_age_specific_rates returns correct structure", {
  py <- create_person_year_data(test_data, 50, 80)
  rates <- calculate_age_specific_rates(py, age_group_width = 5, min_age = 50)
  expect_s3_class(rates, "data.table")
  expect_true(all(c("age_min", "age_max", "events", "person_years", "rate") %in% names(rates)))
})

test_that("calculate_age_specific_rates handles empty data", {
  empty <- test_data[0, ]
  py <- create_person_year_data(empty, 50, 80)
  expect_error(calculate_age_specific_rates(py, 5, 50), NA)
}) 