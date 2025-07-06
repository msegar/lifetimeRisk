library(testthat)
source("helper-testdata.R")

context("calculate_cumulative_incidence")

test_that("calculate_cumulative_incidence returns expected output", {
  py <- create_person_year_data(test_data, 50, 80)
  ci <- calculate_cumulative_incidence(py, age_free = 50)
  expect_type(ci, "list")
  expect_true(all(c("unadjusted", "adjusted") %in% names(ci)))
  expect_true(all(c("age", "est", "lcl", "ucl") %in% names(ci$unadjusted)))
})

test_that("calculate_cumulative_incidence handles empty data", {
  empty <- test_data[0, ]
  py <- create_person_year_data(empty, 50, 80)
  expect_null(calculate_cumulative_incidence(py, 50))
}) 