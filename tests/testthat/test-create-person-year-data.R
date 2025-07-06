library(testthat)

source("helper-testdata.R")

context("create_person_year_data")

test_that("create_person_year_data works as expected", {
  py <- create_person_year_data(test_data, min_age = 50, max_age = 80)
  expect_s3_class(py, "data.table")
  expect_true(all(py$age >= 50 & py$age <= 80))
  expect_true(all(c("ids", "age", "status", "astatus", "weight", "group") %in% names(py)))
})

test_that("create_person_year_data handles empty data", {
  empty <- test_data[0, ]
  expect_error(create_person_year_data(empty, 50, 80), NA)
})

test_that("create_person_year_data errors on missing columns", {
  bad_data <- test_data[, -which(names(test_data) == "status")]
  expect_error(create_person_year_data(bad_data, 50, 80))
}) 