library(testthat)
source("helper-testdata.R")

context("pie_analysis")

test_that("pie_analysis runs and returns expected structure", {
  result <- pie_analysis(test_data, min_age = 50, max_age = 80, age_group_width = 5, group_var = "group", age_free = 50)
  expect_s3_class(result, "pie_analysis")
  expect_true("cumulative_incidence" %in% names(result))
  expect_true(all(c("A", "B") %in% names(result$cumulative_incidence)))
})

test_that("pie_analysis handles single group", {
  td <- test_data; td$group <- NULL
  result <- pie_analysis(td, min_age = 50, max_age = 80, age_group_width = 5, group_var = NULL, age_free = 50)
  expect_s3_class(result, "pie_analysis")
  expect_true("overall" %in% names(result$cumulative_incidence))
}) 