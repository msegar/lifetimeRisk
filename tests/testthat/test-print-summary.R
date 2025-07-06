library(testthat)
source("helper-testdata.R")

context("S3 print and summary methods")

test_that("print and summary methods for pie_analysis run without error", {
  result <- pie_analysis(test_data, min_age = 50, max_age = 80, age_group_width = 5, group_var = "group", age_free = 50)
  expect_output(print(result))
  expect_output(summary(result))
})

test_that("print methods for survival_results run without error", {
  res <- calculate_disease_free_survival(
    data = test_data,
    time_event_col = "survage",
    time_death_col = "survage",
    event_col = "status",
    death_col = "astatus",
    index_age = 50,
    n_bootstrap = 5
  )
  expect_output(print(res))
  if (inherits(res, "stratified_survival_results")) {
    expect_output(print(res))
  }
}) 