library(testthat)
source("helper-testdata.R")

context("plotting and CSV export")

test_that("plot_lifetime_risk runs without error", {
  result <- pie_analysis(test_data, min_age = 50, max_age = 80, age_group_width = 5, group_var = "group", age_free = 50)
  expect_silent(
    p <- plot_lifetime_risk(result, adjusted = TRUE)
  )
  expect_true("ggplot" %in% class(p))
})

test_that("create_lifetime_csv creates a file and returns a data frame", {
  result <- pie_analysis(test_data, min_age = 50, max_age = 80, age_group_width = 5, group_var = "group", age_free = 50)
  tmpfile <- tempfile(fileext = ".csv")
  df <- create_lifetime_csv(result, adjusted = TRUE, output_file = tmpfile)
  expect_true(file.exists(tmpfile))
  expect_s3_class(df, "data.frame")
  unlink(tmpfile)
}) 