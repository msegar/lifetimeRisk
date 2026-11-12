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
test_that("pie_analysis stores summary datasets and number at risk can be extracted", {
  result <- pie_analysis(test_data, min_age = 50, max_age = 80, age_group_width = 5,
                         group_var = "group", age_free = 50)
  expect_named(result$summary_datasets, c("A", "B"))
  nar <- get_number_at_risk_from_analysis(result, age_points = c(50, 60))
  expect_named(nar, c("Age", "A", "B"))
  expect_equal(nar$A[1], 1L)
})
