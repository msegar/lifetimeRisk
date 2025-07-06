# tests/testthat/test-pie-simple.R

library(testthat)

test_that("pie_analysis works with no grouping", {
  data <- data.frame(
    ids = 1:100,
    entryage = 40,
    survage = 50,
    status = sample(0:1, 100, replace = TRUE),
    astatus = sample(0:1, 100, replace = TRUE)
  )

  result <- pie_analysis(data, 40, 80, 5, group_var = NULL, age_free = 40)

  expect_equal(result$parameters$group_levels, "overall")
  expect_null(result$parameters$group_var)
  expect_named(result$cumulative_incidence, "overall")
})

test_that("pie_analysis works with multiple groups", {
  data <- data.frame(
    ids = 1:100,
    entryage = 40,
    survage = 50,
    status = sample(0:1, 100, replace = TRUE),
    astatus = sample(0:1, 100, replace = TRUE),
    sex = sample(c("M", "F"), 100, replace = TRUE)
  )

  result <- pie_analysis(data, 40, 80, 5, group_var = "sex", age_free = 40)

  expect_equal(result$parameters$group_var, "sex")
  expect_setequal(result$parameters$group_levels, c("F", "M"))
  expect_named(result$cumulative_incidence, c("F", "M"))
})

test_that("lifetime risk table works for overall population", {
  data <- data.frame(
    ids = 1:50,
    entryage = 40,
    survage = 50,
    status = sample(0:1, 50, replace = TRUE),
    astatus = sample(0:1, 50, replace = TRUE)
  )

  result <- create_lifetime_risk_table(data, c(40, 50), 80, group_var = NULL)

  expect_true("Overall" %in% names(result))
  expect_equal(nrow(result), 2)
})
