test_that("print and summary methods for pie_analysis run without error", {
  result <- pie_analysis(test_data, min_age = 50, max_age = 80, age_group_width = 5, group_var = "group", age_free = 50)
  expect_output(print(result))
  expect_output(summary(result))
})

test_that("print methods for survival_results run without error", {
  res <- calculate_disease_free_survival(
    data = make_dfs_data(),
    time_event_col = "time_to_event",
    time_death_col = "time_to_death",
    event_col = "event",
    death_col = "death",
    baseline_age_col = "Age",
    index_age = 50,
    n_bootstrap = 20
  )
  expect_output(print(res))
  if (inherits(res, "stratified_survival_results")) {
    expect_output(print(res))
  }
}) 