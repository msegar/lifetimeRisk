test_that("calculate_disease_free_survival returns expected structure", {
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
  expect_true(is.list(res))
  expect_true(any(class(res) %in% c("survival_results", "stratified_survival_results")))
})

test_that("calculate_disease_free_survival handles empty data", {
  expect_error(calculate_disease_free_survival(
    data = make_dfs_data()[0, ],
    time_event_col = "time_to_event",
    time_death_col = "time_to_death",
    event_col = "event",
    death_col = "death",
    baseline_age_col = "Age",
    index_age = 50,
    n_bootstrap = 20
  ))
})

test_that("calculate_disease_free_survival is reproducible with set.seed", {
  set.seed(123)
  res1 <- calculate_disease_free_survival(
    data = make_dfs_data(),
    time_event_col = "time_to_event",
    time_death_col = "time_to_death",
    event_col = "event",
    death_col = "death",
    baseline_age_col = "Age",
    index_age = 50,
    n_bootstrap = 20
  )
  set.seed(123)
  res2 <- calculate_disease_free_survival(
    data = make_dfs_data(),
    time_event_col = "time_to_event",
    time_death_col = "time_to_death",
    event_col = "event",
    death_col = "death",
    baseline_age_col = "Age",
    index_age = 50,
    n_bootstrap = 20
  )
  expect_equal(res1$event_free_survival, res2$event_free_survival)
  expect_equal(res1$se_event_free, res2$se_event_free)
}) 
test_that("stratified and IPTW-adjusted analyses run and give sensible output", {
  skip_if_not_installed("nnet")
  d <- make_dfs_data(n = 200)
  d$bmi <- rnorm(nrow(d), 28, 4)
  args <- list(data = d, time_event_col = "time_to_event", time_death_col = "time_to_death",
               event_col = "event", death_col = "death", baseline_age_col = "Age",
               index_age = 50, restriction_time = 120, strata_col = "grp", n_bootstrap = 20)
  res <- suppressMessages(do.call(calculate_disease_free_survival, args))
  res_adj <- suppressMessages(do.call(calculate_disease_free_survival, c(args, list(adjust_cols = "bmi"))))
  for (r in list(res, res_adj)) {
    expect_s3_class(r, "stratified_survival_results")
    expect_named(r$results, c("high", "low"))
    for (s in r$results) {
      expect_true(s$event_free_survival <= s$overall_survival)
      expect_true(is.finite(s$se_event_free) && s$se_event_free > 0)
    }
  }
  expect_true(res_adj$adjusted)
})
