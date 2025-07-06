library(testthat)
source("helper-testdata.R")

context("calculate_disease_free_survival")

test_that("calculate_disease_free_survival returns expected structure", {
  res <- calculate_disease_free_survival(
    data = test_data,
    time_event_col = "survage",
    time_death_col = "survage",
    event_col = "status",
    death_col = "astatus",
    index_age = 50,
    n_bootstrap = 5
  )
  expect_true(is.list(res))
  expect_true(any(class(res) %in% c("survival_results", "stratified_survival_results")))
})

test_that("calculate_disease_free_survival handles empty data", {
  empty <- test_data[0, ]
  expect_error(calculate_disease_free_survival(
    data = empty,
    time_event_col = "survage",
    time_death_col = "survage",
    event_col = "status",
    death_col = "astatus",
    index_age = 50,
    n_bootstrap = 5
  ))
})

test_that("calculate_disease_free_survival is reproducible with set.seed", {
  set.seed(123)
  res1 <- calculate_disease_free_survival(
    data = test_data,
    time_event_col = "survage",
    time_death_col = "survage",
    event_col = "status",
    death_col = "astatus",
    index_age = 50,
    n_bootstrap = 5
  )
  set.seed(123)
  res2 <- calculate_disease_free_survival(
    data = test_data,
    time_event_col = "survage",
    time_death_col = "survage",
    event_col = "status",
    death_col = "astatus",
    index_age = 50,
    n_bootstrap = 5
  )
  expect_equal(res1$event_free_survival, res2$event_free_survival)
}) 