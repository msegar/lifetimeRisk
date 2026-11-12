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
  ci <- calculate_cumulative_incidence(py, 50)
  expect_null(ci)
}) 
test_that("calculate_cumulative_incidence does not modify its input by reference", {
  py <- create_person_year_data(test_data, 50, 80)
  before <- data.table::copy(py)
  calculate_cumulative_incidence(py, age_free = 50)
  expect_identical(py, before)
})

test_that("confidence interval width varies over age (SE is not a constant)", {
  set.seed(1)
  d <- data.frame(ids = 1:300, entryage = sample(50:55, 300, TRUE))
  d$survage <- d$entryage + sample(2:25, 300, TRUE)
  d$status <- rbinom(300, 1, 0.4)
  d$astatus <- ifelse(d$status == 1, 0, rbinom(300, 1, 0.2))
  d$group <- "all"
  ci <- calculate_cumulative_incidence(create_person_year_data(d, 50, 80), 50)
  width <- ci$unadjusted$ucl - ci$unadjusted$lcl
  expect_gt(length(unique(round(width, 6))), 1)
  first_event <- which(ci$unadjusted$est > 0)[1]
  expect_true(all(width[seq_len(first_event - 1)] == 0))
})
