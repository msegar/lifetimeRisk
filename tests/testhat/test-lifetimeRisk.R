# tests/testthat/test-pie.R

test_that("pie function returns expected structure", {
  # Create some dummy data
  data <- data.frame(
    id = 1:100,
    age = sample(30:80, 100, replace = TRUE),
    status = sample(0:1, 100, replace = TRUE),
    group = sample(c("A", "B"), 100, replace = TRUE)
  )

  result <- pie(data, min_age = 30, max_age = 80, age_group_width = 5,
                group = "group", level1 = "A", level2 = "B",
                age_free = 30, o1 = "plot1.png", o2 = "plot2.png",
                study_label = "Test Study", min_age_group = 30)

  expect_type(result, "list")
  expect_named(result, c("incidence_data", "aa2g_result", "lr_data", "plots"))
})
