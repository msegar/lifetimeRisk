# Example test data for use in testthat tests

test_data <- data.frame(
  ids = 1:5,
  entryage = c(50, 55, 60, 65, 70),
  survage = c(60, 65, 70, 75, 80),
  status = c(1, 0, 1, 0, 1),
  astatus = c(0, 1, 0, 1, 0),
  group = c("A", "A", "B", "B", "A")
) 

# Example data for calculate_disease_free_survival(): times are years from enrollment
make_dfs_data <- function(n = 80, seed = 42) {
  set.seed(seed)
  age <- sample(45:60, n, replace = TRUE)
  time_to_death <- runif(n, 5, 35)
  time_to_event <- pmin(runif(n, 1, 40), time_to_death)
  data.frame(
    Age = age,
    time_to_event = time_to_event,
    time_to_death = time_to_death,
    event = as.integer(time_to_event < time_to_death),
    death = rbinom(n, 1, 0.6),
    grp = sample(c("low", "high"), n, replace = TRUE)
  )
}
