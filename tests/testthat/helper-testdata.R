# Example test data for use in testthat tests

test_data <- data.frame(
  ids = 1:5,
  entryage = c(50, 55, 60, 65, 70),
  survage = c(60, 65, 70, 75, 80),
  status = c(1, 0, 1, 0, 1),
  astatus = c(0, 1, 0, 1, 0),
  group = c("A", "A", "B", "B", "A")
) 