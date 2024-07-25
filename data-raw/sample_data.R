

## code to prepare `sample_data` dataset goes here

# Create sample data
set.seed(123)  # For reproducibility
n <- 1000

sample_data <- data.frame(
  ids = 1:n,
  entryage = round(runif(n, min = 30, max = 70), 0),  # Entry age between 30 and 70
  followup_time = round(runif(n, min = 2, max = 20), 0),  # Follow-up time up to 20 years
  status = rbinom(n, 1, 0.3),  # 30% chance of primary event
  astatus = rbinom(n, 1, 0.2),  # 20% chance of competing event
  group = sample(c("A", "B"), n, replace = TRUE)  # Two groups
) %>%
  mutate(
    survage = entryage + followup_time,
    status = ifelse(astatus == 1, 0, status)  # If competing event occurred, primary event didn't
  ) %>%
  select(ids, entryage, survage, status, astatus, group)

usethis::use_data(sample_data, overwrite = TRUE)
