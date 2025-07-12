library(lifetimeRisk)
library(microbenchmark)
library(dplyr)

# Function to generate test data of specified size
generate_test_data <- function(n, seed = 123) {
  set.seed(seed)

  data.frame(
    ids = 1:n,
    entryage = round(runif(n, min = 45, max = 65)),
    followup_time = round(runif(n, min = 2, max = 20)),
    status = rbinom(n, 1, 0.3),
    astatus = rbinom(n, 1, 0.2),
    sex = sample(c("Male", "Female"), n, replace = TRUE)
  ) %>%
    mutate(
      survage = entryage + followup_time,
      status = ifelse(astatus == 1, 0, status)
    ) %>%
    select(ids, entryage, survage, status, astatus, sex)
}

# Generate datasets of different sizes
dataset_sizes <- c(1000, 5000, 10000, 25000, 50000, 100000, 1000000)
datasets <- list()

cat("Generating datasets...\n")
for(size in dataset_sizes) {
  cat(sprintf("Creating dataset with %d observations...\n", size))
  datasets[[as.character(size)]] <- generate_test_data(size)
}

# Function to run pie_analysis with error handling
run_pie_analysis <- function(data) {
  tryCatch({
    pie_analysis(
      data = data,
      min_age = 45,
      max_age = 90,
      age_group_width = 5,
      group_var = NULL,
      age_free = 45
    )
  }, error = function(e) {
    message("Error in analysis: ", e$message)
    NULL
  })
}

# Performance benchmarking
cat("Running performance benchmarks...\n")
benchmark_results <- list()

for(size in dataset_sizes) {
  cat(sprintf("Benchmarking dataset size: %d\n", size))

  # Run microbenchmark
  mb_result <- microbenchmark(
    pie_analysis = run_pie_analysis(datasets[[as.character(size)]]),
    times = 5,  # Number of repetitions
    unit = "s"   # Report in seconds
  )

  # Extract timing statistics
  timing_stats <- summary(mb_result)

  benchmark_results[[as.character(size)]] <- data.frame(
    dataset_size = size,
    min_time = timing_stats$min,
    median_time = timing_stats$median,
    mean_time = timing_stats$mean,
    max_time = timing_stats$max,
    memory_usage = object.size(datasets[[as.character(size)]]) / 1024^2  # MB
  )
}

# Combine results into a single dataframe
performance_table <- do.call(rbind, benchmark_results)
rownames(performance_table) <- NULL

# Format the table for presentation
performance_table_formatted <- performance_table %>%
  mutate(
    dataset_size = format(dataset_size, big.mark = ","),
    min_time = sprintf("%.3f", min_time),
    median_time = sprintf("%.3f", median_time),
    mean_time = sprintf("%.3f", mean_time),
    max_time = sprintf("%.3f", max_time),
    memory_usage = sprintf("%.2f MB", memory_usage)
  )

# Print results
cat("\n=== PERFORMANCE BENCHMARK RESULTS ===\n")
print(performance_table_formatted)

# Create a summary table for manuscript inclusion
manuscript_table <- performance_table %>%
  mutate(
    `Dataset Size` = format(dataset_size, big.mark = ","),
    `Median Time (seconds)` = sprintf("%.2f", median_time),
    `Memory Usage (MB)` = sprintf("%.1f", memory_usage),
    `Observations per Second` = sprintf("%.0f", dataset_size / median_time)
  ) %>%
  select(`Dataset Size`, `Median Time (seconds)`, `Memory Usage (MB)`, `Observations per Second`)

cat("\n=== MANUSCRIPT TABLE ===\n")
print(manuscript_table)

# Optional: Save results
write.csv(performance_table, "performance_benchmark_detailed.csv", row.names = FALSE)
write.csv(manuscript_table, "performance_benchmark_summary.csv", row.names = FALSE)

cat("\nBenchmarking complete! Results saved to CSV files.\n")

# Optional: Create a simple plot
if(require(ggplot2, quietly = TRUE)) {
  p <- ggplot(performance_table, aes(x = dataset_size, y = median_time)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(labels = scales::comma) +
    labs(
      title = "lifetimeRisk Package Performance",
      x = "Dataset Size (observations)",
      y = "Median Execution Time (seconds)",
      caption = "Based on 5 replications per dataset size"
    ) +
    theme_minimal()

  ggsave("performance_plot.png", p, width = 8, height = 6, dpi = 300)
  cat("Performance plot saved as 'performance_plot.png'\n")
}
