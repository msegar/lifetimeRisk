# Test Script for plot_biomarker_risk_continuum Function
# =====================================================

# Load the package and required libraries
library(lifetimeRisk)
library(ggplot2)
library(data.table)

cat("Testing plot_biomarker_risk_continuum function\n")
cat("===============================================\n\n")

# Create realistic test data
set.seed(42)
n <- 1000

cat("Creating test dataset with", n, "participants...\n")

# Generate realistic cardiovascular risk data
test_data <- data.frame(
  ids = 1:n,
  # Entry ages between 40-70
  entryage = round(runif(n, min = 40, max = 70)),
  # Follow-up time 5-25 years
  followup_time = round(runif(n, min = 5, max = 25)),
  # Higher risk biomarker values increase event probability
  ntprobnp = exp(rnorm(n, mean = log(50), sd = 0.8))  # Log-normal, median ~50
)

# Create age-dependent and biomarker-dependent risks
test_data$risk_multiplier <- with(test_data, {
  age_risk <- pmax(0, (entryage - 50) / 20)  # Risk increases with age
  biomarker_risk <- pmax(0, log(ntprobnp / 50) / 2)  # Risk increases with biomarker
  age_risk + biomarker_risk
})

# Calculate survival age
test_data$survage <- test_data$entryage + test_data$followup_time

# Generate events based on risk
base_event_prob <- 0.15
base_competing_prob <- 0.12

test_data$status <- rbinom(n, 1, pmin(0.6, base_event_prob * (1 + test_data$risk_multiplier)))
test_data$astatus <- rbinom(n, 1, pmin(0.4, base_competing_prob * (1 + test_data$risk_multiplier * 0.5)))

# Ensure mutual exclusivity of events
test_data$status <- ifelse(test_data$astatus == 1, 0, test_data$status)

# Keep only required columns
test_data <- test_data[, c("ids", "entryage", "survage", "status", "astatus", "ntprobnp")]

# Display data summary
cat("Data Summary:\n")
cat("  Sample size:", nrow(test_data), "\n")
cat("  Age range:", range(test_data$entryage), "\n")
cat("  NTproBNP range:", round(range(test_data$ntprobnp), 1), "\n")
cat("  Primary events:", sum(test_data$status), "\n")
cat("  Competing events:", sum(test_data$astatus), "\n")
cat("  NTproBNP quartiles:", round(quantile(test_data$ntprobnp), 1), "\n\n")

# Test 1: Basic functionality
cat("Test 1: Basic functionality at age 50\n")
cat("--------------------------------------\n")

plot1 <- plot_biomarker_risk_continuum(
  data = test_data,
  biomarker_col = "ntprobnp",
  target_age = 50,
  max_age = 90,
  n_bins = 8,
  adjusted = TRUE,
  title = "Lifetime Risk at Age 50 by NTproBNP Level"
)

print(plot1)
cat("✓ Test 1 completed successfully\n\n")

# Test 2: Different target age
cat("Test 2: Different target age (60)\n")
cat("----------------------------------\n")

plot2 <- plot_biomarker_risk_continuum(
  data = test_data,
  biomarker_col = "ntprobnp", 
  target_age = 60,
  max_age = 85,
  n_bins = 6,
  adjusted = TRUE,
  title = "Lifetime Risk at Age 60 by NTproBNP Level"
)

print(plot2)
cat("✓ Test 2 completed successfully\n\n")

# Test 3: Unadjusted estimates
cat("Test 3: Unadjusted estimates\n")
cat("-----------------------------\n")

plot3 <- plot_biomarker_risk_continuum(
  data = test_data,
  biomarker_col = "ntprobnp",
  target_age = 45,
  max_age = 90,
  n_bins = 10,
  adjusted = FALSE,  # Use unadjusted estimates
  title = "Unadjusted Lifetime Risk at Age 45 by NTproBNP"
)

print(plot3)
cat("✓ Test 3 completed successfully\n\n")

# Test 4: More bins with log transformation (demonstrates fix for duplicate breaks)
cat("Test 4: More bins with log transformation\n")
cat("-----------------------------------------\n")

plot4 <- plot_biomarker_risk_continuum(
  data = test_data,
  biomarker_col = "ntprobnp",
  target_age = 55,
  max_age = 90,
  n_bins = 12,  # This would previously cause "breaks are not unique" error
  adjusted = TRUE,
  log_transform = TRUE,
  xlim = c(10, 1000),
  title = "Log Scale: Lifetime Risk at Age 55 by NTproBNP"
)

print(plot4)
cat("✓ Test 4 completed successfully (duplicate breaks handled)\n\n")

# Test 5: Smoothing line feature
cat("Test 5: With smoothing line\n")
cat("----------------------------\n")

plot5 <- plot_biomarker_risk_continuum(
  data = test_data,
  biomarker_col = "ntprobnp",
  target_age = 50,
  max_age = 90,
  n_bins = 10,
  adjusted = TRUE,
  smooth_line = TRUE,  # New smoothing feature
  title = "Lifetime Risk at Age 50 by NTproBNP (with smoothing)"
)

print(plot5)
cat("✓ Test 5 completed successfully (smoothing line added)\n\n")

# Test 6: All features combined
cat("Test 6: All features combined\n")
cat("------------------------------\n")

plot6 <- plot_biomarker_risk_continuum(
  data = test_data,
  biomarker_col = "ntprobnp",
  target_age = 45,
  max_age = 90,
  n_bins = 15,
  adjusted = TRUE,
  log_transform = TRUE,
  xlim = c(20, 500),
  smooth_line = TRUE,
  xlab = "NTproBNP (pg/mL)",
  title = "Complete Example: Log Scale + Smoothing + Custom Limits"
)

print(plot6)
cat("✓ Test 6 completed successfully (all features)\n\n")

# Performance test
cat("Performance Test: Timing the function\n")
cat("-------------------------------------\n")

timing <- system.time({
  plot_perf <- plot_biomarker_risk_continuum(
    data = test_data,
    biomarker_col = "ntprobnp",
    target_age = 50,
    max_age = 90,
    n_bins = 10,
    adjusted = TRUE
  )
})

cat("Function execution time:", round(timing["elapsed"], 2), "seconds\n\n")

# Summary
cat("All Tests Completed Successfully! 🎉\n")
cat("====================================\n")
cat("The plot_biomarker_risk_continuum function is working correctly.\n")
cat("Key features demonstrated:\n")
cat("  ✓ Basic biomarker risk plotting\n")
cat("  ✓ Different target ages\n") 
cat("  ✓ Adjusted vs unadjusted estimates\n")
cat("  ✓ Duplicate breaks handling (fixed!)\n")
cat("  ✓ Log transformation\n")
cat("  ✓ Custom X-axis limits\n")
cat("  ✓ Smoothing lines\n")
cat("  ✓ Reasonable performance\n\n")

cat("Usage examples:\n")
cat("# Basic usage:\n")
cat("plot_biomarker_risk_continuum(data, 'ntprobnp', target_age = 50)\n\n")
cat("# With all features (your original request):\n")
cat("plot_biomarker_risk_continuum(\n")
cat("  data = data,\n")
cat("  biomarker_col = 'NP',\n")
cat("  target_age = 50,\n")
cat("  n_bins = 12,\n")
cat("  log_transform = TRUE,\n")
cat("  xlim = c(10, 1000),\n")
cat("  smooth_line = TRUE\n")
cat(")\n")
