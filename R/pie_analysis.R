#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line labs theme_minimal theme
#' @importFrom ggplot2 element_text element_blank scale_color_manual scale_fill_manual
#' @importFrom ggplot2 geom_point geom_errorbar
#' @importFrom tidyr gather separate
#' @importFrom dplyr %>%
NULL

#' Main Analysis Function
#'
#' Performs person-year and lifetime risk analysis, including age-specific rates and cumulative incidence, for the overall population or by group.
#'
#' @param data A data frame containing the input data. Must include: ids, entryage, survage, status, astatus, and optionally grouping variables.
#' @param min_age Minimum age to consider
#' @param max_age Maximum age to consider
#' @param age_group_width Width of age groups (e.g., 5 for 5-year intervals)
#' @param group_var Name of grouping variable (string, optional, NULL for overall analysis)
#' @param group_levels Vector of group levels to analyze (NULL to auto-detect all levels)
#' @param age_free Starting age for survival analysis
#' @param debug If TRUE, save intermediate datasets to global environment
#' @return An S3 object of class 'pie_analysis' containing:
#'   - age_specific_rates: list of data.tables by group
#'   - cumulative_incidence: list of data.frames by group
#'   - summary_datasets: list of summary tables by group
#'   - parameters: list of analysis parameters
#' @details
#' This function implements the full PIE macro workflow, including person-year expansion, age-specific rates, and cumulative incidence with and without competing risk adjustment. Handles any number of groups.
#' @examples
#' result <- pie_analysis(test_data, min_age = 50, max_age = 80, age_group_width = 5, group_var = "group", age_free = 50)
#' summary(result)
#' @export
pie_analysis <- function(data, min_age, max_age, age_group_width,
                         group_var = NULL, group_levels = NULL, age_free,
                         output1 = NULL, output2 = NULL, debug = FALSE) {

  # Handle grouping BEFORE creating person-year data
  if (is.null(group_var)) {
    # Overall analysis - everyone same group
    data$group <- "overall"
    group_levels <- "overall"
  } else {
    # Copy the grouping variable to 'group' column
    data$group <- data[[group_var]]

    if (is.null(group_levels)) {
      group_levels <- sort(unique(data[[group_var]]))
    }
  }

  # Now create_person_year_data always finds the 'group' column it expects
  py_data <- create_person_year_data(data, min_age, max_age, debug)

  results <- list(
    age_specific_rates = list(),
    cumulative_incidence = list(),
    summary_datasets = list(),
    parameters = list(
      min_age = min_age,
      max_age = max_age,
      age_group_width = age_group_width,
      group_var = group_var,  # Keep original parameter
      group_levels = group_levels,
      age_free = age_free
    )
  )

  for (level in group_levels) {
    data_group <- py_data[py_data$group == level, ]  # Always use 'group' column

    rates_group <- calculate_age_specific_rates(data_group, age_group_width, min_age)
    ci_group <- calculate_cumulative_incidence(data_group, age_free, debug)

    results$age_specific_rates[[as.character(level)]] <- rates_group
    results$cumulative_incidence[[as.character(level)]] <- ci_group
  }

  class(results) <- "pie_analysis"
  return(results)
}

#' Create Lifetime Risk Table from Different Starting Ages
#'
#' Summarizes lifetime risk estimates for multiple starting ages and groups.
#'
#' @param data Input dataset
#' @param index_ages Vector of starting ages
#' @param max_age Maximum age to consider
#' @param group_var Group variable name (optional)
#' @param group_levels Vector of group levels to include (optional)
#' @param adjusted Logical, whether to use competing risk adjusted estimates
#' @param age_group_width Width of age groups (default = 5)
#' @return A data.frame with lifetime risk estimates for each group and starting age
#' @details
#' For each group and starting age, runs the full analysis and extracts the final cumulative incidence (estimate and 95% CI).
#' @examples
#' create_lifetime_risk_table(test_data, index_ages = c(50, 60), max_age = 80, group_var = "group")
#' @export
create_lifetime_risk_table <- function(data, index_ages, max_age,
                                       group_var = NULL, group_levels = NULL,
                                       adjusted = FALSE, age_group_width = 5) {

  results <- data.frame(
    starting_age = paste0(index_ages, " (until age ", max_age, ")")
  )

  # Handle grouping
  if (is.null(group_var)) {
    group_levels <- "overall"
  } else if (is.null(group_levels)) {
    group_levels <- sort(unique(data[[group_var]]))
  }

  # Calculate for each group
  for (level in group_levels) {
    risks <- sapply(index_ages, function(start_age) {
      tryCatch({
        result <- pie_analysis(
          data = data,
          min_age = start_age,
          max_age = max_age,
          age_group_width = age_group_width,
          group_var = group_var,
          group_levels = level,
          age_free = start_age
        )

        # Get final risk
        if (adjusted) {
          final_data <- result$cumulative_incidence[[as.character(level)]]$adjusted
        } else {
          final_data <- result$cumulative_incidence[[as.character(level)]]$unadjusted
        }

        final_row <- tail(final_data, 1)
        c(final_row$est, final_row$lcl, final_row$ucl)
      }, error = function(e) {
        rep(NA, 3)
      })
    })

    col_name <- if(is.null(group_var)) "Overall" else as.character(level)
    results[[col_name]] <- sprintf("%.1f (%.1f, %.1f)",
                                   risks[1,], risks[2,], risks[3,])
  }

  names(results)[1] <- "Starting Age"
  return(results)
}

#' Print method for pie_analysis objects
#'
#' Prints a summary of analysis parameters. Use summary() for detailed results.
#' @param x A pie_analysis object
#' @param ... Additional arguments (not used)
#' @export
print.pie_analysis <- function(x, ...) {
  cat("Person-Year and Lifetime Risk Analysis\n")
  cat("=====================================\n")
  cat("Analysis parameters:\n")
  cat("  Age range:", x$parameters$min_age, "to", x$parameters$max_age, "years\n")
  cat("  Age group width:", x$parameters$age_group_width, "years\n")
  cat("  Starting age for lifetime risk:", x$parameters$age_free, "years\n")
  cat("\nUse summary() for detailed results\n")
}

#' Summary method for pie_analysis objects
#'
#' Prints a detailed summary of results for each group, including final cumulative incidence and confidence intervals.
#' @param object A pie_analysis object
#' @param ... Additional arguments (not used)
#' @export
summary.pie_analysis <- function(object, ...) {
  cat("Summary of Person-Year and Lifetime Risk Analysis\n")
  cat("===============================================\n\n")

  # Print parameters
  cat("Analysis Parameters:\n")
  cat("-----------------------\n")
  cat("Age range:", object$parameters$min_age, "to", object$parameters$max_age, "years\n")
  cat("Age group width:", object$parameters$age_group_width, "years\n")
  cat("Starting age for lifetime risk:", object$parameters$age_free, "years\n\n")

  # Loop over all groups
  for (group in names(object$cumulative_incidence)) {
    cat("Results for Group", group, ":\n")
    cat("-----------------------\n")
    group_data <- object$cumulative_incidence[[group]]$unadjusted
    if (!is.null(group_data) && nrow(group_data) > 0) {
      final_risk <- tail(group_data, 1)
      cat("Final cumulative incidence:",
          sprintf("%.1f%% (%.1f%%, %.1f%%)\n",
                  final_risk$est, final_risk$lcl, final_risk$ucl))
    } else {
      cat("No data available for this group.\n")
    }
    cat("\n")
  }
  cat("Note: Values shown as estimate (95% CI)\n")
}
