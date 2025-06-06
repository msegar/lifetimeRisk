#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line labs theme_minimal theme
#' @importFrom ggplot2 element_text element_blank scale_color_manual scale_fill_manual
#' @importFrom ggplot2 geom_point geom_errorbar
#' @importFrom tidyr gather separate
#' @importFrom dplyr %>%
NULL

#' Main Analysis Function
#'
#' @param data A data frame containing the input data. It should have the following columns:
#'   \itemize{
#'     \item ids: Unique identifier for each individual
#'     \item entryage: Age at which the individual entered the study
#'     \item survage: Age at which the individual was last observed (due to event occurrence or end of follow-up)
#'     \item status: Indicator for the primary event of interest (1 if occurred, 0 if not)
#'     \item astatus: Indicator for a competing event (1 if occurred, 0 if not)
#'   }
#' @param min_age Minimum age to consider
#' @param max_age Maximum age to consider
#' @param age_group_width Width of age groups
#' @param group_var Name of grouping variable (optional, NULL for overall analysis)
#' @param group_levels Vector of group levels (replaces group1/group2)
#' @param age_free Starting age for survival analysis
#' @param debug If TRUE, save intermediate datasets
#' @return A list containing all analysis results
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
#' @param data Input dataset
#' @param index_ages Vector of starting ages
#' @param max_age Maximum age to consider
#' @param age_group_width Width of age groups (default = 5)
#' @param group_var Group variable name
#' @param levels Vector of group levels to include
#' @param adjusted Logical, whether to use competing risk adjusted estimates
#' @return A data frame containing lifetime risks from different starting ages
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

  # Print group 1 results
  cat("Results for Group", object$parameters$group1, ":\n")
  cat("-----------------------\n")
  final_risk1 <- tail(object$cumulative_incidence$group1$unadjusted, 1)
  cat("Final cumulative incidence:",
      sprintf("%.1f%% (%.1f%%, %.1f%%)\n",
              final_risk1$est, final_risk1$lcl, final_risk1$ucl))

  # Print group 2 results
  cat("\nResults for Group", object$parameters$group2, ":\n")
  cat("-----------------------\n")
  final_risk2 <- tail(object$cumulative_incidence$group2$unadjusted, 1)
  cat("Final cumulative incidence:",
      sprintf("%.1f%% (%.1f%%, %.1f%%)\n",
              final_risk2$est, final_risk2$lcl, final_risk2$ucl))

  cat("\nNote: Values shown as estimate (95% CI)\n")
}
