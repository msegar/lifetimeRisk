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
#' @param group_var Name of grouping variable
#' @param group1 First group level
#' @param group2 Second group level
#' @param age_free Starting age for survival analysis
#' @param output1 Name for first output dataset
#' @param output2 Name for second output dataset
#' @param debug If TRUE, save intermediate datasets
#' @return A list containing all analysis results
#' @export
pie_analysis <- function(data, min_age, max_age, age_group_width,
                         group_var, group1, group2, age_free,
                         output1 = NULL, output2 = NULL,
                         debug = FALSE) {

  # Create person-year dataset
  py_data <- create_person_year_data(data, min_age, max_age, debug)

  # Create summary datasets with explicit names
  sds <- create_summary_dataset(py_data, group_var, c(group1, group2), "SDS", debug)
  sds1 <- create_summary_dataset(py_data, group_var, group1, "SDS1", debug)
  sds2 <- create_summary_dataset(py_data, group_var, group2, "SDS2", debug)

  # Calculate rates by group
  data_group1 <- py_data[get(group_var) == group1]
  data_group2 <- py_data[get(group_var) == group2]

  rates_group1 <- calculate_age_specific_rates(data_group1, age_group_width, min_age)
  rates_group2 <- calculate_age_specific_rates(data_group2, age_group_width, min_age)

  # Calculate cumulative incidence
  ci_group1 <- calculate_cumulative_incidence(data_group1, age_free, debug)
  ci_group2 <- calculate_cumulative_incidence(data_group2, age_free, debug)

  # Save outputs if specified
  if (!is.null(output1)) assign(output1, ci_group1, envir = .GlobalEnv)
  if (!is.null(output2)) assign(output2, ci_group2, envir = .GlobalEnv)

  # Create result object
  result <- structure(
    list(
      age_specific_rates = list(
        group1 = rates_group1,
        group2 = rates_group2
      ),
      cumulative_incidence = list(
        group1 = ci_group1,
        group2 = ci_group2
      ),
      parameters = list(
        min_age = min_age,
        max_age = max_age,
        age_group_width = age_group_width,
        group_var = group_var,
        group1 = group1,
        group2 = group2,
        age_free = age_free
      )
    ),
    class = "pie_analysis"
  )

  return(result)
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
create_lifetime_risk_table <- function(data, index_ages, max_age, group_var, levels,
                                       adjusted = FALSE, age_group_width = 5) {
  # Input validation
  if (!is.data.frame(data)) stop("data must be a data frame")
  if (!all(c("ids", "entryage", "survage", "status", "astatus", group_var) %in% names(data))) {
    stop("Missing required columns in data")
  }

  # Initialize results
  results <- data.frame(
    starting_age = paste0(index_ages, " (until age ", max_age, ")")
  )

  message("Calculating lifetime risks for different starting ages...")

  # Calculate for each pair of levels
  for (i in 1:(length(levels)-1)) {
    for (j in (i+1):length(levels)) {
      message(sprintf("Processing groups %s vs %s...", levels[i], levels[j]))

      # Calculate risks for each starting age
      risks <- sapply(index_ages, function(start_age) {
        tryCatch({
          # Run pie_analysis with current starting age
          result <- pie_analysis(
            data = data,
            min_age = start_age,
            max_age = max_age,
            age_group_width = age_group_width,
            group_var = group_var,
            group1 = levels[i],
            group2 = levels[j],
            age_free = start_age
          )

          # Get the final risks (last row) for both groups
          if (adjusted) {
            g1 <- result$cumulative_incidence$group1$adjusted
            g2 <- result$cumulative_incidence$group2$adjusted
          } else {
            g1 <- result$cumulative_incidence$group1$unadjusted
            g2 <- result$cumulative_incidence$group2$unadjusted
          }

          # Extract last row values
          g1_last <- tail(g1, 1)
          g2_last <- tail(g2, 1)

          c(g1_last$est, g1_last$lcl, g1_last$ucl,
            g2_last$est, g2_last$lcl, g2_last$ucl)
        }, error = function(e) {
          warning(sprintf("Error calculating risks for age %d: %s",
                          start_age, conditionMessage(e)))
          rep(NA, 6)
        })
      })

      # Add to results if we have valid calculations
      if (!all(is.na(risks))) {
        results[[as.character(levels[i])]] <- sprintf("%.1f (%.1f, %.1f)",
                                                      risks[1,], risks[2,], risks[3,])
        results[[as.character(levels[j])]] <- sprintf("%.1f (%.1f, %.1f)",
                                                      risks[4,], risks[5,], risks[6,])
      }
    }
  }

  # Add column names
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
