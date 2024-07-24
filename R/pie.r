# pie.r

#' Calculate and Plot Lifetime Risk
#'
#' This function calculates and plots incidence rates for two groups based on the provided data.
#'
#' @param data A data frame containing the input data. It should have the following columns:
#'   \itemize{
#'     \item ids: Unique identifier for each individual
#'     \item entryage: Age at which the individual entered the study
#'     \item survage: Age at which the individual was last observed (due to event occurrence or end of follow-up)
#'     \item status: Indicator for the primary event of interest (1 if occurred, 0 if not)
#'     \item astatus: Indicator for a competing event (1 if occurred, 0 if not)
#'     \item group: Categorical variable indicating which group the individual belongs to
#'   }
#' @param min_age Minimum age to consider in the analysis
#' @param max_age Maximum age to consider in the analysis
#' @param age_group_width Width of age groups for aggregating data
#' @param group Name of the column in 'data' that contains the group information
#' @param level1 First level of the group variable to analyze
#' @param level2 Second level of the group variable to analyze
#' @param age_free Minimum age to consider for lifetime risk calculations
#' @param o1 Output file name for the plot of the first group (level1). If NULL, plot is not saved.
#' @param o2 Output file name for the plot of the second group (level2). If NULL, plot is not saved.
#' @param study_label Label to use in plot titles. Default is "Lifetime Risk Study"
#' @param min_age_group Minimum age group to consider in the analysis. Default is the same as min_age
#' @param verbose Logical, if TRUE, print detailed output. Default is FALSE.
#'
#' @return A list of class "pie_lifetime_risk" containing:
#'   \itemize{
#'     \item incidence_data: List of incidence rate data for both groups and combined
#'       \itemize{
#'         \item i: Combined incidence data
#'         \item i1: Incidence data for level1
#'         \item i2: Incidence data for level2
#'       }
#'     \item aa2g_result: Age-adjusted incidence rates
#'     \item lr_data: List of lifetime risk data for both groups
#'       \itemize{
#'         \item lr1: Lifetime risk data for level1
#'         \item lr2: Lifetime risk data for level2
#'       }
#'     \item plots: List of ggplot objects for both groups
#'       \itemize{
#'         \item plot1: Plot for level1
#'         \item plot2: Plot for level2
#'       }
#'     \item parameters: List of input parameters used in the analysis
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' result <- pie(
#'   data = sample_data,
#'   min_age = 30,
#'   max_age = 90,
#'   age_group_width = 5,
#'   group = "group",
#'   level1 = "A",
#'   level2 = "B",
#'   age_free = 30
#' )
#'
#' # Print summary of results
#' summary(result)
#'
#' # Access specific components of the result
#' result$lr_data$lr1  # Lifetime risk data for level1
#' result$plots$plot1  # ggplot object for level1
#'
#' # Save plots with custom names
#' result <- pie(
#'   data = sample_data,
#'   min_age = 30,
#'   max_age = 90,
#'   age_group_width = 5,
#'   group = "group",
#'   level1 = "A",
#'   level2 = "B",
#'   age_free = 30,
#'   o1 = "plot_group_A.png",
#'   o2 = "plot_group_B.png"
#' )
#'
#' # Run with verbose output and custom study label
#' result <- pie(
#'   data = sample_data,
#'   min_age = 30,
#'   max_age = 90,
#'   age_group_width = 5,
#'   group = "group",
#'   level1 = "A",
#'   level2 = "B",
#'   age_free = 30,
#'   study_label = "My Custom Study",
#'   verbose = TRUE
#' )
#'
#' # Print lifetime risk data for level1
#' print_lr_data(result, group = "level1")
#'
#' # Export adjusted lifetime risk data for level2 to CSV
#' print_lr_data(result, group = "level2", adjusted = TRUE, output = "risk_data_B.csv")
#' }
#'
#' @importFrom dplyr filter mutate group_by summarize
#' @importFrom ggplot2 ggplot aes geom_line geom_point theme_minimal labs
#' @importFrom readr write_csv
pie <- function(data, min_age, max_age, age_group_width, group, level1, level2,
                age_free, o1 = NULL, o2 = NULL,
                study_label = "Lifetime Risk Study", min_age_group = min_age,
                verbose = FALSE) {

  # Preprocess data
  pds <- preprocess_data(data, max_age, min_age, group)

  # Create summary datasets (equivalent to %SDSMAC in SAS)
  sds <- create_summary_dataset(pds, group, c(level1, level2))
  sds1 <- create_summary_dataset(pds, group, level1)
  sds2 <- create_summary_dataset(pds, group, level2)

  # Calculate incidence rates (equivalent to %INCID in SAS)
  i <- calculate_incidence(sds, min_age, max_age, age_group_width, min_age_group)
  i1 <- calculate_incidence(sds1, min_age, max_age, age_group_width, min_age_group)
  i2 <- calculate_incidence(sds2, min_age, max_age, age_group_width, min_age_group)

  # Perform AA2G analysis
  aa2g_result <- calculate_aa2g(i1, i2, level1, level2)

  # Calculate cumulative incidence (equivalent to %LR in SAS)
  lr1 <- calculate_lr(sds1, age_free, max_age, age_group_width, verbose)
  lr2 <- calculate_lr(sds2, age_free, max_age, age_group_width, verbose)

  # Print summary results
  #if (verbose) {
  #  cat("\n=== Results for", level1, "===\n")
  #  summarize_lifetime_risk(lr1)
  #  cat("\n=== Results for", level2, "===\n")
  #  summarize_lifetime_risk(lr2)
  #}

  # Generate plots
  plot1 <- generate_lr_plot(lr1, title = paste(study_label, group, "=", level1))
  plot2 <- generate_lr_plot(lr2, title = paste(study_label, group, "=", level2))

  # Save plots if output filenames are provided
  if (!is.null(o1)) {
    #ggsave(o1, plot1)
    if (verbose) cat("Plot for", level1, "saved as", o1, "\n")
  }
  if (!is.null(o2)) {
    #ggsave(o2, plot2)
    if (verbose) cat("Plot for", level2, "saved as", o2, "\n")
  }

  # Create the result object
  result <- list(
    incidence_data = list(i = i, i1 = i1, i2 = i2),
    aa2g_result = aa2g_result,
    lr_data = list(lr1 = lr1, lr2 = lr2),
    plots = list(plot1 = plot1, plot2 = plot2),
    parameters = list(
      level1 = level1,
      level2 = level2,
      group = group,
      study_label = study_label
    )
  )

  # Assign a custom class to the result
  class(result) <- "pie_lifetime_risk"

  return(result)
}

#' Print method for pie_lifetime_risk objects
#'
#' @param x A pie_lifetime_risk object
#' @param ... Additional arguments (not used)
#'
#' @export
print.pie_lifetime_risk <- function(x, ...) {
  cat("Lifetime Risk Analysis Results\n")
  cat("==============================\n")
  cat("Study:", x$parameters$study_label, "\n")
  cat("Group variable:", x$parameters$group, "\n")
  cat("Levels compared:", x$parameters$level1, "vs", x$parameters$level2, "\n\n")
  cat("Use summary() for detailed lifetime risk estimates.\n")
  cat("Access full results and plots through the returned object.\n")
}

#' Summary method for pie_lifetime_risk objects
#'
#' @param object A pie_lifetime_risk object
#' @param ... Additional arguments (not used)
#'
#' @export
summary.pie_lifetime_risk <- function(object, ...) {
  cat("Summary of Lifetime Risk Analysis\n")
  cat("==================================\n")
  cat("Study:", object$parameters$study_label, "\n")
  cat("Group variable:", object$parameters$group, "\n")
  cat("Age range:", min(object$lr_data$lr1$age), "to", max(object$lr_data$lr1$age), "years\n\n")

  cat("Results for", object$parameters$level1, "\n")
  cat("----------------------------\n")
  cat(summarize_lifetime_risk(object$lr_data$lr1))

  cat("\n\nResults for", object$parameters$level2, "\n")
  cat("----------------------------\n")
  cat(summarize_lifetime_risk(object$lr_data$lr2))

  cat("\n\nNote: Adjusted risk accounts for competing risks (e.g., death from other causes).\n")
}
