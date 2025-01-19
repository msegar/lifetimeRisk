#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line labs theme_minimal theme
#' @importFrom ggplot2 element_text element_blank scale_color_manual scale_fill_manual
#' @importFrom ggplot2 geom_point geom_errorbar
#' @importFrom tidyr gather separate
#' @importFrom dplyr %>%
NULL


#' Plot Lifetime Risk Results
#'
#' @param result Output from pie_analysis function
#' @param adjusted Logical; use adjusted (TRUE) or unadjusted (FALSE) estimates (default: TRUE)
#' @param label_names Named vector for group labels, e.g. c("0"="Control", "1"="Treatment")
#' @param title Plot title (optional)
#' @param theme A custom ggplot theme (optional)
#' @param colors Vector of colors for groups (optional)
#' @return A ggplot object
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line labs theme_minimal theme
#' @importFrom ggplot2 element_text element_blank scale_color_manual scale_fill_manual
#' @export
plot_lifetime_risk <- function(result,
                               adjusted = TRUE,
                               label_names = NULL,
                               title = "Cumulative Incidence by Group",
                               theme = NULL,
                               colors = NULL) {

  # Extract data from result object
  data1 <- if(adjusted) result$cumulative_incidence$group1$adjusted
  else result$cumulative_incidence$group1$unadjusted
  data2 <- if(adjusted) result$cumulative_incidence$group2$adjusted
  else result$cumulative_incidence$group2$unadjusted

  # Create combined dataset for plotting
  plot_data <- rbind(
    data.frame(
      age = data1$age,
      estimate = data1$est,
      lcl = data1$lcl,
      ucl = data1$ucl,
      group = result$parameters$group1
    ),
    data.frame(
      age = data2$age,
      estimate = data2$est,
      lcl = data2$lcl,
      ucl = data2$ucl,
      group = result$parameters$group2
    )
  )

  # Convert group to factor and apply labels if provided
  plot_data$group <- as.factor(plot_data$group)
  if(!is.null(label_names)) {
    levels(plot_data$group) <- label_names[levels(plot_data$group)]
  }

  # Create base plot
  p <- ggplot(plot_data, aes(x = age, y = estimate, color = group, fill = group)) +
    # Add confidence intervals
    geom_ribbon(aes(ymin = lcl, ymax = ucl, color = NULL), alpha = 0.2) +
    # Add lines (using linewidth instead of size)
    geom_line(linewidth = 1) +
    # Labels
    labs(
      title = title,
      x = "Age (years)",
      y = if(adjusted) "Adjusted Cumulative Incidence (%)"
      else "Cumulative Incidence (%)",
      color = "Group",
      fill = "Group"
    ) +
    # Theme
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.position = "bottom",
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10),
      panel.grid.minor = element_blank()
    )

  # Apply custom colors if provided
  if(!is.null(colors)) {
    p <- p + scale_color_manual(values = colors) +
      scale_fill_manual(values = colors)
  }

  # Apply custom theme if provided
  if(!is.null(theme)) {
    p <- p + theme
  }

  return(p)
}


#' Create CSV File of Lifetime Risk Estimates
#'
#' @description
#' Creates a CSV file containing lifetime risk estimates from one or more analysis objects.
#' Each analysis object contributes two columns of estimates (one for each group), with
#' age-specific estimates arranged in rows.
#'
#' @param ... One or more analysis objects, each containing cumulative incidence estimates
#'   for two groups. Each object must be a list of length 3 with the required structure
#'   containing adjusted and unadjusted estimates.
#' @param adjusted Logical indicating whether to use adjusted estimates (TRUE) or
#'   unadjusted estimates (FALSE). Default is TRUE.
#' @param output_file Character string specifying the path and name of the output CSV file.
#'   Default is "analysis_output.csv".
#'
#' @details
#' The function processes each analysis object and extracts either the adjusted or
#' unadjusted lifetime risk estimates for both groups. For a single object, the output
#' will contain columns: age, group1, group2. For multiple objects, columns will be
#' named with object suffixes: age, group1_obj1, group2_obj1, group1_obj2, group2_obj2, etc.
#'
#' @return
#' Invisibly returns a data frame containing the combined estimates. The data frame
#' is also written to a CSV file at the specified location.
#'
#' @examples
#' \dontrun{
#' # Single analysis object
#' create_lifetime_csv(analysis1)
#'
#' # Multiple analysis objects
#' create_lifetime_csv(analysis1, analysis2,
#'                    output_file = "combined_lifetime.csv")
#'
#' # Using unadjusted estimates
#' create_lifetime_csv(analysis1, analysis2,
#'                    adjusted = FALSE,
#'                    output_file = "unadjusted_lifetime.csv")
#' }
#'
#' @export
create_lifetime_csv <- function(..., adjusted = TRUE, output_file = "analysis_output.csv") {
  # Get list of all objects passed to function
  objects <- list(...)

  if (length(objects) == 0) {
    stop("At least one analysis object must be provided")
  }

  # Validate each object is correct structure
  for (i in seq_along(objects)) {
    if (!is.list(objects[[i]]) || length(objects[[i]]) != 3) {
      stop(sprintf("Object %d is not a valid analysis object (must be list of length 3)", i))
    }
  }

  # Initialize result data frame with age column
  result <- data.frame(age = objects[[1]]$cumulative_incidence$group1$adjusted$age)

  # Process each object
  for (i in seq_along(objects)) {
    obj <- objects[[i]]

    # Get correct data based on adjusted parameter
    group1_data <- if(adjusted) obj$cumulative_incidence$group1$adjusted else obj$cumulative_incidence$group1$unadjusted
    group2_data <- if(adjusted) obj$cumulative_incidence$group2$adjusted else obj$cumulative_incidence$group2$unadjusted

    # Add columns with appropriate names
    suffix <- if(length(objects) > 1) paste0("_obj", i) else ""
    result[[paste0("group1", suffix)]] <- group1_data$est
    result[[paste0("group2", suffix)]] <- group2_data$est
  }

  # Write to CSV
  write.csv(result, file = output_file, row.names = FALSE)

  # Return result invisibly
  invisible(result)
}
