#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line labs theme_minimal theme
#' @importFrom ggplot2 element_text element_blank scale_color_manual scale_fill_manual
#' @importFrom ggplot2 geom_point geom_errorbar scale_x_log10 xlim coord_cartesian geom_smooth
#' @importFrom tidyr gather separate
#' @importFrom dplyr %>%
NULL


#' Plot Lifetime Risk Results
#'
#' Plots cumulative incidence curves (adjusted or unadjusted) for two groups from a pie_analysis result.
#'
#' @param result Output from pie_analysis function
#' @param adjusted Logical; use adjusted (TRUE) or unadjusted (FALSE) estimates (default: TRUE)
#' @param label_names Named vector for group labels, e.g. c("0"="Control", "1"="Treatment")
#' @param title Plot title (optional)
#' @param theme A custom ggplot theme (optional)
#' @param colors Vector of colors for groups (optional)
#' @return A ggplot object
#' @details
#' Plots cumulative incidence with confidence intervals for two groups. Use label_names to customize legend labels.
#' @examples
#' result <- pie_analysis(test_data, 50, 80, 5, group_var = "group", age_free = 50)
#' plot_lifetime_risk(result, adjusted = TRUE)
#' @export
plot_lifetime_risk <- function(result,
                               adjusted = TRUE,
                               label_names = NULL,
                               title = "Cumulative Incidence by Group",
                               theme = NULL,
                               colors = NULL) {
  # Dynamically get group names
  group_names <- names(result$cumulative_incidence)
  plot_data_list <- list()
  for (group in group_names) {
    data <- if (adjusted) result$cumulative_incidence[[group]]$adjusted
            else result$cumulative_incidence[[group]]$unadjusted
    # Skip if data is missing or malformed
    if (is.null(data) || !all(c("age", "est", "lcl", "ucl") %in% names(data))) next
    plot_data_list[[group]] <- data.frame(
      age = data$age,
      estimate = data$est,
      lcl = data$lcl,
      ucl = data$ucl,
      group = group
    )
  }
  if (length(plot_data_list) == 0) stop("No valid group data to plot.")
  plot_data <- do.call(rbind, plot_data_list)
  plot_data$group <- as.factor(plot_data$group)
  if (!is.null(label_names)) {
    levels(plot_data$group) <- label_names[levels(plot_data$group)]
  }
  p <- ggplot(plot_data, aes(x = age, y = estimate, color = group, fill = group)) +
    geom_ribbon(aes(ymin = lcl, ymax = ucl, color = NULL), alpha = 0.2) +
    geom_line(linewidth = 1) +
    labs(
      title = title,
      x = "Age (years)",
      y = if (adjusted) "Adjusted Cumulative Incidence (%)" else "Cumulative Incidence (%)",
      color = "Group",
      fill = "Group"
    ) +
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
  if (!is.null(colors)) {
    p <- p + scale_color_manual(values = colors) +
      scale_fill_manual(values = colors)
  }
  if (!is.null(theme)) {
    p <- p + theme
  }
  return(p)
}


#' Create CSV File of Lifetime Risk Estimates
#'
#' Exports lifetime risk estimates for one or more analysis objects to a CSV file.
#'
#' @param ... One or more analysis objects (from pie_analysis)
#' @param adjusted Logical indicating whether to use adjusted estimates (TRUE) or unadjusted (FALSE). Default is TRUE.
#' @param output_file Path and name of the output CSV file. Default is "analysis_output.csv".
#' @return Invisibly returns a data frame containing the combined estimates.
#' @details
#' Each analysis object contributes two columns (one per group). For multiple objects, columns are suffixed.
#' @examples
#' result <- pie_analysis(test_data, 50, 80, 5, group_var = "group", age_free = 50)
#' create_lifetime_csv(result, adjusted = TRUE, output_file = tempfile())
#' @export
create_lifetime_csv <- function(..., adjusted = TRUE, output_file = "analysis_output.csv") {
  objects <- list(...)
  # Accept full pie_analysis object and extract cumulative_incidence if present
  objects <- lapply(objects, function(obj) {
    if (inherits(obj, "pie_analysis")) {
      obj$cumulative_incidence
    } else {
      obj
    }
  })
  if (length(objects) == 0) {
    stop("At least one analysis object must be provided")
  }
  # Validate each object is correct structure
  for (i in seq_along(objects)) {
    if (!is.list(objects[[i]]) || length(objects[[i]]) < 1) {
      stop(sprintf("Object %d is not a valid analysis object (must be a list with group cumulative incidence)", i))
    }
  }
  # Build a list of data.frames to merge by age
  merge_list <- list()
  for (i in seq_along(objects)) {
    obj <- objects[[i]]
    for (group in names(obj)) {
      group_data <- if (adjusted) obj[[group]]$adjusted else obj[[group]]$unadjusted
      suffix <- if (length(objects) > 1) paste0("_obj", i) else ""
      colname <- paste0(group, suffix)
      df <- data.frame(age = group_data$age, est = group_data$est)
      names(df)[2] <- colname
      merge_list[[length(merge_list) + 1]] <- df
    }
  }
  # Merge all by age (full join)
  result <- Reduce(function(x, y) merge(x, y, by = "age", all = TRUE), merge_list)
  write.csv(result, file = output_file, row.names = FALSE)
  invisible(result)
}


#' Extract Number at Risk from Lifetable Analysis
#'
#' Extracts the number at risk at specified ages for each group from a pie_analysis result.
#'
#' @param analysis_obj An object returned by pie_analysis
#' @param group1_label Label for group1 in the output
#' @param group2_label Label for group2 in the output
#' @param age_points Vector of age points at which to display counts
#' @return A dataframe with number at risk by group and age
#' @details
#' Requires that pie_analysis was run with debug=TRUE and that summary datasets (SDS1, SDS2) are available in the global environment.
#' @examples
#' # Only works if SDS1 and SDS2 exist in global environment
#' @export
get_number_at_risk_from_analysis <- function(analysis_obj,
                                             group1_label = "Group 1",
                                             group2_label = "Group 2",
                                             age_points = c(50, 60, 70, 80, 90)) {

  # Initialize result dataframe
  result <- data.frame(Age = age_points)

  # Extract summary data (SDS objects) from the analysis
  # If we have access to the internal summary datasets:
  if (exists("SDS1", envir = .GlobalEnv) && exists("SDS2", envir = .GlobalEnv)) {
    sds1 <- get("SDS1", envir = .GlobalEnv)
    sds2 <- get("SDS2", envir = .GlobalEnv)

    # Calculate counts at each age point for group 1
    counts1 <- sapply(age_points, function(age_point) {
      # Find the closest age row in the summary data
      row <- sds1[which.min(abs(sds1$age - age_point)), ]
      # Return the number at risk (R column)
      return(row$R)
    })

    # Calculate counts for group 2
    counts2 <- sapply(age_points, function(age_point) {
      row <- sds2[which.min(abs(sds2$age - age_point)), ]
      return(row$R)
    })

    # Add to result dataframe
    result[[group1_label]] <- counts1
    result[[group2_label]] <- counts2
  } else {
    warning("SDS1 and SDS2 objects not found. Make sure to run pie_analysis with debug=TRUE.")
  }

  return(result)
}
