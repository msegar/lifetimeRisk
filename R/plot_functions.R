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


#' Plot Biomarker Risk Continuum
#'
#' Creates a plot showing lifetime risk across the continuum of a biomarker at a specific age.
#'
#' @param data Input dataset with required columns: ids, entryage, survage, status, astatus
#' @param biomarker_col Name of the biomarker column
#' @param target_age Age at which to estimate lifetime risk (e.g., 45)
#' @param max_age Maximum age for follow-up (default: 90)
#' @param n_bins Number of biomarker bins to create (default: 10)
#' @param adjusted Use competing risk adjusted estimates (default: TRUE)
#' @param title Plot title (optional)
#' @param xlim Numeric vector of length 2 specifying X-axis limits (optional)
#' @param log_transform Logical; apply log10 transformation to X-axis (default: FALSE)
#' @param xlab X-axis label (optional, defaults to biomarker column name)
#' @param smooth_line Logical; use smoothed line and confidence interval instead of connecting points (default: FALSE)
#' @param show_points Logical; show individual data points (default: TRUE)
#' @return A ggplot object showing biomarker values vs lifetime risk
#' @details
#' This function bins the biomarker into quantiles, runs PIE analysis on each bin,
#' and plots the resulting lifetime risk estimates. X-axis can be customized with limits
#' and log transformation for better visualization of biomarkers with wide ranges.
#' When smooth_line=TRUE, replaces the point-to-point line with a LOESS smooth and 
#' replaces the PIE confidence intervals with smooth-based confidence intervals.
#' @examples
#' # Add biomarker to test data
#' test_data$biomarker <- rnorm(nrow(test_data), 100, 20)
#' plot_biomarker_risk_continuum(test_data, "biomarker", target_age = 50)
#' 
#' # With log transformation and smoothing (good for biomarkers like NTproBNP)
#' plot_biomarker_risk_continuum(test_data, "ntprobnp", target_age = 50, 
#'                               log_transform = TRUE, xlim = c(10, 1000), smooth_line = TRUE)
#' 
#' # Clean smooth line without points
#' plot_biomarker_risk_continuum(test_data, "ntprobnp", target_age = 50,
#'                               smooth_line = TRUE, show_points = FALSE)
#' @export
plot_biomarker_risk_continuum <- function(data, biomarker_col, target_age, 
                                         max_age = 90, n_bins = 10, 
                                         adjusted = TRUE, title = NULL,
                                         xlim = NULL, log_transform = FALSE,
                                         xlab = NULL, smooth_line = FALSE,
                                         show_points = TRUE) {
  
  # Validate inputs
  if (!biomarker_col %in% names(data)) {
    stop("Biomarker column '", biomarker_col, "' not found in data")
  }
  
  required_cols <- c("ids", "entryage", "survage", "status", "astatus")
  if (!all(required_cols %in% names(data))) {
    stop("Data must contain columns: ", paste(required_cols, collapse = ", "))
  }
  
  # Remove missing biomarker values
  data <- data[!is.na(data[[biomarker_col]]), ]
  
  if (nrow(data) == 0) {
    stop("No valid data after removing missing biomarker values")
  }
  
  # Create biomarker bins using quantiles
  biomarker_values <- data[[biomarker_col]]
  
  # Handle duplicate values in quantiles
  unique_vals <- length(unique(biomarker_values))
  if (unique_vals < n_bins) {
    warning("Only ", unique_vals, " unique biomarker values. Reducing bins to ", unique_vals - 1)
    n_bins <- unique_vals - 1
  }
  
  # Create unique breaks
  bin_breaks <- quantile(biomarker_values, probs = seq(0, 1, length.out = n_bins + 1))
  
  # Ensure breaks are unique by adding small increments if needed
  if (any(duplicated(bin_breaks))) {
    # Use unique breaks and adjust if necessary
    bin_breaks <- unique(bin_breaks)
    if (length(bin_breaks) < 3) {
      stop("Biomarker has too few unique values for binning. Try reducing n_bins or using a different biomarker.")
    }
    n_bins <- length(bin_breaks) - 1
    warning("Adjusted to ", n_bins, " bins due to tied biomarker values")
  }
  
  # Create bin labels and midpoints
  data$biomarker_bin <- cut(biomarker_values, breaks = bin_breaks, include.lowest = TRUE)
  bin_midpoints <- (bin_breaks[-1] + bin_breaks[-length(bin_breaks)]) / 2
  
  # Initialize results storage
  risk_results <- data.frame(
    biomarker_midpoint = numeric(0),
    risk_estimate = numeric(0),
    risk_lcl = numeric(0),
    risk_ucl = numeric(0)
  )
  
  # Run PIE analysis for each bin
  for (i in seq_len(n_bins)) {
    bin_level <- levels(data$biomarker_bin)[i]
    bin_data <- data[data$biomarker_bin == bin_level, ]
    
    if (nrow(bin_data) < 5) next  # Skip bins with too few observations
    
    tryCatch({
      # Run PIE analysis for this bin
      result <- pie_analysis(
        data = bin_data,
        min_age = target_age,
        max_age = max_age,
        age_group_width = 5,
        group_var = NULL,
        age_free = target_age
      )
      
      # Extract final lifetime risk
      ci_data <- if (adjusted) {
        result$cumulative_incidence[["overall"]]$adjusted
      } else {
        result$cumulative_incidence[["overall"]]$unadjusted
      }
      
      if (!is.null(ci_data) && nrow(ci_data) > 0) {
        final_risk <- tail(ci_data, 1)
        
        # Store results
        risk_results <- rbind(risk_results, data.frame(
          biomarker_midpoint = bin_midpoints[i],
          risk_estimate = final_risk$est,
          risk_lcl = final_risk$lcl,
          risk_ucl = final_risk$ucl
        ))
      }
    }, error = function(e) {
      message("Skipping bin ", i, " due to error: ", e$message)
    })
  }
  
  if (nrow(risk_results) == 0) {
    stop("No valid risk estimates could be calculated")
  }
  
  # Create plot
  if (is.null(title)) {
    title <- paste0("Lifetime Risk at Age ", target_age, " by ", biomarker_col)
  }
  
  # Set X-axis label
  if (is.null(xlab)) {
    xlab <- biomarker_col
  }
  
  # Create base plot
  p <- ggplot(risk_results, aes(x = biomarker_midpoint, y = risk_estimate))
  
  # Add points if requested
  if (show_points) {
    p <- p + geom_point(color = "steelblue", size = 2)
  }
  
  # Add either smooth or linear trend
  if (smooth_line) {
    # Replace line and ribbon with smooth versions
    p <- p + geom_smooth(method = "loess", se = TRUE, color = "steelblue", 
                         fill = "steelblue", alpha = 0.3, linewidth = 1)
  } else {
    # Use original line and confidence ribbon from data points
    p <- p + 
      geom_ribbon(aes(ymin = risk_lcl, ymax = risk_ucl), alpha = 0.3, fill = "steelblue") +
      geom_line(color = "steelblue", linewidth = 1)
  }
  
  p <- p +
    labs(
      title = title,
      x = xlab,
      y = if (adjusted) "Adjusted Lifetime Risk (%)" else "Lifetime Risk (%)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      panel.grid.minor = element_blank()
    )
  
  # Apply log transformation if requested
  if (log_transform) {
    p <- p + scale_x_log10()
  }
  
  # Apply X-axis limits if specified
  if (!is.null(xlim)) {
    if (log_transform) {
      # For log scale, use coord_cartesian to avoid data filtering
      p <- p + coord_cartesian(xlim = xlim)
    } else {
      p <- p + xlim(xlim[1], xlim[2])
    }
  }
  
  return(p)
}
