#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line labs theme_minimal theme
#' @importFrom ggplot2 element_text element_blank scale_color_manual scale_fill_manual
#' @importFrom ggplot2 geom_point geom_errorbar scale_x_log10 xlim coord_cartesian geom_smooth
#' @importFrom grDevices rainbow
NULL


#' Plot Biomarker Risk Continuum
#'
#' Creates a plot showing lifetime risk across the continuum of a biomarker at one or more target ages.
#'
#' @param data Input dataset with required columns: ids, entryage, survage, status, astatus
#' @param biomarker_col Name of the biomarker column
#' @param target_age Age(s) at which to estimate lifetime risk (e.g., 45 or c(45, 55))
#' @param max_age Maximum age for follow-up (default: 90)
#' @param n_bins Number of biomarker bins to create (default: 10)
#' @param adjusted Use competing risk adjusted estimates (default: TRUE)
#' @param title Plot title (optional)
#' @param xlim Numeric vector of length 2 specifying X-axis limits (optional)
#' @param log_transform Logical; apply log10 transformation to X-axis (default: FALSE)
#' @param xlab X-axis label (optional, defaults to biomarker column name)
#' @param smooth_line Logical; use smoothed line and confidence interval instead of connecting points (default: FALSE)
#' @param show_points Logical; show individual data points (default: TRUE)
#' @param colors Vector of colors for different target ages (optional, defaults to rainbow colors)
#' @param age_labels Named vector for age labels in legend, e.g. c("45"="Age 45", "55"="Age 55") (optional)
#' @return A ggplot object showing biomarker values vs lifetime risk
#' @details
#' This function bins the biomarker into quantiles, runs PIE analysis on each bin for each target age,
#' and plots the resulting lifetime risk estimates. When multiple target ages are provided, each age
#' gets its own colored line. X-axis can be customized with limits and log transformation for better 
#' visualization of biomarkers with wide ranges.
#' When smooth_line=TRUE, replaces the point-to-point line with a LOESS smooth and 
#' replaces the PIE confidence intervals with smooth-based confidence intervals.
#' @examples
#' # Add biomarker to test data
#' test_data$biomarker <- rnorm(nrow(test_data), 100, 20)
#' 
#' # Single target age (original functionality)
#' plot_biomarker_risk_continuum(test_data, "biomarker", target_age = 50)
#' 
#' # Multiple target ages with different colored lines
#' plot_biomarker_risk_continuum(test_data, "biomarker", target_age = c(45, 55))
#' 
#' # With custom age labels and colors
#' plot_biomarker_risk_continuum(test_data, "biomarker", target_age = c(45, 55),
#'                               age_labels = c("45"="Starting at 45", "55"="Starting at 55"),
#'                               colors = c("red", "blue"))
#' 
#' # With log transformation and smoothing (good for biomarkers like NTproBNP)
#' plot_biomarker_risk_continuum(test_data, "ntprobnp", target_age = c(45, 55), 
#'                               log_transform = TRUE, xlim = c(10, 1000), smooth_line = TRUE)
#' 
#' # Clean smooth line without points
#' plot_biomarker_risk_continuum(test_data, "ntprobnp", target_age = c(45, 55),
#'                               smooth_line = TRUE, show_points = FALSE)
#' @export
plot_biomarker_risk_continuum <- function(data, biomarker_col, target_age, 
                                         max_age = 90, n_bins = 10, 
                                         adjusted = TRUE, title = NULL,
                                         xlim = NULL, log_transform = FALSE,
                                         xlab = NULL, smooth_line = FALSE,
                                         show_points = TRUE, colors = NULL,
                                         age_labels = NULL) {
  
  # Validate inputs
  if (!biomarker_col %in% names(data)) {
    stop("Biomarker column '", biomarker_col, "' not found in data")
  }
  
  required_cols <- c("ids", "entryage", "survage", "status", "astatus")
  if (!all(required_cols %in% names(data))) {
    stop("Data must contain columns: ", paste(required_cols, collapse = ", "))
  }
  
  # Validate target_age
  if (!is.numeric(target_age) || any(target_age <= 0)) {
    stop("target_age must be a positive numeric value or vector")
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
  
  # Initialize results storage with target_age column
  risk_results <- data.frame(
    biomarker_midpoint = numeric(0),
    target_age = numeric(0),
    risk_estimate = numeric(0),
    risk_lcl = numeric(0),
    risk_ucl = numeric(0)
  )
  
  # Run PIE analysis for each target age and each bin
  for (age in target_age) {
    for (i in seq_len(n_bins)) {
      bin_level <- levels(data$biomarker_bin)[i]
      bin_data <- data[data$biomarker_bin == bin_level, ]
      
      if (nrow(bin_data) < 5) next  # Skip bins with too few observations
      
      tryCatch({
        # Run PIE analysis for this bin and age
        result <- pie_analysis(
          data = bin_data,
          min_age = age,
          max_age = max_age,
          age_group_width = 5,
          group_var = NULL,
          age_free = age
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
            target_age = age,
            risk_estimate = final_risk$est,
            risk_lcl = final_risk$lcl,
            risk_ucl = final_risk$ucl
          ))
        }
      }, error = function(e) {
        message("Skipping bin ", i, " for age ", age, " due to error: ", e$message)
      })
    }
  }
  
  if (nrow(risk_results) == 0) {
    stop("No valid risk estimates could be calculated")
  }
  
  # Convert target_age to factor for plotting
  risk_results$target_age_factor <- as.factor(risk_results$target_age)
  
  # Set up colors
  n_ages <- length(target_age)
  if (is.null(colors)) {
    if (n_ages == 1) {
      colors <- "steelblue"
    } else {
      colors <- rainbow(n_ages, start = 0, end = 0.8)  # Avoid red at end of spectrum
    }
  } else if (length(colors) != n_ages) {
    warning("Number of colors (", length(colors), ") doesn't match number of target ages (", 
            n_ages, "). Using default colors.")
    colors <- rainbow(n_ages, start = 0, end = 0.8)
  }
  
  # Create plot title
  if (is.null(title)) {
    if (length(target_age) == 1) {
      title <- paste0("Lifetime Risk at Age ", target_age, " by ", biomarker_col)
    } else {
      age_range <- paste0(min(target_age), "-", max(target_age))
      title <- paste0("Lifetime Risk by ", biomarker_col, " (Ages ", age_range, ")")
    }
  }
  
  # Set X-axis label
  if (is.null(xlab)) {
    xlab <- biomarker_col
  }
  
  # Create base plot with appropriate aesthetics
  if (length(target_age) == 1) {
    # Single age - use original styling
    p <- ggplot(risk_results, aes(x = biomarker_midpoint, y = risk_estimate))
    
    # Add points if requested
    if (show_points) {
      p <- p + geom_point(color = colors[1], size = 2)
    }
    
    # Add either smooth or linear trend
    if (smooth_line) {
      p <- p + geom_smooth(method = "loess", se = TRUE, color = colors[1], 
                           fill = colors[1], alpha = 0.3, linewidth = 1)
    } else {
      p <- p + 
        geom_ribbon(aes(ymin = risk_lcl, ymax = risk_ucl), alpha = 0.3, fill = colors[1]) +
        geom_line(color = colors[1], linewidth = 1)
    }
  } else {
    # Multiple ages - use color aesthetics
    p <- ggplot(risk_results, aes(x = biomarker_midpoint, y = risk_estimate, 
                                  color = target_age_factor, fill = target_age_factor))
    
    # Add points if requested
    if (show_points) {
      p <- p + geom_point(size = 2)
    }
    
    # Add either smooth or linear trend
    if (smooth_line) {
      p <- p + geom_smooth(method = "loess", se = TRUE, alpha = 0.3, linewidth = 1)
    } else {
      p <- p + 
        geom_ribbon(aes(ymin = risk_lcl, ymax = risk_ucl), alpha = 0.3, color = NA) +
        geom_line(linewidth = 1)
    }
    
    # Apply custom colors
    p <- p + 
      scale_color_manual(values = colors) +
      scale_fill_manual(values = colors)
  }
  
  # Add labels and theme
  p <- p +
    labs(
      title = title,
      x = xlab,
      y = if (adjusted) "Adjusted Lifetime Risk (%)" else "Lifetime Risk (%)",
      color = if (length(target_age) > 1) "Target Age" else NULL,
      fill = if (length(target_age) > 1) "Target Age" else NULL
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      panel.grid.minor = element_blank(),
      legend.position = if (length(target_age) > 1) "bottom" else "none",
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10)
    )
  
  # Apply custom age labels if provided
  if (!is.null(age_labels) && length(target_age) > 1) {
    # Validate age_labels
    if (is.character(age_labels) && !is.null(names(age_labels))) {
      # Check if all target ages have labels
      target_age_str <- as.character(target_age)
      if (all(target_age_str %in% names(age_labels))) {
        p <- p + 
          scale_color_manual(values = colors, labels = age_labels[target_age_str]) +
          scale_fill_manual(values = colors, labels = age_labels[target_age_str])
      } else {
        warning("age_labels doesn't contain all target ages. Using default labels.")
      }
    } else {
      warning("age_labels must be a named character vector. Using default labels.")
    }
  }
  
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
