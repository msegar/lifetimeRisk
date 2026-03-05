#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line labs theme_minimal theme
#' @importFrom ggplot2 element_text element_blank scale_color_manual scale_fill_manual
#' @importFrom ggplot2 geom_point geom_errorbar scale_x_log10 xlim coord_cartesian geom_smooth
#' @importFrom ggplot2 scale_linetype_discrete scale_shape_discrete
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
#' @param colors Vector of colors for different target ages or strata levels (optional, defaults to rainbow colors)
#' @param age_labels Named vector for age labels in legend, e.g. c("45"="Age 45", "55"="Age 55") (optional)
#' @param strata Character; name of column in data to stratify by (e.g., "Sex"). When provided,
#'   separate lines are plotted for each stratum level using different colors.
#' @param strata_labels Named vector for strata labels in legend, e.g. c("M"="Male", "F"="Female") (optional)
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
#'
#' # Stratified by Sex
#' plot_biomarker_risk_continuum(test_data, "biomarker", target_age = 50,
#'                               strata = "sex")
#'
#' # Stratified by Sex with custom labels
#' plot_biomarker_risk_continuum(test_data, "biomarker", target_age = 50,
#'                               strata = "sex",
#'                               strata_labels = c("M"="Male", "F"="Female"))
#'
#' # Stratified by Sex with multiple ages (color=strata, linetype=age)
#' plot_biomarker_risk_continuum(test_data, "biomarker", target_age = c(45, 55),
#'                               strata = "sex")
#' @export
plot_biomarker_risk_continuum <- function(data, biomarker_col, target_age,
                                         max_age = 90, n_bins = 10,
                                         adjusted = TRUE, title = NULL,
                                         xlim = NULL, log_transform = FALSE,
                                         xlab = NULL, smooth_line = FALSE,
                                         show_points = TRUE, colors = NULL,
                                         age_labels = NULL, strata = NULL,
                                         strata_labels = NULL) {

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

  # Validate strata if provided
  if (!is.null(strata)) {
    if (!strata %in% names(data)) {
      stop("Strata column '", strata, "' not found in data")
    }
    strata_levels <- unique(data[[strata]])
    strata_levels <- strata_levels[!is.na(strata_levels)]
    if (length(strata_levels) < 2) {
      stop("Strata column must have at least 2 unique non-NA values")
    }
  } else {
    strata_levels <- NULL
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

  # Initialize results storage with target_age and strata columns
  risk_results <- data.frame(
    biomarker_midpoint = numeric(0),
    target_age = numeric(0),
    strata_level = character(0),
    risk_estimate = numeric(0),
    risk_lcl = numeric(0),
    risk_ucl = numeric(0),
    stringsAsFactors = FALSE
  )

  # Set up iteration over strata (use "overall" if no strata)
  if (!is.null(strata)) {
    strata_iter <- as.character(strata_levels)
  } else {
    strata_iter <- "overall"
  }

  # Run PIE analysis for each strata level, target age, and bin
  for (stratum in strata_iter) {
    # Subset data for this stratum
    if (!is.null(strata)) {
      stratum_data <- data[data[[strata]] == stratum, ]
    } else {
      stratum_data <- data
    }

    for (age in target_age) {
      for (i in seq_len(n_bins)) {
        bin_level <- levels(data$biomarker_bin)[i]
        bin_data <- stratum_data[stratum_data$biomarker_bin == bin_level, ]

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
              strata_level = stratum,
              risk_estimate = final_risk$est,
              risk_lcl = final_risk$lcl,
              risk_ucl = final_risk$ucl,
              stringsAsFactors = FALSE
            ))
          }
        }, error = function(e) {
          message("Skipping bin ", i, " for age ", age,
                  if (!is.null(strata)) paste0(", stratum ", stratum) else "",
                  " due to error: ", e$message)
        })
      }
    }
  }

  if (nrow(risk_results) == 0) {
    stop("No valid risk estimates could be calculated")
  }

  # Convert to factors for plotting
  risk_results$target_age_factor <- as.factor(risk_results$target_age)
  risk_results$strata_factor <- as.factor(risk_results$strata_level)

  # Determine plotting mode
  has_strata <- !is.null(strata)
  has_multiple_ages <- length(target_age) > 1
  n_strata <- if (has_strata) length(strata_levels) else 1
  n_ages <- length(target_age)

  # Set up colors based on what varies (strata takes priority for color)
  if (is.null(colors)) {
    if (has_strata) {
      # Colors for strata levels
      if (n_strata == 2) {
        colors <- c("steelblue", "firebrick")
      } else {
        colors <- rainbow(n_strata, start = 0, end = 0.8)
      }
    } else if (has_multiple_ages) {
      colors <- rainbow(n_ages, start = 0, end = 0.8)
    } else {
      colors <- "steelblue"
    }
  } else {
    # Validate provided colors
    expected_n <- if (has_strata) n_strata else n_ages
    if (length(colors) != expected_n && !(length(colors) == 1 && expected_n == 1)) {
      warning("Number of colors (", length(colors), ") doesn't match expected (",
              expected_n, "). Using default colors.")
      if (has_strata) {
        colors <- if (n_strata == 2) c("steelblue", "firebrick") else rainbow(n_strata, start = 0, end = 0.8)
      } else {
        colors <- rainbow(n_ages, start = 0, end = 0.8)
      }
    }
  }

  # Create plot title
  if (is.null(title)) {
    age_part <- if (has_multiple_ages) {
      paste0("Ages ", min(target_age), "-", max(target_age))
    } else {
      paste0("Age ", target_age)
    }
    strata_part <- if (has_strata) paste0(" by ", strata) else ""
    title <- paste0("Lifetime Risk at ", age_part, " by ", biomarker_col, strata_part)
  }

  # Set X-axis label
  if (is.null(xlab)) {
    xlab <- biomarker_col
  }

  # Build the plot based on combinations of strata and multiple ages
  if (!has_strata && !has_multiple_ages) {
    # Simplest case: no strata, single age
    p <- ggplot(risk_results, aes(x = biomarker_midpoint, y = risk_estimate))

    if (show_points) {
      p <- p + geom_point(color = colors[1], size = 2)
    }

    if (smooth_line) {
      p <- p + geom_smooth(method = "loess", se = TRUE, color = colors[1],
                           fill = colors[1], alpha = 0.3, linewidth = 1)
    } else {
      p <- p +
        geom_ribbon(aes(ymin = risk_lcl, ymax = risk_ucl), alpha = 0.3, fill = colors[1]) +
        geom_line(color = colors[1], linewidth = 1)
    }

  } else if (!has_strata && has_multiple_ages) {
    # No strata, multiple ages: color by age
    p <- ggplot(risk_results, aes(x = biomarker_midpoint, y = risk_estimate,
                                  color = target_age_factor, fill = target_age_factor))

    if (show_points) {
      p <- p + geom_point(size = 2)
    }

    if (smooth_line) {
      p <- p + geom_smooth(method = "loess", se = TRUE, alpha = 0.3, linewidth = 1)
    } else {
      p <- p +
        geom_ribbon(aes(ymin = risk_lcl, ymax = risk_ucl), alpha = 0.3, color = NA) +
        geom_line(linewidth = 1)
    }

    p <- p +
      scale_color_manual(values = colors) +
      scale_fill_manual(values = colors)

  } else if (has_strata && !has_multiple_ages) {
    # Strata with single age: color by strata
    p <- ggplot(risk_results, aes(x = biomarker_midpoint, y = risk_estimate,
                                  color = strata_factor, fill = strata_factor))

    if (show_points) {
      p <- p + geom_point(size = 2)
    }

    if (smooth_line) {
      p <- p + geom_smooth(method = "loess", se = TRUE, alpha = 0.3, linewidth = 1)
    } else {
      p <- p +
        geom_ribbon(aes(ymin = risk_lcl, ymax = risk_ucl), alpha = 0.3, color = NA) +
        geom_line(linewidth = 1)
    }

    p <- p +
      scale_color_manual(values = colors) +
      scale_fill_manual(values = colors)

  } else {
    # Both strata and multiple ages: color by strata, linetype by age
    p <- ggplot(risk_results, aes(x = biomarker_midpoint, y = risk_estimate,
                                  color = strata_factor, fill = strata_factor,
                                  linetype = target_age_factor))

    if (show_points) {
      p <- p + geom_point(aes(shape = target_age_factor), size = 2)
    }

    if (smooth_line) {
      p <- p + geom_smooth(method = "loess", se = TRUE, alpha = 0.2, linewidth = 1)
    } else {
      p <- p +
        geom_ribbon(aes(ymin = risk_lcl, ymax = risk_ucl, group = interaction(strata_factor, target_age_factor)),
                    alpha = 0.2, color = NA) +
        geom_line(linewidth = 1)
    }

    p <- p +
      scale_color_manual(values = colors) +
      scale_fill_manual(values = colors)
  }

  # Determine legend labels
  color_legend <- if (has_strata) strata else if (has_multiple_ages) "Target Age" else NULL
  linetype_legend <- if (has_strata && has_multiple_ages) "Target Age" else NULL

  # Add labels and theme
  p <- p +
    labs(
      title = title,
      x = xlab,
      y = if (adjusted) "Adjusted Lifetime Risk (%)" else "Lifetime Risk (%)",
      color = color_legend,
      fill = color_legend,
      linetype = linetype_legend,
      shape = linetype_legend
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      panel.grid.minor = element_blank(),
      legend.position = if (has_strata || has_multiple_ages) "bottom" else "none",
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10)
    )

  # Apply custom strata labels if provided
  if (!is.null(strata_labels) && has_strata) {
    if (is.character(strata_labels) && !is.null(names(strata_labels))) {
      strata_str <- as.character(strata_levels)
      if (all(strata_str %in% names(strata_labels))) {
        p <- p +
          scale_color_manual(values = colors, labels = strata_labels[strata_str]) +
          scale_fill_manual(values = colors, labels = strata_labels[strata_str])
      } else {
        warning("strata_labels doesn't contain all strata levels. Using default labels.")
      }
    } else {
      warning("strata_labels must be a named character vector. Using default labels.")
    }
  }

  # Apply custom age labels if provided (for linetype when both strata and ages)
  if (!is.null(age_labels) && has_multiple_ages) {
    if (is.character(age_labels) && !is.null(names(age_labels))) {
      target_age_str <- as.character(target_age)
      if (all(target_age_str %in% names(age_labels))) {
        if (has_strata) {
          # Age is shown via linetype
          p <- p + scale_linetype_discrete(labels = age_labels[target_age_str])
          if (show_points) {
            p <- p + scale_shape_discrete(labels = age_labels[target_age_str])
          }
        } else {
          # Age is shown via color
          p <- p +
            scale_color_manual(values = colors, labels = age_labels[target_age_str]) +
            scale_fill_manual(values = colors, labels = age_labels[target_age_str])
        }
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


#' Find Inflection Point or Maximum Curvature in Biomarker-Risk Relationship
#'
#' Identifies key transition points in a biomarker-risk continuum by fitting a smoothing spline
#' and finding either the point of maximum curvature or the true inflection point.
#'
#' @param risk_data A data frame containing biomarker-risk data with the following columns:
#'   \itemize{
#'     \item \code{biomarker_midpoint} - Numeric vector of biomarker values
#'     \item \code{risk_estimate} - Numeric vector of corresponding risk estimates (%)
#'   }
#'   This data frame is typically obtained from \code{\link{plot_biomarker_risk_continuum}}
#'   by extracting the \code{$data} element from the returned ggplot object.
#' @param log_transform Logical; if \code{TRUE}, applies log10 transformation to biomarker
#'   values before analysis. Recommended for biomarkers with wide ranges or log-normal
#'   distributions (e.g., NTproBNP, troponin). Default is \code{FALSE}.
#' @param method Character; the method for finding the transition point. Options are:
#'   \itemize{
#'     \item \code{"max_curvature"} (default) - Finds the point where the curve is bending
#'       most sharply (maximum absolute second derivative).
#'     \item \code{"knee"} - Finds where the curve transitions from flat to steep by
#'       identifying where the slope first reaches 25\% of its maximum value. This is
#'       designed for biomarker-risk curves that start flat and become steep.
#'     \item \code{"inflection"} - Finds the true mathematical inflection point where the
#'       curve changes from concave up to concave down (second derivative crosses zero).
#'     \item \code{"segmented2"} - Piecewise linear regression with 2 segments (1 breakpoint)
#'       using the \code{segmented} package. Returns the breakpoint with confidence intervals
#'       via the Davies test. Best for identifying a single transition point.
#'     \item \code{"segmented3"} - Piecewise linear regression with 3 segments (2 breakpoints)
#'       using the \code{segmented} package. Returns the first breakpoint (transition from
#'       flat to steep) with confidence intervals. Best for curves with three regimes
#'       (flat → steep → plateau). The threshold at which risk accelerated was identified using
#'       three-segment piecewise linear regression applied to the log₁₀-transformed biomarker values,
#'       with the first breakpoint defining the transition from a low-gradient to a high-gradient
#'       region of the biomarker–risk continuum.
#'   }
#'
#' @return A list with the following elements:
#'   \describe{
#'     \item{point}{Numeric value indicating the biomarker level at the identified
#'       transition point (in original biomarker units). For \code{"segmented3"}, this
#'       is the first breakpoint.}
#'     \item{method}{Character string indicating which method was used}
#'     \item{model}{The fitted model object: a \code{smooth.spline} object for spline-based
#'       methods, or a \code{segmented} object for segmented regression methods}
#'     \item{breakpoints}{(Segmented methods only) Named numeric vector of all breakpoint
#'       estimates in original biomarker units}
#'     \item{ci}{(Segmented methods only) Matrix of breakpoint confidence intervals
#'       in original biomarker units, with columns \code{Est.}, \code{CI.low}, \code{CI.up}}
#'     \item{slopes}{(Segmented methods only) Named numeric vector of slopes for each segment}
#'   }
#'
#' @details
#' The function identifies transition points using the following approach:
#' \enumerate{
#'   \item Optionally log-transforms the biomarker values
#'   \item Fits the selected model
#'   \item Identifies the transition point(s)
#'   \item Back-transforms to original biomarker scale if needed
#' }
#'
#' \strong{Choosing a method:}
#' \itemize{
#'   \item \code{"max_curvature"} (default) finds where the curve is bending most sharply.
#'   \item \code{"knee"} finds where the slope first becomes substantial (25\% of max),
#'     identifying where the flat region ends and the steep increase begins.
#'   \item \code{"inflection"} finds the mathematical point where concavity changes. Returns
#'     \code{NA} if no inflection point exists.
#'   \item \code{"segmented2"} fits a two-segment (hockey-stick) piecewise linear model.
#'     Most commonly used in biomedical literature. Provides CIs on the breakpoint.
#'   \item \code{"segmented3"} fits a three-segment piecewise linear model. Useful when
#'     the relationship has three distinct phases (e.g., flat baseline, steep rise,
#'     plateau). The first breakpoint typically captures the "takeoff" point.
#' }
#'
#' @examples
#' # Example 1: Find max curvature point (default)
#' p1 <- plot_biomarker_risk_continuum(
#'   data = my_data,
#'   biomarker_col = "ntprobnp",
#'   target_age = 55,
#'   n_bins = 40,
#'   log_transform = TRUE
#' )
#'
#' result <- find_inflection(p1$data, log_transform = TRUE)
#' print(paste("Max curvature at:", round(result$point, 1), "pg/mL"))
#'
#' # Add vertical line to plot
#' p1 + geom_vline(xintercept = result$point,
#'                 linetype = "dashed", color = "red", linewidth = 1)
#'
#' # Example 2: Segmented regression with 2 segments
#' seg2 <- find_inflection(p1$data, log_transform = TRUE, method = "segmented2")
#' print(paste("Breakpoint:", round(seg2$point, 1)))
#' print(seg2$ci)       # confidence intervals
#' print(seg2$slopes)   # slopes per segment
#'
#' # Example 3: Segmented regression with 3 segments
#' seg3 <- find_inflection(p1$data, log_transform = TRUE, method = "segmented3")
#' print(paste("First breakpoint (takeoff):", round(seg3$point, 1)))
#' print(paste("All breakpoints:", paste(round(seg3$breakpoints, 1), collapse = ", ")))
#'
#' # Example 4: Compare all methods
#' methods <- c("max_curvature", "knee", "inflection", "segmented2", "segmented3")
#' results <- lapply(methods, function(m) {
#'   find_inflection(p1$data, log_transform = TRUE, method = m)
#' })
#' sapply(results, function(r) round(r$point, 1))
#'
#' @seealso
#' \code{\link{plot_biomarker_risk_continuum}} for generating the input risk data
#'
#' @export
find_inflection <- function(risk_data, log_transform = FALSE,
                            method = c("max_curvature", "knee", "inflection",
                                       "segmented2", "segmented3")) {

  method <- match.arg(method)

  if (!all(c("biomarker_midpoint", "risk_estimate") %in% names(risk_data))) {
    stop("risk_data must have 'biomarker_midpoint' and 'risk_estimate' columns")
  }

  # Check segmented package availability

  if (method %in% c("segmented2", "segmented3")) {
    if (!requireNamespace("segmented", quietly = TRUE)) {
      stop("Package 'segmented' is required for method '", method,
           "'. Install it with: install.packages('segmented')")
    }
  }

  # Sort by biomarker
  risk_data <- risk_data[order(risk_data$biomarker_midpoint), ]

  # Transform if needed
  if (log_transform) {
    x <- log10(risk_data$biomarker_midpoint)
  } else {
    x <- risk_data$biomarker_midpoint
  }
  y <- risk_data$risk_estimate

  # ---- Segmented regression methods ----
  if (method %in% c("segmented2", "segmented3")) {

    npsi <- ifelse(method == "segmented2", 1, 2)

    # Build data frame with transformed variable
    seg_data <- data.frame(x_var = x, y_var = y)

    # Fit base linear model
    fit_lm <- lm(y_var ~ x_var, data = seg_data)

    # Fit segmented model
    seg_fit <- segmented::segmented(fit_lm, seg.Z = ~x_var, npsi = npsi)

    # Extract breakpoints (in transformed scale)
    bp_matrix <- seg_fit$psi
    bp_est <- bp_matrix[, "Est."]

    # Get confidence intervals
    ci_raw <- segmented::confint.segmented(seg_fit)

    # Extract slope coefficients for each segment
    slope_info <- segmented::slope(seg_fit)
    slope_vals <- slope_info$x_var[, "Est."]
    segment_names <- paste0("segment_", seq_along(slope_vals))
    names(slope_vals) <- segment_names

    # Back-transform breakpoints and CIs
    if (log_transform) {
      bp_orig <- 10^bp_est
      ci_orig <- 10^ci_raw
    } else {
      bp_orig <- bp_est
      ci_orig <- ci_raw
    }

    # Name breakpoints
    bp_names <- paste0("breakpoint_", seq_along(bp_orig))
    names(bp_orig) <- bp_names

    # Primary point is the first breakpoint (the "takeoff")
    point <- bp_orig[1]

    return(list(
      point = unname(point),
      method = method,
      model = seg_fit,
      breakpoints = bp_orig,
      ci = ci_orig,
      slopes = slope_vals
    ))
  }

  # ---- Spline-based methods ----

  # Fit smooth spline
  fit <- smooth.spline(x, y, df = 6)

  # Predict along range
  x_seq <- seq(min(x), max(x), length.out = 200)
  y_pred <- predict(fit, x_seq)$y

  # Find the point based on method

  if (method == "knee") {
    # For flat-to-steep curves (typical biomarker-risk relationships):
    # Find where the slope first reaches a threshold percentage of max slope
    # This identifies where the curve transitions from flat to steep

    # Calculate first derivative (slope) numerically
    dy <- diff(y_pred) / diff(x_seq)

    # Find where slope reaches 25% of maximum slope
    # (this threshold works well for identifying the start of the steep region)
    max_slope <- max(dy)
    threshold <- 0.25 * max_slope

    # Find first point where slope exceeds threshold
    knee_idx <- which(dy >= threshold)[1]

    if (is.na(knee_idx)) {
      # Fallback: use the point of maximum slope change
      d2y <- diff(dy)
      knee_idx <- which.max(d2y)
    }

    point <- x_seq[knee_idx]

  } else if (method == "max_curvature") {
    # Calculate second derivative numerically
    d2y <- diff(diff(y_pred))
    # Find maximum absolute curvature
    max_idx <- which.max(abs(d2y))
    point <- x_seq[max_idx + 1]

  } else {
    # Find true inflection point (where second derivative crosses zero)
    d2y <- diff(diff(y_pred))
    # For biomarker-risk curves, we want where it goes from concave up to concave down
    # (positive to negative second derivative) - this is where acceleration peaks
    sign_changes <- which(diff(sign(d2y)) != 0)

    if (length(sign_changes) > 0) {
      # Filter to sign changes that go from positive to negative (concave up to down)
      # This represents the transition from accelerating to decelerating risk increase
      pos_to_neg <- sign_changes[sapply(sign_changes, function(i) {
        d2y[i] > 0 && d2y[i + 1] < 0
      })]

      if (length(pos_to_neg) > 0) {
        # If multiple, take the one with largest magnitude change (most prominent)
        if (length(pos_to_neg) > 1) {
          magnitudes <- sapply(pos_to_neg, function(i) abs(d2y[i]) + abs(d2y[i + 1]))
          pos_to_neg <- pos_to_neg[which.max(magnitudes)]
        }
        point <- x_seq[pos_to_neg + 1]
      } else {
        # Fall back to any sign change, picking the most prominent one
        magnitudes <- sapply(sign_changes, function(i) abs(d2y[i]) + abs(d2y[i + 1]))
        best_idx <- sign_changes[which.max(magnitudes)]
        point <- x_seq[best_idx + 1]
      }
    } else {
      point <- NA_real_
      warning("No inflection point found (second derivative does not cross zero)")
    }
  }

  # Back-transform
  if (!is.na(point) && log_transform) {
    point <- 10^point
  }

  return(list(
    point = point,
    method = method,
    model = fit
  ))
}
