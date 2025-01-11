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
