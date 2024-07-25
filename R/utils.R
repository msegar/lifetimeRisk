# utils.r

#' Print or Export Lifetime Risk Data
#'
#' This function prints or exports the core lifetime risk data from a pie_lifetime_risk object.
#'
#' @param result A pie_lifetime_risk object returned by the pie() function
#' @param group Which group to export data for: "level1" or "level2"
#' @param adjusted Logical, if TRUE export adjusted risk, if FALSE export unadjusted risk. Default is FALSE.
#' @param output Either "console" to print to console, or a file path to save as CSV. Default is "console".
#' @param delimiter Character to use as delimiter when printing to console. Default is "\t" (tab).
#'
#' @return Invisibly returns the exported data frame
#' @export
#'
#' @examples
#' \dontrun{
#' result <- pie(...)
#' print_lr_data(result, group = "level1")
#' print_lr_data(result, group = "level2", adjusted = TRUE, output = "risk_data.csv")
#' }
#' @importFrom dplyr across where
print_lr_data <- function(result, group = "level1", adjusted = FALSE, output = "console", delimiter = "\t") {
  # Check if the input is a pie_lifetime_risk object
  if (!inherits(result, "pie_lifetime_risk")) {
    stop("Input must be a pie_lifetime_risk object.")
  }

  # Select the appropriate data based on the group
  if (group == "level1") {
    data <- result$lr_data$lr1
  } else if (group == "level2") {
    data <- result$lr_data$lr2
  } else {
    stop("group must be either 'level1' or 'level2'")
  }

  # Prepare the data frame
  if (adjusted) {
    export_data <- data %>%
      select(age, risk = cfstar, lcl = lclstar, ucl = uclstar)
  } else {
    export_data <- data %>%
      select(age, risk = cf, lcl, ucl)
  }

  # Round the numeric columns to two decimal places
  export_data <- export_data %>%
    mutate(across(where(is.numeric), ~ round(., 2)))

  # Export the data
  if (output == "console") {
    # Print to console
    cat("Age", "Risk", "LCL", "UCL", sep = delimiter, "\n")
    utils::write.table(export_data, sep = delimiter, row.names = FALSE, col.names = FALSE, quote = FALSE)
  } else {
    # Save to CSV
    readr::write_csv(export_data, file = output)
    cat("Data exported to", output, "\n")
  }

  # Invisibly return the data frame
  invisible(export_data)
}
