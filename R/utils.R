# utils.r

#' Print or Export Lifetime Risk Data
#'
#' This function prints or exports the core lifetime risk data from a pie_lifetime_risk object.
#'
#' @param result A pie_lifetime_risk object returned by the pie() function
#' @param group Which group to export data for: "level1" or "level2"
#' @param adjusted Logical, if TRUE export adjusted risk, if FALSE export unadjusted risk. Default is FALSE.
#' @param output Either "console" to print to console, or a file path to save as CSV. Default is "console".
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
print_lr_data <- function(result, group = "level1", adjusted = FALSE, output = "console") {
  # Check if the input is a pie_lifetime_risk object
  if (!inherits(result, "pie_lifetime_risk")) {
    stop("Input must be a pie_lifetime_risk object.")
  }

  # Select the appropriate data based on the group
  if (group == "level1") {
    data <- result$lr_data$lr1
    group_label <- result$parameters$level1
  } else if (group == "level2") {
    data <- result$lr_data$lr2
    group_label <- result$parameters$level2
  } else {
    stop("group must be either 'level1' or 'level2'")
  }

  # Prepare the data frame
  if (adjusted) {
    export_data <- data %>%
      select(
        Age = age,
        `Risk (%)` = cfstar,
        `Lower CI` = lclstar,
        `Upper CI` = uclstar
      )
  } else {
    export_data <- data %>%
      select(
        Age = age,
        `Risk (%)` = cf,
        `Lower CI` = lcl,
        `Upper CI` = ucl
      )
  }

  # Round the numeric columns to two decimal places
  export_data <- export_data %>%
    mutate(across(where(is.numeric), ~ round(., 2)))

  # Add attributes for metadata
  attr(export_data, "group") <- group_label
  attr(export_data, "risk_type") <- if(adjusted) "Adjusted" else "Unadjusted"

  # Handle output
  if (output != "console") {
    readr::write_csv(export_data, file = output)
    cat("Data exported to", output, "\n")
  }

  # Return the data frame
  return(export_data)
}


#' Get Lifetime Risk at Specific Age
#'
#' Returns a formatted lifetime risk estimate at a specific age.
#'
#' @param result A pie_lifetime_risk object returned by the pie() function
#' @param group Which group to get risk for: "level1" or "level2"
#' @param age Age at which to get lifetime risk
#' @param adjusted Logical, if TRUE return adjusted risk, if FALSE return unadjusted risk. Default is FALSE.
#'
#' @return A character string containing the risk estimate and confidence interval
#' @export
#'
#' @examples
#' \dontrun{
#' result <- pie(...)
#' get_lr_at(result, "level1", 50)  # Get unadjusted risk at age 50
#' get_lr_at(result, "level2", 60, adjusted = TRUE)  # Get adjusted risk at age 60
#' }
get_lr_at <- function(result, group = "level1", age, adjusted = FALSE) {
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

  # Check if age exists in the data
  if (!age %in% data$age) {
    stop("Age ", age, " not found in the data.")
  }

  # Extract the row for the specified age
  row_data <- data[data$age == age, ]

  # Get the appropriate risk and CI values
  if (adjusted) {
    risk <- row_data$cfstar
    lcl <- row_data$lclstar
    ucl <- row_data$uclstar
  } else {
    risk <- row_data$cf
    lcl <- row_data$lcl
    ucl <- row_data$ucl
  }

  # Format the string with rounded values
  sprintf("%.2f (%.2f, %.2f)", risk, lcl, ucl)
}
