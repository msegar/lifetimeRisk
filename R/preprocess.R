# preprocess.R

#' Preprocess Input Data
#'
#' This function preprocesses the input data according to the logic in the DATA1 macro.
#'
#' @param data A data frame containing the input data
#' @param max_age Maximum age to consider
#' @param min_age Minimum age to consider
#' @param group Group variable name
#' @param verbose Logical, if TRUE, print detailed output. Default is FALSE.
#'
#' @return A preprocessed data frame
#' @export
#'
#' @importFrom dplyr mutate filter select bind_rows arrange distinct
#'   ungroup first rename_with
#' @importFrom tidyr uncount
preprocess_data <- function(data, max_age, min_age, group, verbose = FALSE) {
  processed <- data %>%
    mutate(
      survage = floor(survage),
      full = 0
    ) %>%
    mutate(
      survage = ifelse(survage > max_age, max_age, survage),
      status = ifelse(survage > max_age, 0, status),
      astatus = ifelse(survage > max_age, 0, astatus),
      full = ifelse(survage > max_age, 1, full),
      start = pmax(entryage, min_age),
      stop = survage - 1,
      age = survage,
      weight = ifelse(status == 0 & full != 1, 0.5, 1)
    ) %>%
    filter(age >= start) %>%
    select(ids, age, status, astatus, weight, !!sym(group), start, stop)

  expanded <- processed %>%
    mutate(
      replicate = pmax(1, stop - start + 1)
    ) %>%
    uncount(weights = replicate) %>%
    group_by(ids) %>%
    mutate(
      age = seq(from = first(start), to = first(stop), by = 1),
      status = 0,
      astatus = 0,
      weight = 1
    ) %>%
    ungroup()

  result <- bind_rows(processed, expanded) %>%
    arrange(ids, age) %>%
    distinct(ids, age, .keep_all = TRUE) %>%
    select(ids, age, status, astatus, weight, !!sym(group))

  if (verbose) {
    cat("Preprocessed Data Summary:\n")
    print(summary(result))
    cat("\nFirst few rows of preprocessed data:\n")
    print(head(result))
  }

  return(result)
}

#' Create Summary Data Set for Each Age
#'
#' This function creates a summary data set for each age, similar to the SDSMAC macro in SAS.
#'
#' @param data A data frame containing the preprocessed data
#' @param group Group variable name
#' @param levels A vector of levels to include in the summary
#' @param verbose Logical, if TRUE, print detailed output. Default is FALSE.
#'
#' @return A data frame with summary statistics for each age
#' @export
#'
#' @importFrom dplyr filter group_by summarize n arrange
create_summary_dataset <- function(data, group, levels, verbose = FALSE) {
  result <- data %>%
    filter(!!sym(group) %in% levels) %>%
    group_by(age) %>%
    summarize(
      r = n(),
      e = sum(status),
      c = sum(astatus),
      w = sum(weight),
      .groups = "drop"
    ) %>%
    mutate(age = ifelse(is.na(age), 999, age)) %>%
    arrange(age)

  if (verbose) {
    cat("Summary Dataset:\n")
    print(result)
  }

  return(result)
}
