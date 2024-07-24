# incidence.R

#' Calculate Incidence Rates
#'
#' This function calculates incidence rates, similar to the INCID macro in SAS.
#'
#' @param data A data frame containing the summary dataset
#' @param min_age Minimum age to consider
#' @param max_age Maximum age to consider
#' @param age_group_width Width of age groups
#' @param min_age_group Minimum age group to consider
#' @param verbose Logical, if TRUE, print detailed output. Default is FALSE.
#'
#' @return A data frame with incidence rates
#' @export
#'
#' @importFrom dplyr filter mutate group_by summarize arrange
calculate_incidence <- function(data, min_age, max_age, age_group_width, min_age_group, verbose = FALSE) {
  temp <- data %>%
    filter(age >= min_age, age <= max_age) %>%
    mutate(agegroup = as.numeric(floor((age - min_age_group) / age_group_width)))

  t <- temp %>%
    group_by(agegroup) %>%
    summarize(events = sum(e), wpy = sum(w), .groups = "drop") %>%
    mutate(wtr = 1000 * events / wpy)

  tot <- t %>%
    summarize(agegroup = 88, events = sum(events), wpy = sum(wpy), wtr = 1000 * sum(events) / sum(wpy))

  result <- bind_rows(tot, t) %>%
    arrange(agegroup) %>%
    mutate(
      twpy = first(wpy),
      last = as.numeric(floor((max_age - min_age_group) / age_group_width))
    )

  if (verbose) {
    cat("Incidence Rates:\n")
    print(result)
  }

  return(result)
}
