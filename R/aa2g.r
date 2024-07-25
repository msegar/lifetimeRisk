# aa2g.R

#' Calculate Age-Adjusted Incidence Rates
#'
#' This function calculates age-adjusted incidence rates, similar to the AA2G macro in SAS.
#'
#' @param in1 A data frame containing incidence data for the first group
#' @param in2 A data frame containing incidence data for the second group
#' @param level1 Name of the first group
#' @param level2 Name of the second group
#' @param verbose Logical, if TRUE, print detailed output. Default is FALSE.
#'
#' @return A data frame with age-adjusted incidence rates
#' @export
#'
#' @importFrom dplyr mutate select filter bind_rows arrange full_join
#' @importFrom rlang :=
calculate_aa2g <- function(in1, in2, level1, level2, verbose = FALSE) {
  # Ensure agegroup is numeric
  in1$agegroup <- as.numeric(as.character(in1$agegroup))
  in2$agegroup <- as.numeric(as.character(in2$agegroup))

  # Rename columns
  in1 <- in1 %>%
    rename_with(~ paste0(.x, "_", level1), c(events, wpy, twpy, wtr))
  in2 <- in2 %>%
    rename_with(~ paste0(.x, "_", level2), c(events, wpy, twpy, wtr))

  # Merge data
  a <- full_join(in1, in2, by = "agegroup")

  # Calculate age-adjusted rates
  b <- a %>%
    filter(agegroup != 88) %>%
    mutate(
      atwpy = !!sym(paste0("wpy_", level1)) + !!sym(paste0("wpy_", level2)),
      gtwpy = first(!!sym(paste0("twpy_", level1))) + first(!!sym(paste0("twpy_", level2))),
      wtwpy = atwpy / gtwpy,
      !!paste0("rate_", level1) := 1000 * (!!sym(paste0("events_", level1)) / !!sym(paste0("wpy_", level1))) * wtwpy,
      !!paste0("rate_", level2) := 1000 * (!!sym(paste0("events_", level2)) / !!sym(paste0("wpy_", level2))) * wtwpy
    )

  # Calculate cumulative rates
  c <- b %>%
    mutate(
      !!paste0("cumrate_", level1) := cumsum(!!sym(paste0("rate_", level1))),
      !!paste0("cumrate_", level2) := cumsum(!!sym(paste0("rate_", level2)))
    )

  # Add total row
  total <- c %>%
    summarize(
      agegroup = 99,
      !!paste0("rate_", level1) := sum(!!sym(paste0("rate_", level1))),
      !!paste0("rate_", level2) := sum(!!sym(paste0("rate_", level2))),
      !!paste0("cumrate_", level1) := last(!!sym(paste0("cumrate_", level1))),
      !!paste0("cumrate_", level2) := last(!!sym(paste0("cumrate_", level2)))
    )

  # Combine results
  result <- bind_rows(c, total) %>%
    arrange(agegroup)

  # Print results if verbose is TRUE
  if (verbose) {
    print(result %>% select(agegroup,
                            !!sym(paste0("events_", level1)), !!sym(paste0("wpy_", level1)),
                            !!sym(paste0("rate_", level1)), !!sym(paste0("cumrate_", level1)),
                            !!sym(paste0("events_", level2)), !!sym(paste0("wpy_", level2)),
                            !!sym(paste0("rate_", level2)), !!sym(paste0("cumrate_", level2))))
  }

  return(result)
}
