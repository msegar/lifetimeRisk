library(survival)
library(parallel)

#' Calculate Disease-Free Survival and Overall Survival Times
#'
#' @description
#' Calculates restricted mean survival time for disease-free survival and overall survival,
#' implementing the method described in published cardiovascular research. The function can
#' perform both stratified and non-stratified analyses, and uses bootstrap resampling
#' for standard error estimation.
#'
#' @param data A data frame containing the survival data
#' @param time_event_col Character string specifying the column name for time to disease event
#' @param time_death_col Character string specifying the column name for time to death
#' @param event_col Character string specifying the column name for disease event indicator (1 = event, 0 = censored)
#' @param death_col Character string specifying the column name for death indicator (1 = death, 0 = censored)
#' @param index_age Numeric value specifying the index age for analysis
#' @param restriction_time Numeric value specifying the maximum time point for restricted mean survival calculation.
#'        If NULL, uses 99% of maximum observed time. Default is NULL.
#' @param strata_col Optional character string specifying the column name for stratification.
#'        Can be numeric, factor, or character. For factors, maintains level ordering.
#'        For numeric and character variables, sorts in ascending order. Default is NULL.
#' @param n_bootstrap Integer specifying the number of bootstrap samples for standard error estimation.
#'        Default is 1000.
#' @param small_delta Numeric value added to zero time values to prevent computation issues.
#'        Default is 0.001.
#'
#' @return A list with class either "survival_results" (non-stratified) or
#' "stratified_survival_results" (stratified) containing:
#' \itemize{
#'   \item event_free_survival: Restricted mean survival time free of disease
#'   \item overall_survival: Restricted mean overall survival time
#'   \item years_with_event: Mean years lived with disease
#'   \item se_event_free: Standard error for disease-free survival
#'   \item se_overall: Standard error for overall survival
#'   \item se_difference: Standard error for years with disease
#'   \item n_observations: Number of subjects
#'   \item n_events: Number of disease events
#'   \item n_deaths: Number of deaths
#'   \item restriction_time: Time point used for restricted mean calculation
#' }
#'
#' @details
#' The function implements restricted mean survival time analysis accounting for competing
#' risks. For disease-free survival, both disease onset and death are considered as events.
#' Bootstrap resampling is used to estimate standard errors. The analysis can be performed
#' overall or within strata defined by a grouping variable.
#'
#' Time variables should be in the same units (e.g., years). Event indicators should be
#' binary (1 = event, 0 = censored). Zero time values are adjusted by adding a small
#' constant to prevent computational issues.
#'
#' @examples
#' \dontrun{
#' # Non-stratified analysis
#' results <- calculate_disease_free_survival(
#'   data = my_data,
#'   time_event_col = "time_to_event",
#'   time_death_col = "time_to_death",
#'   event_col = "event_status",
#'   death_col = "death_status",
#'   index_age = 45,
#'   restriction_time = 95
#' )
#'
#' # Print results with standard errors
#' print(results)
#'
#' # Print results with confidence intervals
#' print(results, ci = TRUE)
#'
#' # Stratified analysis by risk group
#' results_strat <- calculate_disease_free_survival(
#'   data = my_data,
#'   time_event_col = "time_to_event",
#'   time_death_col = "time_to_death",
#'   event_col = "event_status",
#'   death_col = "death_status",
#'   index_age = 45,
#'   restriction_time = 95,
#'   strata_col = "risk_group"
#' )
#'
#' # Print stratified results
#' print(results_strat)
#' }
#'
#' @references
#' Based on methods described in cardiovascular epidemiology research using
#' restricted mean survival time analysis.
#'
#' @seealso
#' \code{\link[survival]{Surv}}, \code{\link[survival]{survfit}}
#'
#' @importFrom survival Surv survfit
#' @importFrom parallel makeCluster stopCluster detectCores parLapply clusterExport clusterEvalQ
#'
#' @export
calculate_disease_free_survival <- function(data,
                                            time_event_col,
                                            time_death_col,
                                            event_col,
                                            death_col,
                                            index_age,
                                            restriction_time = NULL,
                                            strata_col = NULL,
                                            n_bootstrap = 1000,
                                            small_delta = 0.001) {

  # Validate inputs
  required_cols <- c(time_event_col, time_death_col, event_col, death_col)
  if (!all(required_cols %in% names(data))) {
    stop("Missing required columns in data")
  }

  # Create a copy of the data
  working_data <- data.frame(data)

  # Remove NA values
  working_data <- na.omit(working_data[, c(time_event_col, time_death_col,
                                           event_col, death_col, strata_col)])

  if (nrow(working_data) == 0) {
    stop("No complete cases after removing NA values")
  }

  # Handle zero times in both time variables
  working_data[[time_event_col]] <- as.numeric(working_data[[time_event_col]])
  working_data[[time_death_col]] <- as.numeric(working_data[[time_death_col]])

  if (any(working_data[[time_event_col]] == 0)) {
    message("Note: Event times with values 0 detected. Adding small constant.")
    working_data[[time_event_col]] <- working_data[[time_event_col]] + small_delta
  }
  if (any(working_data[[time_death_col]] == 0)) {
    message("Note: Zero death time values detected. Adding small constant.")
    working_data[[time_death_col]] <- working_data[[time_death_col]] + small_delta
  }

  # Ensure binary indicators
  working_data[[event_col]] <- as.numeric(working_data[[event_col]]) == 1
  working_data[[death_col]] <- as.numeric(working_data[[death_col]]) == 1

  # Handle restriction time - use maximum of both time variables
  max_observed_time <- max(max(working_data[[time_event_col]]),
                           max(working_data[[time_death_col]]))

  if (is.null(restriction_time)) {
    tau <- max_observed_time * 0.99
    message("Using 99% of maximum observed time (", round(tau, 2), " years)")
  } else {
    requested_tau <- restriction_time - index_age
    tau <- min(requested_tau, max_observed_time * 0.99)
    if (tau < requested_tau) {
      message("Using 99% of maximum observed time (", round(tau, 2),
              " years) instead of requested ", round(requested_tau, 2), " years")
    }
  }

  # Function to analyze one stratum
  analyze_stratum <- function(data, tau, n_bootstrap) {
    # Initial fits using separate time variables
    # For event-free survival, event is either event or death
    composite_event <- as.numeric(data[[event_col]] == 1 | data[[death_col]] == 1)
    fit_event <- survfit(Surv(data[[time_event_col]], composite_event) ~ 1)
    fit_overall <- survfit(Surv(data[[time_death_col]], data[[death_col]]) ~ 1)

    # Calculate initial RMSTs
    rmst_event <- summary(fit_event, rmean = tau)$table["rmean"]
    rmst_overall <- summary(fit_overall, rmean = tau)$table["rmean"]

    # Bootstrap calculations
    n_cores <- max(1, detectCores() - 1)
    cl <- makeCluster(n_cores)
    on.exit(stopCluster(cl))

    # Export required objects to cluster
    clusterExport(cl, c("time_event_col", "time_death_col", "event_col", "death_col"),
                  envir = environment())
    clusterEvalQ(cl, library(survival))

    # Bootstrap function
    boot_fun <- function(i, data, tau) {
      indices <- sample(nrow(data), replace = TRUE)
      boot_data <- data[indices,]

      # For event-free survival, event is either event or death
      boot_composite_event <- as.numeric(boot_data[[event_col]] == 1 | boot_data[[death_col]] == 1)
      boot_fit_event <- survfit(Surv(boot_data[[time_event_col]], boot_composite_event) ~ 1)
      boot_fit_overall <- survfit(Surv(boot_data[[time_death_col]],
                                       boot_data[[death_col]]) ~ 1)

      event_rmst <- summary(boot_fit_event, rmean = tau)$table["rmean"]
      overall_rmst <- summary(boot_fit_overall, rmean = tau)$table["rmean"]

      c(event_rmst, overall_rmst)
    }

    # Run bootstrap
    set.seed(888)
    boot_results <- parLapply(cl, 1:n_bootstrap, boot_fun, data = data, tau = tau)
    boot_matrix <- do.call(rbind, boot_results)

    # Calculate standard errors from bootstrap
    se_event <- sd(boot_matrix[,1])
    se_overall <- sd(boot_matrix[,2])
    se_difference <- sd(boot_matrix[,2] - boot_matrix[,1])

    # Return results
    list(
      event_free_survival = rmst_event,
      overall_survival = rmst_overall,
      years_with_event = rmst_overall - rmst_event,
      se_event_free = se_event,
      se_overall = se_overall,
      se_difference = se_difference,
      n_observations = nrow(data),
      n_events = sum(data[[event_col]]),
      n_deaths = sum(data[[death_col]]),
      restriction_time = tau
    )
  }

  # Handle stratification
  # For strata column, handle different types to ensure correct ordering
  if (!is.null(strata_col)) {
    # Get unique values from strata column
    strata_values <- working_data[[strata_col]]

    # Handle different types
    if (is.factor(strata_values)) {
      # For factors, maintain level ordering
      strata_levels <- levels(strata_values)
    } else if (is.numeric(strata_values)) {
      # For numeric, sort in ascending order
      strata_levels <- sort(unique(strata_values))
    } else {
      # For character or other types, sort alphanumerically
      strata_levels <- sort(unique(strata_values))
    }

    results_list <- list()
    for (level in strata_levels) {
      message("\nAnalyzing stratum: ", level)
      stratum_data <- working_data[working_data[[strata_col]] == level,]
      results_list[[as.character(level)]] <- analyze_stratum(stratum_data, tau, n_bootstrap)
    }

    results <- list(results = results_list)
    class(results) <- "stratified_survival_results"
  } else {
    results <- analyze_stratum(working_data, tau, n_bootstrap)
    class(results) <- "survival_results"
  }

  return(results)
}


#' Print Method for Survival Results
#'
#' @description
#' Prints a formatted summary table of disease-free survival analysis results.
#' Displays number of observations, events, deaths, and survival estimates with
#' either standard errors or confidence intervals.
#'
#' @param x An object of class "survival_results"
#' @param digits Number of decimal places for rounding. Default is 2.
#' @param ci Logical; if TRUE, displays 95% confidence intervals instead of
#'        standard errors. Default is FALSE.
#' @param ... Additional arguments passed to print
#'
#' @details
#' The output includes:
#' \itemize{
#'   \item Number of observations, events, and deaths
#'   \item Disease-free survival estimate with SE or 95% CI
#'   \item Years with disease estimate with SE or 95% CI
#'   \item Overall survival estimate with SE or 95% CI
#' }
#'
#' @examples
#' \dontrun{
#' results <- calculate_disease_free_survival(...)
#'
#' # Print with standard errors
#' print(results)
#'
#' # Print with confidence intervals
#' print(results, ci = TRUE)
#'
#' # Change decimal places
#' print(results, digits = 3)
#' }
#'
#' @method print survival_results
#' @export
print.survival_results <- function(x, digits = 2, ci = FALSE, ...) {
  # Function to format estimate with uncertainty
  format_estimate <- function(est, se) {
    if (ci) {
      lower <- est - 1.96 * se
      upper <- est + 1.96 * se
      sprintf("%.2f (%.2f, %.2f)", est, lower, upper)
    } else {
      sprintf("%.2f (%.2f)", est, se)
    }
  }

  df <- data.frame(
    Observations = x$n_observations,
    Events = x$n_events,
    Deaths = x$n_deaths,
    `Disease-free Survival` = format_estimate(x$event_free_survival, x$se_event_free),
    `Years with Disease` = format_estimate(x$years_with_event, x$se_difference),
    `Overall Survival` = format_estimate(x$overall_survival, x$se_overall),
    check.names = FALSE
  )
  print.data.frame(df, row.names = FALSE)
}

#' Print Method for Stratified Survival Results
#'
#' @description
#' Prints a formatted summary table of stratified disease-free survival analysis results.
#' For each stratum, displays number of observations, events, deaths, and survival
#' estimates with either standard errors or confidence intervals.
#'
#' @param x An object of class "stratified_survival_results"
#' @param digits Number of decimal places for rounding. Default is 2.
#' @param ci Logical; if TRUE, displays 95% confidence intervals instead of
#'        standard errors. Default is FALSE.
#' @param ... Additional arguments passed to print
#'
#' @details
#' For each stratum, the output includes:
#' \itemize{
#'   \item Stratum identifier
#'   \item Number of observations, events, and deaths
#'   \item Disease-free survival estimate with SE or 95% CI
#'   \item Years with disease estimate with SE or 95% CI
#'   \item Overall survival estimate with SE or 95% CI
#' }
#'
#' The strata are ordered according to the original data:
#' \itemize{
#'   \item For factors, maintains the factor level ordering
#'   \item For numeric values, sorts in ascending order
#'   \item For character values, sorts alphanumerically
#' }
#'
#' @examples
#' \dontrun{
#' results_strat <- calculate_disease_free_survival(..., strata_col = "risk_group")
#'
#' # Print with standard errors
#' print(results_strat)
#'
#' # Print with confidence intervals
#' print(results_strat, ci = TRUE)
#'
#' # Change decimal places
#' print(results_strat, digits = 3)
#' }
#'
#' @method print stratified_survival_results
#' @export
print.stratified_survival_results <- function(x, digits = 2, ci = FALSE, ...) {
  # Function to format estimate with uncertainty
  format_estimate <- function(est, se) {
    if (ci) {
      lower <- est - 1.96 * se
      upper <- est + 1.96 * se
      sprintf("%.2f (%.2f, %.2f)", est, lower, upper)
    } else {
      sprintf("%.2f (%.2f)", est, se)
    }
  }

  # Create results data frame
  results_df <- do.call(rbind, lapply(names(x$results), function(stratum) {
    res <- x$results[[stratum]]
    data.frame(
      Stratum = stratum,
      Observations = res$n_observations,
      Events = res$n_events,
      Deaths = res$n_deaths,
      `Disease-free Survival` = format_estimate(res$event_free_survival, res$se_event_free),
      `Years with Disease` = format_estimate(res$years_with_event, res$se_difference),
      `Overall Survival` = format_estimate(res$overall_survival, res$se_overall),
      check.names = FALSE
    )
  }))

  print.data.frame(results_df, row.names = FALSE)
}
