#' Calculate Disease-Free Survival and Overall Survival Times
#'
#' @description
#' Decomposes total remaining life expectancy from a given index age into years lived
#' FREE of disease and years lived WITH disease, using restricted mean survival time (RMST).
#'
#' The method works by estimating two survival curves from the same cohort:
#' \enumerate{
#'   \item \strong{Event-free survival}: time until the COMPOSITE endpoint of disease onset
#'         or death (whichever comes first). This curve drops at every disease event AND
#'         every death, so it always falls below or equals overall survival.
#'   \item \strong{Overall survival}: time until death from any cause.
#' }
#'
#' The area under each curve (up to a restriction time) gives the RMST. The difference
#' (overall RMST - event-free RMST) equals the average years spent alive with disease.
#' This decomposition is guaranteed to be non-negative because both curves are estimated
#' from the same cohort and death always occurs after or at the time of disease onset.
#'
#' \strong{Index age conditioning}: Rather than restricting to participants enrolled at a
#' specific age, the function uses left-truncated survival on the age scale. All participants
#' who were alive and disease-free at the index age contribute to the analysis, entering the
#' risk set at max(enrollment age, index age). This maximizes sample size while properly
#' accounting for delayed entry.
#'
#' \strong{Covariate adjustment via IPTW}: When comparing strata that differ on confounders
#' (e.g., BMI, renal function, age), unadjusted RMST comparisons can be misleading. The
#' optional \code{adjust_cols} parameter enables inverse probability of treatment weighting
#' (IPTW). This fits a multinomial logistic regression predicting stratum membership from
#' the specified covariates, then weights each participant by the inverse of their probability
#' of being in their observed stratum. The effect is to create a "pseudo-population" where
#' the covariate distribution is balanced across strata, isolating the stratum effect from
#' confounding. Stabilized weights (multiplied by the marginal probability of each stratum)
#' are used to reduce variance. Extreme weights are truncated at the 1st and 99th percentiles.
#'
#' @param data A data frame containing the survival data
#' @param time_event_col Character string specifying the column name for time to disease event.
#'        For participants without the disease event, this should be their censoring time
#'        (which should be <= time to death).
#' @param time_death_col Character string specifying the column name for time to death
#' @param event_col Character string specifying the column name for disease event indicator
#'        (1 = event occurred, 0 = censored). Note: this should indicate disease onset ONLY,
#'        not a composite of disease + death. The function constructs the composite internally.
#' @param death_col Character string specifying the column name for death indicator
#'        (1 = death, 0 = censored)
#' @param index_age Numeric value specifying the index age for analysis. The function
#'        conditions on being alive and disease-free at this age.
#' @param restriction_time Numeric value specifying the maximum AGE for restricted mean
#'        survival calculation (e.g., 95 means calculate RMST up to age 95).
#'        If NULL, uses maximum observed follow-up time. Default is NULL.
#' @param baseline_age_col Character string specifying the column name for age at enrollment.
#' @param strata_col Optional character string specifying the column name for stratification.
#'        Can be numeric, factor, or character. For factors, maintains level ordering.
#'        For numeric and character variables, sorts in ascending order. Default is NULL.
#' @param adjust_cols Optional character vector specifying column names for IPTW covariate
#'        adjustment. When provided (and strata_col is also provided), a multinomial logistic
#'        regression is fit predicting strata_col from adjust_cols, and stabilized inverse
#'        probability weights are computed. These weights are passed to survfit() so that
#'        each stratum's survival curves reflect a standardized covariate distribution.
#'        Weights are re-estimated within each bootstrap iteration for proper standard errors.
#'        Requires the nnet package (Suggests). Default is NULL (no adjustment).
#' @param n_bootstrap Integer specifying the number of bootstrap samples for standard error
#'        estimation. Default is 1000. Bootstrap streams are seeded from R's current RNG state,
#'        so call \code{set.seed()} beforehand for reproducible standard errors.
#' @param small_delta Numeric value added to zero time values to prevent computation issues.
#'        Default is 0.001.
#' @param maximum_time Optional decimal. Proportion of restriction time window to use.
#'        Default is 1.
#'
#' @return A list with class either "survival_results" (non-stratified) or
#' "stratified_survival_results" (stratified) containing:
#' \itemize{
#'   \item event_free_survival: RMST free of disease (years from index age)
#'   \item overall_survival: RMST overall (years from index age)
#'   \item years_with_event: Mean years lived with disease (overall - event-free)
#'   \item se_event_free: Bootstrap standard error for disease-free survival
#'   \item se_overall: Bootstrap standard error for overall survival
#'   \item se_difference: Bootstrap standard error for years with disease
#'   \item n_observations: Number of subjects in stratum (after index age filtering)
#'   \item n_events: Number of disease events
#'   \item n_deaths: Number of deaths
#'   \item restriction_time: Time window used for RMST (years from index age)
#' }
#'
#' @details
#' \strong{Key methodological points}:
#' \itemize{
#'   \item The event-free endpoint is a COMPOSITE of disease onset and death. This ensures
#'         the event-free survival curve is always <= overall survival, making the difference
#'         (years with disease) non-negative.
#'   \item Left truncation via Surv(entry, exit, event) properly handles participants enrolled
#'         at different ages. A participant enrolled at age 40 with index_age=45 enters the
#'         risk set at age 45, not 40.
#'   \item For IPTW adjustment, weights are computed on the FULL dataset (all strata together),
#'         then applied within each stratum's survfit() call. Bootstrap resamples the full
#'         dataset and re-estimates weights each iteration.
#'   \item RMST values represent years from index_age, not absolute ages. Add index_age to
#'         convert to age scale (e.g., RMST=30 with index_age=45 means expected age ~75).
#' }
#'
#' @examples
#' \dontrun{
#' # Non-stratified analysis
#' results <- calculate_disease_free_survival(
#'   data = my_data,
#'   time_event_col = "time_to_hf",
#'   time_death_col = "time_to_death",
#'   event_col = "hf_event",
#'   death_col = "death_event",
#'   baseline_age_col = "Age",
#'   index_age = 45,
#'   restriction_time = 95
#' )
#' print(results)
#' print(results, ci = TRUE)
#'
#' # Stratified analysis by biomarker group
#' results_strat <- calculate_disease_free_survival(
#'   data = my_data,
#'   time_event_col = "time_to_hf",
#'   time_death_col = "time_to_death",
#'   event_col = "hf_event",
#'   death_col = "death_event",
#'   baseline_age_col = "Age",
#'   index_age = 45,
#'   restriction_time = 95,
#'   strata_col = "np_group"
#' )
#' print(results_strat)
#'
#' # Covariate-adjusted stratified analysis (IPTW)
#' results_adj <- calculate_disease_free_survival(
#'   data = my_data,
#'   time_event_col = "time_to_hf",
#'   time_death_col = "time_to_death",
#'   event_col = "hf_event",
#'   death_col = "death_event",
#'   baseline_age_col = "Age",
#'   index_age = 45,
#'   restriction_time = 95,
#'   strata_col = "np_group",
#'   adjust_cols = c("Age", "BMI", "SCreat")
#' )
#' print(results_adj)
#' }
#'
#' @references
#' Based on restricted mean survival time decomposition methods for illness-death models.
#' IPTW methodology follows Robins, Hernan & Brumback (2000) for marginal structural models.
#'
#' @seealso
#' \code{\link[survival]{Surv}}, \code{\link[survival]{survfit}}, \code{\link[nnet]{multinom}}
#'
#' @importFrom survival Surv survfit
#' @importFrom parallel makeCluster stopCluster detectCores parLapply
#' @importFrom parallel clusterSetRNGStream
#'
#' @export
calculate_disease_free_survival <- function(data,
                                            time_event_col,
                                            time_death_col,
                                            event_col,
                                            death_col,
                                            index_age,
                                            restriction_time = NULL,
                                            baseline_age_col,
                                            strata_col = NULL,
                                            adjust_cols = NULL,
                                            n_bootstrap = 1000,
                                            small_delta = 0.001,
                                            maximum_time = 1) {

  # Validate inputs
  required_cols <- c(time_event_col, time_death_col, event_col, death_col, baseline_age_col)
  if (!all(required_cols %in% names(data))) {
    stop("Missing required columns in data")
  }

  if (!is.null(adjust_cols) && is.null(strata_col)) {
    warning("adjust_cols specified without strata_col. IPTW adjustment requires stratification. Ignoring adjust_cols.")
    adjust_cols <- NULL
  }

  if (!is.null(adjust_cols)) {
    if (!all(adjust_cols %in% names(data))) {
      stop("Missing adjustment columns in data: ",
           paste(setdiff(adjust_cols, names(data)), collapse = ", "))
    }
    if (!requireNamespace("nnet", quietly = TRUE)) {
      stop("Package 'nnet' is required for IPTW adjustment. Install it with install.packages('nnet').")
    }
  }

  # Create a copy of the data
  working_data <- data.frame(data)

  # Columns to keep
  keep_cols <- unique(c(time_event_col, time_death_col, event_col, death_col,
                        baseline_age_col, strata_col, adjust_cols))

  # Remove NA values
  working_data <- na.omit(working_data[, keep_cols])

  # Calculate age at event and age at death for everyone
  working_data$age_at_event <- working_data[[baseline_age_col]] + working_data[[time_event_col]]
  working_data$age_at_death <- working_data[[baseline_age_col]] + working_data[[time_death_col]]

  # Composite event-free time: min of event time and death time
  # A person is "event-free and alive at index_age" if this composite > index_age
  working_data$age_at_composite <- pmin(working_data$age_at_event, working_data$age_at_death)

  # Keep only those who were event-free AND alive at the index age
  working_data <- working_data[working_data$age_at_composite > index_age, ]

  # Left-truncation entry age: max(enrollment age, index_age)
  # People enrolled before index_age enter the risk set at index_age
  working_data$entry_age <- pmax(working_data[[baseline_age_col]], index_age)

  # Shift to time scale starting at index_age
  working_data[[time_event_col]] <- working_data$age_at_event - index_age
  working_data[[time_death_col]] <- working_data$age_at_death - index_age
  working_data$entry_time <- working_data$entry_age - index_age

  message("After recalculating to index age ", index_age, ": ", nrow(working_data), " participants retained")

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
    tau <- max_observed_time
    message("Using maximum observed time (", round(tau, 2), " years)")
  } else {
    tau <- restriction_time - index_age
    if (tau > max_observed_time) {
      message("Note: Restriction time (", round(tau, 2),
              " years) exceeds maximum observed follow-up (",
              round(max_observed_time, 2), " years). ",
              "RMST will be calculated using the area under the survival curve extrapolated to tau.")
    }
    if (tau <= 0) {
      stop("Restriction time (", restriction_time,
           ") must be greater than index age (", index_age, ")")
    }
  }

  tau <- tau * maximum_time

  # ---------------------------------------------------------------------------
  # IPTW weight computation function
  # ---------------------------------------------------------------------------
  compute_iptw_weights <- function(df, strata_col, adjust_cols) {
    # Ensure stratum is a factor for multinomial regression
    df$.strata_factor <- factor(df[[strata_col]])

    # Build formula for multinomial logistic regression
    formula_str <- paste(".strata_factor ~", paste(adjust_cols, collapse = " + "))
    mnom_fit <- nnet::multinom(as.formula(formula_str), data = df, trace = FALSE)

    # Predicted probabilities for each stratum
    pred_probs <- predict(mnom_fit, newdata = df, type = "probs")

    # For binary strata, multinom returns a vector, not a matrix
    if (is.null(dim(pred_probs))) {
      lvls <- levels(df$.strata_factor)
      pred_probs <- cbind(1 - pred_probs, pred_probs)
      colnames(pred_probs) <- lvls
    }

    # Get each person's probability of being in their observed stratum
    obs_stratum <- as.character(df$.strata_factor)
    p_obs <- pred_probs[cbind(seq_len(nrow(df)), match(obs_stratum, colnames(pred_probs)))]

    # Marginal probability of each stratum (for stabilization)
    marginal_probs <- table(df$.strata_factor) / nrow(df)
    p_marginal <- as.numeric(marginal_probs[obs_stratum])

    # Stabilized weights: marginal / predicted
    w <- p_marginal / p_obs

    # Truncate extreme weights at 1st and 99th percentiles
    w <- pmin(w, quantile(w, 0.99))
    w <- pmax(w, quantile(w, 0.01))

    return(w)
  }

  # ---------------------------------------------------------------------------
  # Fast weighted, left-truncated Kaplan-Meier RMST for the bootstrap.
  # Gives the same value as summary(survfit(Surv(entry, exit, event) ~ 1, weights = w),
  # rmean = tau)$table["rmean"], but the sorting is done once per stratum and each
  # bootstrap resample is expressed as per-row frequency weights. Defined locally (not
  # in the namespace) so the functions serialise cleanly to the parallel workers.
  # ---------------------------------------------------------------------------
  km_prep <- function(entry, exit, event, tau) {
    event <- event == 1
    times <- sort(unique(exit[event]))
    times <- times[times <= tau]
    list(
      J = length(times),
      at_risk_idx = findInterval(exit, times),   # at risk at t_j for j <= idx (exit >= t_j)
      entered_idx = findInterval(entry, times),  # not yet entered at t_j for j <= idx (entry >= t_j)
      event_idx = ifelse(event & exit <= tau, match(exit, times), 0L),
      dt = diff(c(0, times, tau))
    )
  }
  km_rmst <- function(prep, w) {
    J <- prep$J
    if (J == 0) return(sum(prep$dt))
    wsum_by <- function(idx) {
      keep <- idx > 0 & w > 0
      out <- numeric(J)
      if (any(keep)) {
        r <- rowsum(w[keep], idx[keep])
        out[as.integer(rownames(r))] <- r[, 1]
      }
      out
    }
    revcumsum <- function(x) rev(cumsum(rev(x)))
    n_risk <- revcumsum(wsum_by(prep$at_risk_idx)) - revcumsum(wsum_by(prep$entered_idx))
    d <- wsum_by(prep$event_idx)
    h <- ifelse(d > 0, d / n_risk, 0)
    sum(prep$dt * c(1, cumprod(1 - h)))
  }

  # Environment for functions sent to the bootstrap workers: its parent is the stats
  # namespace (serialised by reference), so nothing from this package travels with it
  make_worker_env <- function(objects) {
    list2env(objects, envir = new.env(parent = asNamespace("stats")))
  }
  environment(km_prep) <- make_worker_env(list())
  environment(km_rmst) <- make_worker_env(list())
  environment(compute_iptw_weights) <- make_worker_env(list())

  # ---------------------------------------------------------------------------
  # Compute weights on the full dataset if IPTW is requested
  # ---------------------------------------------------------------------------
  if (!is.null(adjust_cols)) {
    message("Computing IPTW weights using: ", paste(adjust_cols, collapse = ", "))
    working_data$iptw_weight <- compute_iptw_weights(working_data, strata_col, adjust_cols)
    message("Weight summary: min=", round(min(working_data$iptw_weight), 3),
            ", median=", round(median(working_data$iptw_weight), 3),
            ", max=", round(max(working_data$iptw_weight), 3))
  } else {
    working_data$iptw_weight <- 1
  }

  # ---------------------------------------------------------------------------
  # Function to analyze one stratum
  # ---------------------------------------------------------------------------
  analyze_stratum <- function(data, tau, n_bootstrap, full_data = NULL,
                              use_iptw = FALSE) {
    # Initial fits using left-truncated survival
    # Composite event-free: event = disease or death, whichever comes first
    time_composite <- pmin(data[[time_event_col]], data[[time_death_col]])
    composite_event <- as.numeric(data[[event_col]] == 1 | data[[death_col]] == 1)

    w <- data$iptw_weight

    # se.fit = FALSE: the survfit SEs are unused (SEs come from the bootstrap), and with
    # non-integer IPTW weights computing them is very slow
    fit_event <- survfit(Surv(data$entry_time, time_composite, composite_event) ~ 1,
                         weights = w, se.fit = FALSE)
    fit_overall <- survfit(Surv(data$entry_time, data[[time_death_col]], data[[death_col]]) ~ 1,
                           weights = w, se.fit = FALSE)

    # Calculate initial RMSTs
    rmst_event <- summary(fit_event, rmean = tau)$table["rmean"]
    rmst_overall <- summary(fit_overall, rmean = tau)$table["rmean"]

    # Bootstrap calculations use the cluster `cl` created once in the enclosing function

    # Determine which stratum this is
    this_stratum <- NULL
    if (!is.null(strata_col) && strata_col %in% names(data)) {
      this_stratum <- as.character(data[[strata_col]][1])
    }

    # Precompute the sorted risk-set structure for this stratum's rows once
    prep_event <- km_prep(data$entry_time, time_composite, composite_event, tau)
    prep_overall <- km_prep(data$entry_time, data[[time_death_col]], data[[death_col]], tau)
    n_rows <- nrow(data)
    stratum_pos <- NULL
    if (use_iptw && !is.null(full_data)) {
      # Positions of this stratum's rows within full_data (same order as `data`)
      stratum_pos <- which(as.character(full_data[[strata_col]]) == this_stratum)
    }

    # Bootstrap function: a resample is represented as frequency weights on the original rows
    boot_fun <- function(i) {
      if (use_iptw && !is.null(full_data)) {
        # Resample the FULL dataset, re-estimate weights, then keep this stratum's rows.
        # Duplicated rows share covariates and so share a weight, so summing weights by
        # original row is exactly equivalent to analysing the resampled rows.
        boot_idx <- sample(nrow(full_data), replace = TRUE)
        boot_full <- full_data[boot_idx, ]
        boot_w <- compute_iptw_weights(boot_full, strata_col, adjust_cols)
        w_full <- numeric(nrow(full_data))
        agg <- rowsum(boot_w, boot_idx)
        w_full[as.integer(rownames(agg))] <- agg[, 1]
        w_boot <- w_full[stratum_pos]
        n_boot <- sum(as.character(boot_full[[strata_col]]) == this_stratum)
      } else {
        # Resample within stratum
        indices <- sample(n_rows, replace = TRUE)
        w_boot <- w * tabulate(indices, nbins = n_rows)
        n_boot <- n_rows
      }

      if (n_boot < 5) return(c(NA, NA))

      c(km_rmst(prep_event, w_boot), km_rmst(prep_overall, w_boot))
    }

    # Ship only what boot_fun needs. Without this, the closure drags along every enclosing
    # frame (including the full input data) and the package namespace, which forces each
    # worker to load lifetimeRisk and all of its imports.
    environment(boot_fun) <- make_worker_env(list(
      use_iptw = use_iptw, full_data = if (use_iptw) full_data else NULL,
      strata_col = strata_col, adjust_cols = adjust_cols, this_stratum = this_stratum,
      stratum_pos = stratum_pos, n_rows = n_rows, w = w,
      prep_event = prep_event, prep_overall = prep_overall,
      km_rmst = km_rmst, compute_iptw_weights = compute_iptw_weights
    ))

    # Run bootstrap. Seed the worker RNG streams from the caller's RNG state so that
    # set.seed() before calling this function gives reproducible SEs (seeding the master
    # process alone has no effect on PSOCK workers).
    clusterSetRNGStream(cl, iseed = sample.int(.Machine$integer.max, 1))
    boot_results <- parLapply(cl, seq_len(n_bootstrap), boot_fun)
    boot_matrix <- do.call(rbind, boot_results)

    # Remove failed bootstrap iterations
    boot_matrix <- boot_matrix[complete.cases(boot_matrix), , drop = FALSE]
    if (nrow(boot_matrix) < 10) {
      warning("Fewer than 10 successful bootstrap iterations. SEs may be unreliable.")
    }

    # Calculate standard errors from bootstrap
    se_event <- sd(boot_matrix[, 1])
    se_overall <- sd(boot_matrix[, 2])
    se_difference <- sd(boot_matrix[, 2] - boot_matrix[, 1])

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

  # ---------------------------------------------------------------------------
  # Handle stratification
  # ---------------------------------------------------------------------------
  use_iptw <- !is.null(adjust_cols)

  # Start the bootstrap workers once and reuse them for every stratum
  n_cores <- max(1, min(detectCores() - 1, n_bootstrap, na.rm = TRUE))
  cl <- makeCluster(n_cores)
  on.exit(stopCluster(cl), add = TRUE)

  if (!is.null(strata_col)) {
    # Get unique values from strata column
    strata_values <- working_data[[strata_col]]

    # Handle different types
    if (is.factor(strata_values)) {
      strata_levels <- levels(droplevels(strata_values))
    } else {
      strata_levels <- sort(unique(strata_values))
    }

    results_list <- list()
    for (level in strata_levels) {
      message("\nAnalyzing stratum: ", level)
      stratum_data <- working_data[as.character(working_data[[strata_col]]) == as.character(level), ]
      results_list[[as.character(level)]] <- analyze_stratum(
        stratum_data, tau, n_bootstrap,
        full_data = if (use_iptw) working_data else NULL,
        use_iptw = use_iptw
      )
    }

    results <- list(results = results_list, adjusted = use_iptw,
                    adjust_cols = adjust_cols)
    class(results) <- "stratified_survival_results"
  } else {
    results <- analyze_stratum(working_data, tau, n_bootstrap, use_iptw = FALSE)
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
#' @method print survival_results
#' @export
print.survival_results <- function(x, digits = 2, ci = FALSE, ...) {
  format_estimate <- function(est, se) {
    f <- function(z) formatC(z, format = "f", digits = digits)
    if (ci) {
      paste0(f(est), " (", f(est - 1.96 * se), ", ", f(est + 1.96 * se), ")")
    } else {
      paste0(f(est), " (", f(se), ")")
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
  invisible(x)
}

#' Print Method for Stratified Survival Results
#'
#' @description
#' Prints a formatted summary table of stratified disease-free survival analysis results.
#' Notes whether IPTW adjustment was applied and which covariates were used.
#'
#' @param x An object of class "stratified_survival_results"
#' @param digits Number of decimal places for rounding. Default is 2.
#' @param ci Logical; if TRUE, displays 95% confidence intervals instead of
#'        standard errors. Default is FALSE.
#' @param ... Additional arguments passed to print
#'
#' @method print stratified_survival_results
#' @export
print.stratified_survival_results <- function(x, digits = 2, ci = FALSE, ...) {
  format_estimate <- function(est, se) {
    f <- function(z) formatC(z, format = "f", digits = digits)
    if (ci) {
      paste0(f(est), " (", f(est - 1.96 * se), ", ", f(est + 1.96 * se), ")")
    } else {
      paste0(f(est), " (", f(se), ")")
    }
  }

  if (!is.null(x$adjusted) && x$adjusted) {
    cat("IPTW-adjusted analysis (covariates:", paste(x$adjust_cols, collapse = ", "), ")\n\n")
  }

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
  invisible(x)
}
