#' @import data.table
NULL

#' Calculate Age-Specific Incidence Rates
#'
#' Computes age-specific incidence rates from a person-year dataset, grouping ages into intervals.
#'
#' @param data Person-year dataset (data.frame or data.table) with columns: age, status, weight
#' @param age_group_width Width of age groups (e.g., 5 for 5-year intervals)
#' @param min_age Minimum age to consider (start of first group)
#' @return A data.table with columns: age_min, age_max, events, person_years, rate
#' @details
#' The function groups ages into intervals of width `age_group_width` starting at `min_age`,
#' and calculates the number of events, person-years, and incidence rate per 1000 person-years for each group.
#' A total row is added (age_group = 88) summarizing all ages.
#' @examples
#' py <- create_person_year_data(test_data, 50, 80)
#' calculate_age_specific_rates(py, 5, 50)
#' @export
calculate_age_specific_rates <- function(data, age_group_width, min_age) {
  dt <- as.data.table(data)
  if (nrow(dt) == 0) {
    return(dt[0, .(age_min = integer(), age_max = integer(), events = integer(), person_years = numeric(), rate = numeric())])
  }
  # Calculate age groups correctly
  dt[, age_group := min_age + (floor((age - min_age) / age_group_width) * age_group_width)]
  # Calculate rates by age group
  rates <- dt[, .(
    age_min = unique(age_group),
    age_max = unique(age_group) + age_group_width - 1,
    events = sum(status),
    person_years = sum(weight),
    rate = 1000 * sum(status) / sum(weight)
  ), by = age_group][order(age_group)]
  # Add total row (equivalent to AGEGROUP = 88 in SAS) only if data is not empty
  total <- dt[, .(
    age_group = 88,
    age_min = min_age,
    age_max = max(age),
    events = sum(status),
    person_years = sum(weight),
    rate = 1000 * sum(status) / sum(weight)
  )]
  rates <- rbindlist(list(rates, total))
  return(rates[])
}

#' Create person-year dataset for analysis
#'
#' Expands input data to a person-year format, with one row per person-year at risk.
#'
#' @param data Input dataset (data.frame or data.table) with columns: ids, entryage, survage, status, astatus, group
#' @param min_age Minimum age to consider
#' @param max_age Maximum age to consider
#' @param debug If TRUE, save intermediate datasets to global environment (for debugging)
#' @return A data.table with columns: ids, age, status, astatus, weight, group
#' @details
#' For each individual, creates rows for each year at risk between entryage and survage (capped at max_age).
#' Handles partial years and sets weights/status according to SAS PIE macro logic.
#' @examples
#' create_person_year_data(test_data, 50, 80)
#' @export
create_person_year_data <- function(data, min_age, max_age, debug = FALSE) {
  message("Creating person-year dataset...")

  # Convert to data.table
  dt <- as.data.table(copy(data))
  required_cols <- c("ids", "entryage", "survage", "status", "astatus", "group")
  if (!all(required_cols %in% names(dt))) {
    stop("Input data must contain columns: ", paste(required_cols, collapse = ", "))
  }
  if (nrow(dt) == 0) {
    return(dt[0, .(ids = integer(), age = integer(), status = integer(), astatus = integer(), weight = numeric(), group = character())])
  }

  # Handle max age exactly as SAS does
  dt[, `:=`(
    full = ifelse(survage > max_age, 1, 0),
    survage = pmin(floor(survage), max_age),
    start = pmax(entryage, min_age)
  )]

  # Set status and weight according to SAS rules
  dt[full == 1, `:=`(
    status = 0,
    astatus = 0
  )]

  dt[, weight := ifelse(status == 0 & survage < max_age & full != 1, 0.5, 1)]

  # Create final year records
  message("Creating final year records...")
  final_records <- dt[, .(
    ids = ids,
    age = survage,
    status = status,
    astatus = astatus,
    weight = weight,
    group = group,
    start = start
  )]

  final_records <- final_records[age >= start]

  # Create intermediate records
  message("Creating intermediate records...")
  intermediate_records <- dt[survage > start, {
    ages <- seq(start, survage - 1)
    .(
      age = ages,
      status = 0L,
      astatus = 0L,
      weight = 1.0,
      group = rep(group, length(ages)),
      start = rep(start, length(ages))
    )
  }, by = ids]

  # Combine all records
  message("Combining records...")
  all_records <- rbindlist(list(final_records, intermediate_records),
                           use.names = TRUE, fill = TRUE)
  setorder(all_records, ids, age)
  all_records[, start := NULL]

  # Remove rows with NA
  all_records <- na.omit(all_records)

  if(debug) {
    assign("PDS", all_records, envir = .GlobalEnv)
  }

  return(all_records[])
}

#' Create summary dataset (equivalent to SDSMAC)
#'
#' Summarizes person-year data by age and group, including total and 90+ rows.
#'
#' @param data Person-year dataset
#' @param group_var Group variable name (string)
#' @param level Group level(s) to include
#' @param dataset_name Name to save dataset as when debugging
#' @param debug If TRUE, save summary dataset to global environment
#' @return A data.table with columns: age, R (at risk), E (events), C (competing), W (weight)
#' @details
#' Produces summary statistics by age, with special rows for age 90+ and total (age=999).
#' @examples
#' py <- create_person_year_data(test_data, 50, 80)
#' create_summary_dataset(py, group_var = "group", level = "A")
#' @export
create_summary_dataset <- function(data, group_var, level, dataset_name = NULL, debug = FALSE) {
  dt <- as.data.table(data)

  # First, get summaries by age excluding age 90
  summary_data <- dt[get(group_var) %in% level & age < 90,
                     .(R = .N,
                       E = sum(status),
                       C = sum(astatus),
                       W = sum(weight)),
                     by = age]

  # Separate summary for age 90 (including all ages >= 90)
  age_90_plus <- dt[get(group_var) %in% level & age >= 90,
                    .(age = 90L,
                      R = .N,
                      E = sum(status),
                      C = sum(astatus),
                      W = sum(weight))]

  # Add total row (age = 999)
  total_row <- dt[get(group_var) %in% level,
                  .(age = 999L,
                    R = .N,
                    E = sum(status),
                    C = sum(astatus),
                    W = sum(weight))]

  # Combine all parts and sort
  summary_data <- rbindlist(list(summary_data, age_90_plus, total_row))
  setorder(summary_data, age)

  if(debug && !is.null(dataset_name)) {
    assign(dataset_name, summary_data, envir = .GlobalEnv)
  }

  return(summary_data)
}
#' Calculate cumulative incidence with intermediate datasets
#'
#' Computes cumulative incidence and confidence intervals, with and without competing risk adjustment.
#'
#' @param data Person-year dataset
#' @param age_free Starting age for analysis
#' @param debug If TRUE, save intermediate datasets to global environment
#' @return A list with two data.frames: unadjusted and adjusted cumulative incidence (columns: age, est, lcl, ucl)
#' @details
#' Follows the SAS PIE macro algorithm for cumulative incidence and standard error calculation.
#' Returns both unadjusted and competing risk-adjusted estimates.
#' @examples
#' py <- create_person_year_data(test_data, 50, 80)
#' calculate_cumulative_incidence(py, 50)
#' @export
calculate_cumulative_incidence <- function(data, age_free, debug = FALSE) {
  if (nrow(data) == 0) return(NULL)

  # Create time scale starting at age_free
  data <- as.data.table(data)
  data[, ftime := age - age_free]
  setorder(data, ftime)

  # Risk table calculations
  risk_table <- data[, .(
    R = .N,
    E = sum(status),
    C = sum(astatus)
  ), by = ftime]

  if(debug) {
    assign("TEMP", copy(risk_table), envir = .GlobalEnv)
  }

  # Get unique time points
  times <- unique(data$ftime)
  n_times <- length(times)

  # Initialize vectors for calculations
  cs <- rep(1, n_times)  # Current survival
  csa <- rep(1, n_times)  # Survival adjusted for competing risk
  cf <- rep(0, n_times)  # Cumulative incidence
  cfa <- rep(0, n_times)  # Cumulative competing incidence
  cfstar <- rep(0, n_times)  # Cumulative incidence adjusted for competing risk

  # SE calculation vectors
  cvlp <- 0  # Cumulative variance for log product
  v <- 0     # Variance
  cov <- 0   # Covariance
  a <- rep(0, n_times)  # A array from SAS
  b <- rep(0, n_times)  # B array from SAS
  fstara <- rep(0, n_times)  # FSTAR array from SAS

  # Calculate at risk, events, and competing events at each time
  risk_table <- data[, .(
    R = .N,  # Number at risk
    E = sum(status),  # Primary events
    C = sum(astatus)  # Competing events
  ), by = ftime]

  # Calculate cumulative incidence and SE following SAS algorithm
  for (i in seq_len(n_times)) {
    if (i > 1) {
      cs[i] <- cs[i-1]
      csa[i] <- csa[i-1]
      cf[i] <- cf[i-1]
      cfa[i] <- cfa[i-1]
      cfstar[i] <- cfstar[i-1]
    }

    rt <- risk_table[i]
    if (rt$R > 0) {
      # Main calculations
      H <- rt$E / rt$R  # Hazard rate
      HA <- rt$C / rt$R  # Competing hazard rate

      F <- H * cs[i]  # Incidence
      FA <- HA * csa[i]  # Competing event incidence
      FSTAR <- H * csa[i]  # Adjusted incidence

      # Update cumulative values
      cf[i] <- cf[i] + F
      cs[i] <- 1 - cf[i]
      cfa[i] <- cfa[i] + FA
      csa[i] <- 1 - cfa[i]
      cfstar[i] <- cfstar[i] + FSTAR

      # Standard error calculations
      if (rt$E > 0) {
        cvlp <- cvlp + rt$E / (rt$R * (rt$R - rt$E))
      }

      # A and FSTAR arrays
      a[i] <- -1/rt$R
      fstara[i] <- FSTAR

      # B array calculations
      if (i == 1) {
        b[i] <- ifelse(rt$R > rt$C, rt$C / (rt$R * (rt$R - rt$C)), 0)
      } else {
        b[i] <- b[i-1] + ifelse(rt$R > rt$C, rt$C / (rt$R * (rt$R - rt$C)), 0)
      }

      # Covariance calculations
      if (i >= 3) {
        cc <- 0
        for (k in 2:(i-1)) {
          cc <- cc + (fstara[k] * (a[k] + b[k-1]))
        }
        cov <- cov + FSTAR * cc
      }

      # Variance calculations
      bb <- if (i > 1) b[i-1] else 0
      if (rt$E > 0) {
        v <- v + (FSTAR^2) * (((rt$R - rt$E)/(rt$E * rt$R)) + bb)
      }
    }
  }

  # Calculate standard errors
  se_cf <- sqrt((cs^2) * cvlp)
  se_cfstar <- sqrt(v + 2*cov)

  # Create output data frames with confidence intervals and convert time to age
  result <- list(
    unadjusted = data.frame(
      age = times + age_free,  # Convert time to actual age
      est = cf * 100,
      lcl = pmax(0, (cf - 1.96 * se_cf) * 100),
      ucl = pmin(100, (cf + 1.96 * se_cf) * 100)
    ),
    adjusted = data.frame(
      age = times + age_free,  # Convert time to actual age
      est = cfstar * 100,
      lcl = pmax(0, (cfstar - 1.96 * se_cfstar) * 100),
      ucl = pmin(100, (cfstar + 1.96 * se_cfstar) * 100)
    )
  )

  # Save intermediate results if debugging
  if(debug) {
    assign("TEMP1", data.table(
      time = times,
      cs = cs,
      csa = csa,
      cf = cf,
      cfa = cfa,
      cfstar = cfstar
    ), envir = .GlobalEnv)
  }

  return(result)
}
