# lr.R

#' Calculate Cumulative Incidence
#'
#' This function calculates unadjusted cumulative incidence and cumulative incidence adjusted for competing risk,
#' similar to the LR macro in SAS.
#'
#' @param data A data frame containing the input data
#' @param age_free Minimum age to consider
#' @param max_age Maximum age to consider
#' @param age_group_width Width of age groups
#' @param verbose Logical, if TRUE, print detailed output. Default is FALSE.
#'
#' @return A list containing cumulative incidence data and plots
#' @export
#'
#' @importFrom dplyr filter mutate arrange
#' @importFrom tidyr pivot_longer
calculate_lr <- function(data, age_free, max_age, age_group_width, verbose = FALSE) {
  # Filter data for the specified age range
  out <- data %>%
    filter(age >= age_free, age <= max_age) %>%
    arrange(age)

  # Initialize variables
  n <- nrow(out)
  cs <- cvlp <- cfa <- csa <- cov <- v <- cf <- cfstar <- numeric(n)
  a <- b <- fstara <- matrix(0, nrow = n, ncol = max_age - age_free + 1)

  # Main calculation loop
  for (i in 1:n) {
    age <- out$age[i]
    r <- out$r[i]
    e <- out$e[i]
    c <- out$c[i]
    index <- age - age_free + 1

    if (i == 1) {
      cs[i] <- 1
      cvlp[i] <- 0
      cfa[i] <- 0
      csa[i] <- 1
      cov[i] <- 0
      v[i] <- 0
      cf[i] <- 0
      cfstar[i] <- 0
    } else {
      cs[i] <- cs[i-1]
      cvlp[i] <- cvlp[i-1]
      cfa[i] <- cfa[i-1]
      csa[i] <- csa[i-1]
      cov[i] <- cov[i-1]
      v[i] <- v[i-1]
      cf[i] <- cf[i-1]
      cfstar[i] <- cfstar[i-1]
    }

    # Survival estimates
    h <- e / r
    ha <- c / r
    f <- h * cs[i]
    cf[i] <- cf[i] + f
    cs[i] <- 1 - cf[i]
    fa <- ha * csa[i]
    fstar <- h * csa[i]
    cfa[i] <- cfa[i] + fa
    csa[i] <- 1 - cfa[i]
    cfstar[i] <- cfstar[i] + fstar

    # Standard errors
    cvlp[i] <- cvlp[i] + e / (r * (r - e))
    if (age >= age_free) {
      a[i, index] <- -1 / r
      fstara[i, index] <- fstar
    }
    if (age == age_free) {
      b[i, 1] <- c / (r * (r - c))
    } else if (age > age_free) {
      b[i, index] <- b[i-1, index-1] + (c / (r * (r - c)))
    }

    cc <- 0
    if (age >= (age_free + 2)) {
      for (k in 2:(index-1)) {
        cc <- cc + (fstara[i, k] * (a[i, k] + b[i, k-1]))
      }
    }
    cov[i] <- cov[i] + fstar * cc

    bb <- if (age >= (age_free + 1)) b[i, index-1] else 0
    if (e > 0) v[i] <- v[i] + (fstar^2) * (((r - e) / (e * r)) + bb)
  }

  # Calculate standard errors
  secf <- sqrt((cs^2) * cvlp)
  secff <- sqrt(v + 2*cov)

  # Prepare output data
  result <- out %>%
    mutate(
      age = age,  # This is now the actual age, not years since age_free
      cf = cf * 100,
      secf = secf * 100,
      cfstar = cfstar * 100,
      secff = secff * 100,
      lcl = pmax(0, cf - 1.96 * secf),
      ucl = pmin(100, cf + 1.96 * secf),
      lclstar = pmax(0, cfstar - 1.96 * secff),
      uclstar = pmin(100, cfstar + 1.96 * secff)
    ) %>%
    select(age, cf, secf, cfstar, secff, lcl, ucl, lclstar, uclstar)

  # Print results if verbose is TRUE
  if (verbose) {
    print_lr_results(result, age_free, age_group_width)
  }

  return(result)
}

#' Print LR Results
#'
#' This function prints the results of the LR calculation in a more readable format.
#'
#' @param data A data frame containing the LR calculation results
#' @param age_free Minimum age to consider
#' @param age_group_width Width of age groups
#'
#' @importFrom dplyr filter mutate
#' @importFrom knitr kable
print_lr_results <- function(data, age_free, age_group_width) {
  print_data <- data %>%
    filter((age - age_free) %% age_group_width == 0) %>%
    mutate(
      `Unadjusted Risk (%)` = sprintf("%.2f (%.2f - %.2f)", cf, lcl, ucl),
      `Adjusted Risk (%)` = sprintf("%.2f (%.2f - %.2f)", cfstar, lclstar, uclstar)
    ) %>%
    select(Age = age, `Unadjusted Risk (%)`, `Adjusted Risk (%)`)

  cat("\nLifetime Risk Estimates at Different Ages\n")
  cat("(with 95% Confidence Intervals)\n\n")
  print(kable(print_data, format = "pipe", digits = 2))
}

#' Summarize Lifetime Risk
#'
#' This function summarizes the key lifetime risk measurements.
#'
#' @param data A data frame containing the LR calculation results
#'
#' @return A string containing the summary
#' @importFrom dplyr last
summarize_lifetime_risk <- function(data) {
  final_row <- data[nrow(data), ]

  summary <- sprintf(
    "Unadjusted Lifetime Risk: %.2f%% (95%% CI: %.2f%% - %.2f%%)\nAdjusted Lifetime Risk:   %.2f%% (95%% CI: %.2f%% - %.2f%%)",
    final_row$cf, final_row$lcl, final_row$ucl,
    final_row$cfstar, final_row$lclstar, final_row$uclstar
  )

  return(summary)
}

#' Generate LR Plot
#'
#' This function generates a plot of the LR calculation results.
#'
#' @param data A data frame containing the LR calculation results
#' @param title Title for the plot
#'
#' @return A ggplot object
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon theme_minimal labs scale_color_manual
generate_lr_plot <- function(data, title) {
  ggplot(data, aes(x = age)) +  # Changed from 'years' to 'age'
    geom_line(aes(y = cf, color = "Unadjusted")) +
    geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.2) +
    geom_line(aes(y = cfstar, color = "Adjusted")) +
    geom_ribbon(aes(ymin = lclstar, ymax = uclstar), alpha = 0.2) +
    theme_minimal() +
    labs(title = title,
         x = "Age",  # Changed from "Years" to "Age"
         y = "Cumulative Incidence (%)",
         color = "Type") +
    scale_color_manual(values = c("Unadjusted" = "blue", "Adjusted" = "red"))
}
