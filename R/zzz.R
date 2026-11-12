.datatable.aware = TRUE

#' @importFrom stats as.formula complete.cases confint lm median na.omit predict
#' @importFrom stats quantile sd smooth.spline
#' @importFrom utils tail write.csv
NULL

# Column names used via data.table / ggplot2 non-standard evaluation
utils::globalVariables(c(
  ".", "age", "age_group", "astatus", "biomarker_midpoint", "entryage", "estimate",
  "ftime", "full", "group", "ids", "lcl", "risk_estimate", "risk_lcl", "risk_ucl",
  "start", "status", "strata_factor", "survage", "target_age_factor", "ucl", "weight"
))
