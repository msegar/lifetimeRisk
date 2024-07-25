# nocov start

# Global vars ------------------------------------------------------------------

utils::globalVariables(
  c(
    # calculate_aa2g.R
    "events", "wpy", "twpy", "wtr", "agegroup", "atwpy", "gtwpy", "wtwpy",
    # calculate_incidence.R
    "age", "agegroup", "e", "w", "events", "wpy",
    # calculate_lr.R
    "lcl", "ucl", "lclstar", "uclstar",
    # create_summary_dataset.R
    "age", "status", "astatus", "weight",
    # generate_lr_plot
    "age", "cf", "lcl", "ucl", "cfstar", "lclstar", "uclstar",
    # preprocess_data
    "survage", "status", "astatus", "full", "entryage", "age", "start", "ids", "weight", "head",
    # print_lr_data
    "age", "cfstar", "lclstar", "uclstar", "cf", "lcl", "ucl",
    # print_lr_results
    "age", "cf", "lcl", "ucl", "cfstar", "lclstar", "uclstar", "Unadjusted Risk (%)", "Adjusted Risk (%)"
  )
)

# nocov end
