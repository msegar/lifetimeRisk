## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(lifetimeRisk)

## -----------------------------------------------------------------------------
set.seed(123)
n <- 1000
sample_data <- data.frame(
  ids = 1:n,
  entryage = round(runif(n, min = 45, max = 65)),
  followup_time = round(runif(n, min = 2, max = 20)),
  status = rbinom(n, 1, 0.3),
  astatus = rbinom(n, 1, 0.2),
  sex = sample(c("Male", "Female"), n, replace = TRUE),
  race = sample(c("White", "Black", "Hispanic", "Asian"), n, replace = TRUE, 
                prob = c(0.6, 0.2, 0.15, 0.05))
)
sample_data$survage <- sample_data$entryage + sample_data$followup_time
sample_data$status <- ifelse(sample_data$astatus == 1, 0, sample_data$status)
sample_data <- sample_data[, c("ids", "entryage", "survage", "status", "astatus", "sex", "race")]

## -----------------------------------------------------------------------------
result <- pie_analysis(
  data = sample_data,
  min_age = 45,
  max_age = 90,
  age_group_width = 5,
  group_var = "sex",
  group_levels = c("Male", "Female"),
  age_free = 45
)
summary(result)

## ----fig.width=7, fig.height=5------------------------------------------------
plot_lifetime_risk(result, adjusted = TRUE, label_names = c("Male" = "Men", "Female" = "Women"))

## ----eval=FALSE---------------------------------------------------------------
# create_lifetime_csv(result, adjusted = TRUE, output_file = "lifetime_risk_sex.csv")

## -----------------------------------------------------------------------------
lt <- create_lifetime_risk_table(
  data = sample_data,
  index_ages = c(45, 55, 65),
  max_age = 90,
  group_var = "sex"
)
print(lt)

