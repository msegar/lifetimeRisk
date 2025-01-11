
# lifetimeRisk

<div>

<img src="lifetime_risk_sticker.png" width="150px" align="right">

</div>

Calculate lifetime risk of adverse outcomes accounting for competing
risks.

## Overview

`lifetimeRisk` implements statistical methods for calculating lifetime
risk estimates from survival data, with proper handling of competing
risks. The package follows methodologies established in SAS macros for
epidemiological research, ensuring accurate:

- Person-year calculations

- Age-specific incidence rates

- Cumulative incidence with competing risks

- Age-adjusted rates

## Installation

You can install the development version of `lifetimeRisk` from
[GitHub](https://github.com/msegar/lifetimeRisk) with:

``` r
# install.packages("remotes")
remotes::install_github("msegar/lifetimeRisk")
```

## Usage

``` r
library(lifetimeRisk)
library(data.table)
```

### Input Data Format

Your data should contain:

- `ids`: Unique identifier for each individual

- `entryage`: Age at which the individual entered the study

- `survage`: Age at which the individual was last observed

- `status`: Primary event indicator (1 if occurred, 0 if not)

- `astatus`: Competing event indicator (1 if occurred, 0 if not)

- `group`: Group identifier for comparisons

### Example Analysis

``` r
# Create sample data
set.seed(123)
n <- 1000

# Create the base data
sample_data <- data.frame(
  ids = 1:n,
  entryage = round(runif(n, min = 45, max = 65)),
  followup_time = round(runif(n, min = 2, max = 20)),
  status = rbinom(n, 1, 0.3),
  astatus = rbinom(n, 1, 0.2),
  group = rbinom(n, 1, 0.5)  # Two groups (0 and 1)
)

# Add calculated fields
sample_data$survage <- sample_data$entryage + sample_data$followup_time
sample_data$status <- ifelse(sample_data$astatus == 1, 0, sample_data$status)

# Select final columns
sample_data <- sample_data[, c("ids", "entryage", "survage", "status", "astatus", "group")]

# Basic analysis
result <- pie_analysis(
  data = sample_data,
  min_age = 45,
  max_age = 90,
  age_group_width = 5,
  group_var = "group",
  group1 = 0,
  group2 = 1,
  age_free = 45
)
```

``` r
summary(result)
#> Summary of Person-Year and Lifetime Risk Analysis
#> ===============================================
#> 
#> Analysis Parameters:
#> -----------------------
#> Age range: 45 to 90 years
#> Age group width: 5 years
#> Starting age for lifetime risk: 45 years
#> 
#> Results for Group 0 :
#> -----------------------
#> Final cumulative incidence: 82.2% (70.0%, 94.4%)
#> 
#> Results for Group 1 :
#> -----------------------
#> Final cumulative incidence: 83.5% (67.4%, 99.5%)
#> 
#> Note: Values shown as estimate (95% CI)
```

### Key Functions

- `pie_analysis()`: Main analysis function for calculating lifetime risk
- `create_lifetime_risk_table()`: Generate lifetime risk estimates from
  multiple starting ages
- `print()` and `summary()` methods for reviewing results

### Features

- Proper handling of late-age observations
- Accurate competing risk adjustments
- Age-specific and age-adjusted rates
- Confidence interval calculations
- Flexible group comparisons

## Citation

If you use `lifetimeRisk` in your research, please cite:

\[Citation information here\]

## Contributing

Please report any issues or feature requests on the GitHub issues page.
