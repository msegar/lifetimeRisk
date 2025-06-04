# lifetimeRisk
<div>
<img src="lifetime_risk_sticker.png" width="150px" align="right">
</div>

Calculate lifetime risk of adverse outcomes accounting for competing risks.

## Overview

`lifetimeRisk` implements statistical methods for calculating lifetime risk estimates from survival data, with proper handling of competing risks. The package follows methodologies established in SAS macros for epidemiological research, ensuring accurate:

- Person-year calculations
- Age-specific incidence rates  
- Cumulative incidence with competing risks
- Age-adjusted rates
- **Flexible group analysis** - analyze overall population or multiple subgroups
- **Scalable comparisons** - handle any number of groups, not just pairwise

## Installation

You can install the development version of `lifetimeRisk` from [GitHub](https://github.com/msegar/lifetimeRisk) with:

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
- Grouping variables (optional): Any variables for subgroup analysis

### Example Analysis

``` r
# Create sample data
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

# Add calculated fields
sample_data$survage <- sample_data$entryage + sample_data$followup_time
sample_data$status <- ifelse(sample_data$astatus == 1, 0, sample_data$status)

# Select final columns
sample_data <- sample_data[, c("ids", "entryage", "survage", "status", "astatus", "sex", "race")]
```

### Overall Population Analysis

``` r
# Analyze entire population (no grouping)
result_overall <- pie_analysis(
  data = sample_data,
  min_age = 45,
  max_age = 90,
  age_group_width = 5,
  group_var = NULL,  # No grouping
  age_free = 45
)

summary(result_overall)
#> Summary of Person-Year and Lifetime Risk Analysis
#> ===============================================
#> 
#> Analysis Parameters:
#> -----------------------
#> Age range: 45 to 90 years
#> Age group width: 5 years
#> Starting age for lifetime risk: 45 years
#> Analysis type: Overall population
#> 
#> Results for Group overall :
#> -----------------------
#> Final cumulative incidence (unadjusted): 82.8% (77.1%, 88.5%)
#> Final cumulative incidence (adjusted): 79.2% (73.8%, 84.6%)
#> 
#> Note: Values shown as estimate (95% CI)
```

### Two-Group Analysis

``` r
# Compare two groups (sex)
result_sex <- pie_analysis(
  data = sample_data,
  min_age = 45,
  max_age = 90,
  age_group_width = 5,
  group_var = "sex",
  group_levels = c("Male", "Female"),  # Specify which groups
  age_free = 45
)

summary(result_sex)
#> Summary of Person-Year and Lifetime Risk Analysis
#> ===============================================
#> 
#> Analysis Parameters:
#> -----------------------
#> Age range: 45 to 90 years
#> Age group width: 5 years
#> Starting age for lifetime risk: 45 years
#> Group variable: sex
#> Number of groups: 2
#> 
#> Results for Group Male :
#> -----------------------
#> Final cumulative incidence (unadjusted): 81.5% (74.2%, 88.8%)
#> Final cumulative incidence (adjusted): 77.8% (70.9%, 84.7%)
#> 
#> Results for Group Female :
#> -----------------------
#> Final cumulative incidence (unadjusted): 84.1% (76.8%, 91.4%)
#> Final cumulative incidence (adjusted): 80.6% (73.7%, 87.5%)
#> 
#> Note: Values shown as estimate (95% CI)
```

### Multiple Group Analysis

``` r
# Analyze all racial groups (auto-detect all levels)
result_race <- pie_analysis(
  data = sample_data,
  min_age = 45,
  max_age = 90,
  age_group_width = 5,
  group_var = "race",
  group_levels = NULL,  # Auto-detect all levels
  age_free = 45
)

summary(result_race)
#> Summary of Person-Year and Lifetime Risk Analysis
#> ===============================================
#> 
#> Analysis Parameters:
#> -----------------------
#> Age range: 45 to 90 years
#> Age group width: 5 years
#> Starting age for lifetime risk: 45 years
#> Group variable: race
#> Number of groups: 4
#> 
#> Results for Group Asian :
#> -----------------------
#> Final cumulative incidence (unadjusted): 78.9% (58.3%, 99.5%)
#> 
#> Results for Group Black :
#> -----------------------
#> Final cumulative incidence (unadjusted): 85.2% (76.1%, 94.3%)
#> 
#> Results for Group Hispanic :
#> -----------------------
#> Final cumulative incidence (unadjusted): 79.4% (67.8%, 91.0%)
#> 
#> Results for Group White :
#> -----------------------
#> Final cumulative incidence (unadjusted): 83.1% (78.2%, 88.0%)
#> 
#> Note: Values shown as estimate (95% CI)
```

### Lifetime Risk Tables

``` r
# Create lifetime risk table for overall population
lt_overall <- create_lifetime_risk_table(
  data = sample_data,
  index_ages = c(45, 55, 65),
  max_age = 90,
  group_var = NULL
)

print(lt_overall)
#>            Starting Age    Overall
#> 1 45 (until age 90) 82.8 (77.1, 88.5)
#> 2 55 (until age 90) 76.3 (69.8, 82.8)
#> 3 65 (until age 90) 68.1 (60.4, 75.8)

# Create lifetime risk table by sex
lt_sex <- create_lifetime_risk_table(
  data = sample_data,
  index_ages = c(45, 55, 65),
  max_age = 90,
  group_var = "sex"
)

print(lt_sex)
#>            Starting Age       Male     Female
#> 1 45 (until age 90) 81.5 (74.2, 88.8) 84.1 (76.8, 91.4)
#> 2 55 (until age 90) 75.1 (67.2, 83.0) 77.5 (69.1, 85.9)
#> 3 65 (until age 90) 66.8 (57.9, 75.7) 69.4 (59.8, 79.0)
```

### Key Functions

- **`pie_analysis()`**: Main analysis function for calculating lifetime risk
  - `group_var = NULL`: Analyze overall population
  - `group_var = "variable"`: Analyze by subgroups
  - `group_levels = NULL`: Auto-detect all levels
  - `group_levels = c("A", "B")`: Analyze specific groups
- **`create_lifetime_risk_table()`**: Generate lifetime risk estimates from multiple starting ages
- **`print()` and `summary()`** methods for reviewing results

### Features

- **Flexible Analysis Types:**
  - Overall population analysis
  - Two-group comparisons  
  - Multiple group analysis (3+)
  - Auto-detection of group levels
- **Robust Statistical Methods:**
  - Proper handling of late-age observations
  - Accurate competing risk adjustments
  - Age-specific and age-adjusted rates
  - Confidence interval calculations
- **Easy-to-Use Interface:**
  - Minimal parameter changes from original workflow
  - Backward compatible with existing code
  - Clear summary and print methods

## Migration from Previous Versions

If you have existing code using the old parameter structure:

``` r
# OLD (deprecated):
pie_analysis(data, min_age, max_age, age_group_width, 
             group_var = "sex", group1 = "Male", group2 = "Female", age_free)

# NEW (current):
pie_analysis(data, min_age, max_age, age_group_width,
             group_var = "sex", group_levels = c("Male", "Female"), age_free)

# OR (auto-detect):
pie_analysis(data, min_age, max_age, age_group_width,
             group_var = "sex", group_levels = NULL, age_free)
```

## Citation

If you use `lifetimeRisk` in your research, please cite:

[Citation information here]

## Contributing

Please report any issues or feature requests on the GitHub issues page.
