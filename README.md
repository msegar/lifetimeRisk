# lifetimeRisk
[![status](https://joss.theoj.org/papers/fef2f1364107efeeef96588c73c3b059/status.svg)](https://joss.theoj.org/papers/fef2f1364107efeeef96588c73c3b059)

<div>
<img src="lifetime_risk_sticker.png" width="150px" align="right">
</div>

Calculate lifetime risk of adverse outcomes accounting for competing risks.

## Features
- Person-year calculations
- Age-specific incidence rates
- Cumulative incidence with competing risks
- Age-adjusted rates
- Flexible group analysis (overall or subgroups)
- Scalable comparisons (any number of groups)
- Publication-quality plots
- Export results to CSV
- Thorough test suite for reliability

## Installation

You can install the development version of `lifetimeRisk` from [GitHub](https://github.com/msegar/lifetimeRisk) with:

```r
# install.packages("remotes")
remotes::install_github("msegar/lifetimeRisk")
```

## Input Data Format

Your data should contain:
- `ids`: Unique identifier for each individual
- `entryage`: Age at which the individual entered the study
- `survage`: Age at which the individual was last observed
- `status`: Primary event indicator (1 if occurred, 0 if not)
- `astatus`: Competing event indicator (1 if occurred, 0 if not)
- Grouping variables (optional): Any variables for subgroup analysis

**Edge cases:**
- All columns must be present and correctly named
- No missing values in required columns
- Ages should be numeric and within the analysis range

## Usage

```r
library(lifetimeRisk)
library(data.table)
```

### Example Data

```r
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
```

### Main Analysis

#### Overall Population

```r
result_overall <- pie_analysis(
  data = sample_data,
  min_age = 45,
  max_age = 90,
  age_group_width = 5,
  group_var = NULL,
  age_free = 45
)
summary(result_overall)
```

#### By Group

```r
result_sex <- pie_analysis(
  data = sample_data,
  min_age = 45,
  max_age = 90,
  age_group_width = 5,
  group_var = "sex",
  group_levels = c("Male", "Female"),
  age_free = 45
)
summary(result_sex)
```

#### Multiple Groups

```r
result_race <- pie_analysis(
  data = sample_data,
  min_age = 45,
  max_age = 90,
  age_group_width = 5,
  group_var = "race",
  group_levels = NULL,
  age_free = 45
)
summary(result_race)
```

### Plotting

```r
library(ggplot2)
p <- plot_lifetime_risk(result_sex, adjusted = TRUE, label_names = c("Male" = "Men", "Female" = "Women"))
print(p)
```

### Export to CSV

```r
create_lifetime_csv(result_sex, adjusted = TRUE, output_file = "lifetime_risk_sex.csv")
```

### Lifetime Risk Table

```r
lt_overall <- create_lifetime_risk_table(
  data = sample_data,
  index_ages = c(45, 55, 65),
  max_age = 90,
  group_var = NULL
)
print(lt_overall)
```

### Disease-Free Survival (Restricted Mean)

```r
res <- calculate_disease_free_survival(
  data = sample_data,
  time_event_col = "survage",
  time_death_col = "survage",
  event_col = "status",
  death_col = "astatus",
  index_age = 45,
  n_bootstrap = 100
)
print(res)
```

## Functions

- `pie_analysis()`: Main function for person-year and lifetime risk analysis (overall or by group)
- `create_person_year_data()`: Expand data to person-year format for analysis
- `calculate_age_specific_rates()`: Compute age-specific incidence rates
- `calculate_cumulative_incidence()`: Compute cumulative incidence and confidence intervals
- `create_lifetime_risk_table()`: Summarize lifetime risk from different starting ages
- `plot_lifetime_risk()`: Plot cumulative incidence curves with confidence intervals
- `create_lifetime_csv()`: Export lifetime risk estimates to CSV
- `calculate_disease_free_survival()`: Estimate disease-free and overall survival (restricted mean)
- `get_number_at_risk_from_analysis()`: Extract number at risk at specific ages from analysis

## Testing

A comprehensive test suite is provided in `tests/testthat/`.
Run all tests with:

```r
devtools::test()
```

## Reproducibility & Validation
- All statistical results are validated against the original SAS PIE macro.
- Bootstrapping uses a fixed seed for reproducibility.
- Extensive tests cover edge cases and regression.

## Documentation
- See function documentation via `?function_name` in R.
- For detailed examples, see the vignettes and test files.

## Citation
If you use this package, please cite:
- Lloyd-Jones DM, Martin DO, Larson MG, Levy D. Lifetime risk of developing coronary heart disease. Lancet. 1999;353(9147):89-92.
- Beiser A, D'Agostino RB Sr, Seshadri S, Sullivan LM, Wolf PA. Computing estimates of incidence, including lifetime risk: Alzheimer's disease in the Framingham Study. The Practical Incidence Estimators (PIE) macro. Stat Med. 2000 Jun;19(11-12):1495-522. [PubMed PMID: 10844714](https://pubmed.ncbi.nlm.nih.gov/10844714/)

## License
See LICENSE file for details.
