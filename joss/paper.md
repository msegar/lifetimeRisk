---
title: 'lifetimeRisk: An R package for lifetime risk analysis with competing risks'
tags:
  - R
  - epidemiology
  - survival analysis
  - competing risks
  - lifetime risk
  - cardiovascular disease
authors:
  - name: Matthew W. Segar
    orcid: 0000-0001-6100-0897
    corresponding: true
    affiliation: 1
  - name: Byron C. Jaeger
    orcid: 0000-0001-7399-2299
    affiliation: 2
  - name: Ambarish Pandey
    orcid: 0000-0001-9651-3836
    affiliation: 3
affiliations:
  - name: Department of Cardiology, Texas Heart Institute, Houston, TX
    index: 1
  - name: Perisphere Real World Evidence LLC, Austin, Texas
    index: 2
  - name: Division of Caridology, Department of Internal Medicine, UT Southwestern Medical Center, Dallas, TX
    index: 3
date: 7 July 2025
bibliography: paper.bib
---

# Summary

Lifetime risk estimation is fundamental to understanding long-term disease burden and provides important insights for epidemiologists and clinicians assessing population health across diverse medical domains. The `lifetimeRisk` package provides tools for calculating lifetime risk of adverse outcomes while properly accounting for competing risks, implementing the methodology established by the SAS Practical Incidence Estimators (PIE) macro [@beiser2000computing]. Originally developed for Alzheimer's disease research in the Framingham Study, the PIE macro provides a widely-used framework for lifetime risk calculations across epidemiological research, from cardiovascular disease and cancer to dementia and chronic diseases. The package enables person-year calculations, age-specific incidence rates, cumulative incidence with competing risks, and age-adjusted rates using the same statistical algorithms as the original PIE macro. All statistical computations have been validated against the reference SAS implementation to ensure methodological consistency and numerical equivalence for research applications.

# Statement of need

Lifetime risk analysis extends traditional short-term risk assessment and has influenced medical research and clinical practice across multiple disciplines. The fundamental challenge addressed by lifetime risk methodology is that conventional 10-year risk models often underestimate long-term disease burden, particularly in younger populations who may face substantial cumulative risk despite appearing low-risk in short-term assessments. The statistical complexity lies in properly accounting for competing risks, where multiple potential outcomes can preclude the occurrence of the primary endpoint of interest. This is particularly relevant in aging populations where cardiovascular disease, cancer, and other-cause mortality compete as terminal events.

The impact of this methodology is evident in cardiovascular epidemiology, where lifetime risk studies have changed understanding of disease burden and clinical decision-making. Landmark studies by Lloyd-Jones et al. and Berry et al. demonstrated marked differences in lifetime cardiovascular disease risks across racial groups [@lloydJones1999lifetime; @berry2012lifetime]. These studies, along with subsequent publications demonstrating lifetime risks exceeding 30% even for individuals with optimal risk factors [@wilkins2012lifetime] and significant differences across sex-based groups [@pandey2018sex], have established lifetime risk as important for clinical decision-making and population health assessment.

Despite widespread use of these methods in high-impact research, accessible implementations have been limited. While R packages such as `survival`, `cmprsk`, and `etm` provide foundational competing risks methods, none offer the lifetime risk calculation framework with standardized output formats established by the PIE macro. The `lifetimeRisk` package fills this gap by providing an R implementation of the PIE macro methodology and enables researchers to perform lifetime risk analyses without requiring SAS software.

# Implementation

## Statistical methodology

Lifetime risk estimation employs life table methodology to estimate the absolute probability that an individual will develop a disease if they live long enough, accounting for competing risks of death from other causes. This approach differs from Cox proportional hazards models by providing absolute risk estimates rather than relative hazards. The mathematical foundation begins with age-specific incidence rates and mortality rates from cohort data, calculating the probability of developing the disease of interest, dying from the disease, and dying from competing causes for each age interval.

The lifetime risk is computed as the cumulative probability of experiencing the event across all age intervals until a maximum age, typically 85 or 95 years. This requires estimation of age-specific hazard rates $\lambda_d(a)$ for the disease of interest and $\lambda_c(a)$ for competing risks at age $a$, where the cumulative incidence function $F(t) = \int_0^t \lambda_d(a) \times S(a) , da$, and $S(a)$ represents the overall survival function incorporating both disease and competing mortality.

The PIE macro methodology extends basic life table approaches by implementing competing risks adjustments that properly account for interdependence between multiple potential outcomes [@fine1999proportional]. Unlike Kaplan-Meier survival curves that treat competing events as censored observations, the PIE approach explicitly models competing risks to provide unbiased estimates of cumulative incidence. This ensures that the sum of all cause-specific cumulative incidence functions equals one minus the overall survival probability.


## Software architecture

The `lifetimeRisk` package implements this statistical methodology through a software architecture that prioritizes computational efficiency, numerical accuracy, and user accessibility. The core statistical engine translates the PIE macro algorithms into R code. Mathematical equivalence is maintained while leveraging the survival package for fundamental computations, `data.table` for efficient data management and transformation, and `ggplot2` for visualization.

Central to the package's reliability is its validation framework that ensures numerical accuracy and methodological consistency with the reference SAS implementation. All statistical computations have been tested against the original PIE macro through validation studies using both simulated and real-world datasets spanning different study designs, population characteristics, and event rates. The validation process encompasses comparison of age-specific incidence rates, cumulative incidence estimates, confidence intervals, and summary statistics.

Performance benchmarking demonstrates that the package scales efficiently with dataset size. Table 1 shows execution times and memory usage across datasets ranging from 1,000 to 1,000,000 observations. The package maintains consistent processing rates of approximately 50,000-60,000 observations per second for larger datasets, with memory usage scaling linearly with data size. These performance characteristics make the package suitable for large-scale epidemiological studies typical in cardiovascular and population health research.

**Table 1.** Performance benchmarks for `lifetimeRisk` package across different dataset sizes.

| Dataset Size | Median Time (seconds) | Memory Usage (MB) | Observations per Second |
|:-------------|:---------------------|:------------------|:-----------------------|
| 1,000        | 0.04                 | 0.0               | 23,882                 |
| 5,000        | 0.10                 | 0.2               | 50,570                 |
| 10,000       | 0.18                 | 0.4               | 56,204                 |
| 25,000       | 0.54                 | 1.0               | 46,167                 |
| 50,000       | 0.88                 | 1.9               | 56,714                 |
| 100,000      | 1.64                 | 3.8               | 61,024                 |
| 1,000,000    | 14.44                | 38.1              | 69,269                 |

*Note: Benchmarks performed using `microbenchmark` package with 5 replications per dataset size on standard computing hardware.*

\begin{figure}
\includegraphics[width=1\linewidth]{Figure} \caption{ Example output from the `lifetimeRisk` package demonstrating (A) summary statistics for sex-stratified lifetime risk analysis showing final cumulative incidence estimates, and (B) competing risks-adjusted cumulative incidence curves illustrating lifetime risk trajectories between men and women from age 45 to 90 years, with shaded confidence bands indicating 95\% confidence intervals.}\label{fig:fig-1}
\end{figure}

The package provides a function library designed for both novice and expert users. Key functions include `pie_analysis()` for complete lifetime risk analysis implementing the full PIE macro methodology, `calculate_age_specific_rates()` for detailed age-stratified incidence computations, `calculate_cumulative_incidence()` for competing risks-adjusted cumulative incidence calculation (**Figure 1A**), and specialized visualization and export functions. The `plot_lifetime_risk()` function generates publication-ready graphics with customizable confidence intervals (**Figure 1B**), while `create_lifetime_risk_table()` produces standardized summary tables suitable for manuscript inclusion.


# Research applications and impact

The `lifetimeRisk` package addresses a need in epidemiological research for a freely accessible, open-source implementation of life time risk methodology, which has been widely cited in epidemiological literature and has influenced clinical practice guidelines. The package has been successfully applied in cardiovascular epidemiology research, including our recent multicohort study examining P-wave parameters and lifetime atrial fibrillation risk [@segar2025ecg]. In this analysis of 25,508 participants from 4 prospective cohort studies, we used the `lifetimeRisk` package to demonstrate that participants with multiple ECG abnormalities had lifetime AF risks reaching 35.7% compared to 22.9% for those with minimal abnormalities, with participants having 4+ ECG abnormalities living an average of 17.1 years free of AF compared to 21.7 years for those with none. This application demonstrates the package's capability to handle large-scale multicohort analyses and produce clinically meaningful lifetime risk estimates that inform patient care and risk stratification.

The broad applicability of lifetime risk methods extends across medical specialties. Cancer epidemiologists have used these approaches to develop global estimates of lifetime cancer risk, while neurological research has employed lifetime risk calculations to understand dementia patterns. Public health researchers have applied the methodology to examine health disparities and assess long-term health impacts of exposures. Recent applications demonstrate continued relevance, including population-specific lifetime risk tools and comprehensive models for diverse populations.

The `lifetimeRisk` package facilitates these research applications by providing a documented platform that removes technical barriers to implementing lifetime risk analyses. The package includes documentation, worked examples across different research domains, and flexible functions that accommodate various study designs and research questions, enabling researchers without extensive statistical programming backgrounds to apply these methods to their research.


# Acknowledgements

This implementation builds upon the foundational statistical methodology established by Beiser et al. in the original SAS PIE macro and acknowledges the extensive validation and application work conducted in landmark epidemiological studies. We recognize the impact of this methodology on epidemiological research and clinical practice across medical disciplines.

# References






