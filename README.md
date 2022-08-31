## Studying the Equitability of Policy Interventions Using a Heterogenous Policy Effect Approach: An Application to the Dutch Decentralization of the Social Domain


```bash
├── src
## Setup datasets
│   ├── 00a_gen_wmo_demog_data.R
│   ├── 00b_gen_municipality_crosswalk.R
│   ├── 00c_gen_continuous_income_data.R
│   ├── 00_fetch_educ.do
│   ├── 00_fetch_num_income.do
│   ├── 00_functions.R
## Merge data into analysis dataset
│   ├── 01_functions.R
│   ├── 01_merge_data.R
│   ├── 01_visualize_output.R
## Generate basic descriptives
│   ├── 02_basic_descriptives.R
│   ├── 02_functions.R
## Establish basic determinants of social assistance use
│   ├── 03_functions.R
│   ├── 03_model_descriptives.R
## Estimate main models (causal tree, causal forest, ates)
│   ├── 04a_gen_ates.R
│   ├── 04b_grf_heterogeneity.R
│   ├── 04c_ct_heterogeneity.R
│   ├── 04_functions.R
## Analyze main models (causal tree, causal forest)
│   ├── 05a_analyze_grf.R
│   ├── 05a_prep_sets.R
│   ├── 05b_analyze_ct.R
│   ├── 05c_analyze_groups.R
│   ├── 05_functions.R
## Generate output for paper
│   ├── 06a_make_tables.R
│   ├── 06b_make_figures.R
│   ├── 06c_gem_analysis_plots.R
│   ├── 06d_group_plots.r
│   ├── 06_functions.R
## Robustness analyses
│   ├── r_2015_2016_treat.R
│   ├── r_education.R
│   ├── r_functions.R
│   ├── r_pre_policy_trends.R
│   └── styles.R
└── tables
```

### 00.* scripts
This set of scripts is used to merge and clean data at the registry level for analysis. It relies on the standard demographic sets based on the `stapelingsmonitorYYYY` generated yearly by Statistics Netherlands.

These are located in the `H:/data/` folder within CBS' secure Remote Access Environment which is a folder that is not reflected in this repository.

*00a_gen_wmo_demog_data.R* : Merge demographic data with social assistance use data
*00b_gen_municipality_crosswalk.R* : Account for mergers amongst municipalities
*00c_gen_continuous_income_data.R* : Change income data to a numeric
*00_fetch_educ.do* : Get education data from `.dta` files
*00_fetch_num_income.do* : Get income data from `.dta` files

### 01.* scripts
This script stacks population registry data for the years 2016-2019. It includes income data and ensures only individuals living in a municipality that conistently reported data in 2016 and 2019 are included.

*01_merge_data.R* : Stacks 2016 and 2019 and includes income and municipality data.

### 02.* scripts
This script generates a number of descriptive tables, including:
- Overall descriptives of the population in 2016 and 2019 as well as the analysis set.
- Descriptives by social assistance use in 2016 and 2019.
- Number of municipalities submitting data to the registry.
- Use of WMO in 2016 and 2019 by univariate demographic groups
- Differences between use per municipality

*02_basic_descriptives.R* : Generates five descriptive tables.

### 03.* scripts
This script generates basic model insights when relating social assistance use to demographics, including:
- LPM on 2016 and 2019 data
- LPM on 2016 data
- LPM on 2016 data including education
- LPM on 2016 data including random intercepts at municipality level
- LPM on 2016 data including random intercepts at municipality level and education

*03_model_descriptives.R* : Generates 5 model objects and 7 tables describing the model coefficients.

### 04.* scripts
This script estimates the main models for the results section including ATEs using conventional methods and the Causal Tree and GRF approaches.

*04a_gen_ates.R* : Script to generate ATEs using covariate balancing, entropy balancing and propensity score matching.