# Virtual R Data Analyst Internship

**Week 4 of 4 – Comprehensive Data Analysis Report (final integrated project)**

This repository contains the code, data and outputs of the integrated internship project: all R code from data import to modelling, the dataset, every output of Weeks 1–3 plus the Week 4 integration, console transcripts and rendered screenshots. The final report (Word document) is submitted separately through the internship portal.

## Project Overview

The project analyses customer churn for a telecommunications company using the IBM Telco Customer Churn dataset. Weekly work was submitted in separate repositories; this repository re-runs and integrates all of it:

| Week | Topic | Repository |
|---|---|---|
| 1 | Data cleaning and preliminary analysis | [Virtual-R-Data-Analyst-Week1](https://github.com/Pharos-0/Virtual-R-Data-Analyst-Week1) |
| 2 | Data visualisation and insight communication | [Virtual-R-Data-Analyst-Week2](https://github.com/Pharos-0/Virtual-R-Data-Analyst-Week2) |
| 3 | Statistical analysis and predictive modelling | [Virtual-R-Data-Analyst-Week3](https://github.com/Pharos-0/Virtual-R-Data-Analyst-Week3) |
| **4 (this repository)** | Comprehensive final report (integrates 1–3) | [Virtual-R-Data-Analyst-Week4](https://github.com/Pharos-0/Virtual-R-Data-Analyst-Week4) |

## Dataset

- **Name:** Telco Customer Churn (IBM sample data describing a fictional telecommunications company)
- **Source:** [IBM/telco-customer-churn-on-icp4d](https://github.com/IBM/telco-customer-churn-on-icp4d/blob/master/data/Telco-Customer-Churn.csv) – Apache License 2.0
- **Size:** 7,043 rows × 21 columns; target `Churn` (1,869 churned, 26.5%)

## Objectives

Clean and prepare the data, explore and visualise it, test the main patterns statistically, build and validate predictive models, and translate the results into practical, clearly qualified implications – all in a reproducible R workflow.

## Week 1 – Data Cleaning and Preliminary Analysis

`R/01_data_import.R`, `R/02_data_cleaning.R`, `R/03_eda.R` – import and inspection, 11 missing `TotalCharges` set to 0 (zero-tenure customers), duplicates, types, outliers (all plausible, none removed), scaling, encoding, descriptive statistics, correlation. Separate submission: [Week 1 repository](https://github.com/Pharos-0/Virtual-R-Data-Analyst-Week1).

## Week 2 – Data Visualization

`R/04_visualization.R` – eleven `ggplot2` charts (bar, scatter, histogram, line and seven additional) and an interactive `plotly` chart. Separate submission: [Week 2 repository](https://github.com/Pharos-0/Virtual-R-Data-Analyst-Week2).

## Week 3 – Statistical Analysis and Predictive Modeling

`R/05_statistical_analysis.R`, `R/06_predictive_model.R` – eight hypothesis tests with assumption checks; logistic regression and random forest with 10-fold cross-validation and test-set evaluation. Separate submission: [Week 3 repository](https://github.com/Pharos-0/Virtual-R-Data-Analyst-Week3).

## Week 4 – Comprehensive Final Report (this repository)

- **Report:** `Week_4_Final_Report.docx`, submitted through the internship portal (not stored in this repository)
- **Code:** all scripts `R/00_setup.R` – `R/07_final_analysis.R`, run in order by [`run_all.R`](run_all.R). `R/07_final_analysis.R` adds the integration: a driver-evidence table across weeks, customer segments and predicted-risk deciles.
- **Outputs:** `outputs/tables/` (w1–w4 tables), `outputs/metrics/` (all numbers quoted in the reports), `figures/`, `outputs/logs/` (console transcripts), `outputs/interactive/`, `screenshots/`

## Technologies

R 4.3.3 · tidyverse (`readr`, `dplyr`, `tidyr`, `stringr`, `forcats`, `purrr`, `tibble`, `ggplot2`) · `scales` · `ragg` · `janitor` · `skimr` · `corrplot` · `plotly` · `htmlwidgets` · `car` · `nortest` · `broom` · `caret` · `ranger` · `pROC` · `ResourceSelection` · `jsonlite` · `highr`. The analysis was run with `Rscript`; RStudio was not available, so screenshots are renderings of the saved R console transcripts.

## Repository Structure

```text
Virtual-R-Data-Analyst-Week4/
├── README.md
├── run_all.R                    # runs every script in R/ and saves console transcripts
├── R/                           # analysis scripts
│   ├── 00_setup.R
│   ├── 01_data_import.R
│   ├── 02_data_cleaning.R
│   ├── 03_eda.R
│   ├── 04_visualization.R
│   ├── 05_statistical_analysis.R
│   ├── 06_predictive_model.R
│   ├── 07_final_analysis.R
│   ├── render_screenshots.R
│   └── screenshot_specs.R
├── data/
│   ├── README.md
│   ├── raw/                     # Telco-Customer-Churn.csv + Apache-2.0 licence
│   └── processed/               # telco_clean.csv (cleaned data)
├── outputs/
│   ├── tables/                  # 43 CSV tables
│   ├── metrics/                 # 7 JSON files with the key results
│   ├── logs/                    # console transcripts, run summary, session info, package versions
│   └── interactive/             # interactive plotly chart (open the HTML file in a browser)
├── figures/                     # 33 charts (PNG, 300 dpi)
├── screenshots/                 # 48 renderings of R console output + index
└── requirements/
    └── R_packages.txt           # packages and versions used
```

## Methodology

1. **Preparation:** conservative import, inspection, cleaning in a fixed order, validation with `stopifnot()`.
2. **Exploration:** outliers (univariate, group-wise, contextual), scaling, encoding, descriptive statistics, correlation, Cramér's V.
3. **Visualisation:** question-driven charts with one accent colour and computed, finding-based titles.
4. **Statistics:** tests chosen by variable type and assumption checks; Holm correction; effect sizes.
5. **Modelling:** stratified split, 10-fold CV with shared folds, preprocessing inside folds, VIF-based predictor choice, threshold from out-of-fold predictions, single test-set evaluation.
6. **Integration:** evidence combined across methods; segments and risk deciles for practical interpretation.

The full method is described in the Week 4 report.

## Key Findings

- Overall churn **26.5%**; month-to-month **42.7%** vs two-year **2.8%** (Cramér's V = 0.41).
- **55%** of churners left within 12 months; the odds of churn fall by a factor of 0.66 per year of tenure.
- Highest-risk segment: new month-to-month fibre customers, **70%** churn (n = 916); the three highest-churn segments hold 40% of customers but 78% of churners.
- Higher bills of churners reflect the fibre service mix, not higher prices within a service.
- 7 of 8 hypotheses supported; gender shows no association (p = 0.487).
- Test ROC-AUC 0.853 (logistic) and 0.855 (random forest), not significantly different; top risk decile churned at 74%, bottom decile at 1.4%.

All findings are associations in a cross-sectional sample of fictional data, not causal effects.

## Reproducibility

```bash
Rscript run_all.R
```

Runs all seven scripts in order (seed 42; about four minutes), writes console transcripts to `outputs/logs/`, records the session and package versions, and re-renders the screenshots (requires Google Chrome or Chromium). Packages and versions: [`requirements/R_packages.txt`](requirements/R_packages.txt). Install them with the command at the top of that file. A single script can be re-run with, for example, `Rscript run_all.R 02`; the R session information and package versions of the last run are saved in `outputs/logs/`.

**About the screenshots:** RStudio was not available, so the images in `screenshots/` are renderings of genuine R console transcripts saved by `run_all.R`, not RStudio window captures.

## Dataset Source

IBM, *Telco Customer Churn* sample data, repository [IBM/telco-customer-churn-on-icp4d](https://github.com/IBM/telco-customer-churn-on-icp4d), file `data/Telco-Customer-Churn.csv`. Apache License 2.0; see [`data/raw/LICENSE-Apache-2.0_IBM.txt`](data/raw/LICENSE-Apache-2.0_IBM.txt).

## Author

Pharos Sophy Samuel T J – Virtual R Data Analyst Internship, Week 4 (final) submission.
