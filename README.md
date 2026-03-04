# NHANES Metabolic Analysis: Metabolically Unhealthy Normal Weight (MUNW)

This project investigates the prevalence and co-occurrence of metabolic diseases in **normal-weight U.S. adults** (BMI 18.5–24.9 kg/m²) using NHANES data — a phenomenon known as **Metabolically Unhealthy Normal Weight (MUNW)**.

## Data

**Source:** [NHANES](https://www.cdc.gov/nchs/nhanes/) (National Health and Nutrition Examination Survey), accessed via the [`nhanesA`](https://cran.r-project.org/package=nhanesA) R package.

**Cycles used:**
| Cycle | Years | Suffix |
|-------|-------|--------|
| J | 2017–2018 | `_J` |
| L | 2021–2022 | `_L` |

> The 2019–2020 cycle (K) is excluded — data collection was severely disrupted by COVID-19 and the sample is not nationally representative for all subgroups.

## Metabolic Conditions

| Condition | Definition |
|-----------|-----------|
| **Type 2 Diabetes** | HbA1c ≥ 6.5% OR fasting glucose ≥ 126 mg/dL OR self-reported diagnosis/insulin use |
| **Hypertension** | Mean SBP ≥ 130 OR DBP ≥ 80 mmHg (AHA 2017) OR self-reported diagnosis/medication |
| **Dyslipidemia** | LDL-C ≥ 130 mg/dL OR low HDL-C (<40 men/<50 women) OR triglycerides ≥ 150 mg/dL |
| **Metabolic Syndrome** | ≥ 3 of 5 NCEP ATP III criteria |
| **MUNW** | At least one of the above in a normal-weight individual |

## Project Structure

```
nhanes-metabolic-analysis/
├── R/
│   ├── 01_load_data.R              # Download NHANES components via nhanesA
│   ├── 02_clean_merge.R            # Merge components, rename variables
│   ├── 03_bmi_filter.R             # Restrict to adults with BMI 18.5–24.9
│   ├── 04_metabolic_conditions.R   # Define T2DM, HTN, dyslipidemia, MetSyn flags
│   └── 05_analysis.R               # Survey-weighted stats + 4 figures
├── reports/
│   └── nhanes_analysis.Rmd         # Full reproducible report (HTML)
├── output/                         # Generated data + figures (gitignored)
└── nhanes_metabolic.Rproj
```

## How to Run

### Prerequisites

```r
install.packages(c(
  "nhanesA",   # NHANES data download
  "dplyr",
  "tidyr",
  "ggplot2",
  "survey",    # Complex survey analysis
  "purrr",
  "here",
  "knitr",
  "kableExtra",
  "rmarkdown"
))
```

### Execution order

Run scripts sequentially from the project root:

```r
source("R/01_load_data.R")             # ~5–10 min (downloads from CDC)
source("R/02_clean_merge.R")
source("R/03_bmi_filter.R")
source("R/04_metabolic_conditions.R")
source("R/05_analysis.R")
```

Or render the full report:

```r
rmarkdown::render("reports/nhanes_analysis.Rmd")
```

## Key Outputs

| File | Description |
|------|-------------|
| `output/nhanes_metabolic_flagged.rds` | Final analysis dataset with all condition flags |
| `output/fig1_prevalence.png` | Weighted prevalence bar chart |
| `output/fig2_bmi_density.png` | BMI distribution: MUNW vs MHNW |
| `output/fig3_comorbidity_heatmap.png` | Condition co-occurrence correlation |
| `output/fig4_condition_count.png` | Number of conditions per individual |

## Survey Weighting

All prevalence estimates use NHANES complex survey design via the [`survey`](https://cran.r-project.org/package=survey) package. Two-year MEC weights (`WTMEC2YR`) are divided by the number of cycles combined (2) to produce adjusted weights for the pooled sample.

## References

- CDC/NCHS. National Health and Nutrition Examination Survey. https://www.cdc.gov/nchs/nhanes/
- Grundy SM, et al. (2005). Diagnosis and management of the metabolic syndrome. *Circulation*, 112(17), 2735–52.
- Whelton PK, et al. (2018). 2017 ACC/AHA Hypertension Guidelines. *JACC*, 71(19), e127–e248.
