# NHANES Metabolic Analysis: MUNW, Exercise, and the Limits of BMI Classification

This project investigates two related questions using NHANES data:

1. **Metabolically Unhealthy Normal Weight (MUNW)** — prevalence and co-occurrence of metabolic diseases in normal-weight U.S. adults (BMI 18.5–24.9 kg/m²).
2. **Exercise and metabolic risk** — does physical activity reduce metabolic disease risk in normal-weight adults, and do obese individuals who exercise regularly avoid metabolic disorders despite elevated BMI (**Metabolically Healthy Obese, MHO**)? The MHO analysis tests whether the current BMI classification system fails to capture muscle mass, misclassifying fit, metabolically healthy individuals as high-risk.

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
| **MHO** | Zero metabolic conditions in an obese individual (BMI ≥ 30) |

## Physical Activity Variables (PAQ)

Derived from NHANES Physical Activity Questionnaire (PAQ_J / PAQ_L):

| Variable | Description |
|----------|-------------|
| `pa_vig_min_wk` | Vigorous activity minutes per week (work + recreation) |
| `pa_mod_min_wk` | Moderate activity minutes per week (work + transport + recreation) |
| `pa_met_min_wk` | Total MET-minutes per week (8 × vig + 4 × mod) |
| `pa_mod_eq_min_wk` | Moderate-equivalent min/week (2:1 vigorous equivalency) |
| `pa_meets_guidelines` | Meets US Physical Activity Guidelines (≥150 mod-eq min/week) |
| `pa_category` | Ordered factor: Inactive / Insufficient / Active |
| `pa_sedentary_min_day` | Self-reported sedentary minutes per day |

## Project Structure

```
nhanes-metabolic-analysis/
├── R/
│   ├── 01_load_data.R              # Download NHANES components via nhanesA (incl. PAQ)
│   ├── 02_clean_merge.R            # Merge components; derive exercise variables
│   ├── 03_bmi_filter.R             # Restrict to adults with BMI 18.5–24.9 (diagnostic)
│   ├── 04_metabolic_conditions.R   # Metabolic flags for all adults; saves two outputs
│   ├── 05_analysis.R               # Survey-weighted MUNW stats + 4 figures
│   └── 06_exercise_analysis.R      # Exercise × metabolic risk; MHO analysis; 4 figures
│                                   # Includes TODO for XGBoost risk classifier
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
source("R/03_bmi_filter.R")            # optional — diagnostic summary only
source("R/04_metabolic_conditions.R")  # processes all adults; saves two outputs
source("R/05_analysis.R")              # MUNW analysis (healthy-BMI subset)
source("R/06_exercise_analysis.R")     # exercise analysis (all adults)
```

Or render the full report:

```r
rmarkdown::render("reports/nhanes_analysis.Rmd")
```

## Key Outputs

| File | Description |
|------|-------------|
| `output/nhanes_metabolic_all.rds` | All adults with BMI categories, metabolic flags, exercise variables |
| `output/nhanes_metabolic_flagged.rds` | Healthy-BMI subset with all condition flags |
| `output/fig1_prevalence.png` | Weighted metabolic condition prevalence (normal-weight) |
| `output/fig2_bmi_density.png` | BMI distribution: MUNW vs MHNW |
| `output/fig3_comorbidity_heatmap.png` | Condition co-occurrence correlation |
| `output/fig4_condition_count.png` | Number of conditions per individual |
| `output/fig5_exercise_metabolic_risk.png` | Metabolic risk by activity level (normal-weight) |
| `output/fig6_exercise_dose_response.png` | Predicted metabolic risk vs exercise dose |
| `output/fig7_mho_by_exercise.png` | MHO prevalence by activity level (obese adults) |
| `output/fig8_bmi_exercise_metabolic.png` | Exercise distribution by BMI category and metabolic status |

## Pipeline Overview

```
01_load_data
    ↓  raw_nhanes.rds
02_clean_merge
    ↓  nhanes_merged.rds  (includes exercise variables)
03_bmi_filter            [optional diagnostic]
    ↓  nhanes_healthy_bmi.rds
04_metabolic_conditions  (loads nhanes_merged.rds; filters age ≥18, valid BMI)
    ├─ nhanes_metabolic_all.rds     → 06_exercise_analysis.R
    └─ nhanes_metabolic_flagged.rds → 05_analysis.R
```

## Future Work

**`06_exercise_analysis.R`** contains a detailed TODO for an XGBoost-based risk classifier to predict which healthy individuals will develop metabolic disease. Key elements:

- Incorporates DXA body composition (ALMI, fat %) to address BMI–muscle mass confounding
- Considers external longitudinal datasets: UK Biobank, Framingham, MESA, CARDIA
- Uses SHAP values for interpretability and survey-weighted AUC for validation

## Survey Weighting

All prevalence estimates use NHANES complex survey design via the [`survey`](https://cran.r-project.org/package=survey) package. Two-year MEC weights (`WTMEC2YR`) are divided by the number of cycles combined (2) to produce adjusted weights for the pooled sample.

## References

- CDC/NCHS. National Health and Nutrition Examination Survey. https://www.cdc.gov/nchs/nhanes/
- Grundy SM, et al. (2005). Diagnosis and management of the metabolic syndrome. *Circulation*, 112(17), 2735–52.
- Whelton PK, et al. (2018). 2017 ACC/AHA Hypertension Guidelines. *JACC*, 71(19), e127–e248.
- US Department of Health and Human Services. Physical Activity Guidelines for Americans, 2nd ed. (2018).
