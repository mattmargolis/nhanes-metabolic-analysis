# =============================================================================
# 04_metabolic_conditions.R
# Define binary flags for each metabolic condition and a composite MUNW flag.
# Processes all adults (age >= 18) from the merged dataset; saves:
#   - nhanes_metabolic_all.rds    : all adults with BMI + metabolic flags
#   - nhanes_metabolic_flagged.rds: healthy-BMI subset only (for 05_analysis.R)
#
# Definitions:
#
#  Type 2 Diabetes (T2DM)
#    HbA1c >= 6.5%  OR  fasting glucose >= 126 mg/dL
#    OR  self-reported diagnosis (DIQ010 = 1) or insulin use (DIQ050 = 1)
#
#  Hypertension (HTN)
#    Mean SBP >= 130 mmHg  OR  mean DBP >= 80 mmHg  (AHA 2017 threshold)
#    OR  self-reported diagnosis (BPQ020 = 1)
#    OR  currently taking antihypertensive medication
#
#  Dyslipidemia (DYSLIP)
#    LDL-C >= 130 mg/dL  OR  HDL-C < 40 mg/dL (men) / < 50 mg/dL (women)
#    OR  triglycerides >= 150 mg/dL
#
#  Metabolic Syndrome (MetSyn) -- NCEP ATP III criteria (>=3 of 5 components)
#    1. Waist circumference > 102 cm (men) or > 88 cm (women)
#    2. Triglycerides >= 150 mg/dL
#    3. HDL-C < 40 mg/dL (men) or < 50 mg/dL (women)
#    4. BP >= 130/85 mmHg or on antihypertensive medication
#    5. Fasting glucose >= 100 mg/dL
# =============================================================================

df_all <- readRDS("output/nhanes_merged.rds") |>
  dplyr::filter(age >= 18, !is.na(bmi)) |>
  dplyr::mutate(
    # Adjusted survey weight: divide by number of cycles combined (2)
    wt_adj = wt_full / 2,
    # BMI category (WHO standard thresholds)
    bmi_category = dplyr::case_when(
      bmi < 18.5              ~ "Underweight",
      bmi >= 18.5 & bmi < 25 ~ "Normal weight",
      bmi >= 25   & bmi < 30 ~ "Overweight",
      bmi >= 30               ~ "Obese"
    ) |> factor(levels = c("Underweight", "Normal weight", "Overweight", "Obese"))
  )

df_all <- df_all |>
  dplyr::mutate(
    # ── T2DM ──────────────────────────────────────────────────────────────
    t2dm = dplyr::case_when(
      hba1c >= 6.5        ~ TRUE,
      glucose_fast >= 126 ~ TRUE,
      dm_dx == 1          ~ TRUE,
      dm_insulin == 1     ~ TRUE,
      TRUE                ~ FALSE
    ),

    # ── Hypertension ──────────────────────────────────────────────────────
    hypertension = dplyr::case_when(
      sbp_mean >= 130 | dbp_mean >= 80 ~ TRUE,
      htn_dx == 1                      ~ TRUE,
      htn_meds == 1                    ~ TRUE,
      TRUE                             ~ FALSE
    ),

    # ── Dyslipidemia ──────────────────────────────────────────────────────
    low_hdl = dplyr::case_when(
      sex == "Male"   & hdl_c < 40 ~ TRUE,
      sex == "Female" & hdl_c < 50 ~ TRUE,
      TRUE                         ~ FALSE
    ),
    dyslipidemia = dplyr::case_when(
      ldl_c >= 130         ~ TRUE,
      low_hdl              ~ TRUE,
      triglycerides >= 150 ~ TRUE,
      TRUE                 ~ FALSE
    ),

    # ── Metabolic Syndrome components ─────────────────────────────────────
    ms_waist = dplyr::case_when(
      sex == "Male"   & waist_cm > 102 ~ TRUE,
      sex == "Female" & waist_cm > 88  ~ TRUE,
      TRUE                             ~ FALSE
    ),
    ms_trig  = (!is.na(triglycerides) & triglycerides >= 150),
    ms_hdl   = low_hdl,
    ms_bp    = (!is.na(sbp_mean) & (sbp_mean >= 130 | dbp_mean >= 85)) |
               (!is.na(htn_meds) & htn_meds == 1),
    ms_gluc  = (!is.na(glucose_fast) & glucose_fast >= 100),

    ms_score     = rowSums(
      cbind(ms_waist, ms_trig, ms_hdl, ms_bp, ms_gluc),
      na.rm = FALSE
    ),
    met_syndrome  = (!is.na(ms_score) & ms_score >= 3),

    # ── Any metabolic condition ────────────────────────────────────────────
    any_metabolic = t2dm | hypertension | dyslipidemia | met_syndrome,

    # ── Count of conditions ────────────────────────────────────────────────
    n_conditions = as.integer(t2dm) + as.integer(hypertension) +
                   as.integer(dyslipidemia) + as.integer(met_syndrome)
  )

# ── Summary: all adults ──────────────────────────────────────────────────────
cat("\n=== Metabolic conditions in all adults (age >= 18) ===\n\n")
cat(sprintf("  N (all adults)    : %5d\n", nrow(df_all)))
cat(sprintf("  T2DM              : %4d  (%4.1f%%)\n",
            sum(df_all$t2dm), mean(df_all$t2dm) * 100))
cat(sprintf("  Hypertension      : %4d  (%4.1f%%)\n",
            sum(df_all$hypertension), mean(df_all$hypertension) * 100))
cat(sprintf("  Dyslipidemia      : %4d  (%4.1f%%)\n",
            sum(df_all$dyslipidemia), mean(df_all$dyslipidemia) * 100))
cat(sprintf("  Metabolic Syndrome: %4d  (%4.1f%%)\n",
            sum(df_all$met_syndrome, na.rm = TRUE),
            mean(df_all$met_syndrome, na.rm = TRUE) * 100))
cat(sprintf("\n  >=1 condition     : %4d  (%4.1f%%)\n",
            sum(df_all$any_metabolic), mean(df_all$any_metabolic) * 100))

# ── Save: all adults ─────────────────────────────────────────────────────────
saveRDS(df_all, "output/nhanes_metabolic_all.rds")

# ── Healthy-BMI subset: for MUNW analysis (05_analysis.R) ───────────────────
df <- dplyr::filter(df_all, bmi >= 18.5, bmi < 25)

cat("\n=== Metabolic conditions in healthy-BMI adults (BMI 18.5-24.9) ===\n\n")
cat(sprintf("  N (healthy BMI)   : %5d\n", nrow(df)))
cat(sprintf("  T2DM              : %4d  (%4.1f%%)\n",
            sum(df$t2dm), mean(df$t2dm) * 100))
cat(sprintf("  Hypertension      : %4d  (%4.1f%%)\n",
            sum(df$hypertension), mean(df$hypertension) * 100))
cat(sprintf("  Dyslipidemia      : %4d  (%4.1f%%)\n",
            sum(df$dyslipidemia), mean(df$dyslipidemia) * 100))
cat(sprintf("  Metabolic Syndrome: %4d  (%4.1f%%)\n",
            sum(df$met_syndrome, na.rm = TRUE),
            mean(df$met_syndrome, na.rm = TRUE) * 100))
cat(sprintf("\n  >=1 condition (MUNW): %4d  (%4.1f%%)\n",
            sum(df$any_metabolic), mean(df$any_metabolic) * 100))

saveRDS(df, "output/nhanes_metabolic_flagged.rds")
