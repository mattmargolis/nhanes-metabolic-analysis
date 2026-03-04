# =============================================================================
# 04_metabolic_conditions.R
# Define binary flags for each metabolic condition and a composite MUNW flag.
#
# Definitions used:
#
#  Type 2 Diabetes (T2DM)
#    HbA1c ≥ 6.5%  OR  fasting glucose ≥ 126 mg/dL
#    OR  self-reported diagnosis (DIQ010 = 1) or insulin use (DIQ050 = 1)
#
#  Hypertension (HTN)
#    Mean SBP ≥ 130 mmHg  OR  mean DBP ≥ 80 mmHg  (AHA 2017 threshold)
#    OR  self-reported diagnosis (BPQ020 = 1)
#    OR  currently taking antihypertensive medication (BPQ050A = 1)
#
#  Dyslipidemia (DYSLIP)
#    LDL-C ≥ 130 mg/dL  OR  HDL-C < 40 mg/dL (men) / < 50 mg/dL (women)
#    OR  triglycerides ≥ 150 mg/dL
#
#  Metabolic Syndrome (MetSyn) – NCEP ATP III criteria (≥3 of 5 components)
#    1. Waist circumference > 102 cm (men) or > 88 cm (women)
#    2. Triglycerides ≥ 150 mg/dL
#    3. HDL-C < 40 mg/dL (men) or < 50 mg/dL (women)
#    4. BP ≥ 130/85 mmHg or on antihypertensive medication
#    5. Fasting glucose ≥ 100 mg/dL
# =============================================================================

library(dplyr)

df <- readRDS("output/nhanes_healthy_bmi.rds")

df <- df %>%
  mutate(
    # ── T2DM ──────────────────────────────────────────────────────────────────
    t2dm = case_when(
      hba1c >= 6.5                          ~ TRUE,
      glucose_fast >= 126                   ~ TRUE,
      dm_dx == 1                            ~ TRUE,
      dm_insulin == 1                       ~ TRUE,
      TRUE                                  ~ FALSE
    ),

    # ── Hypertension ──────────────────────────────────────────────────────────
    hypertension = case_when(
      sbp_mean >= 130 | dbp_mean >= 80      ~ TRUE,
      htn_dx == 1                           ~ TRUE,
      htn_meds == 1                         ~ TRUE,
      TRUE                                  ~ FALSE
    ),

    # ── Dyslipidemia ──────────────────────────────────────────────────────────
    low_hdl = case_when(
      sex == "Male"   & hdl_c < 40         ~ TRUE,
      sex == "Female" & hdl_c < 50         ~ TRUE,
      TRUE                                  ~ FALSE
    ),
    dyslipidemia = case_when(
      ldl_c >= 130                          ~ TRUE,
      low_hdl                               ~ TRUE,
      triglycerides >= 150                  ~ TRUE,
      TRUE                                  ~ FALSE
    ),

    # ── Metabolic Syndrome components ─────────────────────────────────────────
    ms_waist = case_when(
      sex == "Male"   & waist_cm > 102     ~ TRUE,
      sex == "Female" & waist_cm > 88      ~ TRUE,
      TRUE                                  ~ FALSE
    ),
    ms_trig  = (!is.na(triglycerides) & triglycerides >= 150),
    ms_hdl   = low_hdl,
    ms_bp    = (!is.na(sbp_mean) & (sbp_mean >= 130 | dbp_mean >= 85)) |
               (!is.na(htn_meds) & htn_meds == 1),
    ms_gluc  = (!is.na(glucose_fast) & glucose_fast >= 100),

    ms_score = rowSums(cbind(ms_waist, ms_trig, ms_hdl, ms_bp, ms_gluc),
                       na.rm = FALSE),
    met_syndrome = (!is.na(ms_score) & ms_score >= 3),

    # ── Any metabolic condition ────────────────────────────────────────────────
    any_metabolic = t2dm | hypertension | dyslipidemia | met_syndrome,

    # ── Count of conditions ────────────────────────────────────────────────────
    n_conditions = as.integer(t2dm) + as.integer(hypertension) +
                   as.integer(dyslipidemia) + as.integer(met_syndrome)
  )

# ── Summary ───────────────────────────────────────────────────────────────────
n        <- nrow(df)
cat("\n=== Metabolic conditions in healthy-BMI adults ===\n\n")
cat(sprintf("  T2DM              : %4d  (%4.1f%%)\n", sum(df$t2dm),         mean(df$t2dm) * 100))
cat(sprintf("  Hypertension      : %4d  (%4.1f%%)\n", sum(df$hypertension), mean(df$hypertension) * 100))
cat(sprintf("  Dyslipidemia      : %4d  (%4.1f%%)\n", sum(df$dyslipidemia), mean(df$dyslipidemia) * 100))
cat(sprintf("  Metabolic Syndrome: %4d  (%4.1f%%)\n", sum(df$met_syndrome,  na.rm = TRUE),
            mean(df$met_syndrome, na.rm = TRUE) * 100))
cat(sprintf("\n  ≥1 condition (MUNW): %4d  (%4.1f%%)\n",
            sum(df$any_metabolic), mean(df$any_metabolic) * 100))

saveRDS(df, "output/nhanes_metabolic_flagged.rds")
