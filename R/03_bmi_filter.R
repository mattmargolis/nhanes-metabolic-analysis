# =============================================================================
# 03_bmi_filter.R
# Restrict to adults (≥18 y) with a healthy (normal) BMI (18.5–24.9 kg/m²).
# This defines the "metabolically unhealthy normal weight" (MUNW) study sample.
# =============================================================================

library(dplyr)

nhanes_full <- readRDS("output/nhanes_merged.rds")

nhanes_adults <- nhanes_full %>%
  filter(age >= 18)

nhanes_healthy_bmi <- nhanes_adults %>%
  filter(!is.na(bmi), bmi >= 18.5, bmi < 25.0)

message("Adults in dataset:          ", nrow(nhanes_adults))
message("Adults with healthy BMI:    ", nrow(nhanes_healthy_bmi),
        " (", round(nrow(nhanes_healthy_bmi) / nrow(nhanes_adults) * 100, 1), "%)")

# Quick BMI summary
cat("\nBMI summary (healthy-weight sample):\n")
print(summary(nhanes_healthy_bmi$bmi))

saveRDS(nhanes_healthy_bmi, "output/nhanes_healthy_bmi.rds")
