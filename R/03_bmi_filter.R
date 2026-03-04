# =============================================================================
# 03_bmi_filter.R
# Restrict to adults (>=18 y) with a healthy (normal) BMI (18.5-24.9 kg/m2).
# This defines the "metabolically unhealthy normal weight" (MUNW) study sample.
# =============================================================================

nhanes_full <- readRDS("output/nhanes_merged.rds")

nhanes_adults <- nhanes_full |>
  dplyr::filter(age >= 18)

nhanes_healthy_bmi <- nhanes_adults |>
  dplyr::filter(!is.na(bmi), bmi >= 18.5, bmi < 25.0)

pct_healthy <- round(
  nrow(nhanes_healthy_bmi) / nrow(nhanes_adults) * 100, 1
)

message("Adults in dataset:        ", nrow(nhanes_adults))
message(
  "Adults with healthy BMI:  ", nrow(nhanes_healthy_bmi),
  " (", pct_healthy, "%)"
)

cat("\nBMI summary (healthy-weight sample):\n")
print(summary(nhanes_healthy_bmi$bmi))

saveRDS(nhanes_healthy_bmi, "output/nhanes_healthy_bmi.rds")
