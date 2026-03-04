# =============================================================================
# 01_load_data.R
# Download NHANES components for cycles J (2017-2018) and L (2021-2022)
# Note: 2019-2020 (cycle K) was severely disrupted by COVID and is excluded.
#       The CDC released a "2017-March 2020 Pre-Pandemic" (P-suffix) dataset
#       for some files; where used it is noted below.
# =============================================================================

library(nhanesA)

# Helper: download one file and tag it with cycle year
fetch <- function(file, cycle_label) {
  message("Downloading ", file, " ...")
  dat <- nhanes(file)
  dat$cycle <- cycle_label
  dat
}

# ── Cycles to pull ──────────────────────────────────────────────────────────
cycles <- list(
  J = "2017-2018",
  L = "2021-2022"
)

# ── Components needed ────────────────────────────────────────────────────────
# DEMO  – demographics (age, sex, race, poverty ratio, sample weights)
# BMX   – body measures (BMI, waist circumference)
# BPX   – blood pressure readings
# BPQ   – blood pressure questionnaire (antihypertensive meds)
# DIQ   – diabetes questionnaire (self-reported diagnosis / insulin use)
# GHB   – glycohemoglobin (HbA1c)
# GLU   – plasma fasting glucose (fasting subsample only)
# TCHOL – total cholesterol
# HDL   – HDL cholesterol
# TRIGLY– triglycerides + LDL-C (fasting subsample)

components <- c("DEMO", "BMX", "BPX", "BPQ", "DIQ", "GHB", "GLU",
                "TCHOL", "HDL", "TRIGLY")

# ── Download all files ───────────────────────────────────────────────────────
raw <- list()

for (sfx in names(cycles)) {
  label <- cycles[[sfx]]
  for (comp in components) {
    key        <- paste0(comp, "_", sfx)
    raw[[key]] <- fetch(key, label)
  }
}

# ── Save raw downloads ───────────────────────────────────────────────────────
dir.create("output", showWarnings = FALSE)
saveRDS(raw, "output/raw_nhanes.rds")

message("\nAll downloads complete. Object saved to output/raw_nhanes.rds")
message("Components downloaded: ", paste(names(raw), collapse = ", "))
