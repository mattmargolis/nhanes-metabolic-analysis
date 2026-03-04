# =============================================================================
# 01_load_data.R
# Download NHANES components for cycles J (2017-2018) and L (2021-2022).
# Note: 2019-2020 (cycle K) is excluded — data collection was severely
# disrupted by COVID-19.
# =============================================================================

# Helper: download one file and tag it with cycle year
fetch <- function(file, cycle_label) {
  message("Downloading ", file, " ...")
  dat <- nhanesA::nhanes(file)
  dat$cycle <- cycle_label
  dat
}

# ── Cycles to pull ─────────────────────────────────────────────────────────
cycles <- list(
  J = "2017-2018",
  L = "2021-2022"
)

# ── Components needed ──────────────────────────────────────────────────────
# DEMO   – demographics (age, sex, race, poverty ratio, sample weights)
# BMX    – body measures (BMI, waist circumference)
# BPQ    – blood pressure questionnaire (antihypertensive meds)
# DIQ    – diabetes questionnaire (self-reported diagnosis / insulin use)
# GHB    – glycohemoglobin (HbA1c)
# GLU    – plasma fasting glucose (fasting subsample only)
# TCHOL  – total cholesterol
# HDL    – HDL cholesterol
# TRIGLY – triglycerides + LDL-C (fasting subsample)
# Blood pressure files are cycle-specific (see below).
components <- c(
  "DEMO", "BMX", "BPQ", "DIQ", "GHB", "GLU", "TCHOL", "HDL", "TRIGLY"
)

# ── Download all files ─────────────────────────────────────────────────────
raw <- list()

for (sfx in names(cycles)) {
  label <- cycles[[sfx]]
  for (comp in components) {
    key <- paste0(comp, "_", sfx)
    raw[[key]] <- fetch(key, label)
  }
}

# ── Blood pressure: cycle-specific files ───────────────────────────────────
# BPX_J  – auscultatory (2017-2018)
# BPXO_L – oscillometric (2021-2022; CDC changed measurement method)
raw[["BPX_J"]]  <- fetch("BPX_J",  "2017-2018")
raw[["BPXO_L"]] <- fetch("BPXO_L", "2021-2022")

# ── Save raw downloads ─────────────────────────────────────────────────────
dir.create("output", showWarnings = FALSE)
saveRDS(raw, "output/raw_nhanes.rds")

message("\nAll downloads complete. Object saved to output/raw_nhanes.rds")
message("Components downloaded: ", paste(names(raw), collapse = ", "))
