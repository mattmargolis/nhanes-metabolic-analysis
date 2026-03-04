# =============================================================================
# 02_clean_merge.R
# Merge NHANES components into one analysis-ready data frame.
# Handles blood pressure averaging and applies variable renaming for clarity.
# =============================================================================

library(dplyr)

raw <- readRDS("output/raw_nhanes.rds")

# ── Helper: bind two cycles of the same component ───────────────────────────
bind_cycles <- function(comp) {
  j <- raw[[paste0(comp, "_J")]]
  l <- raw[[paste0(comp, "_L")]]
  # Keep only common columns to avoid bind failures
  shared <- intersect(names(j), names(l))
  bind_rows(j[, shared], l[, shared])
}

# ── Demographics ─────────────────────────────────────────────────────────────
demo <- bind_cycles("DEMO") %>%
  select(
    SEQN,
    cycle,
    age        = RIDAGEYR,
    sex        = RIAGENDR,   # 1 = Male, 2 = Female
    race       = RIDRETH3,
    pir        = INDFMPIR,   # poverty-income ratio
    # Full-sample 2-year MEC weights
    wt_full    = WTMEC2YR,
    sdmvpsu    = SDMVPSU,
    sdmvstra   = SDMVSTRA
  ) %>%
  mutate(
    sex  = factor(sex,  levels = 1:2, labels = c("Male", "Female")),
    race = factor(race, levels = c(1,2,3,4,6,7),
                  labels = c("Mexican American", "Other Hispanic",
                             "Non-Hispanic White", "Non-Hispanic Black",
                             "Non-Hispanic Asian", "Other/Multiracial"))
  )

# ── Body measures ─────────────────────────────────────────────────────────────
bmx <- bind_cycles("BMX") %>%
  select(SEQN, bmi = BMXBMI, waist_cm = BMXWAIST)

# ── Blood pressure – average up to 3 readings ────────────────────────────────
bpx <- bind_cycles("BPX") %>%
  select(SEQN,
         sbp1 = BPXSY1, sbp2 = BPXSY2, sbp3 = BPXSY3,
         dbp1 = BPXDI1, dbp2 = BPXDI2, dbp3 = BPXDI3) %>%
  rowwise() %>%
  mutate(
    sbp_mean = mean(c(sbp1, sbp2, sbp3), na.rm = TRUE),
    dbp_mean = mean(c(dbp1, dbp2, dbp3), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(SEQN, sbp_mean, dbp_mean)

# ── Blood pressure questionnaire ──────────────────────────────────────────────
bpq <- bind_cycles("BPQ") %>%
  select(SEQN,
         htn_dx   = BPQ020,   # Ever told BP high?  1=yes
         htn_meds = BPQ050A)  # Taking BP meds?     1=yes

# ── Diabetes questionnaire ────────────────────────────────────────────────────
diq <- bind_cycles("DIQ") %>%
  select(SEQN,
         dm_dx     = DIQ010,  # Doctor told you have diabetes? 1=yes
         dm_insulin = DIQ050) # Taking insulin? 1=yes

# ── HbA1c ─────────────────────────────────────────────────────────────────────
ghb <- bind_cycles("GHB") %>%
  select(SEQN, hba1c = LBXGH)

# ── Fasting glucose ───────────────────────────────────────────────────────────
glu <- bind_cycles("GLU") %>%
  select(SEQN, glucose_fast = LBXGLU)

# ── Cholesterol ───────────────────────────────────────────────────────────────
tchol  <- bind_cycles("TCHOL") %>% select(SEQN, total_chol = LBXTC)
hdl    <- bind_cycles("HDL")   %>% select(SEQN, hdl_c      = LBDHDD)
trigly <- bind_cycles("TRIGLY") %>%
  select(SEQN,
         triglycerides = LBXTR,
         ldl_c         = LBDLDL)

# ── Merge all components ──────────────────────────────────────────────────────
nhanes_full <- demo %>%
  left_join(bmx,    by = "SEQN") %>%
  left_join(bpx,    by = "SEQN") %>%
  left_join(bpq,    by = "SEQN") %>%
  left_join(diq,    by = "SEQN") %>%
  left_join(ghb,    by = "SEQN") %>%
  left_join(glu,    by = "SEQN") %>%
  left_join(tchol,  by = "SEQN") %>%
  left_join(hdl,    by = "SEQN") %>%
  left_join(trigly, by = "SEQN")

message("Merged dataset: ", nrow(nhanes_full), " rows x ", ncol(nhanes_full), " columns")

saveRDS(nhanes_full, "output/nhanes_merged.rds")
