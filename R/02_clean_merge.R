# =============================================================================
# 02_clean_merge.R
# Merge NHANES components into one analysis-ready data frame.
# Handles blood pressure averaging and applies variable renaming for clarity.
# =============================================================================

library(dplyr)

raw <- readRDS("output/raw_nhanes.rds")

# ── Helper: bind two cycles of the same component ───────────────────────────
# nhanesA sometimes returns factor columns in one cycle and numeric in another.
# Coerce all factors to character so bind_rows doesn't fail on type mismatches.
bind_cycles <- function(comp) {
  j <- raw[[paste0(comp, "_J")]]
  l <- raw[[paste0(comp, "_L")]]
  shared <- intersect(names(j), names(l))
  # Coerce all columns to character to avoid factor/numeric type conflicts,
  # then restore appropriate types with type.convert()
  j_sub <- j[, shared]; j_sub[] <- lapply(j_sub, as.character)
  l_sub <- l[, shared]; l_sub[] <- lapply(l_sub, as.character)
  result <- bind_rows(j_sub, l_sub)
  type.convert(result, as.is = TRUE)
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
    # nhanesA may return numeric codes or pre-translated strings depending on cycle;
    # handle both cases explicitly.
    sex = case_when(
      sex %in% c("1", "Male")   ~ "Male",
      sex %in% c("2", "Female") ~ "Female"
    ) %>% factor(levels = c("Male", "Female")),
    race = case_when(
      race %in% c("1", "Mexican American")    ~ "Mexican American",
      race %in% c("2", "Other Hispanic")      ~ "Other Hispanic",
      race %in% c("3", "Non-Hispanic White")  ~ "Non-Hispanic White",
      race %in% c("4", "Non-Hispanic Black")  ~ "Non-Hispanic Black",
      race %in% c("6", "Non-Hispanic Asian")  ~ "Non-Hispanic Asian",
      race %in% c("7", "Other Race - Including Multi-Racial", "Other/Multiracial") ~ "Other/Multiracial"
    ) %>% factor()
  )

# ── Body measures ─────────────────────────────────────────────────────────────
bmx <- bind_cycles("BMX") %>%
  select(SEQN, bmi = BMXBMI, waist_cm = BMXWAIST)

# ── Blood pressure – average up to 3 readings ────────────────────────────────
# Cycle J (2017-18): auscultatory — columns BPXSY1/BPXDI1 etc.
# Cycle L (2021-22): oscillometric — columns BPXOSY1/BPXODI1 etc.
bpx_j <- raw[["BPX_J"]] %>%
  select(SEQN, sbp1 = BPXSY1, sbp2 = BPXSY2, sbp3 = BPXSY3,
         dbp1 = BPXDI1, dbp2 = BPXDI2, dbp3 = BPXDI3)

bpx_l <- raw[["BPXO_L"]] %>%
  select(SEQN, sbp1 = BPXOSY1, sbp2 = BPXOSY2, sbp3 = BPXOSY3,
         dbp1 = BPXODI1, dbp2 = BPXODI2, dbp3 = BPXODI3)

bpx <- bind_rows(bpx_j, bpx_l) %>%
  rowwise() %>%
  mutate(
    sbp_mean = mean(c(sbp1, sbp2, sbp3), na.rm = TRUE),
    dbp_mean = mean(c(dbp1, dbp2, dbp3), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(SEQN, sbp_mean, dbp_mean)

# ── Blood pressure questionnaire ──────────────────────────────────────────────
# BPQ050A (2017-18) and BPQ150 (2021-22) both ask "taking prescribed BP meds?"
bpq_j <- raw[["BPQ_J"]] %>%
  select(SEQN, htn_dx = BPQ020, htn_meds = BPQ050A)

bpq_l <- raw[["BPQ_L"]] %>%
  select(SEQN, htn_dx = BPQ020, htn_meds = BPQ150)

bpq <- bind_rows(
  bpq_j %>% mutate(across(everything(), as.character)),
  bpq_l %>% mutate(across(everything(), as.character))
) %>% type.convert(as.is = TRUE)

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
# LBXTR (2017-18) was renamed to LBXTLG (2021-22); LDL column is shared.
trigly_j <- raw[["TRIGLY_J"]] %>% select(SEQN, triglycerides = LBXTR,  ldl_c = LBDLDL)
trigly_l <- raw[["TRIGLY_L"]] %>% select(SEQN, triglycerides = LBXTLG, ldl_c = LBDLDL)
trigly <- bind_rows(
  trigly_j %>% mutate(across(everything(), as.character)),
  trigly_l %>% mutate(across(everything(), as.character))
) %>% type.convert(as.is = TRUE)

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
