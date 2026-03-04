# =============================================================================
# 02_clean_merge.R
# Merge NHANES components into one analysis-ready data frame.
# Handles blood pressure averaging and applies variable renaming for clarity.
# =============================================================================

raw <- readRDS("output/raw_nhanes.rds")

# ── Helper: bind two cycles of the same component ─────────────────────────
# nhanesA sometimes returns factor or pre-translated columns in one cycle
# and numeric in another. Coerce to character before binding, then restore
# appropriate types with type.convert().
bind_cycles <- function(comp) {
  j <- raw[[paste0(comp, "_J")]]
  l <- raw[[paste0(comp, "_L")]]
  shared <- intersect(names(j), names(l))
  j_sub <- j[, shared]
  l_sub <- l[, shared]
  j_sub[] <- lapply(j_sub, as.character)
  l_sub[] <- lapply(l_sub, as.character)
  dplyr::bind_rows(j_sub, l_sub) |>
    type.convert(as.is = TRUE)
}

# ── Demographics ───────────────────────────────────────────────────────────
demo <- bind_cycles("DEMO") |>
  dplyr::select(
    SEQN,
    cycle,
    age      = RIDAGEYR,
    sex      = RIAGENDR,
    race     = RIDRETH3,
    pir      = INDFMPIR,   # poverty-income ratio
    wt_full  = WTMEC2YR,   # full-sample 2-year MEC weight
    sdmvpsu  = SDMVPSU,
    sdmvstra = SDMVSTRA
  ) |>
  dplyr::mutate(
    # nhanesA may return numeric codes or pre-translated strings per cycle;
    # handle both explicitly.
    sex = dplyr::case_when(
      sex %in% c("1", "Male")   ~ "Male",
      sex %in% c("2", "Female") ~ "Female"
    ) |> factor(levels = c("Male", "Female")),
    race = dplyr::case_when(
      race %in% c("1", "Mexican American")                        ~ "Mexican American",
      race %in% c("2", "Other Hispanic")                          ~ "Other Hispanic",
      race %in% c("3", "Non-Hispanic White")                      ~ "Non-Hispanic White",
      race %in% c("4", "Non-Hispanic Black")                      ~ "Non-Hispanic Black",
      race %in% c("6", "Non-Hispanic Asian")                      ~ "Non-Hispanic Asian",
      race %in% c("7", "Other Race - Including Multi-Racial",
                  "Other/Multiracial")                            ~ "Other/Multiracial"
    ) |> factor()
  )

# ── Body measures ──────────────────────────────────────────────────────────
bmx <- bind_cycles("BMX") |>
  dplyr::select(SEQN, bmi = BMXBMI, waist_cm = BMXWAIST)

# ── Blood pressure – average up to 3 readings ─────────────────────────────
# Cycle J (2017-18): auscultatory  – columns BPXSY*/BPXDI*
# Cycle L (2021-22): oscillometric – columns BPXOSY*/BPXODI*
bpx_j <- raw[["BPX_J"]] |>
  dplyr::select(
    SEQN,
    sbp1 = BPXSY1, sbp2 = BPXSY2, sbp3 = BPXSY3,
    dbp1 = BPXDI1, dbp2 = BPXDI2, dbp3 = BPXDI3
  )

bpx_l <- raw[["BPXO_L"]] |>
  dplyr::select(
    SEQN,
    sbp1 = BPXOSY1, sbp2 = BPXOSY2, sbp3 = BPXOSY3,
    dbp1 = BPXODI1, dbp2 = BPXODI2, dbp3 = BPXODI3
  )

bpx <- dplyr::bind_rows(bpx_j, bpx_l) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    sbp_mean = mean(c(sbp1, sbp2, sbp3), na.rm = TRUE),
    dbp_mean = mean(c(dbp1, dbp2, dbp3), na.rm = TRUE)
  ) |>
  dplyr::ungroup() |>
  dplyr::select(SEQN, sbp_mean, dbp_mean)

# ── Blood pressure questionnaire ───────────────────────────────────────────
# BPQ050A (2017-18) and BPQ150 (2021-22) both ask "taking prescribed BP meds?"
bpq_j <- raw[["BPQ_J"]] |>
  dplyr::select(SEQN, htn_dx = BPQ020, htn_meds = BPQ050A)

bpq_l <- raw[["BPQ_L"]] |>
  dplyr::select(SEQN, htn_dx = BPQ020, htn_meds = BPQ150)

bpq <- dplyr::bind_rows(
  bpq_j |> dplyr::mutate(dplyr::across(dplyr::everything(), as.character)),
  bpq_l |> dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
) |>
  type.convert(as.is = TRUE)

# ── Diabetes questionnaire ─────────────────────────────────────────────────
diq <- bind_cycles("DIQ") |>
  dplyr::select(
    SEQN,
    dm_dx     = DIQ010,   # Doctor told you have diabetes? 1 = yes
    dm_insulin = DIQ050   # Taking insulin? 1 = yes
  )

# ── HbA1c ──────────────────────────────────────────────────────────────────
ghb <- bind_cycles("GHB") |>
  dplyr::select(SEQN, hba1c = LBXGH)

# ── Fasting glucose ────────────────────────────────────────────────────────
glu <- bind_cycles("GLU") |>
  dplyr::select(SEQN, glucose_fast = LBXGLU)

# ── Cholesterol ────────────────────────────────────────────────────────────
tchol <- bind_cycles("TCHOL") |>
  dplyr::select(SEQN, total_chol = LBXTC)

hdl <- bind_cycles("HDL") |>
  dplyr::select(SEQN, hdl_c = LBDHDD)

# LBXTR (2017-18) was renamed to LBXTLG (2021-22); LDL column is shared.
trigly_j <- raw[["TRIGLY_J"]] |>
  dplyr::select(SEQN, triglycerides = LBXTR, ldl_c = LBDLDL)

trigly_l <- raw[["TRIGLY_L"]] |>
  dplyr::select(SEQN, triglycerides = LBXTLG, ldl_c = LBDLDL)

trigly <- dplyr::bind_rows(
  trigly_j |> dplyr::mutate(dplyr::across(dplyr::everything(), as.character)),
  trigly_l |> dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
) |>
  type.convert(as.is = TRUE)

# ── Physical activity questionnaire ────────────────────────────────────────
# Domains: vigorous work, moderate work, transport (walk/bicycle),
# vigorous recreation, moderate recreation, sedentary time.
# CDC codes: 1 = Yes, 2 = No; nhanesA may pre-translate to "Yes"/"No".
# Variable names are consistent across cycles J and L.
paq <- bind_cycles("PAQ") |>
  dplyr::mutate(
    # Activity participation flags: 1 = Yes, 0 = No / missing
    vig_work_yes  = dplyr::case_when(
      PAQ605 %in% c("1", "Yes") ~ 1L, TRUE ~ 0L
    ),
    mod_work_yes  = dplyr::case_when(
      PAQ620 %in% c("1", "Yes") ~ 1L, TRUE ~ 0L
    ),
    transport_yes = dplyr::case_when(
      PAQ635 %in% c("1", "Yes") ~ 1L, TRUE ~ 0L
    ),
    vig_rec_yes   = dplyr::case_when(
      PAQ650 %in% c("1", "Yes") ~ 1L, TRUE ~ 0L
    ),
    mod_rec_yes   = dplyr::case_when(
      PAQ665 %in% c("1", "Yes") ~ 1L, TRUE ~ 0L
    ),
    # Minutes per week per domain (days/week × min/day); 0 if "No" for domain
    vig_work_min_wk  = dplyr::if_else(
      vig_work_yes  == 1L,
      suppressWarnings(as.numeric(PAQ610)) * suppressWarnings(as.numeric(PAD615)),
      0
    ),
    mod_work_min_wk  = dplyr::if_else(
      mod_work_yes  == 1L,
      suppressWarnings(as.numeric(PAQ625)) * suppressWarnings(as.numeric(PAD630)),
      0
    ),
    transport_min_wk = dplyr::if_else(
      transport_yes == 1L,
      suppressWarnings(as.numeric(PAQ640)) * suppressWarnings(as.numeric(PAD645)),
      0
    ),
    vig_rec_min_wk   = dplyr::if_else(
      vig_rec_yes   == 1L,
      suppressWarnings(as.numeric(PAQ655)) * suppressWarnings(as.numeric(PAD660)),
      0
    ),
    mod_rec_min_wk   = dplyr::if_else(
      mod_rec_yes   == 1L,
      suppressWarnings(as.numeric(PAQ670)) * suppressWarnings(as.numeric(PAD675)),
      0
    ),
    # Totals by intensity (all domains combined)
    pa_vig_min_wk     = vig_work_min_wk + vig_rec_min_wk,
    pa_mod_min_wk     = mod_work_min_wk + transport_min_wk + mod_rec_min_wk,
    # MET-minutes/week (vigorous ≈ 8 METs, moderate ≈ 4 METs)
    pa_met_min_wk     = (8 * pa_vig_min_wk) + (4 * pa_mod_min_wk),
    # Moderate-equivalent min/week using 2:1 vigorous-to-moderate equivalency
    pa_mod_eq_min_wk  = pa_mod_min_wk + (2 * pa_vig_min_wk),
    # Meets US Physical Activity Guidelines: >=150 mod-equivalent min/week
    pa_meets_guidelines = pa_mod_eq_min_wk >= 150,
    # Three-level category (CDC/AHA aligned)
    pa_category = dplyr::case_when(
      pa_mod_eq_min_wk <  10  ~ "Inactive",
      pa_mod_eq_min_wk < 150  ~ "Insufficient",
      TRUE                    ~ "Active"
    ) |> factor(levels = c("Inactive", "Insufficient", "Active"), ordered = TRUE),
    # Sedentary time (min/day)
    pa_sedentary_min_day = suppressWarnings(as.numeric(PAD680))
  ) |>
  dplyr::select(
    SEQN,
    pa_vig_min_wk, pa_mod_min_wk, pa_met_min_wk,
    pa_mod_eq_min_wk, pa_meets_guidelines, pa_category,
    pa_sedentary_min_day
  )

# ── Merge all components ───────────────────────────────────────────────────
nhanes_full <- demo |>
  dplyr::left_join(bmx,    by = "SEQN") |>
  dplyr::left_join(bpx,    by = "SEQN") |>
  dplyr::left_join(bpq,    by = "SEQN") |>
  dplyr::left_join(diq,    by = "SEQN") |>
  dplyr::left_join(ghb,    by = "SEQN") |>
  dplyr::left_join(glu,    by = "SEQN") |>
  dplyr::left_join(tchol,  by = "SEQN") |>
  dplyr::left_join(hdl,    by = "SEQN") |>
  dplyr::left_join(trigly, by = "SEQN") |>
  dplyr::left_join(paq,    by = "SEQN")

message(
  "Merged dataset: ", nrow(nhanes_full),
  " rows x ", ncol(nhanes_full), " columns"
)

saveRDS(nhanes_full, "output/nhanes_merged.rds")
