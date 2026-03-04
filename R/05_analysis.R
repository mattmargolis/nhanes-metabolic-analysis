# =============================================================================
# 05_analysis.R
# Descriptive statistics and visualisations for MUNW vs metabolically healthy
# normal-weight (MHNW) individuals.
# Uses NHANES complex survey design (survey package) for weighted estimates.
# =============================================================================

df <- readRDS("output/nhanes_metabolic_flagged.rds")
dir.create("output", showWarnings = FALSE)

# ── 1. Survey design object ─────────────────────────────────────────────────
# NHANES uses a stratified multistage probability design.
# When combining 2 cycles, divide weights by 2.
df <- df |>
  dplyr::mutate(
    wt_adj = wt_full / 2,
    sex    = factor(sex, levels = c("Male", "Female"))
  )

svy <- survey::svydesign(
  id      = ~sdmvpsu,
  strata  = ~sdmvstra,
  weights = ~wt_adj,
  nest    = TRUE,
  data    = df |> dplyr::filter(!is.na(wt_adj) & wt_adj > 0)
)

# ── 2. Weighted prevalence of each condition ────────────────────────────────
cat("\n=== Weighted prevalence (%) in healthy-BMI adults ===\n")

conditions <- c(
  "t2dm", "hypertension", "dyslipidemia", "met_syndrome", "any_metabolic"
)

prev_table <- purrr::map(conditions, function(cond) {
  p <- survey::svymean(as.formula(paste0("~", cond)), svy, na.rm = TRUE)
  data.frame(
    condition  = cond,
    prevalence = round(coef(p)[2] * 100, 1),
    se         = round(survey::SE(p)[2] * 100, 2)
  )
}) |>
  purrr::list_rbind()

print(prev_table)

# ── 3. Stratify by sex ──────────────────────────────────────────────────────
cat("\n=== Prevalence by sex ===\n")

for (sx in c("Male", "Female")) {
  svy_sx <- subset(svy, sex == sx)
  cat(sprintf("\n%s:\n", sx))
  for (cond in c("t2dm", "hypertension", "dyslipidemia", "met_syndrome")) {
    p <- survey::svymean(
      as.formula(paste0("~", cond)), svy_sx, na.rm = TRUE
    )
    cat(sprintf("  %-20s %.1f%%\n", cond, coef(p)[2] * 100))
  }
}

# ── 4. Cardiometabolic profile: MUNW vs MHNW ───────────────────────────────
svy_munw <- subset(svy, any_metabolic == TRUE)
svy_mhnw <- subset(svy, any_metabolic == FALSE)

compare_mean <- function(var) {
  m1 <- survey::svymean(as.formula(paste0("~", var)), svy_munw, na.rm = TRUE)
  m2 <- survey::svymean(as.formula(paste0("~", var)), svy_mhnw, na.rm = TRUE)
  data.frame(
    variable  = var,
    munw_mean = round(coef(m1), 2),
    mhnw_mean = round(coef(m2), 2)
  )
}

cat("\n=== Mean values: MUNW vs MHNW ===\n")
comparison <- purrr::map(
  c("age", "bmi", "waist_cm", "hba1c",
    "sbp_mean", "dbp_mean", "hdl_c", "ldl_c", "triglycerides"),
  compare_mean
) |>
  purrr::list_rbind()
print(comparison)

# ── 5. Plots ─────────────────────────────────────────────────────────────────

cond_labels <- c(
  "t2dm"         = "Type 2 Diabetes",
  "hypertension" = "Hypertension",
  "dyslipidemia" = "Dyslipidemia",
  "met_syndrome" = "Metabolic Syndrome"
)

# 5a. Prevalence bar chart
p1 <- prev_table |>
  dplyr::filter(condition != "any_metabolic") |>
  dplyr::mutate(condition = dplyr::recode(condition, !!!cond_labels)) |>
  ggplot2::ggplot(
    ggplot2::aes(x = reorder(condition, -prevalence), y = prevalence)
  ) +
  ggplot2::geom_col(fill = "#2c7bb6", width = 0.6) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin = prevalence - 1.96 * se,
      ymax = prevalence + 1.96 * se
    ),
    width = 0.2
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = paste0(prevalence, "%")), vjust = -0.8, size = 3.5
  ) +
  ggplot2::labs(
    title    = "Weighted Prevalence of Metabolic Conditions\nin Normal-Weight U.S. Adults",
    subtitle = "NHANES 2017-2018 & 2021-2022",
    x        = NULL,
    y        = "Prevalence (%)"
  ) +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 15, hjust = 1))

ggplot2::ggsave(
  "output/fig1_prevalence.png", p1, width = 7, height = 5, dpi = 150
)

# 5b. BMI distribution by metabolic status
p2 <- df |>
  dplyr::filter(!is.na(any_metabolic)) |>
  dplyr::mutate(group = ifelse(any_metabolic, "MUNW", "MHNW")) |>
  ggplot2::ggplot(ggplot2::aes(x = bmi, fill = group, colour = group)) +
  ggplot2::geom_density(alpha = 0.4) +
  ggplot2::scale_fill_manual(
    values = c("MHNW" = "#1a9641", "MUNW" = "#d7191c")
  ) +
  ggplot2::scale_colour_manual(
    values = c("MHNW" = "#1a9641", "MUNW" = "#d7191c")
  ) +
  ggplot2::labs(
    title    = "BMI Distribution by Metabolic Health Status",
    subtitle = "Normal-weight adults (BMI 18.5-24.9), NHANES 2017-2022",
    x        = "BMI (kg/m2)",
    y        = "Density",
    fill     = NULL,
    colour   = NULL
  ) +
  ggplot2::theme_minimal(base_size = 13)

ggplot2::ggsave(
  "output/fig2_bmi_density.png", p2, width = 7, height = 4.5, dpi = 150
)

# 5c. Condition co-occurrence heatmap
cond_cols <- c("t2dm", "hypertension", "dyslipidemia", "met_syndrome")

co_mat <- df |>
  dplyr::filter(!is.na(any_metabolic)) |>
  dplyr::select(dplyr::all_of(cond_cols)) |>
  dplyr::mutate(dplyr::across(dplyr::everything(), as.integer))

co_cor <- cor(co_mat, use = "pairwise.complete.obs")

short_labels <- c(
  "t2dm"         = "T2DM",
  "hypertension" = "Hypertension",
  "dyslipidemia" = "Dyslipidemia",
  "met_syndrome" = "MetSyn"
)

co_long <- as.data.frame(as.table(co_cor)) |>
  dplyr::rename(cond1 = Var1, cond2 = Var2, r = Freq) |>
  dplyr::mutate(
    dplyr::across(c(cond1, cond2), ~ dplyr::recode(., !!!short_labels))
  )

p3 <- ggplot2::ggplot(co_long, ggplot2::aes(cond1, cond2, fill = r)) +
  ggplot2::geom_tile(colour = "white") +
  ggplot2::geom_text(ggplot2::aes(label = round(r, 2)), size = 3.5) +
  ggplot2::scale_fill_gradient2(
    low      = "#4575b4",
    mid      = "white",
    high     = "#d73027",
    midpoint = 0,
    limits   = c(-1, 1)
  ) +
  ggplot2::labs(
    title = "Correlation Between Metabolic Conditions\n(Normal-Weight Adults)",
    x     = NULL,
    y     = NULL,
    fill  = "r"
  ) +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)
  )

ggplot2::ggsave(
  "output/fig3_comorbidity_heatmap.png", p3,
  width = 5.5, height = 4.5, dpi = 150
)

# 5d. Number of conditions distribution
p4 <- df |>
  dplyr::filter(!is.na(n_conditions)) |>
  dplyr::count(n_conditions) |>
  dplyr::mutate(pct = n / sum(n) * 100) |>
  ggplot2::ggplot(ggplot2::aes(x = factor(n_conditions), y = pct)) +
  ggplot2::geom_col(fill = "#756bb1", width = 0.55) +
  ggplot2::geom_text(
    ggplot2::aes(label = paste0(round(pct, 1), "%")), vjust = -0.5, size = 3.5
  ) +
  ggplot2::labs(
    title    = "Number of Metabolic Conditions per Normal-Weight Adult",
    subtitle = "NHANES 2017-2018 & 2021-2022",
    x        = "Number of conditions",
    y        = "% of sample"
  ) +
  ggplot2::theme_minimal(base_size = 13)

ggplot2::ggsave(
  "output/fig4_condition_count.png", p4, width = 6, height = 4.5, dpi = 150
)

message("\nAll figures saved to output/")
