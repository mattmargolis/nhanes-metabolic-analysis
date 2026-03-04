# =============================================================================
# 05_analysis.R
# Descriptive statistics and visualisations for MUNW vs metabolically healthy
# normal-weight (MHNW) individuals.
# Uses NHANES complex survey design (survey package) for weighted estimates.
# =============================================================================

library(dplyr)
library(ggplot2)
library(survey)
library(tidyr)

df <- readRDS("output/nhanes_metabolic_flagged.rds")
dir.create("output", showWarnings = FALSE)

# ── 1. Survey design object ───────────────────────────────────────────────────
# NHANES uses a stratified multistage probability design.
# When combining 2 cycles, divide weights by 2.
df <- df %>%
  mutate(wt_adj = wt_full / 2)

df <- df %>%
  mutate(sex = factor(sex, levels = c("Male", "Female")))

svy <- svydesign(
  id      = ~sdmvpsu,
  strata  = ~sdmvstra,
  weights = ~wt_adj,
  nest    = TRUE,
  data    = df %>% filter(!is.na(wt_adj) & wt_adj > 0)
)

# ── 2. Weighted prevalence of each condition ──────────────────────────────────
cat("\n=== Weighted prevalence (%) in healthy-BMI adults ===\n")

conditions <- c("t2dm", "hypertension", "dyslipidemia", "met_syndrome", "any_metabolic")
prev_table <- purrr::map_dfr(conditions, function(cond) {
  p <- svymean(as.formula(paste0("~", cond)), svy, na.rm = TRUE)
  data.frame(
    condition  = cond,
    prevalence = round(coef(p)[2] * 100, 1),
    se         = round(SE(p)[2] * 100, 2)
  )
})
print(prev_table)

# ── 3. Stratify by sex ────────────────────────────────────────────────────────
cat("\n=== Prevalence by sex ===\n")
for (sx in c("Male", "Female")) {
  svy_sx <- subset(svy, sex == sx)
  cat(sprintf("\n%s:\n", sx))
  for (cond in c("t2dm", "hypertension", "dyslipidemia", "met_syndrome")) {
    p <- svymean(as.formula(paste0("~", cond)), svy_sx, na.rm = TRUE)
    cat(sprintf("  %-20s %.1f%%\n", cond, coef(p)[2] * 100))
  }
}

# ── 4. Demographic comparison: MUNW vs MHNW ──────────────────────────────────
svy_munw  <- subset(svy,  any_metabolic == TRUE)
svy_mhnw  <- subset(svy,  any_metabolic == FALSE)

compare_mean <- function(var) {
  m1 <- svymean(as.formula(paste0("~", var)), svy_munw,  na.rm = TRUE)
  m2 <- svymean(as.formula(paste0("~", var)), svy_mhnw, na.rm = TRUE)
  data.frame(variable = var,
             MUNW_mean = round(coef(m1), 2),
             MHNW_mean = round(coef(m2), 2))
}

cat("\n=== Mean values: MUNW vs MHNW ===\n")
comparison <- purrr::map_dfr(c("age", "bmi", "waist_cm", "hba1c",
                                "sbp_mean", "dbp_mean", "hdl_c",
                                "ldl_c", "triglycerides"), compare_mean)
print(comparison)

# ── 5. Plots ──────────────────────────────────────────────────────────────────

# 5a. Prevalence bar chart
p1 <- prev_table %>%
  filter(condition != "any_metabolic") %>%
  mutate(
    condition = recode(condition,
      "t2dm"         = "Type 2 Diabetes",
      "hypertension" = "Hypertension",
      "dyslipidemia" = "Dyslipidemia",
      "met_syndrome" = "Metabolic Syndrome"
    )
  ) %>%
  ggplot(aes(x = reorder(condition, -prevalence), y = prevalence)) +
  geom_col(fill = "#2c7bb6", width = 0.6) +
  geom_errorbar(aes(ymin = prevalence - 1.96 * se,
                    ymax = prevalence + 1.96 * se), width = 0.2) +
  geom_text(aes(label = paste0(prevalence, "%")), vjust = -0.8, size = 3.5) +
  labs(
    title    = "Weighted Prevalence of Metabolic Conditions\nin Normal-Weight U.S. Adults",
    subtitle = "NHANES 2017–2018 & 2021–2022",
    x        = NULL,
    y        = "Prevalence (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

ggsave("output/fig1_prevalence.png", p1, width = 7, height = 5, dpi = 150)

# 5b. BMI distribution by metabolic status
p2 <- df %>%
  filter(!is.na(any_metabolic)) %>%
  mutate(group = ifelse(any_metabolic, "MUNW", "MHNW")) %>%
  ggplot(aes(x = bmi, fill = group, colour = group)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values  = c("MHNW" = "#1a9641", "MUNW" = "#d7191c")) +
  scale_colour_manual(values = c("MHNW" = "#1a9641", "MUNW" = "#d7191c")) +
  labs(
    title    = "BMI Distribution by Metabolic Health Status",
    subtitle = "Normal-weight adults (BMI 18.5–24.9), NHANES 2017–2022",
    x        = "BMI (kg/m²)",
    y        = "Density",
    fill     = NULL, colour = NULL
  ) +
  theme_minimal(base_size = 13)

ggsave("output/fig2_bmi_density.png", p2, width = 7, height = 4.5, dpi = 150)

# 5c. Condition co-occurrence heatmap
cond_cols <- c("t2dm", "hypertension", "dyslipidemia", "met_syndrome")
co_mat <- df %>%
  filter(!is.na(any_metabolic)) %>%
  select(all_of(cond_cols)) %>%
  mutate(across(everything(), as.integer))

co_cor <- cor(co_mat, use = "pairwise.complete.obs")

co_long <- as.data.frame(as.table(co_cor)) %>%
  rename(cond1 = Var1, cond2 = Var2, r = Freq) %>%
  mutate(across(c(cond1, cond2), ~ recode(.,
    "t2dm"         = "T2DM",
    "hypertension" = "Hypertension",
    "dyslipidemia" = "Dyslipidemia",
    "met_syndrome" = "MetSyn"
  )))

p3 <- ggplot(co_long, aes(cond1, cond2, fill = r)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = round(r, 2)), size = 3.5) +
  scale_fill_gradient2(low = "#4575b4", mid = "white", high = "#d73027",
                       midpoint = 0, limits = c(-1, 1)) +
  labs(
    title = "Correlation Between Metabolic Conditions\n(Normal-Weight Adults)",
    x = NULL, y = NULL, fill = "r"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave("output/fig3_comorbidity_heatmap.png", p3, width = 5.5, height = 4.5, dpi = 150)

# 5d. Number of conditions distribution
p4 <- df %>%
  filter(!is.na(n_conditions)) %>%
  count(n_conditions) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = factor(n_conditions), y = pct)) +
  geom_col(fill = "#756bb1", width = 0.55) +
  geom_text(aes(label = paste0(round(pct, 1), "%")), vjust = -0.5, size = 3.5) +
  labs(
    title    = "Number of Metabolic Conditions per Normal-Weight Adult",
    subtitle = "NHANES 2017–2018 & 2021–2022",
    x        = "Number of conditions",
    y        = "% of sample"
  ) +
  theme_minimal(base_size = 13)

ggsave("output/fig4_condition_count.png", p4, width = 6, height = 4.5, dpi = 150)

message("\nAll figures saved to output/")
