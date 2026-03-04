# =============================================================================
# 06_exercise_analysis.R
# Investigates two questions using NHANES physical activity (PAQ) data:
#
#  1. NORMAL-WEIGHT ADULTS (BMI 18.5–24.9):
#     Does exercise reduce the risk of metabolic disorders among individuals
#     who would typically be classified as "healthy" by BMI alone?
#
#  2. METABOLICALLY HEALTHY OBESE (MHO) — OBESE ADULTS (BMI >= 30):
#     Do obese individuals who exercise more frequently avoid metabolic
#     disorders despite their BMI? This tests whether the current BMI-based
#     classification system fails to account for muscle mass and fitness,
#     misclassifying some active, metabolically healthy individuals as
#     high-risk purely due to elevated weight.
#
# Figures produced:
#   fig5_exercise_metabolic_risk.png   – metabolic risk by activity level
#                                        (normal-weight adults)
#   fig6_exercise_dose_response.png    – predicted metabolic risk vs exercise
#                                        dose (logistic regression)
#   fig7_mho_by_exercise.png           – MHO prevalence by activity level
#                                        (obese adults)
#   fig8_bmi_exercise_metabolic.png    – exercise distribution by BMI category
#                                        and metabolic health status
# =============================================================================

df_all <- readRDS("output/nhanes_metabolic_all.rds")
dir.create("output", showWarnings = FALSE)

# ── 1. Survey design objects ─────────────────────────────────────────────────
# Complex multistage design; wt_adj already = wt_full / 2 (see 04_metabolic_conditions.R).

make_svy <- function(data) {
  survey::svydesign(
    id      = ~sdmvpsu,
    strata  = ~sdmvstra,
    weights = ~wt_adj,
    nest    = TRUE,
    data    = data |> dplyr::filter(!is.na(wt_adj) & wt_adj > 0)
  )
}

svy_all    <- make_svy(df_all)
svy_normal <- subset(svy_all, bmi_category == "Normal weight")
svy_obese  <- subset(svy_all, bmi_category == "Obese")

# ── 2. Metabolically Healthy Obese (MHO) flag ───────────────────────────────
# MHO: obese (BMI >= 30) with zero metabolic conditions.
# MUO: metabolically unhealthy obese (>=1 condition).
df_all <- df_all |>
  dplyr::mutate(
    mho = bmi_category == "Obese" & !any_metabolic
  )

svy_all   <- make_svy(df_all)
svy_obese <- subset(svy_all, bmi_category == "Obese")

# ── 3. Analysis 1: Exercise and metabolic risk in normal-weight adults ────────

cat("\n=== ANALYSIS 1: Exercise and metabolic risk (normal-weight adults) ===\n\n")

# Weighted prevalence of any_metabolic by pa_category
prev_pa_nw <- survey::svyby(
  formula  = ~any_metabolic,
  by       = ~pa_category,
  design   = svy_normal,
  FUN      = survey::svymean,
  na.rm    = TRUE,
  keep.names = FALSE
) |>
  dplyr::rename(prevalence = any_metabolicTRUE, se = `se.any_metabolicTRUE`) |>
  dplyr::mutate(
    prevalence_pct = round(prevalence * 100, 1),
    ci_lo          = (prevalence - 1.96 * se) * 100,
    ci_hi          = (prevalence + 1.96 * se) * 100
  )

cat("Weighted prevalence of any metabolic condition by activity level:\n")
print(prev_pa_nw[, c("pa_category", "prevalence_pct", "ci_lo", "ci_hi")])

# Weighted prevalence per condition by activity level
conditions <- c("t2dm", "hypertension", "dyslipidemia", "met_syndrome")
prev_cond_pa <- purrr::map(conditions, function(cond) {
  survey::svyby(
    formula    = stats::as.formula(paste0("~", cond)),
    by         = ~pa_category,
    design     = svy_normal,
    FUN        = survey::svymean,
    na.rm      = TRUE,
    keep.names = FALSE
  ) |>
    dplyr::rename(
      prev = dplyr::ends_with("TRUE"),
      se   = dplyr::starts_with("se.")
    ) |>
    dplyr::mutate(
      condition      = cond,
      prevalence_pct = round(prev * 100, 1),
      ci_lo          = (prev - 1.96 * se) * 100,
      ci_hi          = (prev + 1.96 * se) * 100
    ) |>
    dplyr::select(pa_category, condition, prevalence_pct, ci_lo, ci_hi)
}) |>
  purrr::list_rbind()

# Survey-weighted logistic regression: any_metabolic ~ pa_category + covariates
model_nw <- survey::svyglm(
  any_metabolic ~ pa_category + age + sex + race + pir,
  design = svy_normal,
  family = stats::quasibinomial()
)

cat("\nSurvey-weighted logistic regression (normal-weight adults):\n")
print(summary(model_nw)$coefficients[1:4, , drop = FALSE])

# Predicted probability across pa_met_min_wk range for dose-response curve
nw_data <- df_all |>
  dplyr::filter(
    bmi_category == "Normal weight",
    !is.na(pa_met_min_wk), !is.na(wt_adj), wt_adj > 0
  )

model_nw_cont <- survey::svyglm(
  any_metabolic ~ pa_met_min_wk + age + sex + race + pir,
  design = subset(make_svy(nw_data), !is.na(pa_met_min_wk)),
  family = stats::quasibinomial()
)

# Prediction grid: MET-min/week from 0 to 3000 at median covariate values
pred_grid <- data.frame(
  pa_met_min_wk = seq(0, 3000, by = 50),
  age           = stats::median(nw_data$age,          na.rm = TRUE),
  sex           = factor("Male", levels = levels(nw_data$sex)),
  race          = factor("Non-Hispanic White",         levels = levels(nw_data$race)),
  pir           = stats::median(nw_data$pir,           na.rm = TRUE)
)

pred_prob <- stats::predict(model_nw_cont, newdata = pred_grid, type = "response",
                            se.fit = TRUE)
pred_grid$prob    <- as.numeric(pred_prob$fit)
pred_grid$prob_lo <- pmax(0, pred_grid$prob - 1.96 * as.numeric(pred_prob$se.fit))
pred_grid$prob_hi <- pmin(1, pred_grid$prob + 1.96 * as.numeric(pred_prob$se.fit))

# ── 4. Analysis 2: MHO and exercise in obese adults ──────────────────────────

cat("\n=== ANALYSIS 2: Metabolically Healthy Obese (MHO) and exercise ===\n\n")

# Weighted MHO prevalence overall
mho_overall <- survey::svymean(~mho, subset(svy_all, bmi_category == "Obese"),
                               na.rm = TRUE)
cat(sprintf(
  "Weighted MHO prevalence (obese adults): %.1f%% (SE: %.1f%%)\n",
  coef(mho_overall)["mhoTRUE"] * 100,
  survey::SE(mho_overall)["mhoTRUE"] * 100
))

# Weighted MHO prevalence by pa_category
prev_mho_pa <- survey::svyby(
  formula    = ~mho,
  by         = ~pa_category,
  design     = subset(svy_all, bmi_category == "Obese"),
  FUN        = survey::svymean,
  na.rm      = TRUE,
  keep.names = FALSE
) |>
  dplyr::rename(prevalence = mhoTRUE, se = `se.mhoTRUE`) |>
  dplyr::mutate(
    prevalence_pct = round(prevalence * 100, 1),
    ci_lo          = (prevalence - 1.96 * se) * 100,
    ci_hi          = (prevalence + 1.96 * se) * 100
  )

cat("\nMHO prevalence by activity level (obese adults):\n")
print(prev_mho_pa[, c("pa_category", "prevalence_pct", "ci_lo", "ci_hi")])

# Survey-weighted logistic regression: mho ~ pa_category + age + sex + race
model_mho <- survey::svyglm(
  mho ~ pa_category + age + sex + race + pir,
  design = subset(svy_all, bmi_category == "Obese"),
  family = stats::quasibinomial()
)

cat("\nLogistic regression: odds of MHO by activity level (obese adults):\n")
print(round(exp(stats::coef(model_mho)), 2))

# Exercise distribution by BMI category and metabolic status (for Fig 8)
exercise_summary <- survey::svyby(
  formula    = ~pa_met_min_wk,
  by         = ~bmi_category + any_metabolic,
  design     = subset(svy_all, !is.na(pa_met_min_wk)),
  FUN        = survey::svymean,
  na.rm      = TRUE,
  keep.names = FALSE
) |>
  dplyr::rename(mean_met = pa_met_min_wk, se_met = se) |>
  dplyr::mutate(
    metabolic_status = ifelse(any_metabolic, "Metabolically Unhealthy", "Metabolically Healthy"),
    ci_lo            = pmax(0, mean_met - 1.96 * se_met),
    ci_hi            = mean_met + 1.96 * se_met
  ) |>
  dplyr::filter(!is.na(bmi_category))

# ── 5. Figures ────────────────────────────────────────────────────────────────

cond_labels <- c(
  "t2dm"         = "Type 2 Diabetes",
  "hypertension" = "Hypertension",
  "dyslipidemia" = "Dyslipidemia",
  "met_syndrome" = "Metabolic Syndrome"
)

pa_colours <- c(
  "Inactive"     = "#d7191c",
  "Insufficient" = "#fdae61",
  "Active"       = "#1a9641"
)

# Fig 5: Metabolic condition prevalence by exercise level (normal-weight)
fig5_data <- prev_cond_pa |>
  dplyr::mutate(
    condition = dplyr::recode(condition, !!!cond_labels),
    condition = factor(condition, levels = unname(cond_labels))
  )

p5 <- ggplot2::ggplot(
  fig5_data,
  ggplot2::aes(x = condition, y = prevalence_pct, fill = pa_category)
) +
  ggplot2::geom_col(position = ggplot2::position_dodge(0.75), width = 0.65) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = ci_lo, ymax = ci_hi),
    position = ggplot2::position_dodge(0.75), width = 0.2
  ) +
  ggplot2::scale_fill_manual(
    values = pa_colours,
    name   = "Activity level"
  ) +
  ggplot2::labs(
    title    = "Metabolic Condition Prevalence by Physical Activity Level",
    subtitle = "Normal-weight adults (BMI 18.5-24.9), NHANES 2017-2022",
    x        = NULL,
    y        = "Weighted prevalence (%)"
  ) +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    axis.text.x   = ggplot2::element_text(angle = 15, hjust = 1),
    legend.position = "top"
  )

ggplot2::ggsave(
  "output/fig5_exercise_metabolic_risk.png", p5, width = 8, height = 5.5, dpi = 150
)

# Fig 6: Dose-response — predicted metabolic risk vs MET-min/week
p6 <- ggplot2::ggplot(pred_grid, ggplot2::aes(x = pa_met_min_wk, y = prob * 100)) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = prob_lo * 100, ymax = prob_hi * 100),
    fill = "#2c7bb6", alpha = 0.2
  ) +
  ggplot2::geom_line(colour = "#2c7bb6", linewidth = 1.2) +
  ggplot2::geom_vline(
    xintercept = 600, linetype = "dashed", colour = "#d7191c", linewidth = 0.8
  ) +
  ggplot2::annotate(
    "text", x = 650, y = max(pred_grid$prob_hi * 100) * 0.95,
    label = "WHO\nminimum\n(600 MET-min/wk)",
    hjust = 0, size = 3, colour = "#d7191c"
  ) +
  ggplot2::labs(
    title    = "Exercise Dose-Response: Risk of Any Metabolic Condition",
    subtitle = "Normal-weight adults — survey-weighted logistic regression\n(adjusted for age, sex, race, poverty-income ratio)",
    x        = "Physical activity (MET-minutes per week)",
    y        = "Predicted probability of metabolic condition (%)"
  ) +
  ggplot2::theme_minimal(base_size = 13)

ggplot2::ggsave(
  "output/fig6_exercise_dose_response.png", p6, width = 8, height = 5, dpi = 150
)

# Fig 7: MHO prevalence by exercise level (obese adults)
p7 <- prev_mho_pa |>
  dplyr::mutate(
    pa_category = factor(pa_category, levels = c("Inactive", "Insufficient", "Active"))
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = pa_category, y = prevalence_pct, fill = pa_category)) +
  ggplot2::geom_col(width = 0.6) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = ci_lo, ymax = ci_hi), width = 0.2
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = paste0(prevalence_pct, "%")),
    vjust = -0.7, size = 3.8
  ) +
  ggplot2::scale_fill_manual(values = pa_colours, guide = "none") +
  ggplot2::labs(
    title    = "Metabolically Healthy Obese (MHO) Prevalence by Activity Level",
    subtitle = paste0(
      "Obese adults (BMI \u2265 30), NHANES 2017-2022\n",
      "MHO = obese with zero metabolic conditions"
    ),
    x        = "Physical activity level",
    y        = "Weighted prevalence of MHO (%)"
  ) +
  ggplot2::theme_minimal(base_size = 13)

ggplot2::ggsave(
  "output/fig7_mho_by_exercise.png", p7, width = 7, height = 5, dpi = 150
)

# Fig 8: Mean exercise by BMI category and metabolic status
p8 <- exercise_summary |>
  dplyr::mutate(
    bmi_category = factor(
      bmi_category,
      levels = c("Underweight", "Normal weight", "Overweight", "Obese")
    )
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x    = bmi_category,
      y    = mean_met,
      fill = metabolic_status,
      colour = metabolic_status
    )
  ) +
  ggplot2::geom_col(position = ggplot2::position_dodge(0.75), width = 0.65, alpha = 0.85) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = ci_lo, ymax = ci_hi),
    position = ggplot2::position_dodge(0.75), width = 0.25
  ) +
  ggplot2::scale_fill_manual(
    values = c("Metabolically Healthy" = "#1a9641", "Metabolically Unhealthy" = "#d7191c"),
    name   = NULL
  ) +
  ggplot2::scale_colour_manual(
    values = c("Metabolically Healthy" = "#1a9641", "Metabolically Unhealthy" = "#d7191c"),
    guide  = "none"
  ) +
  ggplot2::labs(
    title    = "Physical Activity by BMI Category and Metabolic Health Status",
    subtitle = paste0(
      "Adults age \u2265 18, NHANES 2017-2022\n",
      "Survey-weighted mean MET-minutes/week (95% CI)"
    ),
    x        = "BMI category",
    y        = "Mean MET-minutes per week"
  ) +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(legend.position = "top")

ggplot2::ggsave(
  "output/fig8_bmi_exercise_metabolic.png", p8, width = 8, height = 5.5, dpi = 150
)

message("\nAll exercise analysis figures saved to output/")

# =============================================================================
# TODO: XGBoost Risk Classifier for Incident Metabolic Disease
# =============================================================================
#
# OBJECTIVE:
#   Build a supervised machine-learning classifier (XGBoost) to identify
#   which currently "healthy" individuals (no metabolic conditions, normal or
#   overweight BMI) are at highest risk of progressing to metabolic disease.
#
# ── NHANES FEATURES (available in current dataset) ──────────────────────────
#   Demographics    : age, sex, race, poverty-income ratio (pir)
#   Anthropometric  : bmi, waist_cm, bmi_category
#   Biomarkers      : hba1c, glucose_fast, hdl_c, ldl_c, triglycerides,
#                     sbp_mean, dbp_mean, total_chol
#   Physical activity: pa_met_min_wk, pa_mod_eq_min_wk, pa_category,
#                     pa_sedentary_min_day, pa_meets_guidelines
#   Sub-threshold   : ms_score (count of MetSyn components even if < 3)
#
# ── ADDITIONAL NHANES MODULES TO INCORPORATE ────────────────────────────────
#   SMQ         – smoking history (SMQ020, SMQ040, pack-years)
#   ALQ         – alcohol use (ALQ130: avg drinks/day)
#   DBQ/DR1TOT  – diet quality (dietary recall; HEI-2020 score)
#   SLQ         – sleep duration/quality (SLQ050, SLQ060)
#   MCQ         – medical history (MCQ160b: coronary heart disease, etc.)
#   FASTQX      – fasting status at blood draw (to flag non-fasting glucose)
#   WHQMEC      – weight history (recent weight change trajectory)
#
#   DXA body composition files — KEY for addressing muscle-mass misclassification:
#     DXXFEM_J / DXXFEM_L : femur DXA (total fat %, lean mass)
#     DXXSPN_J / DXXSPN_L : spine DXA
#     DXXTOT_J / DXXTOT_L : total body DXA (appendicular lean mass index, ALMI)
#   Incorporating ALMI would allow distinction of "high BMI due to muscle mass"
#   from "high BMI due to fat mass," directly addressing the misclassification
#   of muscular individuals identified in the MHO analysis above.
#
# ── EXTERNAL LONGITUDINAL DATASETS ──────────────────────────────────────────
#   UK Biobank (n > 500,000): longitudinal health records, wrist accelerometry
#     (objective PA), imaging-derived body composition, linked EHR outcomes.
#     Ideal for large-scale incident MetSyn prediction with validated labels.
#
#   Framingham Heart Study Offspring cohort: decades of repeated metabolic
#     measurements; gold standard for cardiovascular/metabolic risk modeling.
#
#   MESA (Multi-Ethnic Study of Atherosclerosis): diverse cohort, imaging-
#     derived adiposity, CT-based coronary calcium — good for cross-ethnic
#     generalisability testing.
#
#   CARDIA (Coronary Artery Risk Development in Young Adults): longitudinal
#     from young adulthood (~18-30 y); captures early metabolic trajectories.
#
#   IRAS (Insulin Resistance Atherosclerosis Study): detailed insulin-
#     resistance phenotyping with longitudinal outcomes.
#
# ── NHANES LIMITATION & MITIGATION STRATEGIES ───────────────────────────────
#   NHANES is cross-sectional; direct longitudinal prediction is not feasible
#   within a single cycle. Approaches:
#     1. Train on NHANES feature-rich baseline; validate on UK Biobank /
#        Framingham using overlapping feature sets.
#     2. Define a "sub-threshold progression" proxy: individuals with 1-2
#        MetSyn components (ms_score 1-2) among those with 0 components at
#        "baseline" — models risk of crossing the threshold.
#     3. Use HbA1c 5.7-6.4% (prediabetes) and glucose 100-125 (IFG) as
#        soft positive labels indicating likely future progression.
#
# ── IMPLEMENTATION PLAN ──────────────────────────────────────────────────────
#
#   Step 1 — Feature engineering
#     * Load DXA modules (DXXTOT_J/L) and join ALMI, fat %, trunk-to-limb
#       fat ratio to df_all
#     * Compute HEI-2020 diet quality score from DR1TOT/DR2TOT
#     * Create interaction terms: bmi × pa_met_min_wk, ALMI × ms_score
#     * Bin sub-threshold biomarkers: HbA1c bins (< 5.7, 5.7-6.4, >= 6.5),
#       fasting glucose bins (< 100, 100-125, >= 126)
#
#   Step 2 — Target variable
#     * Cross-sectional proxy: any_metabolic among adults age 18-45 with
#       sub-threshold features (ms_score = 1 or 2) — binary progression risk
#     * Longitudinal (external): incident MetSyn within 5 years
#
#   Step 3 — Model pipeline
#     * Stratified train/test split (80/20) preserving BMI category balance
#     * Survey-weight-aware training: pass wt_adj as sample weights to xgboost
#     * Hyperparameter tuning: tidymodels + finetune (Bayesian optimisation),
#       tune over: eta, max_depth, min_child_weight, subsample, colsample_bytree
#     * SHAP feature importance: SHAPforxgboost::shap.plot.summary()
#     * Calibration: probably::cal_plot_logistic()
#
#   Step 4 — Validation & fairness
#     * Internal: 5-fold stratified CV, survey-weighted AUC + Brier score
#     * Subgroup performance: AUC by sex, race/ethnicity, BMI category
#     * External: transfer to UK Biobank / Framingham feature subset; report
#       AUC degradation as a measure of distributional shift
#     * Fairness audit: equalised odds across race/sex subgroups
#
# ── PACKAGES NEEDED ──────────────────────────────────────────────────────────
#   xgboost, tidymodels, recipes, finetune,
#   SHAPforxgboost, probably, pROC, ggplot2
#
# =============================================================================
