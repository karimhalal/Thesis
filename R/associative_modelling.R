
# PART 1 — CAUSE-SPECIFIC COX HR MODELS (ASSOCIATIVE)
#
# Three exposures:
#   1.  GEN_10              — Sense of belonging (4-cat; ref = 1 = Very strong)
#   2.  CCC_280 + CCC_290   — Mental health: mood + anxiety disorders (binary)
#   3.  first_drug_class    — Prescription initiation (3-cat: 0=No Rx, 1=Opioid, 2=BZD)
#
# Two formulations per exposure:
#   _ff  : centered dummies (*_cat#_c), 4-knot RCS age, pipeline interactions
#   _raw : uncentered dummies (*_cat#), linear age, raw interactions
#
# Minimal adjustment sets (DAG-derived, dags.R):
#   GEN_10          : Age, multiple_conditions, marital_status, material_deprivation,
#                     pain, self_report_MH (GEN_02B)
#   MH              : Age, Alcohol (ALWDWKY), marital_status, material_deprivation,
#                     multiple_conditions, self_reported_stress (GEN_07), sex
#   first_drug_class: Age, Alcohol, material_deprivation, multiple_conditions,
#                     mental health CCCs, pain, self_report_drug_use, sex, cycle
#
# Competing-risks structure:
#   event = 1 (NMS death), event = 2 (other-cause death), event = 0 (censored)
#   Cause-specific Cox for event=1; nuisance model for event=2 (no exposure term).
#
# Data     : completed_list — list of m transformed, imputed data.frames
#            (output of run-imputation.R → run_transform_pipeline())
# Pooling  : manual Rubin's rules on log-HR scale (works for m = 1)


library(survival)
library(dplyr)
library(purrr)
library(tibble)
library(ggplot2)
library(rms)


# Apply rubin's rule for pooling HR estimates 
# Returns a tidy tibble of pooled log-HRs, SEs, HRs, 95% CIs, p-values.
# Handles m = 1 (between-imputation variance collapses to 0).
.pool_cox <- function(fit_list) {
  coefs <- lapply(fit_list, coef)
  vcovs <- lapply(fit_list, vcov)
  m     <- length(fit_list)

  Q_bar <- Reduce("+", coefs) / m
  U_bar <- Reduce("+", vcovs) / m

  B <- if (m > 1L) {
    Reduce("+", lapply(coefs, function(q) outer(q - Q_bar, q - Q_bar))) / (m - 1L)
  } else {
    matrix(0, nrow = length(Q_bar), ncol = length(Q_bar),
           dimnames = list(names(Q_bar), names(Q_bar)))
  }

  T_se <- sqrt(diag(U_bar + (1 + 1 / m) * B))
  z    <- Q_bar / T_se
  p    <- 2 * pnorm(-abs(z))

  tibble(
    term     = names(Q_bar),
    log_hr   = Q_bar,
    se       = T_se,
    hr       = exp(Q_bar),
    hr_lower = exp(Q_bar - 1.96 * T_se),
    hr_upper = exp(Q_bar + 1.96 * T_se),
    z_stat   = z,
    p_value  = p
  )
}


# Coerce a bare data.frame to a length-1 list; pass lists through unchanged.
.as_data_list <- function(x) if (is.data.frame(x)) list(x) else x


# Fit cause specific cox models.
# `data` may be a single data.frame or a list of data.frames (imputed datasets).
# Returns list(fits = list of coxph, pooled = tidy tibble).
.fit_cox_pooled <- function(formula, data) {
  data_list <- .as_data_list(data)
  fits <- lapply(data_list, function(d) {
    coxph(formula, data = d, ties = "efron", x = TRUE, y = TRUE)
  })
  list(fits = fits, pooled = .pool_cox(fits))
}


# Assumption tests
# Outputs: (1) cox.zph Schoenfeld table, (2) log-log plot by exposure group,
#          (3) martingale residuals vs linear predictor, (4) dfbeta plots.
# Time-varying coefficient plots are intentionally excluded.
cox_assumptions <- function(fit, data, exposure_var = NULL, label = "Model") {

  # 1. Schoenfeld residuals — global and per-term PH test
  ph <- cox.zph(fit, transform = "km")
  cat("\n── Schoenfeld PH Test —", label, "──\n")
  print(ph)

  # 2. Log-log plot by exposure group
  if (!is.null(exposure_var) && exposure_var %in% names(data)) {
    grp       <- factor(data[[exposure_var]])
    tmp       <- data
    tmp$.grp  <- grp
    km_fit    <- survfit(Surv(survt, event == 1) ~ .grp, data = tmp)
    km_s      <- summary(km_fit)
    km_df     <- tibble(
      time   = km_s$time,
      surv   = km_s$surv,
      lower  = km_s$lower,
      upper  = km_s$upper,
      strata = sub("^\\.grp=", "", km_s$strata)
    ) %>%
      filter(surv > 0, surv < 1, lower > 0, upper > 0, upper < 1) %>%
      mutate(
        lls       = log(-log(surv)),
        lls_lower = log(-log(upper)),   # log-log inverts CI order
        lls_upper = log(-log(lower)),
        lt        = log(time)
      )

    print(
      ggplot(km_df, aes(x = lt, colour = strata, fill = strata)) +
        geom_ribbon(aes(ymin = lls_lower, ymax = lls_upper),
                    alpha = 0.12, colour = NA) +
        geom_step(aes(y = lls)) +
        labs(title   = paste("Log-Log Plot —", label),
             x       = "log(Time)",
             y       = "log(-log(S(t)))",
             colour  = exposure_var,
             fill    = exposure_var) +
        theme_minimal(base_size = 11) +
        theme(panel.grid.minor = element_blank())
    )
  }

  # 3. Martingale residuals vs linear predictor
  mart <- residuals(fit, type = "martingale")
  lp   <- predict(fit, type = "lp")
  print(
    ggplot(tibble(lp = lp, mart = mart), aes(x = lp, y = mart)) +
      geom_point(alpha = 0.15, size = 0.6) +
      geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),
                  se = TRUE, colour = "steelblue3", linewidth = 0.8) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
      labs(title = paste("Martingale Residuals —", label),
           x     = "Linear Predictor",
           y     = "Martingale Residual") +
      theme_minimal(base_size = 11)
  )

  # 4. dfbeta plots — first min(4, p) terms (exposure term(s) first)
  dfb <- residuals(fit, type = "dfbeta")
  for (k in seq_len(min(4L, ncol(dfb)))) {
    print(
      ggplot(tibble(idx = seq_len(nrow(dfb)), d = dfb[, k]),
             aes(x = idx, y = d)) +
        geom_point(alpha = 0.2, size = 0.5) +
        geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
        labs(title = paste0("dfbeta — ", label, "  |  ", colnames(dfb)[k]),
             x     = "Subject Index",
             y     = "dfbeta") +
        theme_minimal(base_size = 11)
    )
  }

  invisible(list(ph = ph))
}


# function to print pooled HR
print_cox_hr_table <- function(pooled_tbl, title = "") {
  cat("\n", strrep("=", 70), "\n", title, "\n", strrep("=", 70), "\n", sep = "")
  out <- pooled_tbl %>%
    mutate(
      `HR (95% CI)` = sprintf("%.2f (%.2f–%.2f)", hr, hr_lower, hr_upper),
      p             = ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value))
    ) %>%
    select(Term = term, `HR (95% CI)`, p)
  print(as.data.frame(out), row.names = FALSE)
  invisible(pooled_tbl)
}


# Run each exposure set to fit models for event 1 and event 2 (competing risk)
# `data` may be a single data.frame or a list of data.frames.
.run_exposure <- function(f_e1, f_e2, data, label, exposure_var = NULL) {
  data_list <- .as_data_list(data)
  message("\nFitting: ", label)
  e1 <- .fit_cox_pooled(f_e1, data_list)
  e2 <- .fit_cox_pooled(f_e2, data_list)
  cat("\n", strrep("─", 70), "\nASSUMPTION TESTS — ", label,
      "\n", strrep("─", 70), "\n", sep = "")
  cox_assumptions(e1$fits[[1L]], data_list[[1L]],
                  exposure_var = exposure_var, label = label)
  list(event1 = e1, event2 = e2, label = label)
}


# 
# SHARED TERM VECTORS
# _ff  = functional forms (centered dummies, RCS age, pre-built interactions)
# _raw = raw (uncentered dummies, linear age, raw product interactions)
# 

# Age
.age_ff  <- c("DHH_AGE_c_rcs1", "DHH_AGE_c_rcs2", "DHH_AGE_c_rcs3")
.age_raw <- "DHH_AGE"

# All 8  (binary; ref = No = 2)
.ccc_ff  <- c("CCC_031_cat1_c", "CCC_061_cat1_c", "CCC_071_cat1_c",
              "CCC_091_cat1_c", "CCC_121_cat1_c", "CCC_151_cat1_c",
              "CCC_280_cat1_c", "CCC_290_cat1_c")
.ccc_raw <- c("CCC_031_cat1", "CCC_061_cat1", "CCC_071_cat1",
              "CCC_091_cat1", "CCC_121_cat1", "CCC_151_cat1",
              "CCC_280_cat1", "CCC_290_cat1")

# Mental health CCC subset only (for BZD adjustment set)
.mhccc_ff  <- c("CCC_031_cat1_c", "CCC_061_cat1_c",
                "CCC_280_cat1_c", "CCC_290_cat1_c")
.mhccc_raw <- c("CCC_031_cat1", "CCC_061_cat1",
                "CCC_280_cat1", "CCC_290_cat1")

# Marital status (3-cat; ref = 1 = Married → 3 non-ref dummies)
.ms_ff  <- c("DHH_MS_cat1_c", "DHH_MS_cat2_c", "DHH_MS_cat3_c")
.ms_raw <- c("DHH_MS_cat1",   "DHH_MS_cat2",   "DHH_MS_cat3")

# Material deprivation (3-cat; ref = 1 = least deprived → 2 dummies)
.mdep_ff  <- c("material_deprivation_cat1_c", "material_deprivation_cat2_c")
.mdep_raw <- c("material_deprivation_cat1",   "material_deprivation_cat2")

# Pain severity (5-cat; ref = 1 = No pain → 4 dummies)
.pain_ff  <- c("HUPDPAD_cat1_c", "HUPDPAD_cat2_c",
               "HUPDPAD_cat3_c", "HUPDPAD_cat4_c")
.pain_raw <- c("HUPDPAD_cat1",   "HUPDPAD_cat2",
               "HUPDPAD_cat3",   "HUPDPAD_cat4")

# Self-rated mental health GEN_02B (5-cat; ref = 1 = Excellent → 4 dummies)
.gen02b_ff  <- c("GEN_02B_cat1_c", "GEN_02B_cat2_c",
                 "GEN_02B_cat3_c", "GEN_02B_cat4_c")
.gen02b_raw <- c("GEN_02B_cat1",   "GEN_02B_cat2",
                 "GEN_02B_cat3",   "GEN_02B_cat4")

# Life stress GEN_07 (5-cat; ref = 1 = Not at all → 4 dummies)
.stress_ff  <- c("GEN_07_cat1_c", "GEN_07_cat2_c",
                 "GEN_07_cat3_c", "GEN_07_cat4_c")
.stress_raw <- c("GEN_07_cat1",   "GEN_07_cat2",
                 "GEN_07_cat3",   "GEN_07_cat4")

# Sex (binary; ref = 2 = Female)
.sex_ff  <- "DHH_SEX_cat1_c"
.sex_raw <- "DHH_SEX_cat1"

# Past-year illicit drug use drgdvyac (binary; ref = 2 = No)
.drug_ff  <- "drgdvyac_cat1_c"
.drug_raw <- "drgdvyac_cat1"

# Weekly alcohol consumption ALWDWKY (continuous)
.alc_ff  <- "ALWDWKY"
.alc_raw <- "ALWDWKY"

# Multiple chronic conditions (count; linear in both formulations)
.mcc <- "multiple_conditions"

# Survey cycle (3-cat; ref = 3 = 2017-18 → 2 non-ref dummies)
.cycle_ff  <- c("SurveyCycle_cat1_c", "SurveyCycle_cat2_c")
.cycle_raw <- c("SurveyCycle_cat1",   "SurveyCycle_cat2")

# ── Pre-built age interactions (FF) / raw products (raw) ──────────────────────
# Age × CCC subset with pipeline interaction columns
.int_age_ccc_ff  <- c("DHH_AGE_C_X_CCC_071_cat1_C", "DHH_AGE_C_X_CCC_091_cat1_C",
                       "DHH_AGE_C_X_CCC_121_cat1_C", "DHH_AGE_C_X_CCC_151_cat1_C",
                       "DHH_AGE_C_X_CCC_280_cat1_C")
.int_age_ccc_raw <- c("DHH_AGE:CCC_071_cat1", "DHH_AGE:CCC_091_cat1",
                       "DHH_AGE:CCC_121_cat1", "DHH_AGE:CCC_151_cat1",
                       "DHH_AGE:CCC_280_cat1")

# Age × pain
.int_age_pain_ff  <- c("DHH_AGE_C_X_HUPDPAD_cat1_C", "DHH_AGE_C_X_HUPDPAD_cat2_C",
                        "DHH_AGE_C_X_HUPDPAD_cat3_C", "DHH_AGE_C_X_HUPDPAD_cat4_C")
.int_age_pain_raw <- c("DHH_AGE:HUPDPAD_cat1", "DHH_AGE:HUPDPAD_cat2",
                        "DHH_AGE:HUPDPAD_cat3", "DHH_AGE:HUPDPAD_cat4")

# Age × alcohol (ALWDWKY)
.int_age_alc_ff  <- "DHH_AGE_C_X_ALWDWKY_C"
.int_age_alc_raw <- "DHH_AGE:ALWDWKY"

# Age × CCC_280 only (only MH-relevant pre-built interaction; for BZD adj set)
.int_age_mhccc_ff  <- "DHH_AGE_C_X_CCC_280_cat1_C"
.int_age_mhccc_raw <- "DHH_AGE:CCC_280_cat1"

#
.mf <- function(lhs, terms) {
  as.formula(paste(lhs, "~", paste(terms, collapse = " + ")))
}


#formula buidling code

.gen10_adj_ff  <- c(.age_ff,  .ms_ff,  .mdep_ff,
                    .mcc, .pain_ff, .gen02b_ff,
                    .cycle_ff,
                    .int_age_pain_ff)
.gen10_adj_raw <- c(.age_raw, .ms_raw, .mdep_raw,
                    .mcc, .pain_raw, .gen02b_raw,
                    .cycle_raw,
                    .int_age_pain_raw)

frm_gen10_ff_e1  <- .mf("Surv(survt, event == 1)",
                         c("factor(GEN_10)", .gen10_adj_ff))
frm_gen10_ff_e2  <- .mf("Surv(survt, event == 2)", .gen10_adj_ff)
frm_gen10_raw_e1 <- .mf("Surv(survt, event == 1)",
                         c("factor(GEN_10)", .gen10_adj_raw))
frm_gen10_raw_e2 <- .mf("Surv(survt, event == 2)", .gen10_adj_raw)



# FORMULAS — EXPOSURE 2: MENTAL HEALTH (CCC_280 mood disorder + CCC_290 anxiety)
#
# Adj set: Age, Alcohol (ALWDWKY), marital_status, material_deprivation,
#           multiple_chronic_conditions, self_reported_stress (GEN_07), sex

.mh_adj_ff  <- c(.age_ff,  .alc_ff,  .ms_ff,  .mdep_ff,
                  .mcc, .stress_ff, .sex_ff,
                  .cycle_ff,
                  .int_age_alc_ff)
.mh_adj_raw <- c(.age_raw, .alc_raw, .ms_raw, .mdep_raw,
                  .mcc, .stress_raw, .sex_raw,
                  .cycle_raw,
                  .int_age_alc_raw)

frm_mh_ff_e1  <- .mf("Surv(survt, event == 1)",
                      c("CCC_280_cat1_c", "CCC_290_cat1_c", .mh_adj_ff))
frm_mh_ff_e2  <- .mf("Surv(survt, event == 2)", .mh_adj_ff)
frm_mh_raw_e1 <- .mf("Surv(survt, event == 1)",
                      c("CCC_280_cat1", "CCC_290_cat1", .mh_adj_raw))
frm_mh_raw_e2 <- .mf("Surv(survt, event == 2)", .mh_adj_raw)



# FORMULAS — EXPOSURE 3: FIRST DRUG CLASS
#
# first_drug_class: 3-category (0 = No prescription [ref], 1 = Opioid, 2 = BZD)
# Adj set: Age, Alcohol, material_deprivation, multiple_conditions,
#          mental health CCCs, pain, self_report_drug_use, sex, cycle

.fdrug_adj_ff  <- c(.age_ff,  .alc_ff,  .mdep_ff, .mcc, .mhccc_ff,
                    .pain_ff, .drug_ff, .sex_ff,
                    .cycle_ff,
                    .int_age_pain_ff, .int_age_alc_ff)
.fdrug_adj_raw <- c(.age_raw, .alc_raw, .mdep_raw, .mcc, .mhccc_raw,
                    .pain_raw, .drug_raw, .sex_raw,
                    .cycle_raw,
                    .int_age_pain_raw, .int_age_alc_raw)

frm_fdrug_ff_e1  <- .mf("Surv(survt, event == 1)",
                          c("factor(first_drug_class)", .fdrug_adj_ff))
frm_fdrug_ff_e2  <- .mf("Surv(survt, event == 2)", .fdrug_adj_ff)
frm_fdrug_raw_e1 <- .mf("Surv(survt, event == 1)",
                          c("factor(first_drug_class)", .fdrug_adj_raw))
frm_fdrug_raw_e2 <- .mf("Surv(survt, event == 2)", .fdrug_adj_raw)


#Fit Models
cox_results <- list(

  # raw and transformed var models
  gen10_ff  = .run_exposure(frm_gen10_ff_e1,  frm_gen10_ff_e2,
                             completed_list,
                             "GEN_10 — Functional Forms",
                             exposure_var = "GEN_10"),

  gen10_raw = .run_exposure(frm_gen10_raw_e1, frm_gen10_raw_e2,
                             completed_list,
                             "GEN_10 — Raw",
                             exposure_var = "GEN_10"),

  # Exposure #2 
  mh_ff     = .run_exposure(frm_mh_ff_e1,  frm_mh_ff_e2,
                             completed_list,
                             "Mental Health (CCC_280/290) — Functional Forms",
                             exposure_var = "CCC_280"),

  mh_raw    = .run_exposure(frm_mh_raw_e1, frm_mh_raw_e2,
                             completed_list,
                             "Mental Health (CCC_280/290) — Raw",
                             exposure_var = "CCC_280"),

  # ── Exposure 3: First drug class (0=No Rx, 1=Opioid, 2=BZD)
  fdrug_ff  = .run_exposure(frm_fdrug_ff_e1,  frm_fdrug_ff_e2,
                             completed_list,
                             "First Drug Class — Functional Forms",
                             exposure_var = "first_drug_class"),

  fdrug_raw = .run_exposure(frm_fdrug_raw_e1, frm_fdrug_raw_e2,
                             completed_list,
                             "First Drug Class — Raw",
                             exposure_var = "first_drug_class")
)


#HR TABLE implementation

for (.nm in names(cox_results)) {
  print_cox_hr_table(cox_results[[.nm]]$event1$pooled, title = .nm)
}
rm(.nm)



#
# Demonstrates the full associative modelling workflow for one exposure
# (GEN_10) using the building-block functions directly, rather than the
# batch cox_results list above.  Useful for:
#   • Interactive inspection / debugging a specific exposure
#   • Adapting the pipeline to a new exposure not in cox_results
#   • Understanding what .run_exposure() does under the hood
#
# Requires: completed_list from run-imputation.R

# Step 1 — fit cause-specific Cox models for event=1 and event=2 separately.
# .fit_cox_pooled() accepts a single data.frame or a list of imputed datasets;
# pooling via Rubin's rules is applied automatically.
gen10_e1 <- .fit_cox_pooled(frm_gen10_ff_e1, completed_list)
gen10_e2 <- .fit_cox_pooled(frm_gen10_ff_e2, completed_list)

# Step 2 — print the pooled HR table for the event-of-interest model.
print_cox_hr_table(gen10_e1$pooled, title = "GEN_10 — Sense of Belonging (event = 1)")

# Step 3 — run assumption diagnostics on the first imputed dataset.
# Produces: Schoenfeld PH test, log-log plot, martingale residuals, dfbeta plots.
cox_assumptions(
  fit          = gen10_e1$fits[[1L]],
  data         = completed_list[[1L]],
  exposure_var = "GEN_10",
  label        = "GEN_10 ff"
)

# Step 4 — extract raw coefficients and variance-covariance matrices if needed
# for downstream analyses (e.g., custom contrasts, G-computation seeding).
gen10_coefs <- lapply(gen10_e1$fits, coef)
gen10_vcovs <- lapply(gen10_e1$fits, vcov)

# Step 5 — inspect per-imputation variation in the main exposure HRs.
# Rows are imputed datasets; each column is a GEN_10 contrast vs. ref.
do.call(rbind, lapply(gen10_e1$fits, function(f) {
  b <- coef(f)
  exp(b[grep("GEN_10", names(b))])
}))
