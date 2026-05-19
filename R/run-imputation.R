library(here)
library(dplyr)

source(here("R", "impute-data-manual.R"))
source(here("R", "transform-pipeline.R"))

# cchs_all_h expected in environment (created externally via recode pipeline)

# ─── Phase 0: Prepare raw data ────────────────────────────────────────────────
# id_year is the row key required by merge_and_overwrite() inside every
# transform function.  It is attached here once and carried through all phases.
data <- cchs_all_h
data$id_year <- paste0(seq_len(nrow(data)), "_", data$SurveyCycle)


# ─── Phase 1: Calibration ─────────────────────────────────────────────────────
# Run the full transform pipeline on the observed (pre-imputation) data to
# estimate all learnable constants:
#   • dummy reference levels  → transformation_info$dummy_refs
#   • centering means         → transformation_info$center_values
#   • RCS knot locations      → transformation_info$knot_locations
#
# The transformed data produced here is DISCARDED.  Only transformation_info
# and the two interaction-name lists are kept.  All completed datasets produced
# after imputation will be transformed with these same constants so that
# coefficients are comparable across imputations and with the calibration run.
calib               <- run_transform_pipeline(data)
transformation_info <- calib$transformation_info
interactions_main   <- calib$interactions_main
interactions_imputation <- calib$interactions_imputation
rm(calib)


# ─── Phase 2: Nelson-Aalen cumulative hazard ──────────────────────────────────
# event = 1 (NMS death), 2 (other death), 0 (censored).
# Cause-specific estimator: codes 2 and 0 are treated as censored per
# White & Royston (2009).  Added to raw data so it is available as a predictor
# in the imputation model.
data <- add_nelson_aalen_h(
  data,
  time_var          = "survt",
  event_var         = "event",
  event_of_interest = 1L
)


# ─── Phase 3: Passive MICE formulas for interaction variables ─────────────────
# MICE re-derives each interaction column from the actively imputed base
# variables at every iteration, keeping interactions consistent with their
# imputed parts.  The inline centering expressions reproduce the exact constants
# stored in transformation_info$center_values so that centered interactions
# inside MICE match those produced by run_transform_pipeline() after imputation.

# Helper: build the inline R expression that re-centres a variable using the
# constant recorded during calibration.
#
# For dummy components (<source>_cat<i>_c):
#   as.integer(as.numeric(<source>) == <code>) - <mean>
#
# For continuous components (<source>_c):
#   (as.numeric(<source>) - <mean>)
.centered_expr <- function(comp_var, data, transformation_info) {
  base <- sub("_c$", "", comp_var)
  m    <- regmatches(base, regexec("^(.+)_cat(\\d+)$", base, perl = TRUE))[[1]]

  if (length(m) == 3L) {
    source_var <- m[2]
    cat_idx    <- as.integer(m[3])
    ref        <- transformation_info$dummy_refs[[source_var]]
    col_vals   <- as.character(haven::zap_labels(data[[source_var]]))
    cats       <- sort(unique(na.omit(col_vals)))
    code       <- as.integer(cats[cats != ref][cat_idx])
    cv         <- transformation_info$center_values[[base]]
    sprintf("(as.integer(as.numeric(%s) == %dL) - %s)", source_var, code, deparse(cv))
  } else {
    cv <- transformation_info$center_values[[base]]
    sprintf("(as.numeric(%s) - %s)", base, deparse(cv))
  }
}

# Main analysis interactions: products of two centered variables.
interactions_main_passive <- setNames(
  lapply(names(interactions_main), function(nm) {
    vars <- interactions_main[[nm]]
    e1   <- .centered_expr(vars[1], data, transformation_info)
    e2   <- .centered_expr(vars[2], data, transformation_info)
    sprintf("~ I(%s * %s)", e1, e2)
  }),
  names(interactions_main)
)

# Imputation-model interactions: raw age × raw comorbidity/risk factor.
interactions_imputation_passive <- setNames(
  lapply(names(interactions_imputation), function(nm) {
    vars <- interactions_imputation[[nm]]
    sprintf("~ I(as.numeric(%s) * as.numeric(%s))", vars[1], vars[2])
  }),
  names(interactions_imputation)
)


# ─── Phase 4: Variables to impute ────────────────────────────────────────────
# Single unified list — no distinction between covariates missing at random and
# predictor variables that happen to have missing data.
impute_vars <- c(
  # Continuous
  "ALWDWKY",
  "bmi_adj",
  # General health measures
  "GEN_01",
  "GEN_02B",
  "GEN_07",
  "GEN_10",
  # Smoking (5-category collapsed); also base of age_X_smoking_status
  "SMKDSTY_cat5",
  "pack_years",
  # Substance use and alcohol
  "drgdvyac",
  "drgdvlac",
  "ALCDTTM",
  # Sociodemographic
  "DHH_MS",
  "EDUDR03",
  "DHH_OWN",
  "race_binary",
  # Pain, deprivation, food security
  "HUPDPAD",
  "material_deprivation",
  "FSCDHFS2",
  # Mental health and income (absent in some cycles)
  "CMH_01K",
  "CMH_01L",
  "INCDRRS",
  # Chronic conditions — base variables for main-analysis dummies and
  # imputation interaction terms
  "CCC_031", "CCC_061", "CCC_071", "CCC_091",
  "CCC_121", "CCC_151", "CCC_280", "CCC_290",
  "CCC_051", "CCC_101", "CCC_171",
  "CCC_131",            # cancer; base of age_X_cancer
  # Count of chronic conditions; base of age_X_number_conditions
  "multiple_conditions"
)
impute_vars <- intersect(impute_vars, colnames(data))

# Complete auxiliary predictors (White & Royston 2009).
additional_predictors <- c(
  "DHH_AGE",
  "DHH_SEX",
  "rural",
  "SurveyCycle",
  "material_deprivation",
  "nelson_aalen_h",
  "event",
  "survt"
)
additional_predictors <- intersect(additional_predictors, colnames(data))


# ─── Phase 4: Run MICE ────────────────────────────────────────────────────────
# MICE operates on raw base variables only — no pre-computed _cat, _c, or _rcs
# columns are passed in.  The passive formula lists ensure that interaction
# terms stay consistent with imputed bases at every iteration (congeniality).
#
# Dev: m=1, maxit=1.  Production: m=5, maxit=5 (~30-75 min for ~80k rows).
result <- impute_data_manual(
  data                             = data,
  impute_vars                      = impute_vars,
  additional_imputation_predictors = additional_predictors,
  interaction_vars                 = c(interactions_main_passive,
                                       interactions_imputation_passive),
  m     = 1,
  maxit = 1
)


# ─── Phase 5: Post-imputation transformation ───────────────────────────────────
# For each of the m completed datasets:
#   1. Extract the MICE-completed base variables.
#   2. Merge them back into the full original data structure so that variables
#      not imputed (e.g. PACDEE, id_year, nelson_aalen_h) are retained.
#   3. Apply run_transform_pipeline() with the calibration transformation_info
#      so every completed dataset gets fresh _cat, _c, _rcs, and interaction
#      columns derived from its own imputed values, using constants that are
#      identical across all m datasets.
#
# The result is a list of m analysis-ready data.frames.  Use these with
# a pooling workflow:
#   fits   <- lapply(completed_list, function(d) coxph(Surv(...) ~ ..., data = d))
#   pooled <- mice::pool(fits)   # Rubin's rules

completed_list <- lapply(
  seq_len(result$mice_result$m),
  function(i) {
    # MICE-completed columns for imputation i (base variables only)
    completed_i <- mice::complete(result$mice_result, action = i)

    # Start from the original data (all raw columns present) and overwrite
    # each imputed variable with its completed value.
    full_i <- data
    for (v in intersect(colnames(completed_i), colnames(full_i))) {
      full_i[[v]] <- completed_i[[v]]
    }

    # Apply the full transformation cascade using the calibration constants.
    run_transform_pipeline(full_i, transformation_info = transformation_info)$data
  }
)