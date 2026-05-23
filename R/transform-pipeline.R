source("R/functional-forms.R")

# run_transform_pipeline() applies the full four-step transformation cascade
# to a single data.frame and returns the transformed data together with the
# transformation_info object that records every learnable constant used.
#
# Two calling modes:
#
#   Calibration (transformation_info = NULL, default):
#     Constants — dummy reference levels, centering means, RCS knot locations —
#     are derived from `data` and stored in transformation_info.  Pass the
#     returned transformation_info to every subsequent apply call so that all
#     completed datasets are transformed identically.
#
#   Apply (transformation_info = <prior result>):
#     Every constant is read from transformation_info; nothing is re-estimated
#     from data.  This is the correct mode for post-imputation completed datasets.

run_transform_pipeline <- function(data, transformation_info = NULL) {

  if (is.null(transformation_info)) transformation_info <- list()

  vars_to_dummy_main <- c(
    # Binary chronic conditions (ref = 2 = No; output: <var>_cat1 = Yes)
    "CCC_031", "CCC_061", "CCC_071", "CCC_091",
    "CCC_121", "CCC_151", "CCC_280", "CCC_290",
    # Binary: sex (ref = 2 = Female; output: DHH_SEX_cat1 = Male)
    "DHH_SEX",
    # Binary: illicit drug use (ref = 2 = No)
    "drgdvyac", "drgdvlac",
    # Binary: race (ref = 1 = White; output: race_binary_cat1 = Non-white)
    "race_binary",
    # 5-category: smoking status collapsed (ref = 5 = Never smoked)
    "SMKDSTY_cat5",
    # 3-category: marital status (ref = 1 = Married)
    "DHH_MS",
    # 3-category: education (ref = 3 = Post-secondary; output: cat1=<HS, cat2=HS)
    "EDUDR03",
    # 5-category: neighbourhood deprivation (ref = 1 = least deprived)
    "material_deprivation",
    # 3-category: food security (ref = 0 = Food secure)
    "FSCDHFS2",
    # 5-category: self-perceived health (ref = 1 = Excellent)
    "GEN_01",
    # 5-category: self-perceived mental health (ref = 1 = Excellent)
    "GEN_02B",
    # 5-category: life stress (ref = 1 = Not at all stressful)
    "GEN_07",
    # 4-category: sense of belonging (ref = 1 = Very strong)
    "GEN_10",
    # 5-category: pain severity (ref = 1 = No pain)
    "HUPDPAD",
    # Binary: rural status (ref = 2 = Urban)
    "rural",
    # 3-category: drinker type (ref = 1 = Regular drinker)
    "ALCDTTM",
    # 3-category: survey cycle (ref = 3 = 2017-18)
    "SurveyCycle",

    "PACDEE"
  )

  # Pre-load substantively chosen reference levels; skipped on apply runs
  # because dummy_refs is already populated from the calibration run.
  if (is.null(transformation_info$dummy_refs)) {
    transformation_info$dummy_refs <- list(
      race_binary          = "1",   # White
      DHH_MS               = "1",   # Married
      EDUDR03              = "3",   # Post-secondary
      material_deprivation = "1",   # Least deprived
      FSCDHFS2             = "0",   # Food secure
      GEN_01               = "1",   # Excellent health
      GEN_02B              = "1",   # Excellent mental health
      GEN_07               = "1",   # Not at all stressful
      GEN_10               = "1",   # Very strong belonging
      HUPDPAD              = "1",   # No pain
      rural                = "2",   # Urban
      SMKDSTY_cat5         = "5",   # Never smoked
      ALCDTTM              = "3",   # No drinks within the last 12 months
      SurveyCycle          = "3"    # 2017-18
    )
  }

  r    <- variable_dummy(data, vars_to_dummy_main, transformation_info = transformation_info)
  data <- r$data
  transformation_info <- r$transformation_info


  #centering

  vars_to_center_continuous <- c(
    "DHH_AGE",   # → DHH_AGE_c; enters RCS in Step 3
    "bmi_adj",   # → bmi_adj_c; age × BMI interaction
    "ALWDWKY"    # → ALWDWKY_c; age × drinks-last-week interaction and RCS
  )

  vars_to_center_step1 <- c(
    # Binary chronic conditions: _cat1 = Yes
    "CCC_031_cat1", "CCC_061_cat1", "CCC_071_cat1", "CCC_091_cat1",
    "CCC_121_cat1", "CCC_151_cat1", "CCC_280_cat1", "CCC_290_cat1",
    # Binary sociodemographic
    "DHH_SEX_cat1", "race_binary_cat1", "rural_cat1",
    # Binary substance use
    "drgdvyac_cat1", "drgdvlac_cat1",
    # 5-category smoking collapsed (ref=5=Never; cat1=Daily, cat2=Occasional,
    #   cat3=FormerDaily, cat4=FormerOcc)
    "SMKDSTY_cat5_cat1", "SMKDSTY_cat5_cat2", "SMKDSTY_cat5_cat3", "SMKDSTY_cat5_cat4",
    # 3-category drinker type (ref=1=Regular; cat1=Occasional, cat2=Non-drinker)
    "ALCDTTM_cat1", "ALCDTTM_cat2",
    # 5-category pain (ref=1=No pain; cat1-4 = increasing severity)
    "HUPDPAD_cat1", "HUPDPAD_cat2", "HUPDPAD_cat3", "HUPDPAD_cat4",
    # 3-category marital status (ref=1=Married; cat1=Common-law,
    #   cat2=Wid/Sep/Div, cat3=Single)
    "DHH_MS_cat1", "DHH_MS_cat2", "DHH_MS_cat3",
    # 3-category education (ref=3=Post-secondary; cat1=<Secondary,
    #   cat2=Secondary)
    "EDUDR03_cat1", "EDUDR03_cat2",
    # 5-category material deprivation (ref=1=least deprived; cat1-4 =
    #   increasing deprivation)
    "material_deprivation_cat1", "material_deprivation_cat2",
    "material_deprivation_cat3", "material_deprivation_cat4",
    # 3-category food security (ref=0=Food secure; cat1=Moderate insecure,
    #   cat2=Severe insecure)
    "FSCDHFS2_cat1",
    # 5-category self-perceived health (ref=1=Excellent; cat1-4 = Very good
    #   through Poor)
    "GEN_01_cat1", "GEN_01_cat2", "GEN_01_cat3", "GEN_01_cat4",
    # 5-category self-perceived mental health
    "GEN_02B_cat1", "GEN_02B_cat2", "GEN_02B_cat3", "GEN_02B_cat4",
    # 5-category life stress (ref=1=Not at all stressful)
    "GEN_07_cat1", "GEN_07_cat2", "GEN_07_cat3", "GEN_07_cat4",
    # 4-category sense of belonging (ref=1=Very strong; cat1-3 = Somewhat
    #   strong through Very weak)
    "GEN_10_cat1", "GEN_10_cat2", "GEN_10_cat3",
    # 3-category survey cycle (ref=3=2017-18; cat1=2013-14, cat2=2015-16)
    "SurveyCycle_cat1", "SurveyCycle_cat2"
  )

  vars_to_center_main <- c(vars_to_center_continuous, vars_to_center_step1)

  r    <- center_variables(data, vars_to_center_main, transformation_info = transformation_info)
  data <- r$data
  transformation_info <- r$transformation_info


  # ── Step 3: Restricted cubic splines ───────────────────────────────────────
  # Knot locations are estimated once on the calibration data and stored in
  # transformation_info$knot_locations.  The if (is.null(...)) guards ensure
  # quantiles are only computed on the first call; apply runs skip straight to
  # create_rcs() which reads the stored locations.

  # Age: 5-knot RCS on centered age → DHH_AGE_c_rcs1 (linear) through _rcs4
  if (is.null(transformation_info$knot_locations[["DHH_AGE_c"]])) {
    age_knot_probs <- c(0.05, 0.35, 0.50, 0.65, 0.95)
    transformation_info$knot_locations[["DHH_AGE_c"]] <-
      quantile(data$DHH_AGE_c, probs = age_knot_probs, na.rm = TRUE)
  }

  r    <- create_rcs(
    data,
    vars                = "DHH_AGE_c",
    transformation_info = transformation_info,
    rcs_cols            = list(DHH_AGE_c = 1:4)
  )
  data <- r$data
  transformation_info <- r$transformation_info
  # Outputs: DHH_AGE_c_rcs1 (linear), DHH_AGE_c_rcs2, _rcs3, _rcs4

  # Alcohol: 3-knot RCS on centered drinks/week → ALWDWKY_c_rcs1, _rcs2
  if (is.null(transformation_info$knot_locations[["ALWDWKY_c"]])) {
    transformation_info$knot_locations[["ALWDWKY_c"]] <-
      quantile(data$ALWDWKY_c, probs = c(0.10, 0.50, 0.90), na.rm = TRUE)
  }

  r    <- create_rcs(
    data,
    vars                = "ALWDWKY_c",
    transformation_info = transformation_info,
    rcs_cols            = list(ALWDWKY_c = 1:2)
  )
  data <- r$data
  transformation_info <- r$transformation_info
  # Outputs: ALWDWKY_c_rcs1 (linear), ALWDWKY_c_rcs2 (nonlinear)


  #interactions
  interactions_main <- list(
    DHH_AGE_C_X_ALCDTTM_cat1_C       = c("DHH_AGE_c", "ALCDTTM_cat1_c"),
    DHH_AGE_C_X_ALCDTTM_cat2_C       = c("DHH_AGE_c", "ALCDTTM_cat2_c"),
    DHH_AGE_C_X_ALWDWKY_C            = c("DHH_AGE_c", "ALWDWKY_c"),
    DHH_AGE_C_X_CCC_071_cat1_C       = c("DHH_AGE_c", "CCC_071_cat1_c"),
    DHH_AGE_C_X_CCC_091_cat1_C       = c("DHH_AGE_c", "CCC_091_cat1_c"),
    DHH_AGE_C_X_CCC_121_cat1_C       = c("DHH_AGE_c", "CCC_121_cat1_c"),
    DHH_AGE_C_X_CCC_151_cat1_C       = c("DHH_AGE_c", "CCC_151_cat1_c"),
    DHH_AGE_C_X_CCC_280_cat1_C       = c("DHH_AGE_c", "CCC_280_cat1_c"),
    DHH_AGE_C_X_HUPDPAD_cat1_C       = c("DHH_AGE_c", "HUPDPAD_cat1_c"),
    DHH_AGE_C_X_HUPDPAD_cat2_C       = c("DHH_AGE_c", "HUPDPAD_cat2_c"),
    DHH_AGE_C_X_HUPDPAD_cat3_C       = c("DHH_AGE_c", "HUPDPAD_cat3_c"),
    DHH_AGE_C_X_HUPDPAD_cat4_C       = c("DHH_AGE_c", "HUPDPAD_cat4_c"),
    DHH_AGE_C_X_bmi_adj_C            = c("DHH_AGE_c", "bmi_adj_c")
  )

  r    <- create_interactions(data, interactions_main, transformation_info = transformation_info)
  data <- r$data
  transformation_info <- r$transformation_info


  interactions_imputation <- list(
    age_X_BMI              = c("DHH_AGE", "bmi_adj"),
    age_X_arthritis        = c("DHH_AGE", "CCC_051"),
    age_X_bowel_disorder   = c("DHH_AGE", "CCC_171"),
    age_X_cancer           = c("DHH_AGE", "CCC_131"),
    age_X_diabetes         = c("DHH_AGE", "CCC_101"),
    age_X_drinker_type     = c("DHH_AGE", "ALCDTTM"),
    age_X_drinks_last_week = c("DHH_AGE", "ALWDWKY"),
    age_X_hbp              = c("DHH_AGE", "CCC_071"),
    age_X_heart_disease    = c("DHH_AGE", "CCC_121"),
    age_X_mood_disorder    = c("DHH_AGE", "CCC_280"),
    age_X_COPD             = c("DHH_AGE", "CCC_091"),
    age_X_smoking_status   = c("DHH_AGE", "SMKDSTY_cat5"),
    age_X_stroke           = c("DHH_AGE", "CCC_151"),
    age_X_anxiety          = c("DHH_AGE", "CCC_290"),
    age_X_number_conditions= c("DHH_AGE", "multiple_conditions")
  )

  r    <- create_interactions(data, interactions_imputation, transformation_info = transformation_info)
  data <- r$data
  transformation_info <- r$transformation_info


  list(
    data                    = data,
    transformation_info     = transformation_info,
    interactions_main       = interactions_main,
    interactions_imputation = interactions_imputation
  )
}
