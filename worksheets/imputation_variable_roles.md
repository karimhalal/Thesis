# Imputation Variable Roles

Reference for `run-imputation.R` and `transform-pipeline.R`.
Last updated: 2026-05-18.

---

## Imputation targets — `impute_vars` (unified list)

All variables with potential missingness. No functional distinction between covariates missing at random and predictor variables with missing data — both are passed as `impute_vars`.

| Variable | Content | Notes |
|---|---|---|
| `ALWDWKY` | Drinks last week | Continuous; base of `age_X_drinks_last_week` |
| `bmi_adj` | BMI | Continuous; base of `age_X_BMI` |
| `GEN_01` | Self-perceived health | 5-cat |
| `GEN_02B` | Self-perceived mental health | 5-cat |
| `GEN_07` | Life stress | 5-cat |
| `GEN_10` | Sense of belonging | 4-cat |
| `SMKDSTY_cat5` | Smoking status (5-cat collapsed) | Base of `age_X_smoking_status` |
| `drgdvyac` | Illicit drug use — past year | |
| `drgdvlac` | Illicit drug use — lifetime | |
| `DHH_MS` | Marital status | 4-cat |
| `EDUDR03` | Education (3-cat) | |
| `DHH_OWN` | Dwelling ownership | |
| `race_binary` | Race (binary) | Sheet name: `SDCGCGT` |
| `HUPDPAD` | Pain severity | 5-cat |
| `material_deprivation` | Neighbourhood deprivation | Sheet name: `deprivation_der` |
| `FSCDHFS2` | Food security | 3-cat |
| `CMH_01K` | Consulted MH professional (Y/N) | Absent some cycles |
| `CMH_01L` | MH consultations (count) | Absent some cycles |
| `INCDRRS` | Income ratio | Absent some cycles |
| `INCDRPR` | Income percentile | Absent some cycles |
| `INCDRCA` | Income category | Absent some cycles |
| `CCC_031` | Mood disorder | Base var for main-analysis dummies |
| `CCC_061` | Anxiety disorder | |
| `CCC_071` | Hypertension | Base of `age_X_hbp` |
| `CCC_091` | COPD/emphysema | Base of `age_X_COPD` |
| `CCC_121` | Heart disease | Base of `age_X_heart_disease` |
| `CCC_151` | Stroke | Base of `age_X_stroke` |
| `CCC_280` | Mood disorder (alt) | Base of `age_X_mood_disorder` |
| `CCC_290` | Anxiety (alt) | Base of `age_X_anxiety` |
| `CCC_051` | Arthritis | Base of `age_X_arthritis` |
| `CCC_101` | Diabetes | Base of `age_X_diabetes` |
| `CCC_171` | Bowel disorder | Base of `age_X_bowel_disorder` |
| `CCC_131` | Cancer | Base of `age_X_cancer` |
| `multiple_conditions` | Count of chronic conditions | Base of `age_X_number_conditions` |

---

## Complete auxiliary predictors — `additional_predictors`

Always-complete variables used as predictors in all MICE imputation models. Never imputed.

| Variable | Content | Notes |
|---|---|---|
| `DHH_AGE` | Age (raw) | |
| `DHH_SEX` | Sex | |
| `rural` | Rural/urban | |
| `SurveyCycle` | Survey wave | |
| `DHH_AGE_c_rcs1` | Age RCS spline — linear term | 5-knot spline on centered age |
| `DHH_AGE_c_rcs2` | Age RCS spline — nonlinear 1 | |
| `DHH_AGE_c_rcs3` | Age RCS spline — nonlinear 2 | |
| `DHH_AGE_c_rcs4` | Age RCS spline — nonlinear 3 | |
| `nelson_aalen_h` | Nelson-Aalen cumulative hazard | Cause-specific (event = 1); White & Royston (2009) |
| `event` | Event indicator | 0 = censored, 1 = NMS death, 2 = other death |
| `survt` | Survival/censoring time | |
| `names(interactions_main)` | Main-analysis interaction columns | 9 terms; dynamic via `transform-pipeline.R` |
| `names(interactions_imputation)` | Imputation-only interaction columns | 15 terms; dynamic via `transform-pipeline.R` |

---

## Main-analysis interactions — `interactions_main`

Predictor role. Centered base components. Included in `additional_predictors` dynamically via `names(interactions_main)`.

| Column | Formula |
|---|---|
| `DHH_AGE_C_X_ALCDTTM_cat1_C` | `DHH_AGE_c × ALCDTTM_cat1_c` |
| `DHH_AGE_C_X_ALCDTTM_cat2_C` | `DHH_AGE_c × ALCDTTM_cat2_c` |
| `DHH_AGE_C_X_ALWDWKY_C` | `DHH_AGE_c × ALWDWKY_c` |
| `DHH_AGE_C_X_CCC_071_cat1_C` | `DHH_AGE_c × CCC_071_cat1_c` |
| `DHH_AGE_C_X_CCC_091_cat1_C` | `DHH_AGE_c × CCC_091_cat1_c` |
| `DHH_AGE_C_X_CCC_121_cat1_C` | `DHH_AGE_c × CCC_121_cat1_c` |
| `DHH_AGE_C_X_CCC_151_cat1_C` | `DHH_AGE_c × CCC_151_cat1_c` |
| `DHH_AGE_C_X_CCC_280_cat1_C` | `DHH_AGE_c × CCC_280_cat1_c` |
| `DHH_AGE_C_X_bmi_adj_C` | `DHH_AGE_c × bmi_adj_c` |

---

## Imputation-only interactions — `interactions_imputation`

Imputation-variable role. Raw (uncentered) base components. Included in `additional_predictors` dynamically via `names(interactions_imputation)`.

| Column | Formula | Notes |
|---|---|---|
| `age_X_BMI` | `DHH_AGE × bmi_adj` | |
| `age_X_arthritis` | `DHH_AGE × CCC_051` | |
| `age_X_bowel_disorder` | `DHH_AGE × CCC_171` | |
| `age_X_cancer` | `DHH_AGE × CCC_131` | Added |
| `age_X_diabetes` | `DHH_AGE × CCC_101` | |
| `age_X_drinker_type` | `DHH_AGE × ALCDTTM` | |
| `age_X_drinks_last_week` | `DHH_AGE × ALWDWKY` | |
| `age_X_hbp` | `DHH_AGE × CCC_071` | |
| `age_X_heart_disease` | `DHH_AGE × CCC_121` | |
| `age_X_mood_disorder` | `DHH_AGE × CCC_280` | |
| `age_X_COPD` | `DHH_AGE × CCC_091` | |
| `age_X_smoking_status` | `DHH_AGE × SMKDSTY_cat5` | Re-enabled; base fixed from `smoke_simple` |
| `age_X_stroke` | `DHH_AGE × CCC_151` | |
| `age_X_anxiety` | `DHH_AGE × CCC_290` | |
| `age_X_number_conditions` | `DHH_AGE × multiple_conditions` | |

---

## Excluded legacy variables

Variables present in `variables_sheet.csv` with `imputation-variable` role that are no longer part of the analysis.

| Variable | Reason |
|---|---|
| `DHHDHSZ` | Legacy; excluded from analysis |
| `GEN_09` | Legacy; excluded from analysis |
| `INCDHH` | Legacy; excluded from analysis |
| `HWTDBMI_der` | Legacy name for `bmi_adj` |
| `number_conditions` | Legacy name for `multiple_conditions` |
| `smoke_simple` | Legacy name for `SMKDSTY_cat5` |
| `LBFCDWSS` | Labour/physical activity variable; dropped |
| `ADL_der` | ADL function variable; dropped |
| `ADL_score_5` | ADL function variable; dropped |