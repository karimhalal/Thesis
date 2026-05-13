---
name: impute-data
description: Implement, modify, or debug the MICE-based multiple imputation pipeline for CCHS data in impute-data-manual.R. Use when the user asks about imputation, missing data, MICE, predictor matrices, or changes to impute_data_manual().
---

# MICE Imputation Without Variables Sheet — Skill Prompt

## Objective

Implement a MICE-based multiple imputation pipeline for harmonized CCHS data
that operates **without a `variables_sheet`**, using manually specified variable
vectors and per-variable predictor lists in place of metadata lookups.

See [impute-data-manual.R](../../R/impute-data-manual.R) for the implementation.
See [special-imputations.md](special-imputations.md) for documentation of
conditional (gate-question) relationships.

---

## Main Function Signature

```r
impute_data_manual(
  data,               # harmonized data.frame with tagged NAs ("NA(a)"/"NA(b)"/"NA(c)")
  impute_vars_cont,   # character vector — continuous variables  (method: pmm)
  impute_vars_binary, # character vector — binary variables      (method: logreg)
  impute_vars_multi,  # character vector — multinomial variables (method: polyreg)
  predictor_list,     # named list: variable -> character vector of predictors
  m     = 1,          # number of multiple imputations
  maxit = 1           # number of MICE iterations
)
```

### Return Value

Matches the output of the original `impute_data()`:

| Element        | Type        | Description |
|----------------|-------------|-------------|
| `data`         | data.frame  | Imputed data with CCHS conditional relationships applied |
| `imp_data`     | data.frame  | Imputed data before conditional relationship fixes |
| `mice_result`  | mids object | Raw output of `mice::mice()` — supports `pool()`, `complete()`, etc. |

---

## Usage Example

```r
source("R/impute-data-manual.R")

impute_vars_cont <- c(
  "DHH_AGE", "HWTDHTM", "HWTDWTK", "PACDEE",
  "CMH_01L", "ALWDWKY", "SMK_204"
)

impute_vars_binary <- c(
  "CMH_01K", "ALW_1", "CCC_031", "CCC_051",
  "CCC_071", "CCC_101", "CCC_121", "rural"
)

impute_vars_multi <- c(
  "GEN_01", "GEN_02B", "SMKDSTY", "ALCDTTM",
  "EDUDR04", "SDCDCGT", "material_deprivation"
)

predictor_list <- list(
  DHH_AGE  = c("DHH_SEX", "GEN_01", "EDUDR04", "INCDRCA"),
  CMH_01K  = c("DHH_AGE", "DHH_SEX", "GEN_02B", "CCC_280", "CCC_290"),
  CMH_01L  = c("DHH_AGE", "DHH_SEX", "CMH_01K", "GEN_02B"),
  ALWDWKY  = c("DHH_AGE", "DHH_SEX", "ALCDTTM", "ALW_1"),
  # ... one entry per imputed variable
)

result <- impute_data_manual(
  data               = harmonized_df,
  impute_vars_cont   = impute_vars_cont,
  impute_vars_binary = impute_vars_binary,
  impute_vars_multi  = impute_vars_multi,
  predictor_list     = predictor_list,
  m                  = 5,
  maxit              = 10
)

# Access outputs
final_data    <- result$data
pre_fix_data  <- result$imp_data
mice_obj      <- result$mice_result
```

---

## Internal Architecture

```
impute_data_manual()
    │
    ├── .validate_imputation_inputs()
    │       Stops if any imputation variable is absent from data or
    │       has no entry in predictor_list
    │
    ├── .build_method_vector()
    │       Named character vector, one entry per data column:
    │         continuous  → "pmm"
    │         binary      → "logreg"
    │         multinomial → "polyreg"
    │         all others  → ""  (predictor-only; MICE will not impute)
    │
    ├── .build_predictor_matrix()
    │       n_vars × n_vars integer matrix (dimnames = colnames(data))
    │       Entry [i,j] = 1 iff variable j is listed as a predictor for
    │       variable i in predictor_list. Diagonal forced to 0.
    │
    └── .run_mice_manual()
            │
            ├── .apply_cchs_relationships()   ← PRE-imputation
            │       Zeros out conditional downstream variables for rows
            │       with observed "No" upstream gate responses, so MICE
            │       does not treat those cells as genuinely missing.
            │
            ├── .prepare_data_for_imputation_manual()
            │       • Keeps only imputation targets + their predictors
            │       • Converts "NA(a)"/"NA(b)"/"NA(c)" → real NA
            │       • droplevels() on factor columns
            │       • Dev-mode padding: ≥10 NAs for single-NA categoricals
            │
            ├── mice::mice()
            │       Called with the subsetted method vector and predictor matrix
            │
            ├── mice::complete()
            │       Extracts the first (or only) completed dataset
            │
            └── .apply_cchs_relationships()   ← POST-imputation
                    Corrects rows where the gate variable was imputed as "No",
                    ensuring downstream variables remain consistent.
```

---

## Design Decisions

### Tagged NA handling
Input data is expected to have factor columns with tagged-NA levels
(`"NA(a)"`, `"NA(b)"`, `"NA(c)"`) from the recodeflow harmonization pipeline.
These are converted to real `NA` internally — **no pre-cleaning required**.

### MICE methods by variable type

| Vector               | MICE Method  | Requirement |
|----------------------|--------------|-------------|
| `impute_vars_cont`   | `pmm`        | Numeric column |
| `impute_vars_binary` | `logreg`     | 2-level factor or 0/1 numeric |
| `impute_vars_multi`  | `polyreg`    | Unordered factor with ≥3 levels |

> **Note:** For ordinal categorical variables, consider `polr` (proportional odds) instead of `polyreg`. Change the method in `impute_vars_multi` handling inside `.build_method_vector()` if needed.

### Predictor matrix vs. default MICE behaviour
By default MICE uses all other variables as predictors for every variable.
This function builds a **custom predictor matrix** so that only the predictors
you specify are used — improving convergence and preventing spurious
relationships from data-sparse columns.

### Conditional relationships
Variables that are structurally zero/NA for a subset of participants (e.g.
number of MH consultations = 0 for those who answered "No" to the gate
question) are handled by `.apply_cchs_relationships()`. This prevents MICE
from treating those cells as randomly missing. See
[special-imputations.md](special-imputations.md).

---

## Parameters to Tune

| Parameter | Default | Notes |
|-----------|---------|-------|
| `m`       | `1`     | Increase to 5–20 for final analysis runs |
| `maxit`   | `1`     | Increase to 10–50; monitor convergence with `plot(mice_result)` |
| `nnet.MaxNWts` | `4000` | Hardcoded in `.run_mice_manual()`; increase if polyreg fails on wide data |

---

## Known Limitations / TODOs

- [ ] `logreg` assumes binary variables are 2-level factors or 0/1 numerics.
  CCHS binary vars coded 1/2 as labelled integers must be converted to factors
  before calling this function.
- [ ] `polyreg` does not respect variable ordering. For clearly ordinal
  variables (e.g. `GEN_01` self-perceived health), consider switching to
  `polr` in `.build_method_vector()`.
- [ ] Currently returns `mice::complete(imp_result)` with `action = 1`
  (first imputation only). For `m > 1`, downstream pooling with
  `mice::pool()` should be used on `mice_result` directly.
- [ ] Structural missingness (e.g. `CCC_061` and `CCC_171` all 996 in
  2017-2018) should be excluded from imputation targets for that cycle.
  Pre-filter the imputation vectors by cycle if running cycle-stratified.
- [ ] Add more CCHS conditional relationships to `.apply_cchs_relationships()`
  as they are identified. See [special-imputations.md](special-imputations.md).