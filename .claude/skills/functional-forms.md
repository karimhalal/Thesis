---
name: functional-forms
description: Implement or modify variable transformation functions (centering, dummy coding, restricted cubic splines) for CCHS analysis. Use when the user asks about transforming variables, functional forms, centering, dummy variables, splines, or the _c/_cat/_rcs naming convention.
---

# Variable Transformation Refactor — Skill Prompt

## Objective

Refactor the attached function so that it operates **without the `variables_sheet`**, relying entirely on manual input to specify which variables should be transformed.

---

## Requirements

### Input

The refactored functions should accept **vectors of character values** representing the names of the variables to be transformed:

```r
# Example inputs
vars_to_center  <- c("age", "bmi", "income")
vars_to_dummy   <- c("sex", "province", "education")
```

### Output — Naming Conventions

#### Transformation Cascade

Transformations are applied in the following priority order:

```
Raw Variable
     │
     ▼
[1] Dummy Coding        (categorical variables only)
     │  DHH_SEX → DHH_SEX_cat1, DHH_SEX_cat2
     │
     ▼
[2] Centering           (continuous variables; also applied to each dummy column)
     │  DHH_AGE → DHH_AGE_c
     │  DHH_SEX_cat1 → DHH_SEX_cat1_c
     │
     ▼
[3] Restricted Cubic Spline   (applied to the centered variable)
     │  DHH_AGE_c → DHH_AGE_c_rcs1, DHH_AGE_c_rcs2, ...
     │
     ▼
   Final transformed columns appended to data
```

Not all variables will undergo all transformations. Which variables go through which tranformation is determined by the function user. Transformed variables follow this consistent naming scheme, in cascade order:

| Step | Transformation          | Naming Pattern              | Example                                          |
|------|-------------------------|-----------------------------|--------------------------------------------------|
| 1    | Dummy coded             | `[original_name]_cat[#]`    | `DHH_SEX` → `DHH_SEX_cat1`, `DHH_SEX_cat2`      |
| 2    | Centering               | `[original_name]_c`         | `DHH_AGE` → `DHH_AGE_c`                         |
| 3    | Restricted Cubic Spline | `[original_name]_rcs[#]`    | `DHH_AGE_c` → `DHH_AGE_c_rcs1`, `DHH_AGE_c_rcs2`|

> **Note:** The number at the end of a spline variable corresponds to the knot index, where the total number of knots is determined by the user and can vary between variables. Splines are applied after centering, which in turn is applied after dummy coding for categorical variables.

---

## Function Signatures

### Centering Function

```r
center_variables <- function(data, vars_to_center) {
  # Centers each variable named in vars_to_center
  # Returns data with new _c columns appended
}
```

**Details:**
- Should center by subtracting the column mean
- Should handle `NA` values via `na.rm = TRUE`
- Additional behaviour: For categorical variables that have been dummied, the centering will be applied to each variable in selected list 

---

### Dummy Coding Function

```r
dummy_variables <- function(data, vars_to_dummy, reference_levels = NULL) {
  # Creates dummy variables for each variable named in vars_to_dummy
  # Returns data with new dummy columns appended
}
```

**Details:**
- Reference level handling: The reference level should be the most common category unless otherwise 
- The funciton will drop the dummy reference
- Factor vs character input handling: The function should begin by converting all cahracters into a factor input.
- Additional behaviour: 

---

## Constraints

- Must **not** depend on `variables_sheet` or any external metadata sheet
- Must **not** modify the original columns — only append new ones
- Should be usable as a standalone utility (no side effects)

---

## Additional Notes / Edge Cases

*(Add any edge cases, special variables, or project-specific requirements here)*

---

## Related Files

- [variables-sheet-utils.R](../../R/variables-sheet-utils.R) — original sheet-dependent utilities
- [transformation-type.R](../../R/transformation-type.R) — transformation type logic
- [create-study-data.R](../../R/create-study-data.R) — downstream consumer of transformed data
