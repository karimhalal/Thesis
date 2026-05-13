---
name: special-imputations
description: Reference for CCHS gate-question conditional relationships hardcoded in .apply_cchs_relationships(). Use when adding, modifying, or debugging conditional imputation rules for CCHS variables (gate questions, structural zeros, structural NAs).
---

# Special Imputations — CCHS Conditional Relationships

This document describes variables whose imputation is **conditionally dependent
on another variable** (a "gate question"). Each relationship is hardcoded in
`.apply_cchs_relationships()` inside [impute-data-manual.R](../../R/impute-data-manual.R).

The function is called **twice** during the imputation pipeline:

1. **Pre-imputation** — zeros/NAs out conditional cells for rows where the gate
   variable is already observed, preventing MICE from treating them as randomly
   missing.

2. **Post-imputation** — re-applies the same rules to correct any rows where the
   gate variable itself was imputed.

---

## Relationship Catalogue

### 1. Mental Health Consultations

| Role | Variable | Label | Coding |
|------|----------|-------|--------|
| Gate | `CMH_01K` | Consulted a mental health professional in past year | 1 = Yes, 2 = No, 996 = Not applicable (2013-2018) |
| Downstream | `CMH_01L` | Number of times consulted in past year | Continuous (whole numbers) |

**Rule:** If `CMH_01K == 2` (No), then `CMH_01L = 0`.

**Rationale:** A respondent who has not consulted a mental health professional
cannot have a positive count of consultations. The downstream variable is
structurally zero, not randomly missing.

**Cycle note:** In 2017-2018, `CMH_01K` is entirely coded `996` (not
applicable). Variables will be treated as missing at random with SurveyCycle
being an imputation predictor. 



---

### 2. Alcohol — Weekly Drink Total

| Role | Variable | Label | Coding |
|------|----------|-------|--------|
| Gate | `ALCDTTM` | Type of drinker | 1 = Regular, 2 = Occasional, 3 = Did not drink in past 12 months |
| Downstream | `ALWDWKY` | Total drinks consumed last week | Continuous |

**Rule:** If `ALCDTTM == 3` (did not drink in past 12 months), then `ALWDWKY = 0`.

**Rationale:** Someone who did not drink at all in the past year cannot have a
positive weekly drink count.



---

### 3. Alcohol — Per-Day Drink Counts

| Role | Variable | Label | Coding |
|------|----------|-------|--------|
| Gate | `ALW_1` | Drank alcohol in the past week | 1 = Yes, 2 = No |
| Downstream | `ALW_2A1`–`ALW_2A7` | Number of drinks each day of the week | Continuous |

**Rule:** If `ALW_1 == 2` (No), then `ALW_2A1` through `ALW_2A7 = 0`.

**Rationale:** Someone who did not drink in the past week had zero drinks on
each day of that week.

---

### 4. Smoking — Current Daily Smoker Variables

| Role | Variable | Label | Coding |
|------|----------|-------|--------|
| Gate | `SMKDSTY` | Type of smoker | 1 = Daily, 2 = Occasional, 3 = Always occasional, 4 = Former daily, 5 = Former occasional, 6 = Never smoked |
| Downstream | `SMK_203` | Age started smoking daily (current daily) | Continuous |
| Downstream | `SMK_204` | Number of cigarettes per day (current daily) | Continuous |

**Rule:** If `SMKDSTY != 1` (not a current daily smoker), then
`SMK_203 = Na(a)` and `SMK_204 = NA(a)`.

**Rationale:** Age of smoking onset and current daily cigarette count are only
applicable to current daily smokers. These are structural NAs, not randomly
missing values.

> *(Edit: confirm whether 0 is more appropriate than NA for your regression models)*

---

### 5. Smoking — Current Occasional Smoker Variable

| Role | Variable | Label | Coding |
|------|----------|-------|--------|
| Gate | `SMKDSTY` | Type of smoker | See above |
| Downstream | `SMK_05B` | Number of cigarettes per day (occasional) | Continuous |

**Rule:** If `SMKDSTY` is not 2 (occasional) or 3 (always occasional),
then `SMK_05B = NA`.

**Rationale:** Occasional cigarette count is only meaningful for current
occasional smokers.

---

### 6. Smoking — Former Daily Smoker Variables

| Role | Variable | Label | Coding |
|------|----------|-------|--------|
| Gate | `SMKDSTY` | Type of smoker | See above |
| Downstream | `SMK_207` | Age started smoking daily (former daily) | Continuous |
| Downstream | `SMK_208` | Number of cigarettes per day (former daily) | Continuous |
| Downstream | `SMK_09A` | Stopped smoking daily — when | Categorical: 1 = <1 yr, 2 = 1–2 yr, 3 = 2–3 yr, 4 = 3+ yr |
| Downstream | `SMK_09C` | Years since stopped smoking daily | Continuous |
| Downstream | `SMKDSTP` | Years since stopped smoking completely | Continuous |

**Rule:** If `SMKDSTY != 4` (not a former daily smoker), then all five
downstream variables `= NA`.

**Rationale:** Former daily smoker characteristics are structurally absent for
everyone who is not a former daily smoker.

> *(Edit: clarify whether `SMKDSTP` also applies to former occasional smokers
> (SMKDSTY == 5), since they have also stopped completely)*

---

### 7. Smoking — Never-Daily Smoker Variable

| Role | Variable | Label | Coding |
|------|----------|-------|--------|
| Gate | `SMKDSTY` | Type of smoker | See above |
| Downstream | `SMK_06A` | Stopped smoking (never daily) — when | Categorical: 1 = <1 yr, 2 = 1–2 yr, 3 = 2–3 yr, 4 = 3+ yr |

**Rule:** If `SMKDSTY` is not 3 (always occasional) or 5 (former occasional),
then `SMK_06A = NA`.

**Rationale:** The question of when someone stopped smoking as a never-daily
smoker only applies to people who smoked occasionally but never daily and have
now stopped.

---

### 8. Drug use — Lifetime and yearly variables

| Role | Variable | Label | Coding |
|------|----------|-------|--------|
| Gate | `drgdvlac` | Illicit Drug Use- Lifetime | Categorical: yes=1, no=2 |
| Downstream | `drgdvyac` | Illicit Drug Use- 12 Months | Categorical: yes=1, no=2 |

**Rule:** If `drgdvlac` is coded 2 so will the downstream variable `drgdvyac`.

**Rationale:** A participant can not have used illicit drugs in the past year
if they have already indicated that they have not used drugs over their entire
lifetime

---

## Notes on Zeroing vs. Setting to NA

The choice between setting a conditional downstream variable to **`0`** vs.
**`NA`** depends on how the variable is used in the regression models:

- **Set to `0`** when the variable represents a quantity that is genuinely zero
  for the non-applicable group (e.g. number of drinks = 0 for non-drinkers).
  This preserves the observation and prevents MICE from imputing it.
- **Set to `NA`** when the variable is conceptually undefined for the
  non-applicable group (e.g. age started smoking daily is meaningless for a
  non-smoker). This excludes the observation from analysis of that variable.

Current decisions are noted in each relationship above. Review and update if
your modelling strategy changes.