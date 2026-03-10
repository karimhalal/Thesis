library(dplyr)
library(labelled)
library(haven)
library(gtsummary)
library(tibble)

# ── 1. Prepare table data ─────────────────────────────────────────────────────
# Value and variable labels are already applied during recoding.
# Only set labels for variables not covered in the recode step.

table1_data <- harmonized_data %>%
  set_variable_labels(
    ALWDWKY     = "Number of drinks last week, median (IQR)",
    SurveyCycle = "Survey year"
  )

# ── 2. Create Derivation / Validation cohort variable ─────────────────────────
# NOTE: Adjust the SurveyCycle values below to match what is stored in your data
#       (e.g., "cchs2001_p" vs "2001" vs "2001-2002").

table1_data <- table1_data %>%
  mutate(
    cohort = factor(
      case_when(
        SurveyCycle %in% c(
          "cchs2001_p", "cchs2003_p", "cchs2005_p", "cchs2007_2008_p",
          "2001", "2003", "2005", "2007/2008"
        ) ~ "Derivation",
        SurveyCycle %in% c(
          "cchs2009_2010_p", "cchs2011_2012_p",
          "2009/2010", "2011/2012"
        ) ~ "Validation",
        TRUE ~ NA_character_
      ),
      levels = c("Derivation", "Validation")
    )
  )

# ── 3. Build per-sex stratified tables ────────────────────────────────────────

tbl_vars <- c(
  # Socio-demographic
  DHHGAGE_D, SDCGCGT, EDUDR03, DHHGMS, FSCDHFS2,
  # General health
  GEN_10, GEN_07, GEN_09,
  # Health behaviours
  smoke_simple, ALCDTTM, ALWDWKY,
  # Functional
  HUPDPAD,
  # Health conditions
  CCC_031, CCC_071, CCC_091, CCC_121, CCC_131, CCC_280, CCC_290,
  # Design
  SurveyCycle
)

make_sex_tbl <- function(data, sex_code) {
  data %>%
    filter(DHH_SEX == sex_code) %>%
    tbl_summary(
      by      = cohort,
      include = c(
        DHHGAGE_D, SDCGCGT, EDUDR03, DHHGMS, FSCDHFS2,
        GEN_10, GEN_07, GEN_09,
        smoke_simple, ALCDTTM, ALWDWKY,
        HUPDPAD,
        CCC_031, CCC_071, CCC_091, CCC_121, CCC_131, CCC_280, CCC_290,
        SurveyCycle
      ),
      type = list(
        DHHGAGE_D ~ "continuous",   # show as median (IQR) not frequencies
        ALWDWKY   ~ "continuous"
      ),
      statistic = list(
        all_continuous()  ~ "{median} ({p25} to {p75})",
        all_categorical() ~ "{n} ({p}%)"
      ),
      digits = list(
        all_continuous()  ~ 1,
        all_categorical() ~ c(0, 1)
      ),
      missing      = "ifany",
      missing_text = "Missing"
    )
}

tbl_male   <- make_sex_tbl(table1_data, sex_code = 1)
tbl_female <- make_sex_tbl(table1_data, sex_code = 2)

# ── 4. Merge, add spanners and section headers ────────────────────────────────

# Helper: insert a bold section-header row immediately before a variable
insert_section_header <- function(df, header_text, before_var) {
  idx <- which(df$variable == before_var & df$row_type == "label")
  if (!length(idx)) return(df)
  tibble::add_row(
    df,
    variable = paste0("hdr_", before_var),
    row_type = "label",
    label    = paste0("**", header_text, "**"),
    .before  = idx[1]
  )
}

tbl_1 <- tbl_merge(
  tbls        = list(tbl_male, tbl_female),
  tab_spanner = c("**Male cohort**", "**Female cohort**")
) %>%
  # Column sub-headers (Derivation† / Validation‡)
  modify_header(
    stat_1_1 ~ "**Derivation\u2020**",
    stat_1_2 ~ "**Validation\u2021**",
    stat_2_1 ~ "**Derivation\u2020**",
    stat_2_2 ~ "**Validation\u2021**"
  ) %>%
  bold_labels() %>%
  # Section header rows
  modify_table_body(~ {
    .x %>%
      insert_section_header("Socio-demographic factors", "DHHGAGE_D")   %>%
      insert_section_header("General health",            "GEN_10")       %>%
      insert_section_header("Health behaviours",         "smoke_simple") %>%
      insert_section_header("Functional measures",       "HUPDPAD")      %>%
      insert_section_header("Health conditions",         "CCC_031")      %>%
      insert_section_header("Design",                    "SurveyCycle")
  }) %>%
  # Footnotes for dagger symbols in headers
  modify_footnote(
    stat_1_1 ~ "\u2020 Derivation cohort: survey cycles 2001\u20132008.",
    stat_1_2 ~ "\u2021 Validation cohort: survey cycles 2009\u20132012."
  )

tbl_1
