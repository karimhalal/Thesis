library(dplyr)
library(tableone)
library(flextable)

# ── Variables ──────────────────────────────────────────────────────────────────
t1_vars <- c(
  "DHH_AGE", "race_collapsed", "EDUDR03", "DHH_MS", "FSCDHFS2",
  "GEN_10", "GEN_02B", "GEN_07", "GEN_09", "GENGSWL",
  "smoke_simple", "ALCDTTM", "ALWDWKY",
  "HUPDPAD",
  "CCC_031", "CCC_071", "CCC_091", "CCC_121", "CCC_131", "CCC_280", "CCC_290",
  "mutliple_cond_der", "INCDRCA", "INCDRPR","INCDRRS",
  "material_deprivation","bmi_adj",
  "SurveyCycle"
)
t1_cont <- c("DHH_AGE", "ALWDWKY")   # shown as median (IQR)

# ── Split into 4 sex × cohort groups ──────────────────────────────────────────
male_deriv   <- filter(harmonized_data, DHH_SEX == 1, cohort == "Derivation")
male_valid   <- filter(harmonized_data, DHH_SEX == 1, cohort == "Validation")
female_deriv <- filter(harmonized_data, DHH_SEX == 2, cohort == "Derivation")
female_valid <- filter(harmonized_data, DHH_SEX == 2, cohort == "Validation")

# ── Helper: run tableone → named character vector ─────────────────────────────
run_t1 <- function(data) {
  tbl <- CreateTableOne(
    vars       = t1_vars,
    data       = data,
    factorVars = setdiff(t1_vars, t1_cont)
  )
  mat <- print(tbl,
               nonnormal     = t1_cont,
               showAllLevels = TRUE,
               printToggle   = FALSE,
               noSpaces      = TRUE,
               contDigits    = 1,
               catDigits     = 1,
               quote         = FALSE)
  setNames(as.character(mat[, "Overall"]), rownames(mat))
}

col_md <- run_t1(male_deriv)
col_mv <- run_t1(male_valid)
col_fd <- run_t1(female_deriv)
col_fv <- run_t1(female_valid)

# ── Combine into a single data frame ──────────────────────────────────────────
tbl_body <- data.frame(
  Characteristic = names(col_md),
  Male_Deriv     = col_md,
  Male_Valid     = col_mv,
  Female_Deriv   = col_fd,
  Female_Valid   = col_fv,
  row.names      = NULL,
  stringsAsFactors = FALSE
)

# Capitalise the "n" row from tableone
tbl_body$Characteristic[tbl_body$Characteristic == "n"] <- "N"

# ── Insert section header rows ────────────────────────────────────────────────
# Looks up the variable label (or falls back to variable name) to find the
# correct insertion point in Characteristic.
insert_section <- function(df, label, var_name, ref_data) {
  search <- attr(ref_data[[var_name]], "label")
  if (is.null(search) || !nzchar(search)) search <- var_name
  idx <- grep(search, df$Characteristic, fixed = TRUE)[1]
  if (is.na(idx)) return(df)
  hdr <- data.frame(
    Characteristic = label,
    Male_Deriv = "", Male_Valid = "",
    Female_Deriv = "", Female_Valid = "",
    stringsAsFactors = FALSE
  )
  rbind(df[seq_len(idx - 1), ], hdr, df[idx:nrow(df), ], make.row.names = FALSE)
}

tbl_body <- tbl_body %>%
  insert_section("Socio-demographic factors", "DHHGAGE_D",    harmonized_data) %>%
  insert_section("General health",            "GEN_10",       harmonized_data) %>%
  insert_section("Health behaviours",         "smoke_simple", harmonized_data) %>%
  insert_section("Functional measures",       "HUPDPAD",      harmonized_data) %>%
  insert_section("Health conditions",         "CCC_031",      harmonized_data) %>%
  insert_section("Design",                    "SurveyCycle",  harmonized_data)

# ── Row indices for styling ───────────────────────────────────────────────────
section_labels <- c(
  "Socio-demographic factors", "General health", "Health behaviours",
  "Functional measures", "Health conditions", "Design"
)
section_rows <- which(tbl_body$Characteristic %in% section_labels)
level_rows   <- grep("^  ", tbl_body$Characteristic)   # tableone indents levels with 2 spaces

# ── Build flextable ───────────────────────────────────────────────────────────
tbl_1 <- flextable(tbl_body) %>%
  # Bottom header row: Derivation / Validation labels
  set_header_labels(
    Characteristic = "Characteristic",
    Male_Deriv     = "Derivation\u2020",
    Male_Valid     = "Validation\u2021",
    Female_Deriv   = "Derivation\u2020",
    Female_Valid   = "Validation\u2021"
  ) %>%
  # Top header row: sex-cohort spanners
  add_header_row(
    values    = c("", "Male cohort", "Female cohort"),
    colwidths = c(1, 2, 2)
  ) %>%
  # Section header rows: bold, light background
  bold(i = section_rows, part = "body") %>%
  bg(i = section_rows, bg = "#f2f2f2", part = "body") %>%
  # Indent categorical level rows
  padding(i = level_rows, j = 1, padding.left = 20, part = "body") %>%
  # Header styling
  bold(part = "header") %>%
  align(j = 2:5, align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  # Three-line (booktabs) theme
  theme_booktabs() %>%
  # Typography
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  set_table_properties(layout = "autofit") %>%
  # Footnotes
  add_footer_lines(c(
    "\u2020 Derivation cohort: survey cycles 2001\u20132008.",
    "\u2021 Validation cohort: survey cycles 2009\u20132012."
  )) %>%
  font(fontname = "Times New Roman", part = "footer") %>%
  fontsize(size = 9, part = "footer")

generate_mock_cchs <- function() {
  output <- data.frame()
}
