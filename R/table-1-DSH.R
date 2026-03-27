library(dplyr)
library(flextable)
library(labelled)

# ── Variable configuration ─────────────────────────────────────────────────────
t1_vars <- c(
  "DHH_AGE", "race_collapsed", "EDUDR03", "DHH_MS", "FSCDHFS2",
  "GEN_10", "GEN_02B", "GEN_07", "GEN_09", "GENGSWL",
  "SMKDSTY_cat5", "ALCDTTM", "ALWDWKY",
  "HUPDPAD",
  "CCC_031", "CCC_071", "CCC_091", "CCC_121", "CCC_131", "CCC_280", "CCC_290",
  "mutliple_cond_der", "INCDRCA", "INCDRPR", "INCDRRS",
  "material_deprivation", "rural", "bmi_adj",
  "SurveyCycle"
)
t1_cont <- c("DHH_AGE", "ALWDWKY", "bmi_adj")

# First variable in each section → section title
section_starts <- c(
  "DHH_AGE"      = "Socio-demographic factors",
  "GEN_10"       = "General health",
  "SMKDSTY_cat5" = "Health behaviours",
  "HUPDPAD"      = "Functional measures",
  "CCC_031"      = "Health conditions",
  "SurveyCycle"  = "Design"
)

# ── Helpers ────────────────────────────────────────────────────────────────────
get_label <- function(col, varname) {
  lbl <- attr(col, "label")
  lbl <- lbl[1]   # guard against length > 1 labels from set_data_labels()
  if (!is.null(lbl) && !is.na(lbl) && nzchar(trimws(lbl))) lbl else varname
}

fmt_med_iqr <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x)]
  if (length(x) == 0) return("—")
  sprintf("%.1f [%.1f, %.1f]", median(x), quantile(x, 0.25), quantile(x, 0.75))
}

# Display labels for tagged / string NA codes
.na_display <- c(
  "a" = "Not applicable",
  "b" = "Missing",
  "c" = "Question not asked in survey"
)

# Return col as a factor using value labels when available.
# Tagged NAs and NA(x) string levels are converted to display labels
# so they appear as named categories in the table.
as_factor_safe <- function(col) {
  if (is.labelled(col) && length(val_labels(col)) > 0) {
    # Capture tags BEFORE to_factor() strips them
    tags <- haven::na_tag(col)
    f    <- to_factor(col)
    for (tag in names(.na_display)) {
      mask <- !is.na(tags) & tags == tag
      if (any(mask)) {
        lbl <- .na_display[[tag]]
        if (!lbl %in% levels(f)) levels(f) <- c(levels(f), lbl)
        f[mask] <- lbl
      }
    }
  } else if (is.factor(col)) {
    f    <- col
    lvls <- levels(f)
    # Fallback: rename any NA(a)/NA(b)/NA(c) string levels
    # (not produced by loadData.R, but handled defensively)
    na_map <- c("NA(a)" = "Not applicable",
                "NA(b)" = "Missing",
                "NA(c)" = "Question not asked in survey")
    levels(f) <- ifelse(lvls %in% names(na_map), na_map[lvls], lvls)
  } else {
    f <- factor(as.character(col))
  }
  f
}

# ── Pre-compute levels from the full dataset so both sexes share the same rows ─
all_levels <- setNames(
  lapply(t1_vars, function(v) {
    if (v %in% t1_cont) return(NULL)
    levels(as_factor_safe(harmonized_data[[v]]))
  }),
  t1_vars
)

# ── Build stat rows for one group ──────────────────────────────────────────────
build_rows <- function(data) {
  n   <- nrow(data)
  out <- list()

  for (v in t1_vars) {
    col <- data[[v]]
    lbl <- get_label(col, v)

    if (v %in% t1_cont) {
      out[[length(out) + 1]] <- data.frame(
        Characteristic = as.character(lbl),
        stat           = fmt_med_iqr(col),
        is_level       = FALSE,
        var            = v,
        stringsAsFactors = FALSE
      )
    } else {
      # Variable name row — no stat value
      out[[length(out) + 1]] <- data.frame(
        Characteristic = as.character(lbl),
        stat           = "",
        is_level       = FALSE,
        var            = v,
        stringsAsFactors = FALSE
      )
      # One row per level, using the pre-computed levels so both sexes align
      lvls <- all_levels[[v]]
      x    <- factor(as_factor_safe(col), levels = lvls)
      tbl  <- table(x)
      for (i in seq_along(lvls)) {
        cnt <- as.integer(tbl[i])
        out[[length(out) + 1]] <- data.frame(
          Characteristic = as.character(lvls[i]),
          stat           = sprintf("%d (%.1f%%)", cnt, 100 * cnt / n),
          is_level       = TRUE,
          var            = v,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  bind_rows(out)
}

# ── Compute per-sex tables ─────────────────────────────────────────────────────
male_data   <- filter(harmonized_data, as.character(DHH_SEX) == "Male")
female_data <- filter(harmonized_data, as.character(DHH_SEX) == "Female")

rows_m <- build_rows(male_data)
rows_f <- build_rows(female_data)

tbl_body <- data.frame(
  Characteristic = rows_m$Characteristic,
  Male           = rows_m$stat,
  Female         = rows_f$stat,
  is_level       = rows_m$is_level,
  var            = rows_m$var,
  stringsAsFactors = FALSE
)

# Prepend sample-size row
tbl_body <- rbind(
  data.frame(
    Characteristic = "N",
    Male           = as.character(nrow(male_data)),
    Female         = as.character(nrow(female_data)),
    is_level       = FALSE,
    var            = NA_character_,
    stringsAsFactors = FALSE
  ),
  tbl_body
)

# ── Insert section header rows ─────────────────────────────────────────────────
# Insert from bottom to top so earlier insertions don't shift later indices
insert_section <- function(df, sec_var, sec_label) {
  idx <- which(!df$is_level & !is.na(df$var) & df$var == sec_var)[1]
  if (is.na(idx)) return(df)
  hdr <- data.frame(
    Characteristic = sec_label,
    Male           = "",
    Female         = "",
    is_level       = FALSE,
    var            = NA_character_,
    stringsAsFactors = FALSE
  )
  rbind(df[seq_len(idx - 1L), ], hdr, df[idx:nrow(df), ], make.row.names = FALSE)
}

for (v in rev(names(section_starts))) {
  tbl_body <- insert_section(tbl_body, v, section_starts[[v]])
}

# ── Row indices for styling ────────────────────────────────────────────────────
section_rows <- which(tbl_body$Characteristic %in% as.character(section_starts))
level_rows   <- which(tbl_body$is_level)

# ── Build flextable ────────────────────────────────────────────────────────────
tbl_display <- tbl_body[, c("Characteristic", "Male", "Female")]

tbl_1 <- flextable(tbl_display) %>%
  set_header_labels(
    Characteristic = "Characteristic",
    Male           = sprintf("Male (n = %d)", nrow(male_data)),
    Female         = sprintf("Female (n = %d)", nrow(female_data))
  ) %>%
  bold(i = section_rows, part = "body") %>%
  bg(i = section_rows, bg = "#f2f2f2", part = "body") %>%
  padding(i = level_rows, j = 1, padding.left = 20, part = "body") %>%
  bold(part = "header") %>%
  align(j = 2:3, align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  theme_booktabs() %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  set_table_properties(layout = "autofit")

