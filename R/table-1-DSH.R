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


# ── Weighted helpers ────────────────────────────────────────────────────────────
wtd_quantile <- function(x, w, probs = c(0.25, 0.5, 0.75)) {
  x <- suppressWarnings(as.numeric(x))
  ok <- !is.na(x) & !is.na(w) & w > 0
  x <- x[ok]; w <- w[ok]
  if (length(x) == 0) return(setNames(rep(NA_real_, length(probs)), probs))
  ord <- order(x)
  x <- x[ord]; w <- w[ord]
  cum_w <- cumsum(w) / sum(w)
  sapply(probs, function(p) x[which(cum_w >= p)[1]])
}

fmt_med_iqr_wtd <- function(x, w) {
  q <- wtd_quantile(x, w)
  if (anyNA(q)) return("—")
  sprintf("%.1f [%.1f, %.1f]", q[2], q[1], q[3])
}

# ── Build weighted stat rows for one group ─────────────────────────────────────
build_rows_weighted <- function(data) {
  w   <- data[["WTS_L"]]
  out <- list()

  for (v in t1_vars) {
    col <- data[[v]]
    lbl <- get_label(col, v)

    if (v %in% t1_cont) {
      out[[length(out) + 1]] <- data.frame(
        Characteristic = as.character(lbl),
        stat           = fmt_med_iqr_wtd(col, w),
        is_level       = FALSE,
        var            = v,
        stringsAsFactors = FALSE
      )
    } else {
      out[[length(out) + 1]] <- data.frame(
        Characteristic = as.character(lbl),
        stat           = "",
        is_level       = FALSE,
        var            = v,
        stringsAsFactors = FALSE
      )
      lvls    <- all_levels[[v]]
      x       <- factor(as_factor_safe(col), levels = lvls)
      total_w <- sum(w[!is.na(x)], na.rm = TRUE)
      for (i in seq_along(lvls)) {
        mask  <- !is.na(x) & x == lvls[i]
        cat_w <- sum(w[mask], na.rm = TRUE)
        out[[length(out) + 1]] <- data.frame(
          Characteristic = as.character(lvls[i]),
          stat           = sprintf("%.0f (%.1f%%)", cat_w, 100 * cat_w / total_w),
          is_level       = TRUE,
          var            = v,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  bind_rows(out)
}

# ── Compute weighted per-sex tables ───────────────────────────────────────────
rows_m_wtd <- build_rows_weighted(male_data)
rows_f_wtd <- build_rows_weighted(female_data)

tbl_body_wtd <- data.frame(
  Characteristic = rows_m_wtd$Characteristic,
  Male           = rows_m_wtd$stat,
  Female         = rows_f_wtd$stat,
  is_level       = rows_m_wtd$is_level,
  var            = rows_m_wtd$var,
  stringsAsFactors = FALSE
)

# Prepend weighted N row
tbl_body_wtd <- rbind(
  data.frame(
    Characteristic = "Weighted N",
    Male           = sprintf("%.0f", sum(male_data[["WTS_L"]], na.rm = TRUE)),
    Female         = sprintf("%.0f", sum(female_data[["WTS_L"]], na.rm = TRUE)),
    is_level       = FALSE,
    var            = NA_character_,
    stringsAsFactors = FALSE
  ),
  tbl_body_wtd
)

# Insert section headers (reuse insert_section, but adapted for wtd body)
insert_section_wtd <- function(df, sec_var, sec_label) {
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
  tbl_body_wtd <- insert_section_wtd(tbl_body_wtd, v, section_starts[[v]])
}

# ── Row indices for styling ────────────────────────────────────────────────────
section_rows_wtd <- which(tbl_body_wtd$Characteristic %in% as.character(section_starts))
level_rows_wtd   <- which(tbl_body_wtd$is_level)

# ── Build weighted flextable ───────────────────────────────────────────────────
tbl_display_wtd <- tbl_body_wtd[, c("Characteristic", "Male", "Female")]

tbl_1_wtd <- flextable(tbl_display_wtd) %>%
  set_header_labels(
    Characteristic = "Characteristic",
    Male           = sprintf("Male (n = %d)", nrow(male_data)),
    Female         = sprintf("Female (n = %d)", nrow(female_data))
  ) %>%
  bold(i = section_rows_wtd, part = "body") %>%
  bg(i = section_rows_wtd, bg = "#f2f2f2", part = "body") %>%
  padding(i = level_rows_wtd, j = 1, padding.left = 20, part = "body") %>%
  bold(part = "header") %>%
  align(j = 2:3, align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  theme_booktabs() %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  set_table_properties(layout = "autofit")
