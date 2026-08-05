library(dplyr)
library(flextable)
library(labelled)
library(survey)

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

# get label helper
get_label <- function(col, varname) {
  lbl <- attr(col, "label", exact = TRUE)
  if (!is.null(lbl) && !is.na(lbl) && nzchar(trimws(lbl))) lbl else varname
}

####compute mean and iqr and set up formatting for output in the
fmt_mean_iqr <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x)]
  if (length(x) == 0) return("—")
  sprintf("%.1f [%.1f, %.1f]", mean(x), quantile(x, 0.25), quantile(x, 0.75))
}

# Display labels for tagged / string NA codes
.na_display <- c(
  "a" = "Not applicable",
  "b" = "Missing",
  "c" = "Question not asked in survey"
)

# col is a label-backed double (haven_labelled): value labels via `labels`,
# tagged NAs recoded to display labels so they appear as named categories.
as_factor_safe <- function(col) {
  tags <- haven::na_tag(col)
  #use to_factor to preserve the value labels while converting column to a factor
  f    <- to_factor(col)

  #loop through the names in na_display and assign them to tagged values corresponding to names in na_display
  for (tag in names(.na_display)) {
    mask <- !is.na(tags) & tags == tag
    if (any(mask)) {
      lbl <- .na_display[[tag]]
      if (!lbl %in% levels(f)) levels(f) <- c(levels(f), lbl)
      f[mask] <- lbl
    }
  }
  f
}

# return the levels of the labelled factor in a list format
all_levels <- setNames(
  lapply(t1_vars, function(v) {
    if (v %in% t1_cont) return(NULL)
    levels(as_factor_safe(harmonized_data[[v]]))
  }),
  t1_vars
)



# Build the statistics rows
build_rows <- function(data, n_adj = NULL) {
  if (is.null(n_adj)) n_adj <- nrow(data)
  out <- list()

  #####cycle through the t1 vars, extract the coluymn of the same name, get its label
  ####if a continous variable, make a dataframe with the label under characteristic column, stat column with median and iqr, false for is_level
  for (v in t1_vars) {
    col <- data[[v]]
    lbl <- get_label(col, v)

    if (v %in% t1_cont) {
      out[[length(out) + 1]] <- data.frame(
        Characteristic   = as.character(lbl),
        stat             = fmt_mean_iqr(col),
        is_level         = FALSE,
        var              = v,
        stringsAsFactors = FALSE
      )
    } else {
      # Variable name row — no stat value
      out[[length(out) + 1]] <- data.frame(
        Characteristic   = as.character(lbl),
        stat             = "",
        is_level         = FALSE,
        var              = v,
        stringsAsFactors = FALSE
      )
      # One row per level, using the pre-computed levels so both groups align
      lvls   <- all_levels[[v]]
      x      <- factor(as_factor_safe(col), levels = lvls)
      counts <- as.integer(table(x))

      #loop through the levels of the factor and output their counts and percentages in the stat column
      for (i in seq_along(lvls)) {
        stat_val <- sprintf("%d (%.1f%%)", counts[i], 100 * counts[i] / n_adj)
        out[[length(out) + 1]] <- data.frame(
          Characteristic   = as.character(lvls[i]),
          stat             = stat_val,
          is_level         = TRUE,
          var              = v,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  bind_rows(out)
}

# compute per
bzd_data  <- filter(harmonized_data, as.character(first_drug_class) == "2")
opioid_data <- filter(harmonized_data, as.character(first_drug_class) == "1")
norx_data<- filter(harmonized_data, as.character(first_drug_class) == "0")

bzd_n_adj   <- nrow(bzd_data)
opioid_n_adj <- nrow(opioid_data)
norx_n_adj <- nrow(norx_data)

rows_bzd <- build_rows(bzd_data,   n_adj = bzd_n_adj)
rows_opioid <- build_rows(opioid_data, n_adj = opioid_n_adj)
rows_norx <- build_rows(norx_data, n_adj = norx_n_adj)

tbl_body <- data.frame(
  Characteristic = rows_bzd$Characteristic,
  BZD            = rows_bzd$stat,
  Opioid         = rows_opioid$stat,
  NORX           = rows_norx$stat,
  is_level       = rows_bzd$is_level,
  var            = rows_bzd$var,
  stringsAsFactors = FALSE
)

# Prepend sample-size row
tbl_body <- rbind(
  data.frame(
    Characteristic = "N",
    Male           = as.character(male_n_adj),
    Female         = as.character(female_n_adj),
    is_level       = FALSE,
    var            = NA_character_,
    stringsAsFactors = FALSE
  ),
  tbl_body
)

# at the end of each grouping of variables insert a header indicating a new start of a new set of covariates
insert_section <- function(df, sec_var, sec_label) {
  idx <- which(!df$is_level & !is.na(df$var) & df$var == sec_var)[1]
  if (is.na(idx)) return(df)
  hdr <- data.frame(
    Characteristic = sec_label,
    BZD            = "",
    Opioid         = "",
    NORX           = "",
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
tbl_display <- tbl_body[, c("Characteristic", "BZD", "Opioid", "NORX")]

tbl_1 <- flextable(tbl_display) %>%
  set_header_labels(
    Characteristic = "Characteristic",
    BZD            = sprintf("BZD (n = %d)", bzd_n_adj),
    Opioid         = sprintf("Opioid (n = %d)", opioid_n_adj),
    NORX           = sprintf("NORX (n = %d)", norx_n_adj)
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



#########     WEIGHTED TABLES ############

# Build weighted statistical computation rows using survey::svydesign() for
# design-consistent point estimates: categorical variables via svytotal(),
# continuous variables via svyquantile()

#establish denominator as sum of the weights for each group
build_rows_weighted <- function(data, w_adj = NULL) {
  if (is.null(w_adj)) w_adj <- sum(data[["WTS_L"]], na.rm = TRUE)

  # Materialise a clean, fixed-level factor for every categorical t1 variable
  # so svydesign() sees ordinary factors, not haven_labelled doubles.
  for (v in t1_vars) {
    if (v %in% t1_cont) next
    data[[paste0(".f_", v)]] <- factor(as_factor_safe(data[[v]]), levels = all_levels[[v]])
  }
  design <- survey::svydesign(ids = ~1, weights = ~WTS_L, data = data)

  out <- list()
  for (v in t1_vars) {
    col <- data[[v]]
    lbl <- get_label(col, v)

    if (v %in% t1_cont) {
      m <- tryCatch({
        as.numeric(stats::coef(survey::svymean(reformulate(v), design, na.rm = TRUE)))
      }, error = function(e) NA_real_)
      q <- tryCatch({
        qs <- survey::svyquantile(reformulate(v), design,
                                  quantiles = c(0.25, 0.75), na.rm = TRUE)
        as.numeric(qs[[v]][, "quantile"])
      }, error = function(e) rep(NA_real_, 2))
      stat_val <- if (anyNA(c(m, q))) "—" else sprintf("%.1f [%.1f, %.1f]", m, q[1], q[2])
      out[[length(out) + 1]] <- data.frame(
        Characteristic   = as.character(lbl),
        stat             = stat_val,
        is_level         = FALSE,
        var              = v,
        stringsAsFactors = FALSE
      )
    } else {
      out[[length(out) + 1]] <- data.frame(
        Characteristic   = as.character(lbl),
        stat             = "",
        is_level         = FALSE,
        var              = v,
        stringsAsFactors = FALSE
      )
      lvls <- all_levels[[v]]

      tot <- survey::svytotal(reformulate(paste0(".f_", v)), design, na.rm = TRUE)
      est <- stats::coef(tot)
      names(est) <- sub(paste0("^\\.f_", v), "", names(est))
      cat_w <- setNames(rep(0, length(lvls)), lvls)
      cat_w[names(est)] <- est

      for (i in seq_along(lvls)) {
        stat_val <- sprintf("%.0f (%.1f%%)", cat_w[[lvls[i]]], 100 * cat_w[[lvls[i]]] / w_adj)
        out[[length(out) + 1]] <- data.frame(
          Characteristic   = as.character(lvls[i]),
          stat             = stat_val,
          is_level         = TRUE,
          var              = v,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  bind_rows(out)
}

# ── Compute weighted per-sex tables ───────────────────────────────────────────
male_wtd_adj   <- sum(male_data[["WTS_L"]], na.rm = TRUE)
female_wtd_adj <- sum(female_data[["WTS_L"]], na.rm = TRUE)

rows_m_wtd <- build_rows_weighted(male_data,   w_adj = male_wtd_adj)
rows_f_wtd <- build_rows_weighted(female_data, w_adj = female_wtd_adj)

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
    Male           = sprintf("%.0f", male_wtd_adj),
    Female         = sprintf("%.0f", female_wtd_adj),
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
    Male           = sprintf("Male (n = %d)", male_n_adj),
    Female         = sprintf("Female (n = %d)", female_n_adj)
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







# =============================================================================
# TABLE 2 — STRATIFIED BY OUTCOME (event == 1 vs event != 1)
# Unweighted and weighted versions, same format as tbl_1 / tbl_1_wtd
# =============================================================================

nms_data   <- filter(harmonized_data, event == 1)
nonms_data <- filter(harmonized_data, event != 1)

nms_n_adj   <- nrow(nms_data)
nonms_n_adj <- nrow(nonms_data)

# ── Unweighted ────────────────────────────────────────────────────────────────
rows_nms   <- build_rows(nms_data,   n_adj = nms_n_adj)
rows_nonms <- build_rows(nonms_data, n_adj = nonms_n_adj)

tbl_body_outcome <- data.frame(
  Characteristic = rows_nms$Characteristic,
  NMS            = rows_nms$stat,
  No_NMS         = rows_nonms$stat,
  is_level       = rows_nms$is_level,
  var            = rows_nms$var,
  stringsAsFactors = FALSE
)

tbl_body_outcome <- rbind(
  data.frame(
    Characteristic = "N",
    NMS            = as.character(nms_n_adj),
    No_NMS         = as.character(nonms_n_adj),
    is_level       = FALSE,
    var            = NA_character_,
    stringsAsFactors = FALSE
  ),
  tbl_body_outcome
)

insert_section_outcome <- function(df, sec_var, sec_label) {
  idx <- which(!df$is_level & !is.na(df$var) & df$var == sec_var)[1]
  if (is.na(idx)) return(df)
  hdr <- data.frame(
    Characteristic = sec_label,
    NMS            = "",
    No_NMS         = "",
    is_level       = FALSE,
    var            = NA_character_,
    stringsAsFactors = FALSE
  )
  rbind(df[seq_len(idx - 1L), ], hdr, df[idx:nrow(df), ], make.row.names = FALSE)
}

for (v in rev(names(section_starts))) {
  tbl_body_outcome <- insert_section_outcome(tbl_body_outcome, v, section_starts[[v]])
}

section_rows_outcome <- which(tbl_body_outcome$Characteristic %in% as.character(section_starts))
level_rows_outcome   <- which(tbl_body_outcome$is_level)

tbl_display_outcome <- tbl_body_outcome[, c("Characteristic", "NMS", "No_NMS")]

tbl_outcome <- flextable(tbl_display_outcome) %>%
  set_header_labels(
    Characteristic = "Characteristic",
    NMS            = sprintf("NMS Death (n = %d)", nms_n_adj),
    No_NMS         = sprintf("No NMS Death (n = %d)", nonms_n_adj)
  ) %>%
  bold(i = section_rows_outcome, part = "body") %>%
  bg(i = section_rows_outcome, bg = "#f2f2f2", part = "body") %>%
  padding(i = level_rows_outcome, j = 1, padding.left = 20, part = "body") %>%
  bold(part = "header") %>%
  align(j = 2:3, align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  theme_booktabs() %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  set_table_properties(layout = "autofit")


# ── Weighted ──────────────────────────────────────────────────────────────────
nms_wtd_adj   <- sum(nms_data[["WTS_L"]], na.rm = TRUE)
nonms_wtd_adj <- sum(nonms_data[["WTS_L"]], na.rm = TRUE)

rows_nms_wtd   <- build_rows_weighted(nms_data,   w_adj = nms_wtd_adj)
rows_nonms_wtd <- build_rows_weighted(nonms_data, w_adj = nonms_wtd_adj)

tbl_body_outcome_wtd <- data.frame(
  Characteristic = rows_nms_wtd$Characteristic,
  NMS            = rows_nms_wtd$stat,
  No_NMS         = rows_nonms_wtd$stat,
  is_level       = rows_nms_wtd$is_level,
  var            = rows_nms_wtd$var,
  stringsAsFactors = FALSE
)

tbl_body_outcome_wtd <- rbind(
  data.frame(
    Characteristic = "Weighted N",
    NMS            = sprintf("%.0f", nms_wtd_adj),
    No_NMS         = sprintf("%.0f", nonms_wtd_adj),
    is_level       = FALSE,
    var            = NA_character_,
    stringsAsFactors = FALSE
  ),
  tbl_body_outcome_wtd
)

for (v in rev(names(section_starts))) {
  tbl_body_outcome_wtd <- insert_section_outcome(tbl_body_outcome_wtd, v, section_starts[[v]])
}

section_rows_outcome_wtd <- which(tbl_body_outcome_wtd$Characteristic %in% as.character(section_starts))
level_rows_outcome_wtd   <- which(tbl_body_outcome_wtd$is_level)

tbl_display_outcome_wtd <- tbl_body_outcome_wtd[, c("Characteristic", "NMS", "No_NMS")]

tbl_outcome_wtd <- flextable(tbl_display_outcome_wtd) %>%
  set_header_labels(
    Characteristic = "Characteristic",
    NMS            = sprintf("NMS Death (n = %d)", nms_n_adj),
    No_NMS         = sprintf("No NMS Death (n = %d)", nonms_n_adj)
  ) %>%
  bold(i = section_rows_outcome_wtd, part = "body") %>%
  bg(i = section_rows_outcome_wtd, bg = "#f2f2f2", part = "body") %>%
  padding(i = level_rows_outcome_wtd, j = 1, padding.left = 20, part = "body") %>%
  bold(part = "header") %>%
  align(j = 2:3, align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  theme_booktabs() %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  set_table_properties(layout = "autofit")
