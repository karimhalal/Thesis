# Variance inflation factors (VIFs) for the analysis model.
#
# Computed from a SIMPLIFIED specification of formula_event1 (causal_models.R):
# the raw base variables only, with spline, centering, and interaction terms
# removed.  Each categorical predictor is wrapped in factor() so collinearity is
# assessed on its dummy contrasts.  VIFs are read from the fitted Cox model via
# rms::vif (one value per model coefficient).

calculate_simplified_vif <- function(data) {
  # Chronic conditions are coded 1 = Yes, 2 = No.  Relevel so "No" (2) is the
  # reference; each coefficient then represents the "Yes" contrast and is named
  # "<var>1" (e.g. CCC_0311).
  cond_vars <- c("CCC_031", "CCC_051", "CCC_061", "CCC_071", "CCC_091",
                 "CCC_101", "CCC_121", "CCC_131", "CCC_171", "CCC_280", "CCC_290")
  data[cond_vars] <- lapply(data[cond_vars],
                            function(x) relevel(factor(x), ref = "2"))

  simplified_formula <- survival::Surv(survt, event1 == 1) ~
    factor(GEN_10) +                 # exposure: sense of belonging
    DHH_AGE +                        # age (linear; no spline)
    factor(DHH_MS) +                 # marital status
    factor(material_deprivation) +   # material deprivation
    CCC_031 +                        # asthma            (ref = No)
    CCC_051 +                        # arthritis         (ref = No)
    CCC_061 +                        # back problems     (ref = No)
    CCC_071 +                        # high blood pressure (ref = No)
    CCC_091 +                        # COPD              (ref = No)
    CCC_101 +                        # diabetes          (ref = No)
    CCC_121 +                        # heart disease     (ref = No)
    CCC_131 +                        # cancer            (ref = No)
    CCC_171 +                        # bowel disorder    (ref = No)
    factor(rural) +                  # rural living status
    ALWDWKY +                        # weekly alcohol intake
    CCC_280 +                        # mood disorder     (ref = No)
    CCC_290 +                        # anxiety disorder  (ref = No)
    factor(GEN_02B) +                # self-perceived mental health
    factor(HUPDPAD)                  # pain severity

  fit <- survival::coxph(simplified_formula, data = data)
  rms::vif(fit)
}

simplified_formula

# Print one VIF vector as a labelled table block.  Returns the per-row data
# frame invisibly.  Shared by the overall and per-stratum paths.
print_vif_block <- function(vifs, labels = NULL, threshold = 2.5) {
  clean <- function(nm) gsub("factor\\((.*?)\\)", "\\1 = ", nm)
  disp  <- vapply(names(vifs), function(nm) {
    if (!is.null(labels) && nm %in% names(labels)) labels[[nm]] else clean(nm)
  }, character(1L))

  lab_w <- max(nchar(disp)) + 2L

  cat(strrep(" ", lab_w), "VIF\n", sep = "")

  flagged <- character(0)
  for (i in seq_along(vifs)) {
    v <- vifs[[i]]
    cat(formatC(disp[i], width = lab_w, flag = "-"),
        sprintf("%.3f", v), if (v > threshold) "*" else "", "\n", sep = "")
    if (v > threshold) flagged <- c(flagged, disp[i])
  }

  cat("\n* Significantly collinear (VIF > ", threshold, ")", sep = "")
  if (length(flagged) > 0L)
    cat(" - flagged: ", paste(flagged, collapse = "; "), sep = "")
  cat("\n\n")

  data.frame(variable = disp, VIF = round(as.numeric(vifs), 3),
             row.names = NULL)
}

# Format the VIF vector as an appendix table.  `labels` is an optional named
# vector mapping coefficient names (as returned by rms::vif) to display labels;
# any coefficient without an entry is shown with its factor(...) wrapper cleaned.
#
# `strata_var` (optional): name of a column in `data`.  When supplied, the data
# are split on its levels and a separate VIF table is printed for each stratum
# (collinearity can differ by subgroup, e.g. by sex).  The returned data frame
# then carries an extra `stratum` column identifying the level.
build_vif_appendix <- function(data, labels = NULL, threshold = 2.5,
                               strata_var = NULL) {
  cat("\nAppendix - Multicollinearity assessment: variance inflation factors (VIFs)\n\n")

  if (is.null(strata_var)) {
    vifs <- calculate_simplified_vif(data)
    return(invisible(print_vif_block(vifs, labels, threshold)))
  }

  if (!strata_var %in% names(data))
    stop("strata_var '", strata_var, "' not found in data.")

  levs <- levels(factor(data[[strata_var]]))
  out  <- list()
  for (lv in levs) {
    cat("--- ", strata_var, " = ", lv, " ---\n", sep = "")
    sub  <- data[!is.na(data[[strata_var]]) & data[[strata_var]] == lv, , drop = FALSE]
    vifs <- calculate_simplified_vif(sub)
    blk  <- print_vif_block(vifs, labels, threshold)
    blk$stratum <- lv
    out[[lv]] <- blk
  }

  invisible(do.call(rbind, c(out, list(make.row.names = FALSE))))
}

# vif_tbl <- build_vif_appendix(completed_list[[1]])

#necessary labels: 
vif_labels <- c(
  "factor(GEN_10)2"                = "Belonging: somewhat strong",
  "factor(GEN_10)3"                = "Belonging: somewhat weak",
  "factor(GEN_10)4"                = "Belonging: very weak",
  "DHH_AGE"                        = "Age",
  "factor(DHH_MS)2"                = "Marital: widowed/sep/divorced",
  "factor(DHH_MS)3"                = "Marital: single",
  "factor(DHH_MS)4"                = "Marital: common-law/other",
  "factor(material_deprivation)2"  = "Deprivation Q2",
  "factor(material_deprivation)3"  = "Deprivation Q3",
  "factor(material_deprivation)4"  = "Deprivation Q4",
  "factor(material_deprivation)5"  = "Deprivation Q5 (most)",
  # Chronic conditions — releveled so No is the reference; coefficient = "Yes"
  "CCC_0311"                       = "Asthma",
  "CCC_0511"                       = "Arthritis",
  "CCC_0611"                       = "Back problems",
  "CCC_0711"                       = "High blood pressure",
  "CCC_0911"                       = "COPD",
  "CCC_1011"                       = "Diabetes",
  "CCC_1211"                       = "Heart disease",
  "CCC_1311"                       = "Cancer",
  "CCC_1711"                       = "Bowel disorder",
  "CCC_2801"                       = "Mood disorder",
  "CCC_2901"                       = "Anxiety disorder",
  # rural coded 1=Rural (ref), 2=Urban
  "factor(rural)2"                 = "Urban (vs rural)",
  "ALWDWKY"                        = "Weekly alcohol intake",
  "factor(GEN_02B)2"               = "Mental health: very good",
  "factor(GEN_02B)3"               = "Mental health: good",
  "factor(GEN_02B)4"               = "Mental health: fair",
  "factor(GEN_02B)5"               = "Mental health: poor",
  "factor(HUPDPAD)2"               = "Pain: mild",
  "factor(HUPDPAD)3"               = "Pain: moderate",
  "factor(HUPDPAD)4"               = "Pain: severe",
  "factor(HUPDPAD)5"               = "Pain: very severe"
)

vif_tbl <- build_vif_appendix(imp_set, labels = vif_labels)

# Stratified: one VIF table per level of a variable (e.g. sex).
# vif_tbl_by_sex <- build_vif_appendix(imp_set, labels = vif_labels,
#                                      strata_var = "DHH_SEX")
