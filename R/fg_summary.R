#'
#' @param model_full      A coxph object for the full Fine-Gray model
#' @param model_reduced   A coxph object for the reduced Fine-Gray model
#' @param labels          Optional named character vector mapping predictor
#'                        column names to display labels, e.g.
#'                        c(DHH_AGE_c_rcs1 = "Age (spline 1)"). Any variable not
#'                        present falls back to its raw column name.
#' @param model_name      Optional caption string (e.g. "Males")
#' @return A kableExtra / knitr_kable table object
create_regression_parameters_table <- function(model_full, model_reduced,
                                               labels = NULL, model_name = NULL) {

  fmt_shr <- function(shr, lo, hi) {
    sprintf("%.2f (%.2f, %.2f)", shr, lo, hi)
  }

  cn_full    <- names(coef(model_full))
  shr_full   <- exp(coef(model_full))
  ci_full    <- exp(confint(model_full))

  cn_reduced   <- names(coef(model_reduced))
  shr_reduced  <- exp(coef(model_reduced))
  ci_reduced   <- exp(confint(model_reduced))

  all_vars <- union(cn_full, cn_reduced)

  get_label <- function(v) {
    lbl <- if (!is.null(labels) && v %in% names(labels)) labels[[v]] else NA
    if (!is.na(lbl) && nzchar(lbl)) lbl else v
  }

  tbl <- data.frame(
    variable    = all_vars,
    description = vapply(all_vars, get_label, character(1)),
    full_shr    = vapply(all_vars, function(v) {
      if (v %in% cn_full)
        fmt_shr(shr_full[v], ci_full[v, 1], ci_full[v, 2])
      else "\u2014"
    }, character(1)),
    reduced_shr = vapply(all_vars, function(v) {
      if (v %in% cn_reduced)
        fmt_shr(shr_reduced[v], ci_reduced[v, 1], ci_reduced[v, 2])
      else "\u2014"
    }, character(1)),
    stringsAsFactors = FALSE
  )

  cap <- if (!is.null(model_name)) {
    paste("Subdistribution hazard ratios \u2014", model_name)
  } else {
    "Subdistribution hazard ratios"
  }

  colnames(tbl) <- c("Variable", "Description", "Full model", "Reduced model")

  footnote_txt <- paste(
    "Abbreviations: SHR, subdistribution hazard ratio; CI, confidence interval.",
    "\u2014 = predictor not selected in reduced model."
  )

  tbl |>
    kableExtra::kbl(
      caption = cap,
      align   = c("l", "l", "r", "r"),
      booktabs = TRUE,
      escape  = TRUE,
      row.names = FALSE
    ) |>
    kableExtra::kable_styling(
      latex_options = c("hold_position"),
      full_width    = FALSE
    ) |>
    kableExtra::add_header_above(
      c(" " = 2, "SHR (95% CI)" = 2),
      bold = TRUE
    ) |>
    kableExtra::row_spec(0, bold = TRUE) |>
    kableExtra::footnote(
      general          = footnote_txt,
      general_title    = "",
      footnote_as_chunk = TRUE,
      escape           = FALSE
    )
}
