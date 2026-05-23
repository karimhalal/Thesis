library(mice)
library(magrittr)

#' Add Nelson-Aalen cumulative hazard estimate as an auxiliary imputation variable
#'
#' Implements the White & Royston (2009) recommendation: including the
#' Nelson-Aalen estimator as a predictor in imputation models for survival data
#' preserves the association between imputed covariates and the censored outcome.
#'
#' @param data data.frame containing the survival variables
#' @param survival_time character name of the column holding observed survival/
#'   censoring times
#' @param event_indicator character name of the event indicator column
#'   (1 = event occurred, 0 = censored)
#' @return \code{data} with an additional column \code{nelson_aalen_h} containing
#'   the Nelson-Aalen cumulative hazard at each observation's survival time
add_nelson_aalen_h <- function(
    data,
    time_var         = "time_to_event",
    event_var        = "event_type",
    event_of_interest = 1L
) {
  event_ind <- as.integer(data[[event_var]] == event_of_interest)
  fit <- survival::survfit(survival::Surv(data[[time_var]], event_ind) ~ 1, type = "fh")
  # Right-continuous step function: H(t) = 0 before first event, then jumps
  H_fn <- stepfun(fit$time, c(0, fit$cumhaz))
  data$nelson_aalen_h <- H_fn(data[[time_var]])
  data
}


#' Imputes a dataset using MICE with manually specified variables and predictors
#'
#'
#' @param data data.frame containing harmonized CCHS data. \code{haven_labelled}
#'   columns are automatically converted to factors before MICE runs; tagged NAs
#'   become real \code{NA}.
#' @param ordered_factor_vars character vector of column names that should be
#'   treated as ordered factors. Applies to \code{haven_labelled}, \code{character},
#'   and plain \code{factor} columns.
#' @param impute_vars character vector of variable names to impute. MICE method
#'   is auto-detected from each column's R class after type coercion.
#' @param missing_imp_vars character vector of predictor variables that are
#'   themselves missing. Imputed jointly with \code{impute_vars} in the same
#'   MICE call. MICE method auto-detected from column class.
#' @param additional_imputation_predictors character vector of column names
#'   already present in \code{data} to include as predictors for all imputed
#'   variables (e.g. \code{"nelson_aalen_h"}, \code{"event_type"}).
#' @param interaction_vars named list where each name is an interaction variable
#'   already present in \code{data} and each element is a length-2 character
#'   vector naming the base components. The interaction variable is passively
#'   re-derived from mean-centred base components at every MICE iteration, so
#'   the interaction is always consistent with its imputed parts. Example:
#'   \code{list(DHH_AGE_C_X_HWTDBMI_C = c("DHH_AGE", "HWTDBMI"))}.
#' @param m integer number of multiple imputations (default=1)
#' @param maxit integer number of MICE iterations (default=1)
#' @return named list with the following items:
#' \describe{
#'   \item{data}{data.frame with imputed values and CCHS conditional
#'     relationships applied (e.g. downstream counts zeroed where the upstream
#'     gate question was answered "No")}
#'   \item{imp_data}{data.frame with imputed values before conditional
#'     relationship fixes are applied}
#'   \item{mice_result}{the \code{mids} object returned by \code{mice::mice()}}
#' }
impute_data_manual <- function(
  data,
  impute_vars,
  ordered_factor_vars              = character(0),
  missing_imp_vars                 = character(0),
  additional_imputation_predictors = character(0),
  interaction_vars                 = list(),
  m     = 1,
  maxit = 1
) {
  # 0. Force ALWDWKY to plain numeric before type coercion.
  # In some CCHS cycles ALWDWKY carries value labels (e.g. "0 drinks" = 0,
  # "1 drink" = 1 …), which causes .coerce_cchs_types to convert it to a
  # factor.  MICE then uses polyreg on a variable with potentially hundreds of
  # levels and fails with all-NA output.  Stripping labels here keeps it
  # continuous so PMM is used instead.
  if ("ALWDWKY" %in% colnames(data) &&
      inherits(data[["ALWDWKY"]], "haven_labelled")) {
    data[["ALWDWKY"]] <- as.numeric(haven::zap_labels(data[["ALWDWKY"]]))
  }

  # 0b. Coerce all columns to canonical R types before any downstream logic runs
  data <- .coerce_cchs_types(data, ordered_factor_vars)

  # 0c. Pre-imputation structural zeros: rows where ALCDTTM is *observed* as 3
  # (non-drinker) have ALWDWKY=NA due to the CCHS skip pattern (996 → na_a()).
  # PMM has no donor cases with ALCDTTM=3 + observed ALWDWKY, so imputing these
  # rows produces all-NA. Pre-fill to 0 for confirmed non-drinkers so MICE only
  # imputes ALWDWKY for rows with genuine random missingness.
  # Rows where ALCDTTM itself is NA are left alone — .apply_cchs_relationships
  # handles any that are later imputed as 3.
  if (all(c("ALCDTTM", "ALWDWKY") %in% colnames(data))) {
    alcdttm_int  <- suppressWarnings(as.integer(data[["ALCDTTM"]]))
    rows_to_fix  <- !is.na(alcdttm_int) & alcdttm_int == 3L & is.na(data[["ALWDWKY"]])
    data[["ALWDWKY"]][rows_to_fix] <- 0
  }

  # 1. Centering constants and passive MICE formulas for interaction terms
  passive_formulas <- character(0)
  if (length(interaction_vars) > 0) {
    centering_constants <- .compute_centering_constants(data, interaction_vars)
    passive_formulas    <- .build_passive_formulas(interaction_vars, centering_constants)
  }

  all_vars <- c(missing_imp_vars, impute_vars)

  .validate_imputation_inputs(data, all_vars)

  imp_result <- .run_mice_manual(
    data, all_vars,
    additional_imputation_predictors,
    passive_formulas,
    m, maxit,
    passive_vars = names(interaction_vars)
  )

  fixed_data <- .apply_cchs_relationships(imp_result$data)

  list(
    mice_result = imp_result$mice_result,
    imp_data    = imp_result$data,
    data        = fixed_data
  )
}

#' @description
#' @param data data.frame
#' @param all_imp_vars character vector of all variables to impute
.validate_imputation_inputs <- function(data, all_imp_vars) {
  missing_from_data <- setdiff(all_imp_vars, colnames(data))
  if (length(missing_from_data) > 0) {
    stop(sprintf(
      "Imputation variable(s) not found in data: %s",
      paste(missing_from_data, collapse = ", ")
    ))
  }
}


#' Compute available-case means for interaction term base variables
#'
#' Only computes constants for centered interactions (list entries with
#' \code{center = TRUE}). Raw interactions (character vector entries) are
#' skipped. Dummy-coded base variables (specified via a \code{dummy_vars}
#' sub-list) are evaluated as \code{mean(as.numeric(source_col == value))}.
#'
#' @param data data.frame (pre-imputation)
#' @param interaction_vars named list — see \code{impute_data_manual()} for
#'   the full format description
#' @return named list keyed by base variable name; each entry contains
#'   \code{center_value} and (optionally) \code{dummy} info
.compute_centering_constants <- function(data, interaction_vars) {
  centering_info <- list()
  for (int_var in names(interaction_vars)) {
    entry <- interaction_vars[[int_var]]
    if (is.character(entry)) next
    center <- entry$center
    if (is.null(center) || isFALSE(center) || identical(center, c(FALSE, FALSE))) next
    bases        <- entry$vars
    dummy_vars   <- entry$dummy_vars
    center_flags <- if (isTRUE(center)) c(TRUE, TRUE) else center
    for (i in seq_along(bases)) {
      if (!center_flags[i]) next
      v <- bases[i]
      if (v %in% names(centering_info)) next
      if (!is.null(dummy_vars) && v %in% names(dummy_vars)) {
        dv   <- dummy_vars[[v]]
        vals <- as.numeric(as.character(data[[dv$source]]) == dv$value)
      } else {
        vals <- as.numeric(data[[v]])
      }
      centering_info[[v]] <- list(
        center_value = mean(vals, na.rm = TRUE),
        dummy        = if (!is.null(dummy_vars) && v %in% names(dummy_vars)) dummy_vars[[v]] else NULL
      )
    }
  }
  centering_info
}


#' Build passive MICE formula strings for interaction variables
#'
#' Three centering modes, controlled by the \code{center} field of each list
#' entry (character vector entries are always raw):
#' \itemize{
#'   \item \code{center = FALSE} or absent, or a plain character vector:
#'         raw product \code{~ I(var1 * var2)}.
#'   \item \code{center = TRUE}: both base variables are mean-centred.
#'   \item \code{center = c(TRUE, FALSE)} (or \code{c(FALSE, TRUE)}):
#'         only the flagged base variable is centred; the other enters raw.
#' }
#' Dummy-coded base variables use \code{as.numeric(as.character(source) == "value")}
#' syntax when specified via \code{dummy_vars}.
#'
#' @param interaction_vars named list — see \code{impute_data_manual()}
#' @param centering_constants named list from \code{.compute_centering_constants()}
#' @return named character vector of passive formula strings suitable for
#'   direct assignment into a MICE method vector
.build_passive_formulas <- function(interaction_vars, centering_constants = list()) {
  nms      <- names(interaction_vars)
  formulas <- setNames(character(length(nms)), nms)

  for (int_var in nms) {
    entry <- interaction_vars[[int_var]]

    if (is.character(entry)) {
      if (length(entry) == 1L && startsWith(trimws(entry), "~")) {
        formulas[int_var] <- entry
        next
      }
      if (length(entry) != 2L)
        stop(sprintf("interaction_vars entry '%s' must name exactly 2 base components", int_var))
      formulas[int_var] <- sprintf("~ I(%s * %s)", entry[1], entry[2])
      next
    }

    bases <- entry$vars
    if (length(bases) != 2L)
      stop(sprintf("interaction_vars entry '%s' must name exactly 2 base components", int_var))

    center <- entry$center
    center_flags <- if (isTRUE(center)) c(TRUE, TRUE)
                    else if (is.logical(center) && length(center) == 2L) center
                    else c(FALSE, FALSE)

    exprs <- sapply(seq_along(bases), function(i) {
      v <- bases[i]
      if (!center_flags[i]) return(v)
      ci   <- centering_constants[[v]]
      cval <- ci$center_value
      if (!is.null(ci$dummy)) {
        dv <- ci$dummy
        sprintf("(as.numeric(as.character(%s) == \"%s\") - %s)", dv$source, dv$value, cval)
      } else {
        sprintf("(%s - %s)", v, cval)
      }
    })
    formulas[int_var] <- sprintf("~ I(%s * %s)", exprs[1], exprs[2])
  }

  formulas
}


# ── Data preparation ──────────────────────────────────────────────────────────

#' Coerce all columns to canonical R types for MICE
#'
#' Converts every column in \code{data} according to its current class:
#' \itemize{
#'   \item \code{haven_labelled} → \code{factor} (levels ordered by numeric
#'     code ascending, via \code{haven::as_factor()}); promoted to
#'     \code{ordered} if the column name is in \code{ordered_factor_vars}.
#'   \item \code{character} → \code{factor} (levels alphabetically sorted);
#'     promoted to \code{ordered} if in \code{ordered_factor_vars}.
#'   \item Unordered \code{factor} in \code{ordered_factor_vars} → \code{ordered}.
#'   \item Plain \code{double} → \code{numeric} (strips any haven attributes).
#'   \item Plain \code{integer} → \code{integer} (strips any haven attributes).
#'   \item Everything else is left unchanged.
#' }
#'
#' @param data data.frame
#' @param ordered_factor_vars character vector of column names to promote to
#'   ordered factor
#' @return data.frame with all columns coerced to canonical types
.coerce_cchs_types <- function(data, ordered_factor_vars = character(0)) {
  for (col in colnames(data)) {
    x <- data[[col]]

    if (inherits(x, "haven_labelled")) {
      labs <- attr(x, "labels")
      # A column is categorical only if it has at least one non-tagged-NA value label.
      # Continuous haven_labelled vars have NULL labels or only tagged-NA sentinel labels.
      has_value_labels <- !is.null(labs) &&
        any(!haven::is_tagged_na(labs))
      if (has_value_labels) {
        real_labs <- labs[!haven::is_tagged_na(labs)]
        if (!is.double(x) || !is.double(real_labs)) {
          x <- haven::labelled(
            as.double(unclass(x)),
            setNames(as.double(labs), names(labs)),
            label = attr(x, "label", exact = TRUE)
          )
        }
        f <- haven::as_factor(x, levels = "labels")
        data[[col]] <- if (col %in% ordered_factor_vars)
          factor(f, levels = levels(f), ordered = TRUE) else f
      } else {
        # No real value labels → continuous; strip class, tagged NAs stay as NA.
        data[[col]] <- haven::zap_labels(x)
      }

    } else if (is.character(x)) {
      lvls <- if (col %in% ordered_factor_vars) sort(unique(x[!is.na(x)])) else NULL
      data[[col]] <- factor(x, levels = lvls, ordered = col %in% ordered_factor_vars)

    } else if (is.factor(x) && col %in% ordered_factor_vars && !is.ordered(x)) {
      data[[col]] <- factor(x, levels = levels(x), ordered = TRUE)

    } else if (is.double(x)) {
      data[[col]] <- as.numeric(x)

    } else if (is.integer(x)) {
      data[[col]] <- as.integer(x)
    }
  }
  data
}


#' Prepare data for MICE
#'
#' Retains only columns that are either imputation targets, appear as
#' predictors, or are passive interaction variables. Converts tagged-NA factor
#' levels to real NAs and drops the now-empty levels.
#'
#' @param data data.frame
#' @param all_imp_vars character vector of imputation target variables
#' @param additional_predictors character vector of extra columns to include
#'   (e.g. nelson_aalen_h, event_type) — all are passed to MICE as predictors
#' @param passive_vars character vector of passive interaction variable names
#'   to include even if not in \code{all_imp_vars}
#' @return named list:
#'   \code{data} — prepared data.frame passed to MICE;
#'   \code{extra_cols} — data.frame of columns excluded from imputation
.prepare_data_for_imputation_manual <- function(data, all_imp_vars,
                                                additional_predictors = character(0),
                                                passive_vars = character(0)) {
  vars_to_keep <- intersect(
    unique(c(all_imp_vars, additional_predictors, passive_vars)),
    colnames(data)
  )

  prepared_data <- data[, vars_to_keep, drop = FALSE]

  factor_vars <- colnames(prepared_data)[sapply(prepared_data, is.factor)]
  if (length(factor_vars) > 0) {
    prepared_data <- prepared_data %>%
      dplyr::mutate(dplyr::across(
        dplyr::all_of(factor_vars),
        ~ dplyr::if_else(.x %in% c("NA(a)", "NA(b)", "NA(c)"), NA, .x)
      )) %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(factor_vars), droplevels))
  }

  list(
    data       = prepared_data,
    extra_cols = data[, setdiff(colnames(data), vars_to_keep), drop = FALSE]
  )
}


#' Run MICE with auto-determined methods and predictor matrix
#'
#' @param data data.frame
#' @param all_imp_vars character vector of imputation target variables
#' @param additional_predictors character vector of extra predictor columns
#'   (e.g. nelson_aalen_h, event_type) to include in the MICE dataset.
#'   These are predictor-only — they are never imputed.
#' @param passive_formulas named character vector of passive formula strings
#'   for interaction variables (from \code{.build_passive_formulas()})
#' @param m integer number of imputations
#' @param maxit integer number of iterations
#' @param passive_vars character vector of passive interaction variable names
#'   to include in MICE data even if absent from \code{all_imp_vars}
#' @return named list: \code{mice_result} (mids object), \code{data} (completed data.frame)
.run_mice_manual <- function(data, all_imp_vars,
                              additional_predictors = character(0),
                              passive_formulas = character(0),
                              m, maxit,
                              passive_vars = character(0)) {
  prepared <- .prepare_data_for_imputation_manual(data, all_imp_vars,
                                                   additional_predictors,
                                                   passive_vars)

  # Build method on the prepared dataset so MICE auto-assigns methods per column type,
  # then overlay passive formulas for interaction variables.
  method <- mice::make.method(prepared$data)
  for (v in intersect(names(passive_formulas), names(method))) {
    if (nzchar(passive_formulas[v])) method[v] <- passive_formulas[v]
  }

  # Set passive interaction columns to all-NA in the prepared data.
  # Pre-computed interaction columns arrive with partial NAs (wherever a base
  # variable is missing). When used as predictors, MICE mean-imputes these NAs
  # to near-constants, collapsing every design matrix to near-singularity and
  # silently producing all-NA imputed values.  Setting them to all-NA replicates
  # the working behaviour of the original impute_data(): MICE's internal fitting
  # routines compute NaN column means for all-NA predictors and drop them from
  # every model automatically.  The passive formula in `method` still re-derives
  # each column from its (imputed) base variables at every iteration, so the
  # completed dataset has consistent interaction terms.
  for (v in intersect(passive_vars, colnames(prepared$data))) {
    prepared$data[[v]] <- NA_real_
  }

  imp_result <- mice::mice(
    prepared$data,
    m            = m,
    maxit        = maxit,
    method       = method,
    nnet.MaxNWts = 10000,
    printFlag    = FALSE
  )

  imp_data  <- mice::complete(imp_result)
  full_data <- cbind(imp_data, prepared$extra_cols)

  list(
    mice_result = imp_result,
    data        = full_data
  )
}

# cchs conditional relationship

#' Apply hardcoded CCHS conditional variable relationships
#'
#' Called after imputation to correct any rows where a gate variable was
#' imputed as "No", ensuring downstream variables remain consistent.
#'
#' See {Skill_files/special_imputations.md} for full documentation of
#' each relationship and the rationale for each zeroing/NA decision.
#'
#' @param data data.frame
#' @return data.frame with conditional relationships enforced
.apply_cchs_relationships <- function(data) {
  col_names <- colnames(data)

  # Smoking: pack years
  # pack_years must be 0 for never-smokers (SMKDSTY_cat5 == 5).
  if (all(c("pack_years", "SMKDSTY_cat5") %in% col_names)) {
    smk_vals <- as.character(haven::zap_labels(data$SMKDSTY_cat5))
    data <- data %>%
      dplyr::mutate(pack_years = dplyr::if_else(smk_vals == "5", 0, pack_years))
  }

  #Illicit drug use
  #drgdvyac (yearly use of illicit drugs) must be 2 (no) if when
  #drgdvyac (lifetime use of illicit drugs) is recorded as 2 (no)
  if(all(c("drgdvyac", "drgdvlac")%in% col_names)){
    data <- data %>%
      dplyr::mutate(drgdvyac = dplyr::case_when(
        drgdvlac == 2 ~ as(2, class(drgdvyac)),
        TRUE ~ drgdvyac
      ))
  }
  # Mental Health cosults: number of times
  # CMH_01L (number of consultations last year, continuous) must be 0 when
  # CMH_01K == 2 ("No, did not consult a mental health professional")
  if (all(c("CMH_01K", "CMH_01L") %in% col_names)) {
    data <- data %>%
      dplyr::mutate(CMH_01L = dplyr::if_else(
        as.integer(CMH_01K) == 2L, 0, CMH_01L
      ))
  }

  # Alcohol: weekly intake
  # ALWDWKY (total drinks last week, continuous) must be 0 when
  # ALCDTTM == 3 ("Did not drink in the past 12 months")
  if (all(c("ALCDTTM", "ALWDWKY") %in% col_names)) {
    data <- data %>%
      dplyr::mutate(ALWDWKY = dplyr::if_else(
        as.integer(ALCDTTM) == 3L, 0, ALWDWKY
      ))
  }

  # Alcohol: daily intake
  # ALW_2A1–ALW_2A7 (drinks each day of the week) must be 0 when
  # ALW_1 == 2 ("No, did not drink in the past week")
  daily_drink_vars   <- c("ALW_2A1", "ALW_2A2", "ALW_2A3", "ALW_2A4",
                           "ALW_2A5", "ALW_2A6", "ALW_2A7")
  present_drink_vars <- intersect(daily_drink_vars, col_names)
  if ("ALW_1" %in% col_names && length(present_drink_vars) > 0) {
    data <- data %>%
      dplyr::mutate(dplyr::across(
        dplyr::all_of(present_drink_vars),
        ~ dplyr::if_else(as.integer(ALW_1) == 2L, 0, .x)
      ))
  }

  # Smoking: current daily smoker variables
  # SMK_203 (age started smoking daily) and SMK_204 (cigs/day, current daily)
  # are only applicable for current daily smokers (SMKDSTY == 1).
  # Set to NA_real_ for all other smoker types so MICE does not impute them.
  daily_smk_vars    <- c("SMK_203", "SMK_204")
  present_daily_smk <- intersect(daily_smk_vars, col_names)
  if ("SMKDSTY" %in% col_names && length(present_daily_smk) > 0) {
    data <- data %>%
      dplyr::mutate(dplyr::across(
        dplyr::all_of(present_daily_smk),
        ~ dplyr::if_else(as.integer(SMKDSTY) != 1L, NA_real_, .x)
      ))
  }

  # Smoking status
  # SMK_05B (cigs/day, occasional) is only applicable for current occasional
  # (SMKDSTY == 2) or always-occasional (SMKDSTY == 3) smokers.
  if (all(c("SMKDSTY", "SMK_05B") %in% col_names)) {
    data <- data %>%
      dplyr::mutate(SMK_05B = dplyr::if_else(
        !as.integer(SMKDSTY) %in% c(2L, 3L), NA_real_, SMK_05B
      ))
  }

  # ── Smoking: former daily smoker variables ────────────────────────────────
  # SMK_207, SMK_208, SMK_09A, SMK_09C, SMKDSTP are only applicable for
  # former daily smokers (SMKDSTY == 4).
  former_daily_vars    <- c("SMK_207", "SMK_208", "SMK_09A", "SMK_09C", "SMKDSTP")
  present_former_daily <- intersect(former_daily_vars, col_names)
  if ("SMKDSTY" %in% col_names && length(present_former_daily) > 0) {
    data <- data %>%
      dplyr::mutate(dplyr::across(
        dplyr::all_of(present_former_daily),
        ~ dplyr::if_else(as.integer(SMKDSTY) != 4L, NA_real_, .x)
      ))
  }

  # ── Smoking: never-daily / former occasional smoker variable ─────────────
  # SMK_06A (stopped smoking never-daily — when) is only applicable for
  # always-occasional (SMKDSTY == 3) or former occasional (SMKDSTY == 5)
  # smokers.
  if (all(c("SMKDSTY", "SMK_06A") %in% col_names)) {
    data <- data %>%
      dplyr::mutate(SMK_06A = dplyr::if_else(
        !as.integer(SMKDSTY) %in% c(3L, 5L), NA, SMK_06A
      ))
  }

  data
}


