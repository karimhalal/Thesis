library(mice)
library(magrittr)

source(here::here("R",""))

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

predictor_list<-c("bmi_adj", "DHH_AGE", "DHH_SEX", "DHH_OWN", "DHH_MS", "PACDEE", "CCC_290", "CCC_280", "rural", "material_deprivation", "EDUDR03", "SDCDCGT", "CCC_051", "CCC", "Surveycycle")
missing_imp_variables_binary<-c("CCC_171", "CCC_061")
impute_vars_cont<-c("ALWDWKY")
impute_vars_multi<-c("drgdvyac", "drgdvlac", "CMH_01L", "HUPDPAD", "INCDVRRS", "INCDVPR", "INCDRCA", "FSCDHFS2", "SMKDSTY")
impute_vars_binary<-c("CMH_01K")
interaction_vars<-list(
  AGE_X_BMI=c("BMI", "DHH_AGE"),
  DHH_SEX_X_CCC_290=
)
#' Imputes a dataset using MICE with manually specified variables and predictors
#'
#'
#' @param data data.frame containing harmonized CCHS data. \code{haven_labelled}
#'   columns are automatically converted to factors before MICE runs; tagged NAs
#'   become real \code{NA}.
#' @param ordered_factor_vars character vector of column names that should be
#'   treated as ordered factors. Applies to \code{haven_labelled}, \code{character},
#'   and plain \code{factor} columns.
#' @param impute_vars_cont character vector of continuous variable names to impute
#'   (MICE method: predictive mean matching)
#' @param impute_vars_binary character vector of binary variable names to impute
#'   (MICE method: logistic regression
#' @param impute_vars_multi character vector of multinomial/ordinal variable names
#'   to impute (MICE method: polynomial regression)
#' @param predictor_list named list where each name is a variable to impute and
#'   each element is a character vector of predictor variable names for that
#'   variable. Every variable across all three imputation vectors (and the
#'   \code{missing_imp_vars_*} vectors) must have an entry here.
#' @param missing_imp_vars_cont character vector of continuous predictor
#'   variables that are themselves missing. Imputed jointly with all other
#'   imputation targets in the same MICE call.
#' @param missing_imp_vars_binary character vector of binary predictor variables
#'   that are themselves missing (MICE method: logistic regression).
#' @param missing_imp_vars_multi character vector of multinomial predictor
#'   variables that are themselves missing (MICE method: polynomial regression).
#'   Default \code{character(0)}.
#' @param survival_time character name of the survival/censoring time column.
#'   When provided together with \code{event_indicator}, a Nelson-Aalen
#'   cumulative hazard estimate (\code{nelson_aalen_h}) is added to the dataset
#'   and appended to every variable's predictor list (White & Royston, 2009).
#' @param event_indicator character name of the event indicator column
#'   (1 = event, 0 = censored). Required when \code{survival_time} is given.
#' @param additional_imputation_predictors character vector of column names
#'   (e.g. \code{"event_type"}) to append to every variable's predictor list.
#' @param interaction_vars named list where each name is an interaction variable
#'   already present in \code{data} and each element is a length-2 character
#'   vector naming the base components. The interaction variable is passively
#'   re-derived from mean-centred base components at every MICE iteration, so
#'   the interaction is always consistent with its imputed parts. Example:
#'   \code{list(DHH_AGE_C_X_HWTDBMI_C = c("DHH_AGE", "HWTDBMI"))}.
#' @param skewness_percentile numeric in (0, 1). When supplied, continuous
#'   imputation variables whose absolute Pearson moment skewness exceeds
#'   \code{skewness_threshold} are truncated at this percentile before MICE.
#' @param skewness_threshold numeric absolute skewness threshold above which
#'   truncation is applied (default 1).
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
  impute_vars_cont,
  impute_vars_binary,
  impute_vars_multi,
  predictor_list,
  ordered_factor_vars              = character(0),
  missing_imp_vars_cont            = character(0),
  missing_imp_vars_binary          = character(0),
  missing_imp_vars_multi           = character(0),
  survival_time                    = NULL,
  event_indicator                  = NULL,
  additional_imputation_predictors = character(0),
  interaction_vars                 = list(),
  skewness_percentile              = NULL,
  skewness_threshold               = 1,
  m     = 1,
  maxit = 1
) {
  # 0. Coerce all columns to canonical R types before any downstream logic runs
  data <- .coerce_cchs_types(data, ordered_factor_vars)

  # 1. Nelson-Aalen: add cumulative hazard as auxiliary variable (White & Royston 2009)
  if (!is.null(survival_time) && !is.null(event_indicator)) {
    data <- add_nelson_aalen_h(data, survival_time, event_indicator)
    for (v in names(predictor_list)) {
      predictor_list[[v]] <- union(predictor_list[[v]], "nelson_aalen_h")
    }
  }

  # 2. Add event_type (or any additional survival predictors) to all predictor lists
  if (length(additional_imputation_predictors) > 0) {
    for (v in names(predictor_list)) {
      predictor_list[[v]] <- union(predictor_list[[v]], additional_imputation_predictors)
    }
  }

  # 3. Pre-imputation skewness check and percentile truncation
  if (!is.null(skewness_percentile)) {
    data <- .truncate_skewed_vars(
      data,
      c(missing_imp_vars_cont, impute_vars_cont),
      skewness_percentile,
      skewness_threshold
    )
  }

  # 4. Centering constants and passive MICE formulas for interaction terms
  passive_formulas <- character(0)
  if (length(interaction_vars) > 0) {
    centering_constants <- .compute_centering_constants(data, interaction_vars)
    passive_formulas    <- .build_passive_formulas(interaction_vars, centering_constants)
  }

  all_imp_vars         <- c(impute_vars_cont, impute_vars_binary, impute_vars_multi)
  all_missing_imp_vars <- c(missing_imp_vars_cont, missing_imp_vars_binary, missing_imp_vars_multi)
  all_vars             <- c(all_missing_imp_vars, all_imp_vars)

  .validate_imputation_inputs(data, all_vars, predictor_list)

  imp_result <- .run_mice_manual(
    data, all_vars, predictor_list,
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
#' @param predictor_list named list of predictor vectors
.validate_imputation_inputs <- function(data, all_imp_vars, predictor_list) {
  col_names <- colnames(data)

  missing_from_data <- setdiff(all_imp_vars, col_names)
  if (length(missing_from_data) > 0) {
    stop(sprintf(
      "Imputation variable(s) not found in data: %s",
      paste(missing_from_data, collapse = ", ")
    ))
  }

  missing_from_list <- setdiff(all_imp_vars, names(predictor_list))
  if (length(missing_from_list) > 0) {
    stop(sprintf(
      "No predictor entry in predictor_list for variable(s): %s",
      paste(missing_from_list, collapse = ", ")
    ))
  }
}


#' Check continuous predictors for skewness and truncate at a given percentile
#'
#' Truncation is only applied to variables whose absolute Pearson moment
#' skewness coefficient exceeds \code{threshold}.
#'
#' @param data data.frame
#' @param vars character vector of continuous variable names to check
#' @param percentile numeric in (0, 1) — upper truncation quantile
#' @param threshold numeric — absolute skewness threshold above which to truncate
#' @return data.frame with skewed variables truncated at \code{percentile}
.truncate_skewed_vars <- function(data, vars, percentile, threshold = 1) {
  for (v in intersect(vars, colnames(data))) {
    vals <- data[[v]]
    obs  <- vals[!is.na(vals)]
    if (length(obs) < 3L) next

    m        <- mean(obs)
    s        <- sd(obs)
    if (s == 0) next
    skewness <- mean(((obs - m) / s)^3)

    if (abs(skewness) > threshold) {
      cutoff    <- quantile(vals, probs = percentile, na.rm = TRUE)
      data[[v]] <- pmin(vals, cutoff)
    }
  }
  data
}


#' Compute available-case means for interaction term base variables
#'
#' Means are computed from pre-imputation data (available cases only) and
#' inlined into passive MICE formulas as centering constants.
#'
#' @param data data.frame (pre-imputation)
#' @param interaction_vars named list: interaction variable -> length-2 character
#'   vector of base component column names
#' @return named numeric vector of available-case means, one per unique base variable
.compute_centering_constants <- function(data, interaction_vars) {
  base_vars <- unique(unlist(interaction_vars))
  vapply(base_vars, function(v) mean(data[[v]], na.rm = TRUE), numeric(1))
}


#' Build passive MICE formula strings for interaction variables
#'
#' Each interaction variable is re-derived from its two mean-centred base
#' components at every MICE iteration. Centering constants from
#' \code{.compute_centering_constants()} are inlined into the formula string.
#'
#' @param interaction_vars named list: interaction variable -> length-2 character
#'   vector of base component names
#' @param centering_constants named numeric vector (from
#'   \code{.compute_centering_constants()})
#' @return named character vector of passive formula strings (entries starting
#'   with \code{~}) suitable for direct assignment into a MICE method vector
.build_passive_formulas <- function(interaction_vars, centering_constants) {
  nms      <- names(interaction_vars)
  formulas <- setNames(character(length(nms)), nms)

  for (int_var in nms) {
    bases <- interaction_vars[[int_var]]
    if (length(bases) != 2L) {
      stop(sprintf(
        "interaction_vars entry '%s' must name exactly 2 base components",
        int_var
      ))
    }
    v1 <- bases[1]; v2 <- bases[2]
    c1 <- round(centering_constants[[v1]], 1)
    c2 <- round(centering_constants[[v2]], 1)

    formulas[int_var] <- sprintf("~ I((%s - %s) * (%s - %s))", v1, c1, v2, c2)
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
      f <- haven::as_factor(x, levels = "labels")
      data[[col]] <- if (col %in% ordered_factor_vars)
        factor(f, levels = levels(f), ordered = TRUE) else f

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
#' @param predictor_list named list of per-variable predictor vectors
#' @param passive_vars character vector of passive interaction variable names
#'   to include even if not in \code{all_imp_vars} or \code{predictor_list}
#' @return named list:
#'   \code{data} — prepared data.frame passed to MICE;
#'   \code{extra_cols} — data.frame of columns excluded from imputation
.prepare_data_for_imputation_manual <- function(data, all_imp_vars, predictor_list,
                                                passive_vars = character(0)) {
  all_predictor_vars <- unique(unlist(predictor_list))
  vars_to_keep       <- intersect(
    unique(c(all_imp_vars, all_predictor_vars, passive_vars)),
    colnames(data)
  )

  prepared_data <- data[, vars_to_keep, drop = FALSE]

  factor_vars <- colnames(prepared_data)[sapply(prepared_data, is.factor)]
  if (length(factor_vars) > 0) {
    prepared_data <- prepared_data %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(factor_vars), droplevels))
  }

  list(
    data       = prepared_data,
    extra_cols = data[, setdiff(colnames(data), vars_to_keep), drop = FALSE]
  )
}


#' Run MICE with auto-determined methods and predictor matrix
#'
#' Applies CCHS relationships before imputation to zero out conditional
#' downstream variables for rows with observed upstream "No" responses,
#' preventing MICE from incorrectly imputing those cells.
#'
#' @param data data.frame
#' @param all_imp_vars character vector of imputation target variables
#' @param predictor_list named list of per-variable predictor vectors
#' @param passive_formulas named character vector of passive formula strings
#'   for interaction variables (from \code{.build_passive_formulas()})
#' @param m integer number of imputations
#' @param maxit integer number of iterations
#' @param passive_vars character vector of passive interaction variable names
#'   to include in MICE data even if absent from \code{all_imp_vars}
#' @return named list: \code{mice_result} (mids object), \code{data} (completed data.frame)
.run_mice_manual <- function(data, all_imp_vars, predictor_list,
                              passive_formulas = character(0),
                              m, maxit,
                              passive_vars = character(0)) {
  # Pre-imputation: zero out conditional variables for observed gate responses
  data <- .apply_cchs_relationships(data)

  prepared  <- .prepare_data_for_imputation_manual(data, all_imp_vars, predictor_list,
                                                    passive_vars)

  # Build method on the prepared dataset so MICE auto-assigns methods per column type,
  # then overlay passive formulas for interaction variables.
  method <- mice::make.method(prepared$data)
  for (v in intersect(names(passive_formulas), names(method))) {
    if (nzchar(passive_formulas[v])) method[v] <- passive_formulas[v]
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
#' Called both before and after imputation:
#'
#'     Before: zeros out conditional downstream variables for rows where
#'     the upstream gate variable is already observed as "No", so MICE does
#'     not treat those cells as missing.
#'
#'     After: corrects any rows where the gate variable was imputed as "No",
#'     ensuring downstream variables remain consistent.
#' }
#'
#' See {Skill_files/special_imputations.md} for full documentation of
#' each relationship and the rationale for each zeroing/NA decision.
#'
#' @param data data.frame
#' @return data.frame with conditional relationships enforced
.apply_cchs_relationships <- function(data) {
  col_names <- colnames(data)

  #Illicit drug use
  #drgdvyac (yearly use of illicit drugs) must be 2 (no) if when
  #drgdvyac (lifetime use of illicit drugs) is recorded as 2 (no)
  if(all(c("drgdvyac", "drgdvlac")%in% col_names)){
    data<-data%>%
      mutate(drgdvyac= case_when(
             drgdvlac==2~2,
             TRUE~drgdvyac
      )
  )
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


