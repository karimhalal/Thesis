library(mice)
library(magrittr)


predictor_list<-c("bmi_adj", "DHH_AGE", "DHH_SEX", "DHH_OWN", "PACDEE", "CCC_290", "CCC_280", "rural", "material deprivation", "EDUDR03", "SDCDCGT", "CCC_051")
missing_imp_variables_binary<-c("CCC_171", "CCC_061")
impute_vars_cont<-c("ALWDWKY")
impute_vars_multi<-c("drgdvyac", "drgdvlac", "CMH_01L", "HUPDPAD", "INCDVRRS", "INCDVPR", "INCDRCA", "FSCDHFS2")
impute_vars_binary<-c("CMH_01K")
#' Imputes a dataset using MICE with manually specified variables and predictors
#'
#'
#' @param data data.frame containing harmonized CCHS data. Factor columns may
#'   contain tagged-NA levels ("NA(a)", "NA(b)", "NA(c)"); these are converted
#'   to real \code{NA} internally before MICE is called.
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
#'   variables that are themselves missing. These are imputed jointly with the
#'   main variables but will be performed first in each MICE iteration and then will be
#'.  used to impute the final predictor variables
#' @param missing_imp_vars_binary character vector of binary predictor variables
#'   that are themselves missing (MICE method: logistic regression). 
#' @param missing_imp_vars_multi character vector of multinomial predictor
#'   variables that are themselves missing (MICE method: polynomial regression).
#'   Default \code{character(0)}.
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
  missing_imp_vars_cont   = character(0),
  missing_imp_vars_binary = character(0),
  missing_imp_vars_multi  = character(0),
  m     = 1,
  maxit = 1
) {
  all_imp_vars         <- c(impute_vars_cont, impute_vars_binary, impute_vars_multi)
  all_missing_imp_vars <- c(missing_imp_vars_cont, missing_imp_vars_binary, missing_imp_vars_multi)
  all_vars             <- c(all_missing_imp_vars, all_imp_vars)

  .validate_imputation_inputs(data, all_vars, predictor_list)

  # Build a single method vector and predictor matrix covering all variables.
  # missing_imp_vars are included with their own methods so MICE imputes them
  # jointly rather than treating them as observed.
  method_vector    <- .build_method_vector(
    c(missing_imp_vars_cont,   impute_vars_cont),
    c(missing_imp_vars_binary, impute_vars_binary),
    c(missing_imp_vars_multi,  impute_vars_multi),
    data
  )
  predictor_matrix <- .build_predictor_matrix(data, all_vars, predictor_list)

  # Visit missing predictor variables first in each MICE iteration so that
  # their imputed values are available when downstream variables are visited.
  visit_sequence <- c(all_missing_imp_vars, all_imp_vars)

  imp_result <- .run_mice_manual(
    data, all_vars, predictor_list,
    method_vector, predictor_matrix,
    visit_sequence, m, maxit
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


#' Build a named MICE method vector for all columns in the data
#'
#' Use variable vectors to assign approapriate MICE imputation methods
#'
#' @param impute_vars_cont character vector — continuous variables
#' @param impute_vars_binary character vector — binary variables
#' @param impute_vars_multi character vector — multinomial variables
#' @param data data.frame
#' @return named character vector, one entry per column in \code{data}
.build_method_vector <- function(impute_vars_cont, impute_vars_binary,
                                  impute_vars_multi, data) {
  var_names  <- colnames(data)
  method_vec <- setNames(rep("", length(var_names)), var_names)

  method_vec[intersect(impute_vars_cont,   var_names)] <- "pmm"
  method_vec[intersect(impute_vars_binary, var_names)] <- "logreg"
  method_vec[intersect(impute_vars_multi,  var_names)] <- "polyreg"

  method_vec
}

# ── Predictor matrix ──────────────────────────────────────────────────────────

#' Build a MICE predictor matrix from a named list of per-variable predictors
#'
#' Rows correspond to variables being imputed; columns to their predictors.

#'
#' @param data data.frame
#' @param all_imp_vars character vector of all variables to impute
#' @param predictor_list named list: variable name -> character vector of predictors
#' @return integer matrix with dimnames matching \code{colnames(data)}
.build_predictor_matrix <- function(data, all_imp_vars, predictor_list) {
  var_names <- colnames(data)
  n_vars    <- length(var_names)

  pred_matrix <- matrix(
    0L,
    nrow = n_vars,
    ncol = n_vars,
    dimnames = list(var_names, var_names)
  )

  for (var in all_imp_vars) {
    predictors    <- predictor_list[[var]]
    valid_preds   <- setdiff(intersect(predictors, var_names), var)
    invalid_preds <- setdiff(predictors, var_names)

    if (length(invalid_preds) > 0) {
      warning(sprintf(
        "Predictor(s) for '%s' not found in data and will be ignored: %s",
        var, paste(invalid_preds, collapse = ", ")
      ))
    }

    pred_matrix[var, valid_preds] <- 1L
  }

  pred_matrix
}

# ── Data preparation ──────────────────────────────────────────────────────────

#' Prepare data for MICE
#'
#' Retains only columns that are either imputation targets or appear as
#' predictors. Converts tagged-NA factor levels to real NAs and drops
#' the now-empty levels.
#'
#' @param data data.frame
#' @param all_imp_vars character vector of imputation target variables
#' @param predictor_list named list of per-variable predictor vectors
#' @return named list:
#'   \code{data} — prepared data.frame passed to MICE;
#'   \code{extra_cols} — data.frame of columns excluded from imputation
.prepare_data_for_imputation_manual <- function(data, all_imp_vars, predictor_list) {
  all_predictor_vars <- unique(unlist(predictor_list))
  vars_to_keep       <- intersect(
    unique(c(all_imp_vars, all_predictor_vars)),
    colnames(data)
  )

  prepared_data <- data[, vars_to_keep, drop = FALSE]

  # Convert tagged NAs in factor columns to real NA
  factor_vars <- colnames(prepared_data)[sapply(prepared_data, is.factor)]

  prepared_data <- prepared_data %>%
    dplyr::mutate(dplyr::across(
      dplyr::all_of(factor_vars),
      ~ dplyr::if_else(.x %in% c("NA(a)", "NA(b)", "NA(c)"), NA, .x)
    )) %>%
    dplyr::mutate(dplyr::across(
      dplyr::all_of(factor_vars),
      droplevels
    ))

  list(
    data       = prepared_data,
    extra_cols = data[, setdiff(colnames(data), vars_to_keep), drop = FALSE]
  )
}


#' Run MICE with a custom method vector, predictor matrix, and visit sequence
#'
#' Applies CCHS relationships before imputation to zero out conditional
#' downstream variables for rows with observed upstream "No" responses,
#' preventing MICE from incorrectly imputing those cells.
#'
#' @param data data.frame
#' @param all_imp_vars character vector of imputation target variables
#' @param predictor_list named list of per-variable predictor vectors
#' @param method_vector named character vector of MICE methods
#' @param predictor_matrix integer matrix
#' @param visit_sequence character vector of variable names in visitation order
#' @param m integer number of imputations
#' @param maxit integer number of iterations
#' @return named list: \code{mice_result} (mids object), \code{data} (completed data.frame)
.run_mice_manual <- function(data, all_imp_vars, predictor_list,
                              method_vector, predictor_matrix,
                              visit_sequence, m, maxit) {
  # Pre-imputation: zero out conditional variables for observed gate responses
  data <- .apply_cchs_relationships(data)

  prepared  <- .prepare_data_for_imputation_manual(data, all_imp_vars, predictor_list)
  prep_cols <- colnames(prepared$data)

  # Subset method vector and predictor matrix to prepared data columns
  sub_methods <- method_vector[prep_cols]
  sub_matrix  <- predictor_matrix[prep_cols, prep_cols, drop = FALSE]

  # Resolve visit sequence to column indices within the prepared data frame,
  # preserving the caller-specified order and dropping any absent variables.
  sub_visit <- intersect(visit_sequence, prep_cols)

  imp_result <- mice::mice(
    prepared$data,
    m               = m,
    maxit           = maxit,
    method          = sub_methods,
    predictorMatrix = sub_matrix,
    visitSequence   = sub_visit,
    nnet.MaxNWts    = 4000
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


