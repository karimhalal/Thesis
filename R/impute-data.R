library(magrittr)

source('R/get-start-var.R')

#' Add Nelson-Aalen cumulative hazard estimate as an imputation auxiliary variable
#'
#' Computes the cause-specific Nelson-Aalen cumulative hazard H(t_i) for each
#' subject at their observed follow-up time, treating competing events as
#' censored. This follows White & Royston (2009): including H(t) alongside the
#' event indicator in the MICE predictor matrix preserves the association
#' between imputed predictors and the survival outcome.
#'
#' The resulting column `nelson_aalen_h` should be passed as an
#' additional_imputation_predictor in impute_data(); it is NOT a model predictor.
#'
#' @param data data.frame containing time_var and event_var columns
#' @param time_var name of the follow-up time column (default "time_to_event")
#' @param event_var name of the event type column (default "event_type");
#'   coded 0 = censored, 1 = event of interest, 2 = competing event
#' @param event_of_interest integer code for the event of interest (default 1)
#' @return data with new column `nelson_aalen_h`
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
source('R/variables-sheet-utils.R')
source('R/transformation-type-utils.R')
source('R/is-na.R')
source('R/list-utils.R')

#' Imputes a dataset using the MICE library
#'
#' @param imputation_vars_role string containing the role assigned to all the
#' variables to impute in a variables sheet
#' @param predictor_vars_role string containing the role assigned to all the
#' predictors in a variables sheet. This is used to ensure that all the
#' predictors have been included as an imputation variable.
#' @param variables_sheet data.frame containing the variables sheet with the
#' role variables
#' @param data data.frame containing the data to impute
#' @param additional_imputation_predictors character vector of additional variable
#' names to use as imputation predictors (e.g., survival variables not in variables_sheet)
#' @param m integer number of multiple imputations (default 1)
#' @param maxit integer number of MICE iterations (default 1)
#' @return named list with the following items:
#' data: data.frame with the imputed data with the relationships for pack years,
#' percent time in Canada, and drinks last week fixed
#' imp_data: data.frame with the imputed data before the relationships are fixed
#' mice_result: The object returned from the MICE function call
impute_data <- function(
  imputation_vars_role,
  predictor_vars_role,
  variables_sheet,
  data,
  additional_imputation_predictors = c(),
  m = 1,
  maxit = 1
) {
  missing_predictor_info <- list()
  predictors <- recodeflow:::select_vars_by_role(
    predictor_vars_role,
    variables_sheet
  )
  imputation_predictors <- recodeflow:::select_vars_by_role(
    imputation_vars_role,
    variables_sheet
  )

  # Add additional imputation predictors (e.g., survival variables)
  imputation_predictors <- c(imputation_predictors, additional_imputation_predictors)

  # Get start variables for predictors (these need to be imputed)
  predictor_start_vars <- unique(get_start_var(predictors, variables_sheet))

  # Predictor-role interaction vars (e.g. DHH_AGE_C_X_HWTDBMI_C) are not captured
  # by get_start_var (which recurses to base vars), so identify them explicitly
  # so they can be passively imputed within MICE.
  predictor_interaction_vars <- predictors[sapply(predictors, function(v) {
    row <- variables_sheet[variables_sheet$variable == v, , drop = FALSE]
    nrow(row) > 0 && is_interaction_variable(row)
  })]

  # Combine: base vars + imputation predictors + predictor interaction vars
  vars_for_mice <- unique(c(predictor_start_vars, imputation_predictors, predictor_interaction_vars))

  for(predictor in predictors) {
    current_predictor_start_vars <- get_start_var(c(predictor), variables_sheet)
    predictor_vars_sheet_row <- get_row_for_variable(predictor, variables_sheet)
    if(!.has_imputation_predictor(
      predictor,
      current_predictor_start_vars,
      vars_for_mice,
      variables_sheet
    )) {
      missing_predictor_info <- append_to_list(
        missing_predictor_info,
        list(
          predictor = predictor,
          start_vars = current_predictor_start_vars
        )
      )
    }
  } 
  if(length(missing_predictor_info) > 0) {
    for(missing_predictor_info_item in missing_predictor_info) {
      print(glue::glue("Missing predictor {missing_predictor_info_item$predictor} in imputation variables"))
      print(glue::glue("Found start variables {paste(missing_predictor_info_item$start_vars, collapse = ',')}"))
    }
    stop("Missing predictors in imputation variables")
  }


  # Compute centering constants from pre-imputation data (available-case means)
  # for use in passive MICE formulas for the predictor-role interaction vars.
  centering_info <- .compute_centering_constants(predictor_interaction_vars, variables_sheet, data)

  # Add predictor interaction vars as NA columns so MICE includes them and can
  # passively derive them each iteration from their base-var components.
  for (v in predictor_interaction_vars) {
    if (!v %in% colnames(data)) data[[v]] <- NA_real_
  }

  passive_methods <- .build_passive_methods(vars_for_mice, variables_sheet, centering_info)

  imp_result <- .impute_data(
    data,
    vars_for_mice,  # Both variables to impute AND predictors
    m,
    maxit,
    passive_methods
  )

  # Fix logical relationships after imputation
  # TODO: These could be derived from variables sheet using an impute-fix role pattern
  # e.g., "impute-fix:smoke_simple=0->0" to specify "if smoke_simple == 0, set to 0"
  # Currently hardcoded for pack_years_der and ALWDWKY
  fixed_data <- imp_result$data  %>%
    dplyr::mutate(
      pack_years_der = ifelse(smoke_simple == 0, 0, pack_years_der)
    ) %>%
    dplyr::mutate(ALWDWKY = ifelse(ALCDTTM == 3, 0, ALWDWKY))

  return(list(
    mice_result = imp_result$mice_result,
    imp_data = imp_result$data,
    data = fixed_data
  )) 
}

#' Check whether a predictor has a corresponding imputation predictor
#'
#' @param predictor string containing the name of the predictor to check
#' @param predictor_start_vars vector of strings containing the starting 
#' variable for the predictor. You can use the `get_start_var` function to 
#' derive this.
#' @param imputation_predictors vector of strings containing the list of 
#' imputation predictors
#' @param variables_sheet data.frame containing the variables sheet that 
#' contains all the predictors
#' @return boolean
.has_imputation_predictor <- function(
  predictor,
  predictor_start_vars,
  vars_for_mice,
  variables_sheet
) {
  predictor_vars_sheet_row <- get_row_for_variable(predictor, variables_sheet)
  if(is_interaction_variable(predictor_vars_sheet_row) &
       !.has_interaction_variable(predictor_start_vars, variables_sheet)) {
    return(FALSE)
  }
  if(length(predictor_start_vars[
    predictor_start_vars %in% vars_for_mice
  ]) == 0) {
    return(FALSE)
  }
  return(TRUE)
}

#' Check whether a variables sheet has an interaction variable
#'
#' @param interacting_vars vector of strings containing the list of variables 
#' that are part of the interaction variable
#' @param variables_sheet data.frame containing the variables sheet
#' @return boolean
.has_interaction_variable <- function(interacting_vars, variables_sheet) {
  for(variables_sheet_index in seq_len(nrow(variables_sheet))) {
    if(is_interaction_variable(variables_sheet[variables_sheet_index, ])) {
      current_interacting_vars <- get_interaction_variable_info(
        variables_sheet[variables_sheet_index, ]
      )$interacting_vars
      if(length(setdiff(interacting_vars, current_interacting_vars)) == 0) {
        return(TRUE)
      } 
    }
  }
  return(FALSE)
}

#' Prepare a dataset for usage in a MICE function call
#'
#' @param data data.frame containing the data to impute
#' @param imp_vars vector of strings containing the list of imputation variables
#' @return named list with the following items:
#' data: data.frame containing the data to use in the MICE function call
#' extra_cols data.frame containing the non-imputation variables removed from 
#' the data
.prepare_data_for_imputation <- function(data, imp_vars) {
  # Remove any columns that will not be imputed otherwise MICE will not impute 
  # anything
  prepared_imp_data <- data[
    ,
    colnames(data) %in% imp_vars
  ]

  # Convert any tagged NAs to regular NA so that MICE will impute them
  factor_variables <- colnames(
    prepared_imp_data %>%
      dplyr::select(where(is.factor))
  )

  prepared_imp_data <- prepared_imp_data %>%
    dplyr::mutate(dplyr::across(
      dplyr::all_of(factor_variables),
      # Don't use the base ifelse since that converts factors to numerics
      ~ dplyr::if_else(.x %in% c("NA(a)", "NA(b)", "NA(c)"), NA, .x)
    )) %>%
    # The levels for the factor variables still include NA(x) even though
    # there are no rows for them. This will result in MICE showing warning
    # that there are not enough rows to impute those categories. Drop those
    # levels as we do not want MICE to impute them anyways.
    dplyr::mutate(dplyr::across(
      dplyr::all_of(factor_variables),
      ~ droplevels(.x)
    ))

  # For runs in the dev environment this makes sure that categorical
  # variables with one NA value have at least 10 values
  # Fixes an issue with running imputation with the MICE library where
  # an error, "dims does not match length of object" was occurring when
  # a categorical variable has only one NA value
  # DO NOT DO THIS IN THE PROD RUN
  # DISABLED: This was adding unwanted missing values
  # if(Sys.getenv("R_CONFIG_ACTIVE") == "dev" ||
  #    Sys.getenv("R_CONFIG_ACTIVE") == "") {
  #   for(column_name in colnames(prepared_imp_data)) {
  #     if(is.factor(prepared_imp_data[[column_name]]) &
  #        sum(is_na(prepared_imp_data[[column_name]])) == 1) {
  #       column <- prepared_imp_data[[column_name]]
  #       column[sample(seq(1, nrow(prepared_imp_data)), 10)] <- NA
  #       prepared_imp_data[[column_name]] <- column
  #     }
  #   }
  # }

  return(list(
    data = prepared_imp_data,
    extra_cols = data[,!colnames(data) %in% imp_vars]
  ))
}

#' Build passive MICE imputation formulae for interaction variables
#'
#' Returns a named character vector over \code{vars}. Two types of interactions
#' are handled:
#' \itemize{
#'   \item \strong{Imputation-predictor interactions} (e.g. \code{age_X_BMI},
#'         \code{interact[DHH_AGE, HWTDBMI]}): simple product of base vars,
#'         \code{~ I(var1 * var2)}.
#'   \item \strong{Predictor-role interactions} (e.g.
#'         \code{DHH_AGE_C_X_HWTDBMI_C}, \code{interact[DHH_AGE_C, HWTDBMI_C]}):
#'         product of centered expressions with inlined constants from
#'         \code{centering_info}, e.g.
#'         \code{~ I((DHH_AGE - 62.3) * (HWTDBMI - 27.1))}.
#' }
#' All other variables get \code{""} so MICE uses its default method.
#'
#' @param vars character vector of variable names in the imputation dataset
#' @param variables_sheet data.frame containing the variables sheet
#' @param centering_info named list from \code{.compute_centering_constants()}
#' @return named character vector of MICE method strings
.build_passive_methods <- function(vars, variables_sheet, centering_info = list()) {
  methods <- setNames(rep("", length(vars)), vars)
  for (v in vars) {
    row <- variables_sheet[variables_sheet$variable == v, , drop = FALSE]
    if (nrow(row) == 0 || !is_interaction_variable(row)) next
    info <- get_interaction_variable_info(row)
    # Predictor-role interactions have centered components (_C vars); use inlined
    # centering constants. Imputation-predictor interactions use raw base vars.
    any_centered <- any(sapply(info$interacting_vars, function(cv) {
      crow <- variables_sheet[variables_sheet$variable == cv, , drop = FALSE]
      nrow(crow) > 0 && is_centered_variable(crow)
    }))
    if (any_centered) {
      methods[v] <- .build_centered_interaction_formula(v, variables_sheet, centering_info)
    } else {
      methods[v] <- paste0("~ I(", info$interacting_vars[1], " * ", info$interacting_vars[2], ")")
    }
  }
  methods
}

#' Compute centering constants for predictor-role interaction passive formulas
#'
#' For each centered component variable of the given interaction variables,
#' computes \code{mean(base_var, na.rm = TRUE)} from the pre-imputation data.
#'
#' @param predictor_interaction_vars character vector of predictor-role
#'   interaction variable names
#' @param variables_sheet data.frame containing the variables sheet
#' @param data data.frame containing the pre-imputation data
#' @return named list keyed by centered variable name, each entry a list with
#'   \code{center_value}
.compute_centering_constants <- function(predictor_interaction_vars, variables_sheet, data) {
  centering_info <- list()
  for (v in predictor_interaction_vars) {
    row  <- variables_sheet[variables_sheet$variable == v, , drop = FALSE]
    info <- get_interaction_variable_info(row)
    for (comp_var in info$interacting_vars) {
      if (comp_var %in% names(centering_info)) next
      comp_row <- variables_sheet[variables_sheet$variable == comp_var, , drop = FALSE]
      if (nrow(comp_row) == 0 || !is_centered_variable(comp_row)) next
      center_info <- get_centered_variable_info(comp_row)
      start_var   <- center_info$center_start_var_name
      start_row   <- variables_sheet[variables_sheet$variable == start_var, , drop = FALSE]
      if (nrow(start_row) > 0 && is_dummy_variable(start_row)) {
        dummy_info <- get_dummy_variable_info(start_row)
        vals <- as.numeric(
          as.character(data[[dummy_info$dummy_start_var_name]]) == dummy_info$dummy_value
        )
      } else {
        vals <- as.numeric(data[[start_var]])
      }
      centering_info[[comp_var]] <- list(center_value = mean(vals, na.rm = TRUE))
    }
  }
  centering_info
}

#' Build a MICE passive formula for a predictor-role interaction variable
#'
#' Expresses \code{interaction_var} in terms of its base variables and
#' pre-computed centering constants, e.g.
#' \code{~ I((DHH_AGE - 62.3) * (HWTDBMI - 27.1))}.
#'
#' @param interaction_var character name of the interaction variable
#' @param variables_sheet data.frame containing the variables sheet
#' @param centering_info named list from \code{.compute_centering_constants()}
#' @return character string MICE passive formula
.build_centered_interaction_formula <- function(interaction_var, variables_sheet, centering_info) {
  row  <- variables_sheet[variables_sheet$variable == interaction_var, , drop = FALSE]
  info <- get_interaction_variable_info(row)
  exprs <- sapply(info$interacting_vars, function(comp_var) {
    comp_row <- variables_sheet[variables_sheet$variable == comp_var, , drop = FALSE]
    if (nrow(comp_row) == 0 || !is_centered_variable(comp_row)) return(comp_var)
    center_info <- get_centered_variable_info(comp_row)
    start_var   <- center_info$center_start_var_name
    center_val  <- centering_info[[comp_var]]$center_value
    start_row   <- variables_sheet[variables_sheet$variable == start_var, , drop = FALSE]
    if (nrow(start_row) > 0 && is_dummy_variable(start_row)) {
      dummy_info <- get_dummy_variable_info(start_row)
      paste0("(as.numeric(as.character(", dummy_info$dummy_start_var_name,
             ") == \"", dummy_info$dummy_value, "\") - ", center_val, ")")
    } else {
      paste0("(", start_var, " - ", center_val, ")")
    }
  })
  paste0("~ I(", paste(exprs, collapse = " * "), ")")
}

#' Imputes a dataset
#' @param data data.frame containing the data to impute
#' @param imp_vars vector of strings containing the list of columns in the
#' data that should be imputed
#' @param passive_methods optional named character vector from
#'   \code{.build_passive_methods()}. Entries with a non-empty string override
#'   the default MICE method for that variable (used for passive imputation of
#'   interaction terms).
#' @return named list containing the following items:
#' mice_result: The result of the MICE function call
#' data: The original data with the imputed variables filled in
.impute_data <- function(data, imp_vars, m = 1, maxit = 1, passive_methods = NULL) {
  prepared_imp_dataset_result <- .prepare_data_for_imputation(
    data,
    imp_vars
  )

  # Start with MICE default methods, then override interaction variables with
  # passive formulae so their relationship to component variables is preserved.
  method <- mice::make.method(prepared_imp_dataset_result$data)
  if (!is.null(passive_methods)) {
    in_data <- intersect(names(passive_methods), names(method))
    for (v in in_data) {
      if (nzchar(passive_methods[v])) method[v] <- passive_methods[v]
    }
  }

  imp_result <- mice::mice(
    prepared_imp_dataset_result$data,
    m = m,
    maxit = maxit,
    method = method,
    nnet.MaxNWts = 10000,
    printFlag = FALSE
  )

  imp_data <- mice::complete(imp_result)

  full_data <- cbind(
    imp_data, 
    prepared_imp_dataset_result$extra_cols
  )

  return(list(
    mice_result = imp_result,
    data = full_data 
  ))
}
