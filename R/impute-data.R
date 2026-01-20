library(mice)
library(magrittr)

source('R/get-start-var.R')
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
#' @return named list with the following items:
#' data: data.frame with the imputed data with the relationships for pack years,
#' percent time in Canada, and drinks last week fixed
#' imp_data: data.frame with the imputed data before the relationships are fixed
#' mice_result: The object returned from the MICE function call
impute_data <- function(
  imputation_vars_role,
  predictor_vars_role,
  variables_sheet,
  data
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
  for(predictor in predictors) {
    predictor_start_vars <- get_start_var(c(predictor), variables_sheet)
    predictor_vars_sheet_row <- get_row_for_variable(predictor, variables_sheet)
    if(!.has_imputation_predictor(
      predictor, 
      predictor_start_vars, 
      imputation_predictors, 
      variables_sheet
    )) { 
      missing_predictor_info <- append_to_list(
        missing_predictor_info,
        list(
          predictor = predictor,
          start_vars = predictor_start_vars
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
  imp_result <- .impute_data(
    data,
    imputation_predictors
  )

  fixed_data <- imp_result$data  %>%
    dplyr::mutate(ALWDWKY_HUI = ifelse(ALCDTTM == 3, 0, ALWDWKY_HUI))

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
  imputation_predictors, 
  variables_sheet
) {
  predictor_vars_sheet_row <- get_row_for_variable(predictor, variables_sheet)
  if(is_interaction_variable(predictor_vars_sheet_row) & 
       !.has_interaction_variable(predictor_start_vars, variables_sheet)) {
    return(FALSE)
  }
  if(length(predictor_start_vars[
    predictor_start_vars %in% imputation_predictors
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
      factor_variables,
      # Don't use the base ifelse since that converts factors to numerics
      ~ dplyr::if_else(.x %in% c("NA(a)", "NA(b)", "NA(c)"), NA, .x)
    )) %>%
    # The levels for the factor variables still include NA(x) even though
    # there are no rows for them. This will result in MICE showing warning
    # that there are not enough rows to impute those categories. Drop those
    # levels as we do not want MICE to impute them anyways.
    dplyr::mutate(dplyr::across(
      factor_variables,
      ~ droplevels(.x)
    ))

  # For runs in the dev environment this makes sure that categorical
  # variables with one NA value have at least 10 values
  # Fixes an issue with running imputation with the MICE library where
  # an error, "dims does not match length of object" was occurring when
  # a categorical variable has only one NA value
  # DO NOT DO THIS IN THE PROD RUN
  # We do this in dev with the assumption that in the prod run with the whole
  # data there will be more NA values
  if(Sys.getenv("R_CONFIG_ACTIVE") == "dev" || 
     Sys.getenv("R_CONFIG_ACTIVE") == "") {
    for(column_name in colnames(prepared_imp_data)) {
      if(is.factor(prepared_imp_data[[column_name]]) & 
         sum(is_na(prepared_imp_data[[column_name]])) == 1) {
        column <- prepared_imp_data[[column_name]]
        column[sample(seq(1, nrow(prepared_imp_data)), 10)] <- NA
        prepared_imp_data[[column_name]] <- column
      }
    }
  }

  return(list(
    data = prepared_imp_data,
    extra_cols = data[,!colnames(data) %in% imp_vars]
  ))
}

#' Imputes a dataset 
#' @param data data.frame containing the data to impute
#' @param imp_vars vector of strings containing the list of columns in the 
#' data that should be imputed
#' @return named list containing the following items:
#' mice_result: The result of the MICE function call
#' data: The original data with the imputed variables filled in
.impute_data <- function(data, imp_vars) {
  prepared_imp_dataset_result <- .prepare_data_for_imputation(
    data,
    imp_vars
  )
  imp_result <- mice::mice(
    prepared_imp_dataset_result$data,
    m = 1,
    maxit = 1,
    nnet.MaxNWts = 4000
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