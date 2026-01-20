source("R/variables-sheet-utils.R")
source("R/transformation-type-utils.R")
source("R/get-dependency-tree.R")

get_start_var <- function(variables, variables_sheet) {
  start_vars <- c()
  for(variable in variables) {
    start_var <- NA
    variable_variables_sheet_row <- get_row_for_variable(
      variable, 
      variables_sheet
    )
    if(is_rcs_variable(variable_variables_sheet_row)) {
      start_var <- get_start_var_for_rcs_predictor(
        variable,
        variables_sheet
      )
    } 
    else if(is_centered_variable(variable_variables_sheet_row)) {
      start_var <- get_start_var_for_centered_predictor(
        variable,
        variables_sheet
      )
    } 
    else if(is_interaction_variable(variable_variables_sheet_row)) {
      start_var <- c() 
      variables_sheet_row <- get_row_for_variable(variable, variables_sheet)
      interacting_vars <- get_interaction_variable_info(
        variables_sheet_row
      )$interacting_vars
      for(interacting_var in interacting_vars) {
        start_var <- c(
          start_var, get_start_var(interacting_var, variables_sheet)
        )
      }
    }
    else {
      start_var <- variable
    }
    start_vars <- c(start_vars, start_var)
  }
  return(unique(start_vars))
}

get_start_var_for_rcs_predictor <- function(
    predictor,
    variables_sheet
) {
  predictor_dependency_tree <- get_dependency_tree(
    predictor,
    variables_sheet,
    "default"
  )
  
  dependencies <- names(predictor_dependency_tree) 
  dependencies <- dependencies[!is.na(dependencies)]

  if(length(dependencies) > 1) {
    return(predictor)
  }

  if(length(dependencies) == 0) {
    return(predictor)
  }
  
  dependency <- dependencies[1]
  variables_sheet_row_for_dependency <- get_row_for_variable(
    dependency,
    variables_sheet
  )
  if(nrow(variables_sheet_row_for_dependency) == 0) {
    return(predictor)
  }
  
  return(get_start_var_for_rcs_predictor(
    dependency,
    variables_sheet
  ))
}

get_start_var_for_centered_predictor <- function(
    predictor,
    variables_sheet
) {
  predictor_variables_sheet_row <- get_row_for_variable(
    predictor,
    variables_sheet
  )
  center_start_var <- get_centered_variable_info(
    predictor_variables_sheet_row
  )$center_start_var_name
  center_start_var_variables_sheet_row <- get_row_for_variable(
    center_start_var,
    variables_sheet
  )
  if(is_dummy_variable(center_start_var_variables_sheet_row)) {
    dummy_variable_info <- get_dummy_variable_info(
      center_start_var_variables_sheet_row
    )
    return(dummy_variable_info$dummy_start_var_name)
  } 
  else if(is_continuous_variable(center_start_var_variables_sheet_row)) {
    return(center_start_var)
  }
  else {
    stop(paste(
      "Unhandled variable type for variable",
      center_start_var,
      "when finding start variable for centered variable",
      predictor
    ))
  }
}
