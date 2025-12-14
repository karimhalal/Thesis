source("R/variables-sheet-utils.R")
source("R/variable-details-sheet-utils.R")
source("R/variable-start-utils.R")

center_regex <- "^center\\[(.*)\\]$"

is_centered_variable <- function(variables_sheet_row) {
  return(grepl(center_regex, variables_sheet_row[1, "transformationType"]))
}

get_centered_variable_info <- function(variables_sheet_row) {
  center_arguments_list <- trimws(gsub('"', "", strsplit(regmatches(
    variables_sheet_row[1, "transformationType"],
    regexec(center_regex, variables_sheet_row[1, "transformationType"])
  )[[1]][2], ",")[[1]])) 
  
  return(list(
    center_var_name = variables_sheet_row[1, "variable"],
    center_start_var_name = center_arguments_list[1],
    center_value_type = center_arguments_list[2]
  ))
}

dummy_regex <- "^dummy\\[(.*)\\]$"

is_dummy_variable <- function(variables_sheet_row) {
  return(grepl(dummy_regex, variables_sheet_row[1, "transformationType"]))
}

get_dummy_variable_info <- function(variables_sheet_row) {
  derived_vars <- get_derived_vars(variables_sheet_row[1, "variableStart"])
  dummy_start_var <- derived_vars[1]

  dummy_arguments_list <- trimws(gsub('"', "", strsplit(regmatches(
    variables_sheet_row[1, "transformationType"],
    regexec(dummy_regex, variables_sheet_row[1, "transformationType"])
  )[[1]][2], ",")[[1]])) 
  
  return(list(
    dummy_var_name = variables_sheet_row[1, "variable"],
    dummy_start_var_name = dummy_start_var,
    dummy_value = trimws(dummy_arguments_list[1]),
    dummy_reference_value = trimws(dummy_arguments_list[2])
  ))
}

rcs_regex <- "^rcs\\[(.*),(.*)\\]$"

is_rcs_variable <- function(variable_sheet_row) {
  return(grepl(rcs_regex, variable_sheet_row[1, "transformationType"]))
}

get_rcs_variable_info <- function(variable_sheet_row) {
  rcs_info <- regmatches(
    variable_sheet_row[1, "transformationType"],
    regexec(rcs_regex, variable_sheet_row[1, "transformationType"])
  )
  
  return(list(
    rcs_var_name = variable_sheet_row[1, "variable"],
    rcs_start_var = rcs_info[[1]][2],
    rcs_index = as.numeric(trimws(rcs_info[[1]][3]))
  ))
}

get_num_knots <- function(
  rcs_variable,
  variables_sheet
) {
  all_rcs_vars <- get_all_rcs_vars(rcs_variable, variables_sheet)
  
  rcs_vars_info <- list()
  for(rcs_var in all_rcs_vars) {
    rcs_var_info <- get_rcs_variable_info(
      variables_sheet[variables_sheet$variable == rcs_var, ]
    )
    rcs_vars_info[[length(rcs_vars_info) + 1]] <- rcs_var_info
  }
  
  num_knots <- NA
  num_rcs_terms <- 0
  for(rcs_var_info in rcs_vars_info) {
    if(num_rcs_terms < rcs_var_info$rcs_index) {
      num_rcs_terms <- rcs_var_info$rcs_index
    }
  }
  num_knots <- num_rcs_terms + 1
  
  return(num_knots)
}


interaction_regex <- "^interact\\[(.*)\\]$"

is_interaction_variable <- function(variables_sheet_row) {
  return(grepl(interaction_regex, variables_sheet_row[1, "transformationType"]))
}

get_interaction_variable_info <- function(variables_sheet_row) {
  interacting_vars_string <- regmatches(
    variables_sheet_row[1, "transformationType"],
    regexec(interaction_regex, variables_sheet_row[1, "transformationType"])
  )[[1]][2]
  
  return(list(
    interacting_vars = trimws(gsub('"', "", strsplit(interacting_vars_string, ",")[[1]]))
  ))
}

is_transformation_variable <- function(variables_sheet_row) {
  return(variables_sheet_row[1, "transformationType"] != "N/A")
}