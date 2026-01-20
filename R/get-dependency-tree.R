source("R/variable-start-utils.R")

get_dependency_tree <- function(
    variable_name, variables_sheet, database_name) {
  rows_for_current_variable <- 
    variables_sheet[variables_sheet$variable == variable_name, ]
  
  dependencies <- list()
  for(row_index in seq_len(nrow(rows_for_current_variable))) {
    current_row <- rows_for_current_variable[row_index, ]
    
    dependencies_in_current_row <- get_dependencies(current_row, database_name)
    
    if(!variable_name %in% dependencies_in_current_row) {
      for(dependency_name in dependencies_in_current_row) {
        current_dependency_tree <- get_dependency_tree(
            dependency_name, 
            variables_sheet, 
            database_name
        )
        
        current_dependencies_list <- list()
        current_dependencies_list[[dependency_name]] <- list(
          "dependencies" = current_dependency_tree
        )
        dependencies <- append(dependencies, current_dependencies_list)
      }
    }
  }
  
  return(dependencies)
}

get_dependencies <- function(
    variables_sheet_row, database_name
) {
  start_var_string <- NA
  if(database_name != "default") {
    db_start_var_regex <- paste(database_name, "::\\[(.+?)\\]", sep = "")
    start_var_string <- regmatches(variables_sheet_row$variableStart, regexec(db_start_var_regex, variables_sheet_row$variableStart))[[1]][2]
  }
  
  if(is.na(start_var_string)) {
    if(is_derived_var(variables_sheet_row$variableStart)) {
      return(get_derived_vars(variables_sheet_row$variableStart))
    } 
    else {
      tryCatch({
        return(get_start_var_name(variables_sheet_row, database_name))
      },
      error = function() {
        stop(paste("No start variable found for database", database_name, "for variable", variables_sheet_row$variable))
      })
    }
  } else {
    if(is_derived_var(start_var_string)) {
      return(get_derived_vars(start_var_string))
    } else {
      tryCatch({
        return(get_start_var_name(variables_sheet_row, database_name))
      }, 
      error = function() {
        return(c())
      })
    }
  }
}

#' Get variable name from variableStart using database name.
#'
#' @param var_details_row A variable details row.
#' @param db_name Name of database to extract from.
#'
#' @return character The name of the start variable.
get_start_var_name <- function(var_details_row, db_name) {
  # The value of the variableStart column for this variable details row
  start_variables = var_details_row$variableStart

  # The regex that will be used to pluck the name of the start variable from a
  # start variable string. For example, if the db_name is cchs2001_p and the
  # list is cchs2001_p::RACA_6A,this regex will pluck out RACA_6A
  start_variable_with_db_regex <- paste0(db_name, "::(.+?)$")
  # Regex to pluck out the start variable from a default variable string
  # For example, if the string is [ADL_01], this regex will pull out ADL_01
  default_start_var_regex <- "\\[(.+?)\\]"
  # Split the start variable column into each start variable string
  # For example, db1::var1, db2::var2, [var3] would be split into
  # [db1::var1, db2::var2, [var3]]
  start_variables_split <- strsplit(start_variables, ",")
  # The name of the start variable for the passed db_name argument
  start_variable_for_db <- NA
  # The name of the default start variable for this start variable string
  default_start_var <- NA
  # Go through each of the start variable strings to find either one for this
  # db or a default one
  for(i in seq_len(length(start_variables_split[[1]]))) {
    current_start_variable_str <- start_variables_split[[1]][i]

    # Get regex match for variable start
    db_var_regex_matches <- regmatches(
      current_start_variable_str,
      regexec(start_variable_with_db_regex, current_start_variable_str))
    possible_db_start_var <- db_var_regex_matches[[1]][2]
    # If we found a start variable for this db then assign it and break out
    # of the loop since we don't care about the default var
    if(!is.na(possible_db_start_var)) {
      start_variable_for_db <- possible_db_start_var
      break
    }

    # Find the matches for the default var regex in this start variable string
    default_var_regex_matches <- regmatches(
      current_start_variable_str,
      regexec(default_start_var_regex, current_start_variable_str))
    # Either the name of the default var or NA if it does not match
    possible_default_var <- default_var_regex_matches[[1]][2]
    if(!is.na(possible_default_var)) {
      default_start_var <- possible_default_var
    }
  }

  if(!is.na(start_variable_for_db)) {
    return(start_variable_for_db)
  } else if(!is.na(default_start_var)) {
    return(default_start_var)
  }
  # Otherwise, throw an error saying we could not find a start variable
  else {
    stop(paste(
      "No start variable found for database ",
      db_name,
      "for column ",
      start_variables
    ))
  }
}
