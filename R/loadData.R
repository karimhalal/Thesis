#load necessary packages
library(yaml)
library(dplyr)
library(cchsflow)
library(recodeflow)
library(here)
library(readxl)

#define config.yaml file for use and load
cchs_config<-yaml::yaml.load_file("config.yml", eval.expr= TRUE)

# Load variable sheets using the paths from the config file.
# This ensures you are always using the correct, centrally-defined files.
variables_sheet <- read.csv(cchs_config$default$variable$variable_names, fileEncoding = "UTF-8-BOM")
variables_details_sheet <- read.csv(cchs_config$default$variable$variable_details)

#create the study data
create_study_data <- function(variables_sheet, variables_details_sheet, cchs_config) {
  harmonized_data <- NULL
  
  # Load data into a new environment
  data_env <- new.env()
  
  # The vector of cchs database names
  data_names <- names(cchs_config$default$data)
  # This for loop will populate the cchs_data variable with all the cchs
  # databases we will be using
  for(data_index in seq_along(data_names)) {
    # Get the name of the current cchs dataset
    data_name <- data_names[[data_index]]
    print(paste("Start harmonization for", data_name))

    # Using the path, load the .RData file into the environment
    load(cchs_config$default$data[[data_name]], envir = data_env)
    
    # select variables to be processed
    vars_to_process <- recodeflow:::select_vars_by_role(
      roles = c("predictor", "table-1-a", "intermediate", "imputation-variable"), 
      variables = variables_sheet # CORRECT: Use the function argument
    )
    
    #use rec_with_tbl to create harmonized 
    current_harmonized_data <- rec_with_table(
      data = get(data_name, envir = data_env),
      variables = vars_to_process,
      database_name = data_name,
      variable_details = variables_details_sheet,
      custom_function_path = here::here("R", "special_functions.R"),
      notes = FALSE
    )

    current_harmonized_data$SurveyCycle <- data_name
    
    # If the harmonized_data has not been initialized then set to the current one, if all data has been initialized, append new rows to existing data 
    if (is.null(harmonized_data)) {
      harmonized_data <- current_harmonized_data
    } else {
      harmonized_data <- dplyr::bind_rows(harmonized_data, current_harmonized_data)
    }
    
    rm(list = data_name, envir = data_env)
    
    print(paste("Done harmonization for", data_name))
  }
  harmonized_data <- cchsflow::set_data_labels(
    harmonized_data, variables_details_sheet, variables_sheet)
  
  return(harmonized_data)
}

harmonized_data<-create_study_data(variables_sheet, variables_details_sheet, cchs_config)
harmonized_data