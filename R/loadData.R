#load dependencies
install.packages("yaml")
library(yaml)
library(dplyr)
library(cchsflow)
library(recodeflow)

#define config.yaml file for use and load
cchs_config<-yaml::yaml.load_file("config.yml", eval.expr= TRUE)
variables_sheet<-read.csv("cchs_variables.csv")
variables_details_sheet<-read.csv(variable_details)

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
    
    current_harmonized_data <- recodeflow::rec_with_table(
      get(data_name, envir = data_env),
      variables = variables_sheet,
      database_name = data_name,
      variable_details = variables_details_sheet,
      id_role_name = "id",
      custom_function_path = "R/custom-functions.R",
      notes = FALSE,
    )
    current_harmonized_data$SurveyCycle <- data_name
    
    # If the harmonized_data has not been initialized then set to the
    # current one.
    # Otherwise row append the current one to the harmonize_data
    if (is.null(harmonized_data)) {
      harmonized_data <- current_harmonized_data
    } else {
      harmonized_data <-
        dplyr::bind_rows(harmonized_data, current_harmonized_data)
    }
    
    rm(list = data_name, envir = data_env)
    
    print(paste("Done harmonization for", data_name))
  }
  
  return(harmonized_data)
}


'/Users/karimhalal/Desktop/The worlds greatest thesis/Thesis/worksheets/ cchs_variables.csv'