# Run this script ONCE in an environment where you have access to din_list2.csv
# It will generate an R file with hardcoded vectors for the selected columns

library(dplyr)
library(stringr)

generate_din_builtin_data <- function(output_file = here::here("R", "din_data_builtin.R")) {

  # Read CSV directly
  din_list <- read.csv(
    here::here("worksheets", "NMS_sheets", "DIN_list2.csv"),
    fileEncoding = "UTF-8-BOM",
    stringsAsFactors = FALSE
  )

  # Clean and process
  din_list <- din_list %>%
    dplyr::distinct(DIN.PIN, .keep_all = TRUE) %>%
    mutate(DIN.PIN = str_pad(DIN.PIN, 8, "left", pad = "0")) %>%
    mutate(across(where(is.character), tolower)) %>%
    select(DIN.PIN, Dosage.Form, Strength, conversion_factor,
           Active.Ingredients, Active.Ingredient.Class.and.Use, Active.Ingredient.Dose)

  # Helper function to escape strings for R code
  escape_string <- function(x) {
    x <- gsub("\\\\", "\\\\\\\\", x)
    x <- gsub('"', '\\\\"', x)
    x <- ifelse(is.na(x) | x == "", "NA_character_", paste0('"', x, '"'))
    return(x)
  }

  format_numeric <- function(x) {
    ifelse(is.na(x), "NA_real_", as.character(x))
  }

  # Build output
  lines <- c(
    "# Built-in DIN Dataset",
    paste0("# Auto-generated on ", Sys.Date()),
    paste0("# Total records: ", nrow(din_list)),
    "",
    "get_din_list_builtin <- function() {",
    "",
    "  din_list <- data.frame(",
    paste0("    DIN.PIN = c(", paste(escape_string(din_list$DIN.PIN), collapse = ", "), "),"),
    paste0("    Dosage.Form = c(", paste(escape_string(din_list$Dosage.Form), collapse = ", "), "),"),
    paste0("    STRENGTH = c(", paste(escape_string(din_list$Strength), collapse = ", "), "),"),
    paste0("    conversion_factor = c(", paste(format_numeric(din_list$conversion_factor), collapse = ", "), "),"),
    paste0("    Active.Ingredients = c(", paste(escape_string(din_list$Active.Ingredients), collapse = ", "), "),"),
    paste0("    Active.Ingredient.Class.and.Use = c(", paste(escape_string(din_list$Active.Ingredient.Class.and.Use), collapse = ", "), "),"),
    paste0("    Active.Ingredient.Dose = c(", paste(escape_string(din_list$Active.Ingredient.Dose), collapse = ", "), "),"),
    "    stringsAsFactors = FALSE",
    "  )",
    "",
    "  return(din_list)",
    "}",
    ""
  )

  writeLines(lines, output_file)
  message("Built-in DIN data file generated: ", output_file)
  message("Records included: ", nrow(din_list))

  invisible(din_list)
}

# Run the generator
# generate_din_builtin_data()
