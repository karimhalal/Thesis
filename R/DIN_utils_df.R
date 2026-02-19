library(dplyr)
library(stringr)

process_din_dataframe <- function(din_df) {
  din_list <- din_df %>%
    dplyr::distinct(DIN.PIN, .keep_all = TRUE) %>%
    mutate(DIN.PIN = str_pad(DIN.PIN, 8, "left", pad = "0")) %>%
    mutate(across(where(is.character), tolower))

  tab_cap_forms <- paste(c("cap", "cr cap", "er cap", "sr cap", "tab", "chew tab",
                           "cr tab", "er tab", "er tab chewable", "sr tab",
                           "tab (chewable)", "sl tab", "sup", "rect sup", "buccal soluble fil"), collapse = "|")
  transdermal_forms <- paste(c("trans patch"), collapse = "|")
  liquid_form <- paste(c("o/l", "o/l 500ml", "oral sol", "oral drops", "susp",
                         "oral concentrate*", "er pd for sol", "rect gel*"), collapse = "|")
  all_forms <- paste(c(liquid_form, transdermal_forms, tab_cap_forms), collapse = "|")

  din_list_transdermal <- din_list %>%
    filter(stringr::str_detect(Dosage.Form, transdermal_forms))
  din_list_liquid <- din_list %>%
    filter(stringr::str_detect(Dosage.Form, liquid_form))
  din_list_tab_cap <- din_list %>%
    filter(stringr::str_detect(Dosage.Form, tab_cap_forms))
  din_list_other <- din_list %>%
    filter(!stringr::str_detect(Dosage.Form, all_forms))

  din_vec_tab_cap <- as.vector(din_list_tab_cap$DIN.PIN)
  din_vec_transdermal <- as.vector(din_list_transdermal$DIN.PIN)
  din_vec_liquid <- as.vector(din_list_liquid$DIN.PIN)

  result <- list(
    din_list = din_list,
    din_list_liquid = din_list_liquid,
    din_list_tab_cap = din_list_tab_cap,
    din_list_transdermal = din_list_transdermal,
    din_list_other = din_list_other,
    din_vec_tab_cap = din_vec_tab_cap,
    din_vec_transdermal = din_vec_transdermal,
    din_vec_liquid = din_vec_liquid
  )

  return(result)
}

get_row_for_din_df <- function(din_number, din_list) {
  return(din_list[din_list$DIN.PIN == din_number, ])
}

get_dinlist_by_class_df <- function(drug_class, din_list) {
  din_list <- din_list %>%
    filter(grepl(drug_class, Active.Ingredient.Class.and.Use, ignore.case = TRUE))
  return(din_list)
}

get_dins_by_class_df <- function(drug_class, din_list) {
  din_list <- get_dinlist_by_class_df(drug_class, din_list)
  dins <- as.vector(din_list$DIN.PIN)
  return(dins)
}

get_dinlist_by_dosage_form_df <- function(dosage_form, din_list) {
  din_list <- din_list %>%
    filter(grepl(dosage_form, Dosage.Form, ignore.case = TRUE))
  return(din_list)
}

get_dins_by_doseform_df <- function(dosage_form, din_list) {
  din_list <- get_dinlist_by_dosage_form_df(dosage_form, din_list)
  dins <- as.vector(din_list$DIN.PIN)
  return(dins)
}

is_valid_din_df <- function(din_number, din_list) {
  if (is.na(din_number)) return(FALSE)
  return(din_number %in% din_list$DIN.PIN)
}

get_conversion_factor_df <- function(din_number, din_list) {
  din_row <- get_row_for_din_df(din_number, din_list)
  if (nrow(din_row) == 0) return(NA)
  return(din_row$conversion_factor)
}

get_brand_name_df <- function(din_number, din_list) {
  din_row <- get_row_for_din_df(din_number, din_list)
  if (nrow(din_row) == 0) return(NA)
  return(din_row$Brand.Name)
}

get_active_ingredient_df <- function(din_number, din_list) {
  din_row <- get_row_for_din_df(din_number, din_list)
  if (nrow(din_row) == 0) return(NA)
  return(din_row$Active.Ingredients)
}

get_dosage_form_df <- function(din_number, din_list) {
  din_row <- get_row_for_din_df(din_number, din_list)
  if (nrow(din_row) == 0) return(NA)
  return(din_row$Dosage.Form)
}

get_strength_df <- function(din_number, din_list) {
  din_row <- get_row_for_din_df(din_number, din_list)
  if (nrow(din_row) == 0) return(NA)
  return(din_row$Strength)
}

get_drug_class_df <- function(din_number, din_list) {
  din_row <- get_row_for_din_df(din_number, din_list)
  if (nrow(din_row) == 0) return(NA)
  class_field <- din_row$Active.Ingredient.Class.and.Use
  if (grepl("opioid", class_field, ignore.case = TRUE)) return("opioid")
  if (grepl("bzd|benzodiazepine", class_field, ignore.case = TRUE)) return("bzd")
  return("other")
}

validate_din_format_df <- function(din_number, din_list) {
  issues <- c()
  if (is.na(din_number)) {
    return(list(is_valid = FALSE, issues = "Missing DIN"))
  }
  din_str <- as.character(din_number)
  if (nchar(din_str) != 8) {
    issues <- c(issues, sprintf("Invalid length: %d (expected 8)", nchar(din_str)))
  }
  if (!grepl("^[0-9]+$", din_str)) {
    issues <- c(issues, "Contains non-numeric characters")
  }
  if (!is_valid_din_df(din_number, din_list)) {
    issues <- c(issues, "DIN not found in master list")
  }
  return(list(
    is_valid = length(issues) == 0,
    issues = if (length(issues) == 0) NULL else issues
  ))
}

get_unique_drug_classes_df <- function(din_list) {
  return(unique(din_list$Active.Ingredient.Class.and.Use))
}

get_unique_dosage_forms_df <- function(din_list) {
  return(unique(din_list$Dosage.Form))
}

get_dinlist_cor_df <- function(din_list) {
  din_list_cor <- din_list[din_list$Active.Ingredient.Dose != "" & !is.na(din_list$Active.Ingredient.Dose), ]
  din_list_cor <- din_list_cor %>%
    select(Active.Ingredient.Dose, DIN.PIN)
  return(din_list_cor)
}
