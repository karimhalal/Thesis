
library(dplyr)
library(stringr)
#' Load combined DIN list
#' @keywords internal
get_din_list_combined <- function() {
  
  #create a new global environment if none exist
  if(!exists("nms.global")){
    nms.global<<-new.env()
  }
  
  #Check if cached
   if (exists("cached_din_list_all", envir = nms.global)) {
   return(nms.global$cached_din_list_all)
  }
  
  # Load config
  config <- yaml::yaml.load_file("config.yml", eval.expr = TRUE)
  
  # Load the combined DIN list
  din_list <- read.csv(
    config$default$variable$din_list,
    fileEncoding="UTF-8-BOM",
    stringsAsFactors= FALSE
  )
  
  # Remove duplicates (keep first occurrence)
  din_list <- din_list %>%
    dplyr::distinct(DIN.PIN, .keep_all = TRUE)%>%
    mutate(DIN.PIN=str_pad(DIN.PIN, 8, "left", pad = "0"))%>%
    mutate(across(where(is.character), tolower))

  tab_cap_forms<-paste(c("cap", "cr cap", "er cap", "sr cap", "tab", "chew tab",
                     "cr tab", "er tab", "er tab chewable", "sr tab",
                     "tab (chewable)", "sl tab", "sup", "rect sup", "buccal soluble fil"), collapse = "|")
  
  transdermal_forms<-paste(c("trans patch"), collapse = "|")
  
  liquid_form<-paste(c("o/l", "o/l 500ml", "oral sol", "oral drops", "susp",
                         "oral concentrate*", "er pd for sol", "rect gel*"), collapse = "|")
  
  all_forms<-paste(c(liquid_form, transdermal_forms, tab_cap_forms), collapse = "|")

  din_list_transdermal<- din_list%>%
    filter(stringr::str_detect(Dosage.Form, transdermal_forms))

  #generate din_list for oral liquids
  din_list_liquid<-din_list%>%
    filter(stringr::str_detect(Dosage.Form, liquid_form))


  din_list_tab_cap<- din_list%>%
    filter(stringr::str_detect(Dosage.Form, tab_cap_forms))


  #create
  din_list_other<-din_list%>%
    filter(!stringr::str_detect(Dosage.Form, all_forms))

  din_vec_tab_cap<-as.vector(din_list_tab_cap$DIN.PIN)
  din_vec_transdermal<-as.vector(din_list_transdermal$DIN.PIN)
  din_vec_liquid<-as.vector(din_list_liquid$DIN.PIN)


  #create list of processed datasets
  result<-list(
  din_list=din_list,
  din_list_liquid=din_list_liquid,
  din_list_tab_cap=din_list_tab_cap,
  din_list_transdermal= din_list_transdermal,
  din_list_other= din_list_other,
  din_vec_tab_cap= din_vec_tab_cap,
  din_vec_transdermal= din_vec_transdermal,
  din_vec_liquid=din_vec_liquid

)
  # Cache the result so that it can be called directly when it exists
  nms.global$cached_din_list_all <- result


  return(result)
}


#' #' Get DIN row by DIN number- drug information quick lookup
#' @param din_list combined din list used to extract
get_row_for_din <- function(din_number, din_list = NULL) {
  if (is.null(din_list)) {
    din_list <- get_din_list_combined()
    din_list<-din_list[["din_list"]]
  }
  return(din_list[din_list$DIN.PIN == din_number, ])
}

#' Get all DINs and corresponding information for a specific drug class
#' @param drug_class The drug class to filter by ("opioid", "bzd", "stimulant")
#' @param din_list The DIN list data frame (optional). If no list is provided
#' @return Data frame with all matching DINs
get_dinlist_by_class <- function(drug_class, din_list = NULL) {
  if (is.null(din_list)) {
    din_list <- get_din_list_combined()
    din_list<-din_list[["din_list"]]
  }
  din_list<-din_list%>%
    filter(grepl(drug_class, Active.Ingredient.Class.and.Use, ignore.case= TRUE))

  #assign the approapriate data names based on the
  obj_name<-paste0("din_list_", drug_class)
  assign(obj_name, din_list)

  return(din_list)

}

#'Get relevant DIN's per drug class
get_dins_by_class<-function(drug_class, din_list=NULL){
  
  din_list<-get_dinlist_by_class(drug_class, din_list)
  dins<-as.vector(din_list$DIN.PIN)

  return(dins)
}

#' Get all DINs for a specific dosage form
#' @param dosage_form The dosage form to filter by (e.g., "tab", "trans patch")
#' @param din_list The DIN list data frame (optional)
#' @return Data frame with all matching DINs
get_dinlist_by_dosage_form <- function(dosage_form, din_list = NULL) {
  if (is.null(din_list)) {
    din_list <- get_din_list_combined()
    din_list<-din_list[["din_list"]]
  }
  din_list<-din_list%>%
    filter(grepl(dosage_form, din_list$Dosage.Form, ignore.case = TRUE))

  obj_name<-paste0("din_list_", dosage_form)
  assign(obj_name, din_list)

  return(din_list)
 
}

get_dins_by_doseform<-function(dosage_form, din_list=NULL){
  
  din_list<-get_dinlist_by_dosage_form(dosage_form, din_list)
  dins<-as.vector(din_list$DIN.PIN)

  return(dins)
}

#' Check if DIN exists in the master list
#' @param din_number The DIN to validate
#' @param din_list The DIN list data frame (optional)
#' @return Logical TRUE if DIN exists, FALSE otherwise
is_valid_din <- function(din_number, din_list = NULL) {
  if (is.na(din_number)) return(FALSE)
  if (is.null(din_list)) {
    din_list <- get_din_list_combined()
    din_list<-din_list[["din_list"]]
  }
  return(din_number %in% din_list$DIN.PIN)
}


#' Extract conversion factor for a DIN
#' @param din_number The DIN number
#' @param din_list The DIN list data frame (optional)
#' @return Numeric conversion factor, or NA if not found
get_conversion_factor <- function(din_number, din_list = NULL) {
  if (is.null(din_list)) {
    din_list <- get_din_list_combined()
    din_list<-din_list[["din_list"]]
  }
  din_row <- get_row_for_din(din_number, din_list)
  if (nrow(din_row) == 0) return(NA)
  return(din_row$conversion_factor)
}

#' Extract brand name for a DIN
#' @param din_number The DIN number
#' @param din_list The DIN list data frame (optional)
#' @return Character brand name, or NA if not found
get_brand_name <- function(din_number, din_list = NULL) {
  if (is.null(din_list)) {
    din_list <- get_din_list_combined()
    din_list<-din_list[["din_list"]]
  }
  din_row <- get_row_for_din(din_number, din_list)
  if (nrow(din_row) == 0) return(NA)
  return(din_row$Brand.Name)
}

#' Extract active ingredient for a DIN
#' @param din_number The DIN number
#' @param din_list The DIN list data frame (optional)
#' @return Character active ingredient, or NA if not found
get_active_ingredient <- function(din_number, din_list = NULL) {
  if (is.null(din_list)) {
    din_list <- get_din_list_combined()
  }
  din_row <- get_row_for_din(din_number, din_list)
  if (nrow(din_row) == 0) return(NA)
  return(din_row$Active.Ingredients)
}

#' Extract dosage form for a DIN
#' @param din_number The DIN number
#' @param din_list The DIN list data frame (optional)
#' @return Character dosage form, or NA if not found
get_dosage_form <- function(din_number, din_list = NULL) {
  if (is.null(din_list)) {
    din_list <- get_din_list_combined()
  }
  din_row <- get_row_for_din(din_number, din_list)
  if (nrow(din_row) == 0) return(NA)
  return(din_row$Dosage.Form)
}

#' Extract strength for a DIN
#' @param din_number The DIN number
#' @param din_list The DIN list data frame (optional)
#' @return Character strength, or NA if not found
get_strength <- function(din_number, din_list = NULL) {
  if (is.null(din_list)) {
    din_list <- get_din_list_combined()
  }
  din_row <- get_row_for_din(din_number, din_list)
  if (nrow(din_row) == 0) return(NA)
  return(din_row$Strength)
}

#' Extract drug class for a DIN
#' @param din_number The DIN number
#' @param din_list The DIN list data frame (optional)
#' @return Character drug class, or NA if not found
get_drug_class <- function(din_number, din_list = NULL) {
  if (is.null(din_list)) {
    din_list <- get_din_list_combined()
  }
  din_row <- get_row_for_din(din_number, din_list)
  if (nrow(din_row) == 0) return(NA)
  
  class_field <- din_row$Active.Ingredient.Class.and.Use
  if (grepl("opioid", class_field, ignore.case = TRUE)) return("opioid")
  if (grepl("bzd|benzodiazepine", class_field, ignore.case = TRUE)) return("bzd")
  return("other")
}



#' Validate DIN format
#' @param din_number The DIN to validate
#' @return List with validation results (is_valid, issues)
validate_din_format <- function(din_number) {
  issues <- c()
  
  if (is.na(din_number)) {
    return(list(is_valid = FALSE, issues = "Missing DIN"))
  }
  
  din_str <- as.character(din_number)
  
  # Check length
  if (nchar(din_str) != 8) {
    issues <- c(issues, sprintf("Invalid length: %d (expected 8)", nchar(din_str)))
  }
  
  # Check if numeric
  if (!grepl("^[0-9]+$", din_str)) {
    issues <- c(issues, "Contains non-numeric characters")
  }
  
  # Check if in master list
  if (!is_valid_din(din_number)) {
    issues <- c(issues, "DIN not found in master list")
  }
  
  return(list(
    is_valid = length(issues) == 0,
    issues = if (length(issues) == 0) NULL else issues
  ))
}

#' Get unique drug classes from DIN list
#' @param din_list The DIN list data frame (optional)
#' @return Character vector of unique drug classes
get_unique_drug_classes <- function(din_list = NULL) {
  if (is.null(din_list)) {
    din_list <- get_din_list_combined()
  }
  return(unique(din_list$Active.Ingredient.Class.and.Use))
}

#' Get unique dosage forms from DIN list
#' @param din_list The DIN list data frame (optional)
#' @return Character vector of unique dosage forms
get_unique_dosage_forms <- function(din_list = NULL) {
  if (is.null(din_list)) {
    din_list <- get_din_list_combined()
  }
  return(unique(din_list$Dosage.Form))
}

#find din list for the drugs that needs correction
get_dinlist_cor<-function(din_list){
  if(is.null(din_list)){
    din_list<-get_din_list_combined()
    din_list<-din_list[["din_list"]]
  }
#match the the ingredients that need to be recoded (improper dosages)
  din_list_cor<-din_list[din_list$Active.Ingredient.Dose!="" & !is.na(din_list$Active.Ingredient.Dose),]
  din_list_cor<-din_list_cor%>%
    select(Active.Ingredient.Dose, DIN.PIN)

  return(din_list_cor)

}