#' @title Categorical Dose Function (cumulative)
#'
#' @description This function categorizes equivalent units into dose levels
#'              based on established guidelines for opioid and benzodiazepine prescribing.
#'
#' @param meq_daily [numeric] A numeric representing the daily standardized milligram equivalents
#' (see oral_eq_fun and transpatch_eq_fun).
#'
#' @param DIN [integer] Drug identification number used to map to DIN_list to determine drug class
#' for appropriate category mapping
#' 
#' 
#' @return [integer] The MEQ dose category:
#' 
#'           **Categories: Opiates**
#'   - 1: Low dose (meq_daily < 50)
#'   - 2: Moderate dose (50 <= meq < 100)
#'   - 3: High dose (MEQ >= 100)
#'   - `haven::tagged_na("b")`: Missing
#' 
#' #'           **Categories: Benzodiazepines**
#'   - 1: Low dose (meq_daily < 5)
#'   - 2: Moderate dose (5 < meq_daily < 15)
#'   - 3: High dose (meq_daily >= 15)
#'   - `haven::tagged_na("b")`: Missing
#'
#' @details This function applies established guidelines to categorize opioid/benzodiazepine prescription risk levels.
#'
#'
#'          **Missing Data Codes:**
#'          - Propagates tagged NAs from the input `daily_meq`.
#'
#' @examples
#' # Scalar usage: Single prescription
#' dose_cat_fun(DIN, daily_meq)
#' dose_cat_fun(02245284, 45) # Returns: 1
#' dose_cat_fun(02245284, 75) # Returns: 2
#' categorize_MEQ_risk(,25) # Returns: 3
#'
#' # Multiple prescriptions
#' categorize_MEQ_risk(DIN= c(02245284, 02245284,),daily _meq= c(45, 75, 25))
#' # Returns: c(1, 2, 3)
#'
#' # Database usage
#' library(dplyr)
#' # nms_data %>%
#' #   mutate(meq_risk = categorize_MEQ_risk(meq))
#'
#' @seealso [calculate_MEQ()]
#' @references 
#' Dowell D, Haegerich TM, Chou R. CDC Guideline for Prescribing Opioids for Chronic Pain.
#' MMWR Recomm Rep 2016;65(No. RR-1):1–49.
#' 
#'
#' @export
dose_cat_fun() <- function(meq_daily, DIN, DIN_LIST=NULL) {
  
  # Load DIN list if not provided
  if (is.null(din_list)) {
    din_list <- get_din_list_combined()  # Loads all drug classes
  }
  
  # Create input data frame to extract required columns from dataset
  input_data <- tibble::tibble(
    DIN.PIN = DIN,
    meq_daily = meq_daily
  )
  
  # Join with DIN list to get drug class
  result <- input_data %>%
    dplyr::left_join(
      din_list %>% 
        dplyr::select(DIN.PIN, Active.Ingredient.Class.and.Use),
      by = "DIN.PIN"
    ) %>%
    dplyr::mutate(
      # create temporary drug class variable from notes enclosed in the Active ingredient class and use column in datasheet
      drug_class = dplyr::case_when(
        is.na(Active.Ingredient.Class.and.Use) ~ NA_character_,
        grepl("opioid", Active.Ingredient.Class.and.Use, ignore.case = TRUE) ~ "opioid",
        grepl("bzd", Active.Ingredient.Class.and.Use, ignore.case = TRUE) ~ "bzd",
        TRUE ~ "other"
      ),
      
      # Categorize based on drug class and dose
      dose_category = .categorize_by_drug_class(meq_daily, drug_class)
    ) %>%
    dplyr::pull(dose_category)
  
  return(result)
}

#' Internal function to categorize dose by drug class
#' @keywords internal
.categorize_by_drug_class <- function(meq_daily, drug_class) {
  dplyr::case_when(
    # Propagate existing tagged NAs
    haven::is_tagged_na(meq_daily, "a") ~ haven::tagged_na("a"),
    haven::is_tagged_na(meq_daily, "b") ~ haven::tagged_na("b"),
    haven::is_tagged_na(meq_daily, "c") ~ haven::tagged_na("c"),
    
    # Missing or invalid values
    is.na(meq_daily) ~ haven::tagged_na("b"),
    meq_daily < 0 ~ haven::tagged_na("b"),
    
    # Drug class could not be determined
    is.na(drug_class) | drug_class == "other" ~ haven::tagged_na("d"),
    
    # === OPIOID CATEGORIES (MEQ) ===
    drug_class == "opioid" & meq_daily < 50 ~ 1L,
    drug_class == "opioid" & meq_daily >= 50 & meq_daily < 100 ~ 2L,
    drug_class == "opioid" & meq_daily >= 100 ~ 3L,
    
    # === BENZODIAZEPINE CATEGORIES (DME) ===
    drug_class == "bzd" & meq_daily <= 5 ~ 1L,
    drug_class == "bzd" & meq_daily > 5 & meq_daily < 15 ~ 2L,
    drug_class == "bzd" & meq_daily >= 15 ~ 3L,
    
    # Default to missing
    .default = haven::tagged_na("b")
  )
}
