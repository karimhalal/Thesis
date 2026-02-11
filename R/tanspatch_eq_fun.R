#' @title Transdermal Standardized Daily Equivalents
#'
#' @description This function creates a derived variable for average daily milligram 
#' morphine equivalents (MEQ) for transdermal patches in opioids
#' 
#' MEQ is a standardized measure of opioid content allowing for direct comparisons of potency between
#' drugs of different strengths and formulations. This is done through the conversion
#' of their dose to their equivalent amount in milligrams of morphine.
#' 
#' 
#' Due to difference in applications and dosing, this function only applies to drugs with trandermal 
#' routes of administration
#' 
#'
#' This function utilizes dependencies from both NMS dataset and DIN_list transdermal subset, using
#' each drug's unique identification number (DIN) to link both datasets to map each drug
#' in NMS dataset to its corresponding characteristics in din_list_transdermal.
#' 
#' @details This function implements the standard MEQ calculation used in opioid prescription monitoring:
#'
#'          **Clinical Significance:**
#'          MEQ calculations are essential for:
#'          - Monitoring opioid prescription patterns
#'          - Identifying high-dose opioid use
#'          - Overdose risk assessment
#'          - Clinical decision support
#' 
#' The MEQ formula for trandermal opioids is as follows:
#'
#' MEQ (daily milligram morphine equivalents) = (Q X D X CF) / DS
#' 
#' Where:
#' Q = qunaitty on the NMS claim (in units of administration)
#' D = strength of transdermal drug. A variable derived from NMS through conversion of internal
#' dose variable (character) into a numeric variable (see dose_conv_func)
#' DS = Days supply of drug- an apporximation of number of days 
#' CF= Drug-specific conversion factor derived from Adams et. al (2025) and Gomes et. al (2022)
#'
#'
#' @param DAYSSUPL continuous days supply variable (integer).
#'
#' @param DIN 8 digit drug identification number. Used in function to map
#' NMS entry to the corresponding drug class and active ingredient in the 
#' DIN_list datasheet
#'
#' @param num_dose [numeric] dose of each individual unit of perscription drug
#'
#' @param class drug class. In order for MEQ to be computed, CLASS must=="Opioid"
#'
#' @param QUANTITY the number of units of the drug that are perscribed 
#' 
#' @return [numeric ] The calculated daily morphine milligram equivalents (MEQ).
#'         Tagged NA will be returned if input is invalid, DIN is not found due to
#'         drug exclusions or record-keeping errors, or if there is missing data.
#'
#' 
#'           **Process:**
#'          1. Match DIN to conversion factor from reference table
#'          2. Extract days supply and quantity (# of units) corresponding to perscription
#'          3. Apply MEQ formula using matched conversion factor
#'          4. Return daily MEQ value
#'
#'        
#'          **Missing Data Handling:**
#'          - Invalid or missing DIN: returns `haven::tagged_na("b")`
#'          - Missing DAYS_SUPPLY, quantity or STRENGTH: returns `haven::tagged_na("b")`
#'          - DIN not found in conversion table: returns `haven::tagged_na("b")`
#' 
#' 
#' @examples 
#' # Scalar usage: Single prescription
#' # Example 1: Calculate MEQ for a single prescription
#' oral_eq_fun(
#'   DIN = "02238645",
#'   DAYS_SUPPLY = 5,
#'   Quantity= 20,
#'   STRENGTH = 30
#' )
#' #output: 18 MEQ/day
#' 
#' 
#' # Example 2: Invalid DIN returns tagged NA
#' oral_eq_fun(
#' DIN = "00112345", 
#' DAYS_SUPPLY = 30, 
#' STRENGTH = 5, 
#' QUANTITY=10
#' )
#' #output: NA(b). haven::is_tagged_na(result, "b") # Shows: TRUE. DIN is not real.

cchs_config<-yaml::yaml.load_file("config.yml", eval.expr= TRUE)

# Load variable sheets using the paths from the config file.
# This ensures you are always using the correct, centrally-defined files.
din_list <- read.csv(cchs_config$default$variable$din_list_oral, fileEncoding = "UTF-8-BOM")
#'#file management still under development. DIN_list will be called manually
#'#
#' # Multiple prescriptions, processing occurs before flat-filing, each row corresponds to unique perscription
#' calculate_meq(
#'   DIN = c("02238645", "02243503", "02245622"),
#'   DAYS_SUPPLY = c(30, 30, 15),
#'   STRENGTH = c(5, 10, 20),
#'   QUANTITY= c(10,15,20)
#' )
#'
#' # Database usage: Applied to prescription datasets
#' library(dplyr)
#' # nms_data %>%
#' # mutate(daily_meq = calculate_meq(DIN, DAYS_SUPPLY, STRENGTH, QUANTITY))
#'
#' @references 
#' 
#' The Ontario Drug Policy Research Network. (2022). 
#' ODPRN suggested calculation of opioid milligrams of morphine equivalents.
#' https://odprn.ca/wp-content/uploads/2020/11/Opioid-Milligrams-of-Morphine-Equivalents_FINAL.pdf
#'
#' Adams et. al. The Journal of the International Association for the Study of Pain.(2025)
#' Standardizing research methods for opioid dose comparison: the NIH HEAL morphine milligram morphine
#' equivalent calculator.
#' DOI: 10.1097/j.pain.0000000000003529
#' 
#' @export
calculate_oral_eq <- function(DIN, DAYSSUPL, QUANTITY, STRENGTH) {
  #Create a data frame from inputs for easier matching
input_data <- tibble::tibble(
    DIN.PIN = DIN,
    DAYSSUPL = DAYSSUPL,
    QUANTITY = QUANTITY,
    STRENGTH = STRENGTH
  )
  
  # Join with conversion factors from din_list and calculate MEQ
  result <- input_data %>%
    dplyr::left_join(din_list, by = "DIN.PIN") %>%
    dplyr::mutate(
      MEQ = dplyr::case_when(
        # Missing or invalid DIN
        is.na(DIN) | DIN == "" ~ haven::tagged_na("b"),
        
        # Missing or invalid DAYSSUPL
        is.na(DAYSSUPL) | DAYSSUPL <= 0 ~ haven::tagged_na("b"),
        
        # Missing or invalid QUANTITY
        is.na(QUANTITY) | QUANTITY <= 0 ~ haven::tagged_na("b"),
        
        # Missing or invalid STRENGTH
        is.na(STRENGTH) | STRENGTH <= 0 ~ haven::tagged_na("b"),
        
        # DIN not found in conversion table (CONVERSION_FACTOR will be NA). 
        #Cause by error in DIN entry or
        is.na(conversion_factor) ~ haven::tagged_na("b"),

        #Limit to only the


        
        # Calculate MEQ: (Quantity × Strength × Conversion Factor) / Days Supply
        TRUE ~ (QUANTITY * STRENGTH * conversion_factor) / DAYSSUPL,
        
        # Default to missing
        .default = haven::tagged_na("b")
      )
    ) %>%
    dplyr::pull(MEQ)
  
  return(result)
}