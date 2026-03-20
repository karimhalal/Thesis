source(here::here("R", "DIN_utils_df.R"))
source(here::here("R", "din_data_builtin.R"))
source(here::here("R", "dose_parsing_fun.R"))
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
#' This function utilizes dependencies from both NMS dataset which are matched to DIN List entries containing additional
#' drug metadata, this uses each drug's unique identification number (DIN) to link both datasets to map each drug
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
#'
#' The total MEQ formula for trandermal opioids is as follows:
#' MEQ (total milligram morphine equivalents) = Q X D X CF X DS
#'
#' The daily MEQ formula for trandermal opioids is as follows:
#' MEQ (daily milligram morphine equivalents) = Q X D X CF
#'
#' Where:
#' Q = quantity of transdermal patches on the NMS claim (in units of administration)
#' D = strength of transdermal drug. A variable derived from NMS through conversion of internal
#' dose variable (character) into a numeric variable (see dose_parse_fun)
#' DS = Days supply of drug- an approximation of number of days. This will either be 2 or 3. If the Dayssupl
#' is exactly 2 then the value inputted into the function would be 2. However, if it is any other value, then
#' the inputted value should be 3.
#' CF= Drug-specific conversion factor derived from Adams et. al (2025) and Gomes et. al (2022)
#'
#'
#' @param DAYSSUPL continuous days supply variable (integer).
#'
#' @param DIN.PIN 8 digit drug identification number. Used in function to map
#' NMS entry to the corresponding drug class and active ingredient in the
#' DIN_list datasheet
#'
#' @param num_dose [numeric] dose of each individual unit of perscription drug
#'
#' @param class drug class. In order for MEQ to be computed, CLASS must=="Opioid"
#'
#' @param QUANTITY the number of units of the drug that are perscribed
#'
#' @param dosage_form the form of the dose (liquid, transdermal, tab, cap). Only transdermal is acceptable for this function
#'
#' @return [numeric ] The calculated daily morphine milligram equivalents (MEQ).
#'         Tagged NA will be returned if input is invalid,
#'
#'           **Process:**
#'          1. Match DIN to conversion factor from reference table
#'          2. Extract days supply and quantity (# of units) corresponding to perscription
#'          3. Derive DS value: 2 if DAYSSUPL == 2, otherwise 3
#'          4. Apply MEQ formula using matched conversion factor
#'          5. Return total and daily MEQ values
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
#' # Example 1: Calculate MEQ for a single transdermal prescription
#' calculate_transpatch_eq(
#'   DIN.PIN = "02245385",
#'   DAYSSUPL = 3,
#'   QUANTITY = 5,
#'   STRENGTH = 25
#' )
#'
#' # Example 2: Invalid DIN returns tagged NA
#' calculate_transpatch_eq(
#' DIN.PIN = "00112345",
#' DAYSSUPL = 3,
#' STRENGTH = 25,
#' QUANTITY = 5
#' )
#' #output: NA(b). haven::is_tagged_na(result, "b") # Shows: TRUE. DIN is not real.

cchs_config <- yaml::yaml.load_file("config.yml", eval.expr = TRUE)

# Load variable sheets using the paths from the config file.
# This ensures you are always using the correct, centrally-defined files.
din_list_transdermal <- read.csv(cchs_config$default$variable$din_list_transdermal, fileEncoding = "UTF-8-BOM")
#'#file management still under development. DIN_list will be called manually
#'
#' # Multiple prescriptions, processing occurs before flat-filing, each row corresponds to unique perscription
#' calculate_transpatch_eq(
#'   DIN.PIN = c("02245385", "02243503", "02245622"),
#'   DAYSSUPL = c(3, 3, 2),
#'   STRENGTH = c(25, 50, 12),
#'   QUANTITY = c(5, 10, 4)
#' )
#'
#' # Database usage: Applied to prescription datasets
#' library(dplyr)
#' # nms_data %>%
#' # mutate(daily_meq = calculate_transpatch_eq(DIN.PIN, DAYSSUPL, STRENGTH, QUANTITY))
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
calculate_transpatch_eq <- function(.data = NULL, DIN.PIN = NULL, DAYSSUPL = NULL, QUANTITY = NULL, STRENGTH = NULL) {
  # Accept either a dataframe or individual vectors
  if (!is.null(.data)) {
    input_data <- .data
  } else {
    input_data <- tibble::tibble(
      DIN.PIN  = DIN.PIN,
      DAYSSUPL = DAYSSUPL,
      QUANTITY = QUANTITY,
      STRENGTH = STRENGTH
    )
  }

  result <- input_data %>%
    dplyr::mutate(
      # Derive DS: transdermal patches last either 2 or 3 days
      ds_value = dplyr::if_else(DAYSSUPL == 2, 2, 3),

      daily_dose = dplyr::case_when(
        # Missing or invalid DIN
        is.na(DIN.PIN) | DIN.PIN == "" ~ haven::tagged_na("b"),

        # Missing or invalid DAYSSUPL
        is.na(DAYSSUPL) | DAYSSUPL <= 0 ~ haven::tagged_na("b"),

        # Missing or invalid QUANTITY
        is.na(QUANTITY) | QUANTITY <= 0 ~ haven::tagged_na("b"),

        # Missing or invalid STRENGTH
        is.na(STRENGTH) | STRENGTH <= 0 ~ haven::tagged_na("b"),

        # DIN not found in conversion table (conversion_factor will be NA).
        # Caused by error in DIN entry or exclusions
        is.na(conversion_factor) ~ haven::tagged_na("b"),

        # Calculate total MEQ: (Quantity × Strength × Conversion Factor)
        TRUE ~ (QUANTITY * STRENGTH * conversion_factor),

        # Default to missing
        .default = haven::tagged_na("b")
      ),

      total_dose = dplyr::case_when(
        # Missing or invalid DIN
        is.na(DIN.PIN) | DIN.PIN == "" ~ haven::tagged_na("b"),

        # Missing or invalid DAYSSUPL
        is.na(DAYSSUPL) | DAYSSUPL <= 0 ~ haven::tagged_na("b"),

        # Missing or invalid QUANTITY
        is.na(QUANTITY) | QUANTITY <= 0 ~ haven::tagged_na("b"),

        # Missing or invalid STRENGTH
        is.na(STRENGTH) | STRENGTH <= 0 ~ haven::tagged_na("b"),

        # DIN not found in conversion table (conversion_factor will be NA).
        # Caused by error in DIN entry or exclusions
        is.na(conversion_factor) ~ haven::tagged_na("b"),

        # Calculate daily MEQ: (Quantity × Strength × Conversion Factor × DS)
        TRUE ~ (QUANTITY * STRENGTH * conversion_factor * ds_value),

        # Default to missing
        .default = haven::tagged_na("b")
      )
    )

  return(dplyr::select(result, -ds_value))
}
