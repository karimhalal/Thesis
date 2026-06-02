source(here::here("R", "oral_eq_fun.R"))
source(here::here("R", "transpatch_eq_fun.R"))

#' @title Dose Standardization Function
#'
#' @description Wrapper function that routes each prescription row to the
#' appropriate equivalence calculation and returns harmonized total_dose and 
#' daily_dose columns to the existing dataframes
#'
#' Routing logic:
#' {Transdermal opioid}:  class == "Opioid"} and
#'         dosage_form == "transdermal"
#'         Standardization is handled by: calculate_transpatch_eq()}
#'  {All other rows}: handled by: calculate_oral_eq()
#' }
#'
#' Both sub-functions are called with the full dataframe slice so that all
#' original columns are preserved. The results are recombined in the original
#' row order.
#'
#' @param .data A dataframe containing at minimum the columns din,
#'   DAYSSUPL, QUANTITY, STRENGTH, conversion_factor,
#'   class, and dosage_form.
#'
#' @return The input dataframe with two additional columns:
#' total_dose: Total standardized dose (MEQ, DME, milligram methylphenidate or milligram amphetamine) for the prescription
#'     period.
#' daily_dose: Average daily standardized dose (MEQ, DME, milligram methylphenidate or milligram amphetamine) per day of perscription
#'
#' 
#' @seealso calculate_oral_eq.R, and calculate_transpatch_eq.R
#'
#' @export
calculate_dose_standardization <- function(input_data) {
  # Preserve original row order
  input_data <- dplyr::mutate(input_data, .row_id = dplyr::row_number())

  # Routing condition: transdermal opioids vs everything else
  is_transpatch <- !is.na(input_data$class) &
    !is.na(input_data$dose_group) &
    input_data$class      == "Opioid" &
    input_data$dose_group == "transdermal"

  data_transpatch <- input_data[is_transpatch, ]
  data_oral       <- input_data[!is_transpatch, ]

  result_transpatch <- calculate_transpatch_eq(.data = data_transpatch)
  result_oral       <- calculate_oral_eq(.data = data_oral)

  # Recombine and restore original row order, then drop helper column
  dplyr::bind_rows(result_transpatch, result_oral) %>%
    dplyr::arrange(.row_id) %>%
    dplyr::select(-.row_id)
}
