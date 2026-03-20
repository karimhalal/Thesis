#' @title Categorical Dose Function
#'
#' @description Categorizes daily standardized dose equivalents into dose levels
#' based on established guidelines for opioid and benzodiazepine prescribing.
#' Relies on the \code{daily_dose} and \code{class} columns already present in
#' the dataframe produced by \code{calculate_dose_standardization()}.
#' No DIN list lookup or join is required.
#'
#' @param input_data A dataframe containing at minimum \code{daily_dose} (numeric)
#' and \code{class} (character: "Opioid" or "BZD") columns, as produced by
#' \code{calculate_dose_standardization()}.
#'
#' @return The input dataframe with an additional \code{dose_category} column:
#'
#'   **Categories: Opioids (MEQ)**
#'   - 1: Low dose (daily_dose < 50)
#'   - 2: Moderate dose (50 <= daily_dose < 100)
#'   - 3: High dose (daily_dose >= 100)
#'
#'   **Categories: Benzodiazepines (DME)**
#'   - 1: Low dose (daily_dose <= 5)
#'   - 2: Moderate dose (5 < daily_dose < 15)
#'   - 3: High dose (daily_dose >= 15)
#'
#'   - \code{haven::tagged_na("b")}: Missing or invalid dose
#'   - \code{haven::tagged_na("d")}: Class could not be determined
#'
#' @seealso \code{calculate_dose_standardization()}, \code{oral_eq_fun.R},
#' \code{tanspatch_eq_fun.R}
#'
#' @references
#' Dowell D, Haegerich TM, Chou R. CDC Guideline for Prescribing Opioids for
#' Chronic Pain. MMWR Recomm Rep 2016;65(No. RR-1):1-49.
#'
#' @export
dose_cat_fun <- function(input_data) {
  input_data %>%
    dplyr::mutate(
      dose_category = .categorize_by_drug_class(daily_dose, drug_class = class)
    )
}

#' Internal function to categorize dose by drug class
#' @keywords internal
.categorize_by_drug_class <- function(daily_dose, drug_class) {
  dplyr::case_when(
    # Propagate existing tagged NAs
    haven::is_tagged_na(daily_dose, "a") ~ haven::tagged_na("a"),
    haven::is_tagged_na(daily_dose, "b") ~ haven::tagged_na("b"),
    haven::is_tagged_na(daily_dose, "c") ~ haven::tagged_na("c"),

    # Missing or invalid values
    is.na(daily_dose) | daily_dose < 0 ~ haven::tagged_na("b"),

    # Drug class could not be determined
    is.na(drug_class) ~ haven::tagged_na("d"),

    # OPIOID CATEGORIES (MEQ)
    tolower(drug_class) == "opioid" & daily_dose < 50                       ~ 1,
    tolower(drug_class) == "opioid" & daily_dose >= 50 & daily_dose < 100  ~ 2,
    tolower(drug_class) == "opioid" & daily_dose >= 100                     ~ 3,

    # BZD CATEGORIES (DME)
    tolower(drug_class) == "bzd" & daily_dose <= 5                          ~ 1,
    tolower(drug_class) == "bzd" & daily_dose > 5  & daily_dose < 15       ~ 2,
    tolower(drug_class) == "bzd" & daily_dose >= 15                         ~ 3,

    # Default to missing
    .default = haven::tagged_na("b")
  )
}
