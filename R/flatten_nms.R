#Load dependencies
source(here::here("R", "DIN_utils_df.R"))
source(here::here("R", "din_data_builtin.R"))
source(here::here("R", "dose_parsing_fun.R"))
source(here::here("R", "oral_eq_fun.R"))
source(here::here("R", "transpatch_eq_fun.R"))
source(here::here("R", "dose_cat_fun.R"))

#' @title Flatten NMS Prescription Data to One Row Per Patient
#'
#' @description Converts NMS data from long format (one row per prescription) to wide
#' format (one row per patient / IKN). Each prescription becomes a numbered set of
#' columns: {DIN_1}, {STRENGTH_1}, {DAYSSUPL_1}, ..., {DIN_2},
#' {STRENGTH_2}, {DAYSSUPL_2}, etc., ordered chronologically within each patient.
#'
#' All prescription-level columns are preserved. Patients with fewer prescriptions than
#' the maximum will have {NA} in the corresponding numbered columns.
#'
#' @param nms_data [data.frame / tibble] NMS dataset in long format. Must have been processed
#' by dose_parsing_fun (adds Drug.Class, Active.Ing), calculate_dose_standardization (adds
#' daily_dose), and dose_cat_fun (adds dose_category) before being passed here.
#'
#' @param id_col [character] Name of the patient identifier column. Default is "ikn".
#'
#' @param sort_by [character] Name of the column used to order prescriptions within each
#'   patient before pivoting (chronological order). Default is {"dt_of_serv_ts"}.
#'
#' @return A tibble with one row per unique patient. In addition to the numbered
#' prescription columns, the following index-prescription columns are appended immediately
#' after \code{n_prescriptions}:
#' \describe{
#'   \item{first_drug_class}{Binary integer: 1 = opioid, 2 = benzodiazepine, NA = other/unknown.
#'         Derived from Drug.Class of the patient's chronologically first prescription.}
#'   \item{first_dose_category}{Dose category of the first prescription (from dose_cat_fun).}
#'   \item{first_daily_dose}{Standardised daily dose (MEQ or DME) of the first prescription.}
#'   \item{first_active_ing}{Active ingredient of the first prescription (from Active.Ing).}
#' }
#'
#' @details
#' Prescriptions within each patient are sorted chronologically before numbering.
#' Index-prescription variables are extracted from the first (earliest) prescription per patient.
#'
#' To return to long format, use: tidyr::pivot_longer() on the numbered columns.
#'
#' @examples
#' #mock_nms <- generate_mock_nms_data(n_records = 1000, n_patients = 200)
#' #flat <- flatten_nms(mock_nms)
#'
#' # Inspect output
#' #head(flat[, 1:10])
#' #flat$DIN_1
#' #flat$DT_OF_SERV_TS_3  # Date of 3rd prescription (NA if patient has < 3)
#'
#' @export
flatten_nms <- function(nms_data, id_col = "ikn", sort_by = "dt_of_serv_ts") {

  if (!id_col %in% names(nms_data)) {
    stop(sprintf("id_col '%s' not found in nms_data. Available columns: %s",
                 id_col, paste(names(nms_data), collapse = ", ")))
  }

  required_cols <- c("Drug.Class", "dose_category", "daily_dose", "Active.Ing")
  missing_cols  <- required_cols[!required_cols %in% names(nms_data)]
  if (length(missing_cols) > 0) {
    stop(sprintf("Required column(s) not found in nms_data: %s",
                 paste(missing_cols, collapse = ", ")))
  }

  if (!sort_by %in% names(nms_data)) {
    warning(sprintf("sort_by column '%s' not found. Prescriptions will not be sorted.", sort_by))
    sorted_data <- nms_data
  } else {
    sorted_data <- dplyr::arrange(nms_data, .data[[id_col]], .data[[sort_by]])
  }

  rx_cols <- setdiff(names(nms_data), id_col)

  # Count prescriptions per patient before pivoting
  n_rx <- sorted_data %>%
    dplyr::group_by(.data[[id_col]]) %>%
    dplyr::summarise(n_prescriptions = dplyr::n(), .groups = "drop")

  # Extract index-prescription variables from each patient's first (earliest) prescription
  first_rx <- sorted_data %>%
    dplyr::group_by(.data[[id_col]]) %>%
    dplyr::slice(1L) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      first_drug_class = dplyr::case_when(
        tolower(Drug.Class) == "opioid" ~ 1L,
        tolower(Drug.Class) == "bzd"    ~ 2L,
        TRUE                            ~ NA_integer_
      )
    ) %>%
    dplyr::select(
      dplyr::all_of(id_col),
      first_drug_class,
      first_dose_category = dose_category,
      first_daily_dose    = daily_dose,
      first_active_ing    = Active.Ing
    )

  flat <- sorted_data %>%
    dplyr::group_by(.data[[id_col]]) %>%
    dplyr::mutate(.rx_num = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      id_cols     = dplyr::all_of(id_col),
      names_from  = .rx_num,
      values_from = dplyr::all_of(rx_cols),
      names_glue  = "{.value}_{.name}",
      names_vary  = "slowest"
    ) %>%
    dplyr::left_join(n_rx, by = id_col) %>%
    dplyr::relocate(n_prescriptions, .after = dplyr::all_of(id_col)) %>%
    dplyr::left_join(first_rx, by = id_col) %>%
    dplyr::relocate(
      first_drug_class, first_dose_category, first_daily_dose, first_active_ing,
      .after = n_prescriptions
    )

  return(flat)
}


