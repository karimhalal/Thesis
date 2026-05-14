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
#' @param nms_data [data.frame / tibble] NMS dataset in long format. This data has been subject
#' to dose_parsing_fun and has had 
#'
#' @param id_col [character] Name of the patient identifier column. Default is "IKN".
#'
#' @param sort_by [character] Name of the column used to order prescriptions within each
#'   patient before pivoting (chronological order). Default is {"DT_OF_SERV_TS"}.
#'
#' @return A tibble with one row per unique patient.
#'
#'
#' @details
#' Prescriptions within each patient are sorted by perscription date by default before numbering
#'
#' To return to long format, use: tidyr::pivot_longer() on the joined columns.
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
    dplyr::relocate(n_prescriptions, .after = dplyr::all_of(id_col))

  return(flat)
}

flatten_nms<-function(nms_data=NULL, id_col=NULL, sort_col=NULL){
  
  #SANITY CHECKS
  if(!id_col%in% names(nms_data)){
    warning(sprintf("Column '%s' not found for id generation", id_col))
  }
  if(!sort_col %in% names(nms_data)){
    warning(sprintf("Column '%s' not found to for sorting of grouped data"))
  }
  else{
    sorted<- dplyr::arrange(nms_data, .data[[id_col]], .data[[sort_col]])
  }
}

