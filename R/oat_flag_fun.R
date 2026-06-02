# Flag OAT prescriptions occurring after non-OAT opioid initiation.
#
# OAT = any prescription whose Active.Ing contains "methadone" or "buprenorphine",
# excluding pain-use buprenorphine products identified by din_desc.
# Opioid initiation = patient's earliest Drug.Class == "opioid" row that is NOT OAT.
#
# Returns a separate one-row-per-patient dataset for joining to the flattened NMS:
#   ikn                   - patient identifier
#   opioid_init_date      - date of first non-OAT opioid prescription (NA if none)
#   oat_after_opioid_init - 1 = OAT followed initiation; 0 = no OAT followed; NA = no initiation
#   oat_first_rx_date     - date of first OAT prescription after initiation (NA otherwise)
#   oat_din_desc          - din_desc of that first qualifying OAT prescription (NA otherwise)
#
# Intended call order: dose_parsing_fun --> ... --> flag_oat_after_opioid_init (separate)
#                      --> flatten_nms --> join oat_flags + co_prescription to flat NMS

flag_oat_after_opioid_init <- function(
    nms_data,
    id_col       = "ikn",
    date_col     = "dt_of_serv_ts",
    class_col    = "Drug.Class",
    active_col   = "Active.Ing",
    desc_col     = "din_desc",
    oat_desc_exclude = c("butrans5", "butrans10", "butrans15", "cophylac drops")
) {

  required_cols <- c(id_col, date_col, class_col, active_col, desc_col)
  missing_cols  <- required_cols[!required_cols %in% names(nms_data)]
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "flag_oat_after_opioid_init: required column(s) not found in nms_data: %s",
      paste(missing_cols, collapse = ", ")
    ))
  }

  oat_pattern     <- "methadone|buprenorphine"
  exclude_pattern <- paste(oat_desc_exclude, collapse = "|")

  tagged <- nms_data %>%
    dplyr::mutate(
      .is_oat           = grepl(oat_pattern, .data[[active_col]], ignore.case = TRUE) &
                          !grepl(exclude_pattern, .data[[desc_col]], ignore.case = TRUE),
      .is_nonoat_opioid = tolower(.data[[class_col]]) == "opioid" & !.is_oat,
      .date             = as.Date(.data[[date_col]])
    )

  # First non-OAT opioid date per patient
  opioid_init <- tagged %>%
    dplyr::filter(.is_nonoat_opioid) %>%
    dplyr::group_by(.data[[id_col]]) %>%
    dplyr::summarise(opioid_init_date = min(.date, na.rm = TRUE), .groups = "drop")

  # First OAT date and din_desc strictly after opioid initiation per patient
  oat_after <- tagged %>%
    dplyr::filter(.is_oat) %>%
    dplyr::left_join(opioid_init, by = id_col) %>%
    dplyr::filter(!is.na(opioid_init_date) & .date > opioid_init_date) %>%
    dplyr::group_by(.data[[id_col]]) %>%
    dplyr::arrange(.date, .by_group = TRUE) %>%
    dplyr::slice(1L) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(c(id_col, desc_col)), oat_first_rx_date = .date) %>%
    dplyr::rename(oat_din_desc = dplyr::all_of(desc_col))

  patient_summary <- opioid_init %>%
    dplyr::left_join(oat_after, by = id_col) %>%
    dplyr::mutate(
      oat_after_opioid_init = dplyr::case_when(
        is.na(opioid_init_date)   ~ NA_integer_,
        !is.na(oat_first_rx_date) ~ 1L,
        TRUE                      ~ 0L
      )
    )

  patient_summary
}