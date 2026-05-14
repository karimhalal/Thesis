#' @title Derive Co-Prescription Flags from NMS Long-Format Data
#'
#' @description Operates on long-format NMS data (one row per prescription) and
#' returns a patient-level tibble with five derived variables:
#'
#' ######OUTPUTTED VALUES########
#' 
#' red_flag: {Potential inappropriate use: patient received >=2 same-class
#'     prescriptions that (a) overlap the baseline by >=7 days, (b) originate from a
#'     different prescriber than the baseline, and (c) both meet the quantity threshold
#'     (oral >=30 units; transdermal patch >=6 units).}
#' 
#' concurrent_use: {Numeric 1-5. Drug-class combination present among prescriptions
#'     that overlap the baseline by >=7 days (including the baseline itself):
#'     1=none, 2=BZD+Opioid, 3=Opioid+Stimulant, 4=BZD+Stimulant, 5=all three.}
#' 
#' baseline_within_2yr: Baseline prescription falls within +-1 year of index_date (survey admin)
#' 
#' baseline_within_4yr: Baseline prescription falls within +-2 years of index_date
#' 
#' baseline_within_6yr: Baseline prescription falls within +-3 years of index_date
#' 
#'
#' Applyflatten_nms() to the NMS data after this function; join the
#' returned patient-level table to the flattened output by IKN
#'
#' @param nms_data      [data.frame / tibble] Long-format NMS data (one row per prescription).
#' @param id_col        [character] Patient identifier column. Default="ikn"
#' @param date_col      [character] Prescription start date column (R Date). Default" "dt_of_serv_ts".
#' @param days_col      [character] Days supply column (integer). Default: "dayssupl".
#' @param class_col     [character] Drug class column. 
#' @param form_col      [character] Dosage form column.
#' @param qty_col       [character] Quantity/units column (numeric). Default: "quantity"
#' @param prescriber_col [character] Prescriber/agency identifier column. Default: agency_id_enc}.
#' @param index_col     [character] CCHS survey index date column (R Date). Default: "index_date".
#'
#' @return A tibble with one row per unique patient containing columns:
#'   
#'
#' @export
co_prescription_fun <- function(nms_data,
                                id_col         = "ikn",
                                date_col       = "dt_of_serv_ts",
                                days_col       = "dayssupl",
                                class_col      = "Drug.Class",
                                form_col       = "Dosage.Form",
                                qty_col        = "quantity",
                                prescriber_col = "agency_id_enc",
                                index_col      = "index_date") {

  #valdiate inputs
  required_cols <- c(id_col, date_col, days_col, class_col,
                     form_col, qty_col, prescriber_col, index_col)
  missing_cols  <- setdiff(required_cols, names(nms_data))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }

  #Step 1: Compute prescription windows 
  data <- nms_data %>%
    dplyr::mutate(
      .row_id  = dplyr::row_number(),
      rx_start = .data[[date_col]],
      rx_end   = .data[[date_col]] + .data[[days_col]]
    )

  #Step 2: Identify baseline
  rename_map <- c(
    ".base_row_id"    = ".row_id",
    "base_start"      = "rx_start",
    "base_end"        = "rx_end",
    "base_class"      = class_col,
    "base_form"       = form_col,
    "base_qty"        = qty_col,
    "base_prescriber" = prescriber_col,
    "base_index_date" = index_col
  )

  baseline <- data %>%
    dplyr::group_by(.data[[id_col]]) %>%
    dplyr::arrange(.data[[date_col]], .by_group = TRUE) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      dplyr::all_of(id_col), .row_id, rx_start, rx_end,
      dplyr::all_of(c(class_col, form_col, qty_col, prescriber_col, index_col))
    ) %>%
    dplyr::rename(!!!rename_map)

  # Step 3: Join all prescriptions to their baseline; compute overlap 
  joined <- data %>%
    dplyr::left_join(baseline, by = id_col) %>%
    dplyr::mutate(
      overlap_days = as.numeric(
        pmin(rx_end, base_end) - pmax(rx_start, base_start)
      ),
      is_baseline = .row_id == .base_row_id
    )

  # Step 4: Derive red_flag
  # Qualifying pair (baseline + candidate): same Drug.Class, different prescriber,
  # >=7 days overlap, quantity threshold met on BOTH prescriptions.
  red_flag_tbl <- joined %>%
    dplyr::filter(
      !is_baseline,
      .data[[class_col]]      == base_class,
      .data[[prescriber_col]] != base_prescriber,
      overlap_days >= 7,
      dplyr::if_else(
        tolower(base_form) == "trans patch",
        base_qty >= 6,
        base_qty >= 30
      ),
      dplyr::if_else(
        tolower(.data[[form_col]]) == "trans patch",
        .data[[qty_col]] >= 6,
        .data[[qty_col]] >= 30
      )
    ) %>%
    dplyr::group_by(.data[[id_col]]) %>%
    dplyr::summarise(red_flag = TRUE, .groups = "drop")

  # Step 5: Derive concurrent_use table
  # Concurrent = non-baseline prescription overlapping baseline by >=7 days.
  # Value is determined by which drug classes are present (baseline class included).
  concurrent_classes_tbl <- joined %>%
    dplyr::filter(!is_baseline, overlap_days >= 7) %>%
    dplyr::group_by(.data[[id_col]]) %>%
    dplyr::summarise(
      conc_classes = list(unique(tolower(.data[[class_col]]))),
      .groups = "drop"
    )

  concurrent_use_tbl <- baseline %>%
    dplyr::select(dplyr::all_of(id_col), base_class) %>%
    dplyr::left_join(concurrent_classes_tbl, by = id_col) %>%
    dplyr::mutate(
      # Flatten list-column; patients with no concurrent prescriptions get empty vector
      conc_classes = purrr::map(conc_classes, function(x) {
        vals <- unlist(x)
        vals[!is.na(vals)]
      }),
      all_classes   = purrr::map2(
        tolower(base_class), conc_classes,
        ~ unique(c(.x, .y))
      ),
      has_bzd       = purrr::map_lgl(all_classes, ~ "bzd"       %in% .x),
      has_opioid    = purrr::map_lgl(all_classes, ~ "opioid"    %in% .x),
      has_stimulant = purrr::map_lgl(all_classes, ~ "stimulant" %in% .x),
      concurrent_use = dplyr::case_when(
        has_bzd & has_opioid & has_stimulant ~ 5L,
        has_bzd & has_opioid                 ~ 2L,
        has_opioid & has_stimulant           ~ 3L,
        has_bzd & has_stimulant              ~ 4L,
        TRUE                                 ~ 1L
      )
    ) %>%
    dplyr::select(dplyr::all_of(id_col), concurrent_use)

  # --- Step 6: Derive CCHS proximity flags ------------------------------------
  proximity_tbl <- baseline %>%
    dplyr::mutate(
      .days_diff          = abs(as.numeric(base_start - base_index_date)),
      baseline_within_2yr = .days_diff <= 365,
      baseline_within_4yr = .days_diff <= 730,
      baseline_within_6yr = .days_diff <= 1095
    ) %>%
    dplyr::select(
      dplyr::all_of(id_col),
      baseline_within_2yr, baseline_within_4yr, baseline_within_6yr
    )

  # output patient level data
  baseline %>%
    dplyr::select(dplyr::all_of(id_col)) %>%
    dplyr::left_join(red_flag_tbl,       by = id_col) %>%
    dplyr::mutate(red_flag = tidyr::replace_na(red_flag, FALSE)) %>%
    dplyr::left_join(concurrent_use_tbl, by = id_col) %>%
    dplyr::left_join(proximity_tbl,      by = id_col)
}
