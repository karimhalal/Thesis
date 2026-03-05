library(dplyr)
library(haven)

is_na_a <- function(x) {
  return(x == 6 | x == 96 | is.na(x) & haven::is_tagged_na(x, "a"))
}

is_na_b <- function(x) {
  return(x %in% c(7, 8, 9, 97, 98, 99) | (is.na(x) & haven::is_tagged_na(x, "b")))
}

na_a <- function() {
  return(haven::tagged_na("a"))
}

na_b <- function() {
  return(haven::tagged_na("b"))
}

recode_DHH_SEX <- function(x) {
  dplyr::case_when(
    x == 1 ~ 1L,
    x == 2 ~ 2L,
    x == 6 ~ na_a(),
    x %in% c(7, 8, 9) ~ na_b(),
    TRUE ~ na_b()
  )
}

recode_DHHGAGE_D <- function(x, cycle) {
  if (cycle %in% c("cchs2001_p", "cchs2003_p")) {
    dplyr::case_when(
      x %in% c(1, 2) ~ 1L,
      x %in% c(3, 4) ~ 2L,
      x %in% c(5, 6) ~ 3L,
      x %in% c(7, 8) ~ 4L,
      x %in% c(9, 10) ~ 5L,
      x %in% c(11, 12) ~ 6L,
      x %in% c(13, 14) ~ 7L,
      x == 15 ~ 8L,
      x == 96 ~ na_a(),
      x %in% c(97, 98, 99) ~ na_b(),
      TRUE ~ na_b()
    )
  } else if (cycle %in% c("cchs2005_p", "cchs2007_2008_p", "cchs2009_2010_p",
                          "cchs2010_p", "cchs2011_2012_p", "cchs2012_p",
                          "cchs2013_2014_p", "cchs2014_p", "cchs2015_2016_p",
                          "cchs2017_2018_p")) {
    dplyr::case_when(
      x %in% c(1, 2, 3) ~ 1L,
      x %in% c(4, 5) ~ 2L,
      x %in% c(6, 7) ~ 3L,
      x %in% c(8, 9) ~ 4L,
      x %in% c(10, 11) ~ 5L,
      x %in% c(12, 13) ~ 6L,
      x %in% c(14, 15) ~ 7L,
      x == 16 ~ 8L,
      x == 96 ~ na_a(),
      x %in% c(97, 98, 99) ~ na_b(),
      TRUE ~ na_b()
    )
  } else if (cycle %in% c("cchs2009_s", "cchs2010_s", "cchs2012_s")) {
    dplyr::case_when(
      x >= 12 & x < 20 ~ 1L,
      x >= 20 & x < 30 ~ 2L,
      x >= 30 & x < 40 ~ 3L,
      x >= 40 & x < 50 ~ 4L,
      x >= 50 & x < 60 ~ 5L,
      x >= 60 & x < 70 ~ 6L,
      x >= 70 & x < 80 ~ 7L,
      x >= 80 & x <= 102 ~ 8L,
      x == 96 ~ na_a(),
      x %in% c(97, 98, 99) ~ na_b(),
      TRUE ~ na_b()
    )
  } else {
    na_b()
  }
}

recode_DHHGAGE_cont <- function(x) {
  dplyr::case_when(
    x == 1 ~ 15.5,
    x == 2 ~ 24.5,
    x == 3 ~ 34.5,
    x == 4 ~ 44.5,
    x == 5 ~ 54.5,
    x == 6 ~ 64.5,
    x == 7 ~ 74.5,
    x == 8 ~ 85.0,
    TRUE ~ NA_real_
  )
}

recode_ALCDTTM <- function(x, cycle) {
  if (cycle %in% c("cchs2001_p", "cchs2003_p", "cchs2005_p")) {
    dplyr::case_when(
      x == 1 ~ 1L,
      x == 2 ~ 2L,
      x == 3 ~ 3L,
      x == 4 ~ 3L,
      x == 6 ~ na_a(),
      x %in% c(7, 8, 9) ~ na_b(),
      TRUE ~ na_b()
    )
  } else {
    dplyr::case_when(
      x == 1 ~ 1L,
      x == 2 ~ 2L,
      x == 3 ~ 3L,
      x == 6 ~ na_a(),
      x %in% c(7, 8, 9) ~ na_b(),
      TRUE ~ na_b()
    )
  }
}

recode_ALW_1 <- function(x) {
  dplyr::case_when(
    x == 1 ~ 1L,
    x == 2 ~ 2L,
    x == 6 ~ na_a(),
    x %in% c(7, 8, 9) ~ na_b(),
    TRUE ~ na_b()
  )
}

recode_CCC_generic <- function(x) {
  dplyr::case_when(
    x == 1 ~ 1L,
    x == 2 ~ 2L,
    x == 6 ~ na_a(),
    x %in% c(7, 8, 9) ~ na_b(),
    TRUE ~ na_b()
  )
}

recode_CCC_031 <- recode_CCC_generic
recode_CCC_051 <- recode_CCC_generic
recode_CCC_071 <- recode_CCC_generic
recode_CCC_091 <- recode_CCC_generic
recode_CCC_101 <- recode_CCC_generic
recode_CCC_121 <- recode_CCC_generic
recode_CCC_131 <- recode_CCC_generic
recode_CCC_151 <- recode_CCC_generic
recode_CCC_171 <- recode_CCC_generic
recode_CCC_280 <- recode_CCC_generic
recode_CCC_290 <- recode_CCC_generic

recode_ADL_generic <- function(x) {
  dplyr::case_when(
    x == 1 ~ 1L,
    x == 2 ~ 2L,
    x == 6 ~ na_a(),
    x %in% c(7, 8, 9) ~ na_b(),
    TRUE ~ na_b()
  )
}

recode_ADL_01 <- recode_ADL_generic
recode_ADL_02 <- recode_ADL_generic
recode_ADL_03 <- recode_ADL_generic
recode_ADL_04 <- recode_ADL_generic
recode_ADL_05 <- recode_ADL_generic

recode_GEN_01 <- function(x) {
  dplyr::case_when(
    x == 1 ~ 1L,
    x == 2 ~ 2L,
    x == 3 ~ 3L,
    x == 4 ~ 4L,
    x == 5 ~ 5L,
    x == 6 ~ na_a(),
    x %in% c(7, 8, 9) ~ na_b(),
    TRUE ~ na_b()
  )
}

recode_GEN_02B <- function(x) {
  dplyr::case_when(
    x == 1 ~ 1L,
    x == 2 ~ 2L,
    x == 3 ~ 3L,
    x == 4 ~ 4L,
    x == 5 ~ 5L,
    x == 6 ~ na_a(),
    x %in% c(7, 8, 9) ~ na_b(),
    TRUE ~ na_b()
  )
}

recode_GEN_07 <- function(x) {
  dplyr::case_when(
    x == 1 ~ 1L,
    x == 2 ~ 2L,
    x == 3 ~ 3L,
    x == 4 ~ 4L,
    x == 5 ~ 5L,
    x == 6 ~ na_a(),
    x %in% c(7, 8, 9) ~ na_b(),
    TRUE ~ na_b()
  )
}

recode_GEN_10 <- function(x) {
  dplyr::case_when(
    x == 1 ~ 1L,
    x == 2 ~ 2L,
    x == 3 ~ 3L,
    x == 4 ~ 4L,
    x == 6 ~ na_a(),
    x %in% c(7, 8, 9) ~ na_b(),
    TRUE ~ na_b()
  )
}

recode_GEOGPRV <- function(x) {
  dplyr::case_when(
    x == 10 ~ 10L,
    x == 11 ~ 11L,
    x == 12 ~ 12L,
    x == 13 ~ 13L,
    x == 24 ~ 24L,
    x == 35 ~ 35L,
    x == 46 ~ 46L,
    x == 47 ~ 47L,
    x == 48 ~ 48L,
    x == 59 ~ 59L,
    x == 60 ~ 60L,
    x == 96 ~ na_a(),
    x %in% c(97, 98, 99) ~ na_b(),
    TRUE ~ na_b()
  )
}

recode_DHHGMS <- function(x) {
  dplyr::case_when(
    x == 1 ~ 1L,
    x == 2 ~ 1L,
    x == 3 ~ 2L,
    x == 4 ~ 2L,
    x == 5 ~ 2L,
    x == 6 ~ 3L,
    x == 96 ~ na_a(),
    x %in% c(97, 98, 99) ~ na_b(),
    TRUE ~ na_b()
  )
}

recode_EDUDR03 <- function(x, cycle) {
  if (cycle %in% c("cchs2015_2016_p", "cchs2017_2018_p")) {
    dplyr::case_when(
      x == 1 ~ 1L,
      x == 2 ~ 2L,
      x == 3 ~ 3L,
      x == 6 ~ na_a(),
      x %in% c(7, 8, 9) ~ na_b(),
      TRUE ~ na_b()
    )
  } else {
    dplyr::case_when(
      x == 1 ~ 1L,
      x == 2 ~ 2L,
      x == 3 ~ 3L,
      x == 4 ~ 3L,
      x == 6 ~ na_a(),
      x %in% c(7, 8, 9) ~ na_b(),
      TRUE ~ na_b()
    )
  }
}

recode_SMKDSTY_cat5 <- function(x, cycle) {
  if (cycle %in% c("cchs2015_2016_p", "cchs2017_2018_p")) {
    dplyr::case_when(
      x == 1 ~ 1L,
      x == 2 ~ 2L,
      x == 3 ~ 3L,
      x == 4 ~ 4L,
      x == 5 ~ 4L,
      x == 6 ~ 5L,
      x == 96 ~ na_a(),
      x %in% c(97, 98, 99) ~ na_b(),
      TRUE ~ na_b()
    )
  } else {
    dplyr::case_when(
      x == 1 ~ 1L,
      x == 2 ~ 2L,
      x == 3 ~ 2L,
      x == 4 ~ 3L,
      x == 5 ~ 4L,
      x == 6 ~ 5L,
      x == 96 ~ na_a(),
      x %in% c(97, 98, 99) ~ na_b(),
      TRUE ~ na_b()
    )
  }
}

recode_SMK_01A <- function(x) {
  dplyr::case_when(
    x == 1 ~ 1L,
    x == 2 ~ 2L,
    x == 6 ~ na_a(),
    x %in% c(7, 8, 9) ~ na_b(),
    TRUE ~ na_b()
  )
}

recode_FSCDHFS2 <- function(x) {
  dplyr::case_when(
    x == 1 ~ 1L,
    x == 2 ~ 2L,
    x == 3 ~ 3L,
    x == 6 ~ na_a(),
    x %in% c(7, 8, 9) ~ na_b(),
    TRUE ~ na_b()
  )
}

recode_INCGHH_cont <- function(x) {
  dplyr::case_when(
    x == 1 ~ 2500,
    x == 2 ~ 7500,
    x == 3 ~ 12500,
    x == 4 ~ 17500,
    x == 5 ~ 22500,
    x == 6 ~ 35000,
    x == 7 ~ 45000,
    x == 8 ~ 60000,
    x == 9 ~ 70000,
    x == 10 ~ 90000,
    x == 11 ~ 125000,
    TRUE ~ NA_real_
  )
}

recode_HUPDPAD <- function(x) {
  dplyr::case_when(
    x == 1 ~ 1L,
    x == 2 ~ 2L,
    x == 3 ~ 3L,
    x == 4 ~ 4L,
    x == 5 ~ 5L,
    x == 6 ~ na_a(),
    x %in% c(7, 8, 9) ~ na_b(),
    TRUE ~ na_b()
  )
}

recode_SDCGCGT <- function(x) {
  dplyr::case_when(
    x == 1 ~ 1L,
    x == 2 ~ 2L,
    x == 6 ~ na_a(),
    x %in% c(7, 8, 9) ~ na_b(),
    TRUE ~ na_b()
  )
}

derive_binge_drinker <- function(DHH_SEX, ALW_1, ALW_2A1, ALW_2A2, ALW_2A3,
                                  ALW_2A4, ALW_2A5, ALW_2A6, ALW_2A7) {
  threshold <- dplyr::if_else(DHH_SEX == 1, 5, 4)
  max_drinks <- pmax(ALW_2A1, ALW_2A2, ALW_2A3, ALW_2A4, ALW_2A5, ALW_2A6, ALW_2A7,
                     na.rm = TRUE)
  dplyr::case_when(
    ALW_1 == 2 ~ 2L,
    max_drinks >= threshold ~ 1L,
    max_drinks < threshold ~ 2L,
    TRUE ~ na_b()
  )
}

derive_ADL_der <- function(ADL_01, ADL_02, ADL_03, ADL_04, ADL_05) {
  dplyr::case_when(
    ADL_01 == 1 | ADL_02 == 1 | ADL_03 == 1 | ADL_04 == 1 | ADL_05 == 1 ~ 1L,
    ADL_01 == 2 & ADL_02 == 2 & ADL_03 == 2 & ADL_04 == 2 & ADL_05 == 2 ~ 2L,
    TRUE ~ na_b()
  )
}

derive_ADL_score_5 <- function(ADL_01, ADL_02, ADL_03, ADL_04, ADL_05) {
  score <- (ADL_01 == 1) + (ADL_02 == 1) + (ADL_03 == 1) +
           (ADL_04 == 1) + (ADL_05 == 1)
  return(as.integer(score))
}

derive_resp_condition <- function(DHHGAGE_cont, CCC_091, CCC_031) {
  has_condition <- CCC_091 == 1 | CCC_031 == 1
  dplyr::case_when(
    DHHGAGE_cont >= 35 & has_condition ~ 1L,
    DHHGAGE_cont < 35 & has_condition ~ 2L,
    !has_condition ~ 3L,
    TRUE ~ na_b()
  )
}

derive_number_conditions <- function(CCC_121, CCC_131, CCC_151, CCC_171,
                                      CCC_280, resp_condition_der, CCC_051) {
  count <- (CCC_121 == 1) + (CCC_131 == 1) + (CCC_151 == 1) +
           (CCC_171 == 1) + (CCC_280 == 1) +
           (resp_condition_der %in% c(1, 2)) +
           (CCC_051 == 1)
  dplyr::if_else(count >= 5, "5+", as.character(count))
}

derive_smoke_simple <- function(SMKDSTY_cat5, time_quit_smoking) {
  dplyr::case_when(
    SMKDSTY_cat5 %in% c(1, 2) ~ 1L,
    SMKDSTY_cat5 %in% c(3, 4) & time_quit_smoking < 2 ~ 2L,
    SMKDSTY_cat5 %in% c(3, 4) & time_quit_smoking >= 2 ~ 3L,
    SMKDSTY_cat5 == 5 ~ 4L,
    TRUE ~ na_b()
  )
}

center_variable <- function(x, center_type = "mean") {
  x_numeric <- as.numeric(x)
  valid_values <- x_numeric[!is.na(x_numeric)]
  if (length(valid_values) == 0) {
    return(x_numeric)
  }
  if (center_type == "mean") {
    center_value <- mean(valid_values)
  } else if (center_type == "median") {
    center_value <- median(valid_values)
  } else {
    stop(paste("Unknown center_type:", center_type))
  }
  return(x_numeric - center_value)
}

center_DHHGAGE_cont <- function(data) {
  data$DHHGAGE_cont_C <- center_variable(data$DHHGAGE_cont, "mean")
  return(data)
}

center_CCC_031 <- function(data) {
  data$CCC_031_C <- center_variable(data$CCC_031, "mean")
  return(data)
}

center_CCC_051 <- function(data) {
  data$CCC_051_C <- center_variable(data$CCC_051, "mean")
  return(data)
}

center_CCC_071 <- function(data) {
  data$CCC_071_C <- center_variable(data$CCC_071, "mean")
  return(data)
}

center_CCC_091 <- function(data) {
  data$CCC_091_C <- center_variable(data$CCC_091, "mean")
  return(data)
}

center_CCC_121 <- function(data) {
  data$CCC_121_C <- center_variable(data$CCC_121, "mean")
  return(data)
}

center_CCC_131 <- function(data) {
  data$CCC_131_C <- center_variable(data$CCC_131, "mean")
  return(data)
}

center_CCC_151 <- function(data) {
  data$CCC_151_C <- center_variable(data$CCC_151, "mean")
  return(data)
}

center_CCC_280 <- function(data) {
  data$CCC_280_C <- center_variable(data$CCC_280, "mean")
  return(data)
}

center_CCC_290 <- function(data) {
  data$CCC_290_C <- center_variable(data$CCC_290, "mean")
  return(data)
}

center_DHH_SEX <- function(data) {
  data$DHH_SEX_C <- center_variable(data$DHH_SEX, "mean")
  return(data)
}

center_all_variables <- function(data) {
  if ("DHHGAGE_cont" %in% names(data)) data <- center_DHHGAGE_cont(data)
  if ("CCC_031" %in% names(data)) data <- center_CCC_031(data)
  if ("CCC_051" %in% names(data)) data <- center_CCC_051(data)
  if ("CCC_071" %in% names(data)) data <- center_CCC_071(data)
  if ("CCC_091" %in% names(data)) data <- center_CCC_091(data)
  if ("CCC_121" %in% names(data)) data <- center_CCC_121(data)
  if ("CCC_131" %in% names(data)) data <- center_CCC_131(data)
  if ("CCC_151" %in% names(data)) data <- center_CCC_151(data)
  if ("CCC_280" %in% names(data)) data <- center_CCC_280(data)
  if ("CCC_290" %in% names(data)) data <- center_CCC_290(data)
  if ("DHH_SEX" %in% names(data)) data <- center_DHH_SEX(data)
  return(data)
}

recode_cchs_data <- function(data, cycle, variable_mapping = NULL) {
  if ("DHH_SEX" %in% names(data) || !is.null(variable_mapping$DHH_SEX)) {
    var_name <- if (!is.null(variable_mapping$DHH_SEX)) variable_mapping$DHH_SEX else "DHH_SEX"
    data$DHH_SEX <- recode_DHH_SEX(data[[var_name]])
  }

  if ("DHHGAGE" %in% names(data) || !is.null(variable_mapping$DHHGAGE)) {
    var_name <- if (!is.null(variable_mapping$DHHGAGE)) variable_mapping$DHHGAGE else "DHHGAGE"
    data$DHHGAGE_D <- recode_DHHGAGE_D(data[[var_name]], cycle)
    data$DHHGAGE_cont <- recode_DHHGAGE_cont(data$DHHGAGE_D)
  }

  if ("ALCDTTM" %in% names(data) || !is.null(variable_mapping$ALCDTTM)) {
    var_name <- if (!is.null(variable_mapping$ALCDTTM)) variable_mapping$ALCDTTM else "ALCDTTM"
    data$ALCDTTM <- recode_ALCDTTM(data[[var_name]], cycle)
  }

  for (ccc_var in c("CCC_031", "CCC_051", "CCC_071", "CCC_091", "CCC_101",
                     "CCC_121", "CCC_131", "CCC_151", "CCC_171", "CCC_280", "CCC_290")) {
    if (ccc_var %in% names(data) || !is.null(variable_mapping[[ccc_var]])) {
      var_name <- if (!is.null(variable_mapping[[ccc_var]])) variable_mapping[[ccc_var]] else ccc_var
      data[[ccc_var]] <- recode_CCC_generic(data[[var_name]])
    }
  }

  for (adl_var in c("ADL_01", "ADL_02", "ADL_03", "ADL_04", "ADL_05")) {
    if (adl_var %in% names(data) || !is.null(variable_mapping[[adl_var]])) {
      var_name <- if (!is.null(variable_mapping[[adl_var]])) variable_mapping[[adl_var]] else adl_var
      data[[adl_var]] <- recode_ADL_generic(data[[var_name]])
    }
  }

  if ("GEN_01" %in% names(data) || !is.null(variable_mapping$GEN_01)) {
    var_name <- if (!is.null(variable_mapping$GEN_01)) variable_mapping$GEN_01 else "GEN_01"
    data$GEN_01 <- recode_GEN_01(data[[var_name]])
  }

  if ("GEN_02B" %in% names(data) || !is.null(variable_mapping$GEN_02B)) {
    var_name <- if (!is.null(variable_mapping$GEN_02B)) variable_mapping$GEN_02B else "GEN_02B"
    data$GEN_02B <- recode_GEN_02B(data[[var_name]])
  }

  if ("GEN_07" %in% names(data) || !is.null(variable_mapping$GEN_07)) {
    var_name <- if (!is.null(variable_mapping$GEN_07)) variable_mapping$GEN_07 else "GEN_07"
    data$GEN_07 <- recode_GEN_07(data[[var_name]])
  }

  if ("GEN_10" %in% names(data) || !is.null(variable_mapping$GEN_10)) {
    var_name <- if (!is.null(variable_mapping$GEN_10)) variable_mapping$GEN_10 else "GEN_10"
    data$GEN_10 <- recode_GEN_10(data[[var_name]])
  }

  if ("GEOGPRV" %in% names(data) || !is.null(variable_mapping$GEOGPRV)) {
    var_name <- if (!is.null(variable_mapping$GEOGPRV)) variable_mapping$GEOGPRV else "GEOGPRV"
    data$GEOGPRV <- recode_GEOGPRV(data[[var_name]])
  }

  if ("DHHGMS" %in% names(data) || !is.null(variable_mapping$DHHGMS)) {
    var_name <- if (!is.null(variable_mapping$DHHGMS)) variable_mapping$DHHGMS else "DHHGMS"
    data$DHHGMS <- recode_DHHGMS(data[[var_name]])
  }

  if ("EDUDR04" %in% names(data) || !is.null(variable_mapping$EDUDR04)) {
    var_name <- if (!is.null(variable_mapping$EDUDR04)) variable_mapping$EDUDR04 else "EDUDR04"
    data$EDUDR03 <- recode_EDUDR03(data[[var_name]], cycle)
  }

  if ("SMKDSTY" %in% names(data) || !is.null(variable_mapping$SMKDSTY)) {
    var_name <- if (!is.null(variable_mapping$SMKDSTY)) variable_mapping$SMKDSTY else "SMKDSTY"
    data$SMKDSTY_cat5 <- recode_SMKDSTY_cat5(data[[var_name]], cycle)
  }

  if ("SDCGCGT" %in% names(data) || !is.null(variable_mapping$SDCGCGT)) {
    var_name <- if (!is.null(variable_mapping$SDCGCGT)) variable_mapping$SDCGCGT else "SDCGCGT"
    data$SDCGCGT <- recode_SDCGCGT(data[[var_name]])
  }

  return(data)
}

get_variable_mapping <- function(cycle) {
  mappings <- list(
    "cchs2001_p" = list(
      DHH_SEX = "DHHA_SEX",
      DHHGAGE = "DHHAGAGE",
      DHHGMS = "DHHAGMS",
      ALCDTTM = "ALCADTYP",
      ALW_1 = "ALCA_5",
      CCC_031 = "CCCA_031",
      CCC_051 = "CCCA_051",
      CCC_071 = "CCCA_071",
      CCC_091 = "CCCA_91B",
      CCC_101 = "CCCA_101",
      CCC_121 = "CCCA_121",
      CCC_131 = "CCCA_131",
      CCC_151 = "CCCA_151",
      CCC_171 = "CCCA_171",
      GEN_01 = "GENA_01",
      GEN_07 = "GENA_07",
      GEN_10 = "GENA_10",
      GEOGPRV = "GEOAGPRV",
      EDUDR04 = "EDUADR04",
      SMKDSTY = "SMKADSTY",
      SMK_01A = "SMKA_01A",
      HUPDPAD = "HUIADPAD",
      INCGHH = "INCAGHH",
      SDCGCGT = "SDCAGRAC",
      WTS_M = "WTSAM"
    ),
    "cchs2003_p" = list(
      DHH_SEX = "DHHC_SEX",
      DHHGAGE = "DHHCGAGE",
      DHHGMS = "DHHCGMS",
      ALCDTTM = "ALCCDTYP",
      ALW_1 = "ALCC_5",
      CCC_031 = "CCCC_031",
      CCC_051 = "CCCC_051",
      CCC_071 = "CCCC_071",
      CCC_091 = "CCCC_91B",
      CCC_101 = "CCCC_101",
      CCC_121 = "CCCC_121",
      CCC_131 = "CCCC_131",
      CCC_151 = "CCCC_151",
      CCC_171 = "CCCC_171",
      CCC_280 = "CCCC_280",
      CCC_290 = "CCCC_290",
      GEN_01 = "GENC_01",
      GEN_02B = "GENC_02B",
      GEN_07 = "GENC_07",
      GEN_10 = "GENC_10",
      GEOGPRV = "GEOCGPRV",
      EDUDR04 = "EDUCDR04",
      SMKDSTY = "SMKCDSTY",
      SMK_01A = "SMKC_01A",
      HUPDPAD = "HUICDPAD",
      INCGHH = "INCCGHH",
      SDCGCGT = "SDCCGRAC",
      WTS_M = "WTSC_M"
    ),
    "cchs2005_p" = list(
      DHH_SEX = "DHHE_SEX",
      DHHGAGE = "DHHEGAGE",
      DHHGMS = "DHHEGMS",
      ALCDTTM = "ALCEDTYP",
      ALW_1 = "ALCE_5",
      CCC_031 = "CCCE_031",
      CCC_051 = "CCCE_051",
      CCC_071 = "CCCE_071",
      CCC_101 = "CCCE_101",
      CCC_121 = "CCCE_121",
      CCC_131 = "CCCE_131",
      CCC_151 = "CCCE_151",
      CCC_171 = "CCCE_171",
      CCC_280 = "CCCE_280",
      CCC_290 = "CCCE_290",
      GEN_01 = "GENE_01",
      GEN_02B = "GENE_02B",
      GEN_07 = "GENE_07",
      GEN_10 = "GENE_10",
      GEOGPRV = "GEOEGPRV",
      EDUDR04 = "EDUEDR04",
      SMKDSTY = "SMKEDSTY",
      SMK_01A = "SMKE_01A",
      HUPDPAD = "HUIEDPAD",
      INCGHH = "INCEGHH",
      SDCGCGT = "SDCEGCGT",
      WTS_M = "WTSE_M"
    ),
    "cchs2007_2008_p" = list(),
    "cchs2009_2010_p" = list(),
    "cchs2010_p" = list(),
    "cchs2011_2012_p" = list(),
    "cchs2012_p" = list(),
    "cchs2013_2014_p" = list(),
    "cchs2014_p" = list(),
    "cchs2015_2016_p" = list(
      ALCDTTM = "ALCDVTTM",
      ALW_1 = "ALW_005",
      CCC_031 = "CCC_015",
      CCC_051 = "CCC_050",
      CCC_071 = "CCC_065",
      CCC_091 = "CCC_030",
      CCC_101 = "CCC_095",
      CCC_121 = "CCC_085",
      CCC_131 = "CCC_130",
      CCC_151 = "CCC_090",
      CCC_280 = "CCC_195",
      CCC_290 = "CCC_200",
      GEN_01 = "GEN_005",
      GEN_02B = "GEN_015",
      GEN_07 = "GEN_020",
      GEN_10 = "GEN_030",
      GEOGPRV = "GEO_PRV",
      EDUDR04 = "EHG2DVR3",
      SMKDSTY = "SMKDVSTY",
      SMK_01A = "SMK_020",
      HUPDPAD = "HUIDVPAD",
      INCGHH = "INCDGHH",
      SDCGCGT = "SDCDGCGT"
    ),
    "cchs2017_2018_p" = list(
      ALCDTTM = "ALCDVTTM",
      ALW_1 = "ALW_005",
      CCC_031 = "CCC_015",
      CCC_051 = "CCC_050",
      CCC_071 = "CCC_065",
      CCC_091 = "CCC_030",
      CCC_101 = "CCC_095",
      CCC_121 = "CCC_085",
      CCC_131 = "CCC_130",
      CCC_151 = "CCC_090",
      CCC_280 = "CCC_195",
      CCC_290 = "CCC_200",
      GEN_01 = "GEN_005",
      GEN_02B = "GEN_015",
      GEN_07 = "GEN_020",
      GEN_10 = "GEN_030",
      GEOGPRV = "GEO_PRV",
      EDUDR04 = "EHG2DVR3",
      SMKDSTY = "SMKDVSTY",
      SMK_01A = "SMK_020",
      INCGHH = "INCDGHH",
      SDCGCGT = "SDCDGCGT"
    ),
    "cchs2009_s" = list(
      DHHGAGE = "DHH_AGE",
      DHHGMS = "DHH_MS",
      GEOGPRV = "GEO_PRV",
      INCGHH = "INCDHH"
    ),
    "cchs2010_s" = list(
      DHHGAGE = "DHH_AGE",
      DHHGMS = "DHH_MS",
      GEOGPRV = "GEO_PRV",
      INCGHH = "INCDHH"
    ),
    "cchs2012_s" = list(
      DHHGAGE = "DHH_AGE",
      DHHGMS = "DHH_MS",
      GEOGPRV = "GEO_PRV",
      INCGHH = "INCDHH"
    )
  )

  if (cycle %in% names(mappings)) {
    return(mappings[[cycle]])
  } else {
    warning(paste("Unknown cycle:", cycle, "- returning empty mapping"))
    return(list())
  }
}


center_vars<-function()