
############################################################
             ###Check Skewnewss and truncate###
############################################################


#change
#' @param data a data.frame containing the study data
#' @param continuous_vars character vector of continuous variable names to check
#' @param skewness_threshold numeric threshold for |skewness| (default: 1).
#'   Variables with |skewness| >= threshold will be marked for truncation
#'
#' @return a list containing:
#'   - vars_to_truncate: character vector of variable names to truncate
#'   - skewness_summary: data.frame with skewness statistics for all checked variables
check_skewness <- function(data, continuous_vars, skewness_threshold = 1) {
  library(DescTools)

  vars_to_truncate <- c()
  skewness_summary <- data.frame(
    variable = character(),
    skewness = numeric(),
    abs_skewness = numeric(),
    action = character(),
    stringsAsFactors = FALSE
  )

  for (var in continuous_vars) {

    if (var %in% names(data)) {
      var_data <- data[[var]]
      non_missing <- var_data[!is.na(var_data)]

      if (length(non_missing) > 0) {

        # Compute skewness using DescTools::Skew
        skew_val <- tryCatch(
          DescTools::Skew(non_missing, method = 1, na.rm = TRUE),
          error = function(e) NA
        )

        abs_skew <- abs(skew_val)

        # Mark for truncation if skewness exceeds threshold
        if (!is.na(abs_skew) && abs_skew >= skewness_threshold) {
          vars_to_truncate <- c(vars_to_truncate, var)
          action <- "TRUNCATE"
        } else {
          action <- "keep"
        }

        skewness_summary <- rbind(
          skewness_summary,
          data.frame(
            variable = var,
            skewness = skew_val,
            abs_skewness = abs_skew,
            action = action,
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }

  return(list(
    vars_to_truncate = vars_to_truncate,
    skewness_summary = skewness_summary
  ))
}

#' Truncates the data for data cleaning purposes
#'
#' @param data a data.frame containing the data to truncate
#' @param variables_to_truncate string vector containing the continuous
#' variables to truncate
#' @param truncate_percentile number between 0 and 100 containing the
#' percentile to truncate the variables to
#'
#' @return a data.frame containing the truncated data
truncate_data <- function(data, variables_to_truncate, truncate_percentile) {
  truncated_data <- data

  for(variable in variables_to_truncate) {
    variable_data <- data[[variable]]

    truncated_data[[variable]] <- .truncate(
      variable_data,
      truncate_percentile
    )
  }

  return(truncated_data)
}

#' Truncates values
#'
#' @param data a vector containing the values to truncate
#' @param percentile a number between 0 and 100 containing the percentile to
#' truncate to
#'
#' @return a vector containing the truncated data
.truncate <- function(data, percentile) {
  truncate_value <- quantile(data, percentile/100, na.rm = TRUE)
  # Use dplyr::if_else with missing parameter otherwise the tagged NA values
  # don't get propagated
  truncated_data <- dplyr::if_else(
    data > truncate_value,
    truncate_value,
    data,
    missing = data
  )
  return(truncated_data)
}

#######Centering, RCS and Dummied variables#######

#dummy variables
variable_dummy <- function(data, vars_to_dummy) {

  for (var in vars_to_dummy) {
    cats <- sort(unique(na.omit(data[[var]])))

    # Drop the most common category (reference level)
    ref_cat <- names(which.max(table(data[[var]], useNA = "no")))
    cats_to_dummy <- cats[cats != ref_cat]

    for (i in seq_along(cats_to_dummy)) {
      dummy_name <- paste0(var, "_cat", i)
      data[[dummy_name]] <- as.integer(data[[var]] == cats_to_dummy[i])
    }
  }

  return(data)
}
