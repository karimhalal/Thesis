
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

#dummy variables function
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


#' Center continuous variables by subtracting their mean
#'
#' @param data data.frame containing the variables to center
#' @param vars character vector of column names to center; each will produce a
#'   new column named <var>_C
#' @param center_values optional named numeric vector of pre-computed means
#'   (e.g. from a training set); if NULL, means are computed from data
#' @return named list:
#'   data:          data with new <var>_C columns appended
#'   center_values: named numeric vector of means used (one per var)
center_variables <- function(data, vars, center_values = NULL) {
  for (var in vars) {
    cv <- if (!is.null(center_values) && !is.null(center_values[[var]])) {
      center_values[[var]]
    } else {
      mean(as.numeric(data[[var]]), na.rm = TRUE)
    }

    data[[paste0(var, "_C")]] <- as.numeric(data[[var]]) - cv

    if (is.null(center_values)) center_values <- c()
    center_values[[var]] <- cv
  }

  list(data = data, center_values = center_values)
}


#code to generate interactions

#' Create interaction terms as the product of two or more variables
#'
#' Factor/tagged-NA values are coerced to numeric before multiplication,
#' matching the behaviour in create-transformed-variables.R.
#'
#' @param data data.frame containing the source variables
#' @param interactions named list where each name is the output column name and
#'   each value is a character vector of the variables to multiply together,
#'   e.g. list(age_X_arthritis = c("DHHGAGE_D", "CCC_051"))
#' @return data with new interaction columns appended
create_interactions <- function(data, interactions) {
  for (out_name in names(interactions)) {
    vars      <- interactions[[out_name]]
    result    <- NULL

    for (var in vars) {
      vals <- data[[var]]
      # Convert tagged NAs (e.g. NA(a)) produced by recodeflow to real NA
      # before coercing to numeric, otherwise they become non-NA integers
      if (is.factor(vals)) {
        tagged <- grep("^NA\\([a-z]\\)$", levels(vals), value = TRUE)
        vals[vals %in% tagged] <- NA
      }
      vals <- as.numeric(vals)

      result <- if (is.null(result)) vals else result * vals
    }

    data[[out_name]] <- result
  }

  data
}


# RCS code

#' Create restricted cubic spline basis columns for continuous variables
#'
#' For each variable a set of columns named <var>_rcs1, <var>_rcs2, ...
#' (one per spline term) is appended to data.
#'
#' @param data data.frame containing the variables to expand
#' @param vars character vector of column names to expand with RCS
#' @param n_knots integer number of knots (default 4); ignored when
#'   knot_locations is supplied for a given variable
#' @param knot_locations optional named list of pre-computed knot location
#'   vectors (e.g. from a training set), keyed by variable name
#' @return named list:
#'   data:           data with new <var>_rcs<k> columns appended
#'   knot_locations: named list of knot vectors used (one per var)
create_rcs <- function(data, vars, n_knots = 4, knot_locations = NULL) {
  for (var in vars) {
    knots <- if (!is.null(knot_locations) && !is.null(knot_locations[[var]])) {
      knot_locations[[var]]
    } else {
      n_knots
    }

    spline_calc   <- rms::rcs(data[[var]], knots)
    spline_matrix <- unname(as.matrix(spline_calc))
    knots_used    <- attr(spline_calc, "parms")
    n_terms       <- ncol(spline_matrix)

    for (k in seq_len(n_terms)) {
      data[[paste0(var, "_rcs", k)]] <- as.vector(spline_matrix[, k])
    }

    if (is.null(knot_locations)) knot_locations <- list()
    knot_locations[[var]] <- knots_used
  }

  list(data = data, knot_locations = knot_locations)
}
