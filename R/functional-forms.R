source("R/merge-and-overwrite.R")
source("R/is_na.R")


#transformation infor 
# A named list that collects every learnable parameter produced during a
# transformation run so they can be reapplied identically to subsequent data
# (e.g. a validation cohort or bootstrap resample) without re-estimating from it.
#
# Fields (all optional; populated automatically on first run):
#   $center_values  — named numeric vector: source variable -> mean used
#   $knot_locations — named list:           source variable -> knot location vector
#   $dummy_refs     — named list:           source variable -> reference level used
#
# Typical usage:
#
#   # --- first run (derive parameters from main data) ---
#   r    <- variable_dummy(data, c("DHH_SEX", "EDUDR03"))
#   data <- r$data
#   ti   <- r$transformation_info
#
#   r    <- center_variables(data, c("DHH_AGE"), transformation_info = ti)
#   data <- r$data
#   ti   <- r$transformation_info
#
#   r    <- create_rcs(data, c("DHH_AGE_c"), transformation_info = ti)
#   data <- r$data
#   ti   <- r$transformation_info
#
#   # --- subsequent run (apply same parameters to new data) ---
#   r         <- variable_dummy(new_data, c("DHH_SEX", "EDUDR03"), transformation_info = ti)
#   new_data  <- r$data
#   r         <- center_variables(new_data, c("DHH_AGE"), transformation_info = ti)
#   new_data  <- r$data
#   r         <- create_rcs(new_data, c("DHH_AGE_c"), transformation_info = ti)
#   new_data  <- r$data
# ─────────────────────────────────────────────────────────────────────────────


#' Create dummy variables for categorical columns
#'
#' @param data data.frame containing the variables to dummy code
#' @param vars_to_dummy character vector of column names to dummy code
#' @param transformation_info optional named list from a prior run; if
#'   $dummy_refs contains a reference level for a variable it is reused,
#'   otherwise the most frequent category is used and recorded
#' @param id_col name of the row-identifier column used for merging (default "id_year")
#' @return named list:
#'   data:                data with new <var>_cat<i> columns appended
#'   transformation_info: updated with $dummy_refs entries used
variable_dummy <- function(data, vars_to_dummy, transformation_info = list(), id_col = "id_year") {
  new_cols <- data.frame(data[[id_col]], stringsAsFactors = FALSE)
  names(new_cols) <- id_col

  if (is.null(transformation_info$dummy_refs)) transformation_info$dummy_refs <- list()

  for (var in vars_to_dummy) {
    if (!var %in% names(data)) stop(paste("variable_dummy: variable not found in data:", var))

    col_vals <- as.character(haven::zap_labels(data[[var]]))

    ref_cat <- if (!is.null(transformation_info$dummy_refs[[var]])) {
      transformation_info$dummy_refs[[var]]
    } else {
      names(which.max(table(col_vals, useNA = "no")))
    }
    transformation_info$dummy_refs[[var]] <- ref_cat

    cats          <- sort(unique(na.omit(col_vals)))
    cats_to_dummy <- cats[cats != ref_cat]

    for (i in seq_along(cats_to_dummy)) {
      new_cols[[paste0(var, "_cat", i)]] <- as.integer(col_vals == cats_to_dummy[i])
    }
  }

  list(
    data                = merge_and_overwrite(data, new_cols, by = id_col),
    transformation_info = transformation_info
  )
}


#' Center continuous variables by subtracting their mean
#'
#' @param data data.frame containing the variables to center
#' @param vars character vector of column names to center; each produces a new
#'   column named <var>_c
#' @param transformation_info optional named list from a prior run; if
#'   $center_values contains a mean for a variable it is reused, otherwise the
#'   mean is computed from data and recorded
#' @param id_col name of the row-identifier column used for merging (default "id_year")
#' @return named list:
#'   data:                data with new <var>_c columns appended
#'   transformation_info: updated with $center_values entries used
center_variables <- function(data, vars, transformation_info = list(), id_col = "id_year") {
  new_cols <- data.frame(data[[id_col]], stringsAsFactors = FALSE)
  names(new_cols) <- id_col

  if (is.null(transformation_info$center_values)) transformation_info$center_values <- c()

  for (var in vars) {
    if (!var %in% names(data)) stop(paste("center_variables: variable not found in data:", var))

    cv <- if (!is.null(transformation_info$center_values[[var]])) {
      transformation_info$center_values[[var]]
    } else {
      mean(as.numeric(data[[var]]), na.rm = TRUE)
    }

    new_cols[[paste0(var, "_c")]] <- as.numeric(data[[var]]) - cv
    transformation_info$center_values[[var]] <- cv
  }

  list(
    data                = merge_and_overwrite(data, new_cols, by = id_col),
    transformation_info = transformation_info
  )
}


#' Create interaction terms as the product of two or more variables
#'
#' @param data data.frame containing the source variables
#' @param interactions named list where each name is the output column name and
#'   each value is a character vector of the variables to multiply together,
#'   e.g. list(age_X_arthritis = c("DHH_AGE_c", "CCC_051"))
#' @param factor_interactions optional character vector of output names that
#'   should be stored as factors rather than numeric
#' @param transformation_info optional named list (passed through unchanged;
#'   interactions have no learnable parameters)
#' @param id_col name of the row-identifier column used for merging (default "id_year")
#' @return named list:
#'   data:                data with new interaction columns appended
#'   transformation_info: unchanged input transformation_info
create_interactions <- function(data, interactions, factor_interactions = NULL,
                                transformation_info = list(), id_col = "id_year") {
  new_cols <- data.frame(data[[id_col]], stringsAsFactors = FALSE)
  names(new_cols) <- id_col

  for (out_name in names(interactions)) {
    vars   <- interactions[[out_name]]
    result <- NULL

    for (var in vars) {
      if (!var %in% names(data)) stop(paste("create_interactions: variable not found in data:", var))
      vals <- haven::zap_labels(data[[var]])
      vals[is_na(vals)] <- NA
      vals   <- as.numeric(vals)
      result <- if (is.null(result)) vals else result * vals
    }

    new_cols[[out_name]] <- if (!is.null(factor_interactions) && out_name %in% factor_interactions) {
      as.factor(result)
    } else {
      result
    }
  }

  list(
    data                = merge_and_overwrite(data, new_cols, by = id_col),
    transformation_info = transformation_info
  )
}


#' Create restricted cubic spline basis columns for continuous variables
#'
#' Column 1 of the rms::rcs matrix is the linear term (identical to the input
#' variable) and is excluded by default to avoid collinearity when the centered
#' variable is also in the model. Use rcs_cols to override.
#'
#' @param data data.frame containing the variables to expand
#' @param vars character vector of column names to expand with RCS
#' @param n_knots integer number of knots (default 4); ignored when
#'   transformation_info$knot_locations is supplied for a given variable
#' @param transformation_info optional named list from a prior run; if
#'   $knot_locations contains knots for a variable they are reused, otherwise
#'   knots are placed automatically and recorded
#' @param rcs_cols optional named list of integer vectors specifying which
#'   columns of the rms::rcs matrix to extract per variable (1 = linear term,
#'   2+ = nonlinear terms). Defaults to 2:n_terms for each variable.
#'   e.g. list(DHH_AGE_c = 2:3)
#' @param id_col name of the row-identifier column used for merging (default "id_year")
#' @return named list:
#'   data:                data with new <var>_rcs<k> columns appended
#'   transformation_info: updated with $knot_locations entries used
create_rcs <- function(data, vars, n_knots = 4, transformation_info = list(),
                       rcs_cols = NULL, id_col = "id_year") {
  new_cols <- data.frame(data[[id_col]], stringsAsFactors = FALSE)
  names(new_cols) <- id_col

  if (is.null(transformation_info$knot_locations)) transformation_info$knot_locations <- list()

  for (var in vars) {
    if (!var %in% names(data)) stop(paste("create_rcs: variable not found in data:", var))

    knots <- if (!is.null(transformation_info$knot_locations[[var]])) {
      transformation_info$knot_locations[[var]]
    } else {
      n_knots
    }

    spline_calc   <- rms::rcs(data[[var]], knots)
    spline_matrix <- unname(as.matrix(spline_calc))
    knots_used    <- attr(spline_calc, "parms")
    n_terms       <- ncol(spline_matrix)

    cols <- if (!is.null(rcs_cols) && !is.null(rcs_cols[[var]])) {
      rcs_cols[[var]]
    } else {
      2:n_terms
    }

    for (k in seq_along(cols)) {
      new_cols[[paste0(var, "_rcs", k)]] <- as.vector(spline_matrix[, cols[k]])
    }

    transformation_info$knot_locations[[var]] <- knots_used
  }

  list(
    data                = merge_and_overwrite(data, new_cols, by = id_col),
    transformation_info = transformation_info
  )
}
