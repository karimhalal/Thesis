#list of functions to derive variables of interest
adl_fun <- cchsflow::adl_fun
adl_score_5_fun <- cchsflow::adl_score_5_fun
binge_drinker_fun <- cchsflow::binge_drinker_fun
if_else2 <- cchsflow::if_else2
low_drink_short_fun <- cchsflow::low_drink_short_fun
low_drink_long_fun <- cchsflow::low_drink_long_fun
age_cat_fun <- cchsflow::age_cat_fun
low_drink_score_fun <- cchsflow::low_drink_score_fun
low_drink_score_fun1 <- cchsflow::low_drink_score_fun1
SMKG040_fun <- cchsflow::SMKG040_fun
time_quit_smoking_fun <- cchsflow::time_quit_smoking_fun
smoke_simple_fun <- cchsflow::smoke_simple_fun

# Custom function to simplify the survey cycle name.
# This function is likely what the recodeflow process expects to find
# under the name 'surveycycle_fun' to create a derived variable.
surveycycle_fun <- function(data_name) {
  switch(data_name,
         cchs2013_2014 = "2013-2014",
         cchs2015_2016 = "2015-2016",
         cchs2017_2018 = "2017-2018")
}
#' Function to identify dependencies of package of choice
#'
#' @param pkg Character scalar, package name.
#' @param recursive Logical; include transitive deps? (default TRUE)
#' @param include_suggests Logical; include Suggests/Enhances? (default FALSE)
#' @param installed_only Logical; resolve against installed packages only (default TRUE).
#'        If FALSE, resolve against available packages from repos.
#' @param repos Character vector of repositories to use when installed_only = FALSE.
#' @param include_base Logical; include base/recommended R packages in the result? (default FALSE)
#' @param return_versions Logical; return a data.frame with versions if available (default FALSE)
#'
#' @return Character vector of dependency package names, or a data.frame if return_versions = TRUE.
get_package_deps <- function(
  pkg,
  recursive = TRUE,
  include_suggests = FALSE,
  installed_only = TRUE,
  repos = getOption("repos"),
  include_base = FALSE,
  return_versions = FALSE
) {
  stopifnot(is.character(pkg), length(pkg) == 1, nzchar(pkg))

  # Build the package database to resolve dependencies against
  if (installed_only) {
    db <- utils::installed.packages()
  } else {
    if (is.null(repos) || length(repos) == 0 || all(repos == "@CRAN@")) {
      repos <- c(CRAN = "https://cloud.r-project.org")
    }
    db <- utils::available.packages(repos = repos)
  }

  # Ensure the package is present in the db (or warn if not)
  if (!pkg %in% rownames(db)) {
    stop(sprintf(
      "Package '%s' was not found in the selected database (%s).",
      pkg, if (installed_only) "installed.packages()" else "available.packages()"
    ))
  }

  # Which dependency fields to include
  which_fields <- c("Depends", "Imports", "LinkingTo")
  if (include_suggests) which_fields <- c(which_fields, "Suggests", "Enhances")

  # Compute dependencies
  deps_list <- tools::package_dependencies(
    packages  = pkg,
    db        = db,
    which     = which_fields,
    recursive = recursive
  )

  deps <- unname(unique(deps_list[[pkg]] %||% character(0)))

  # Optionally drop base/recommended packages that are part of R itself
  if (!include_base && length(deps)) {
    base_pkgs <- rownames(db)[db[, "Priority"] %in% c("base", "recommended")]
    deps <- setdiff(deps, base_pkgs)
  }

  # Optionally attach versions (from the same db used above)
  if (return_versions) {
    versions <- db[deps, "Version", drop = TRUE]
    out <- data.frame(package = deps, version = unname(versions), row.names = NULL)
    return(out[order(out$package), , drop = FALSE])
  }

  sort(deps)
}

# null-coalescing helper for base R
`%||%` <- function(x, y) if (is.null(x)) y else x
