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

#' @title resp_condition_fun1
#'
#' @description This is one of 3 functions used to create a derived variable
#'  (resp_condition_der) that determines if a respondents has a respiratory
#'  condition. 3 different functions have been created to account for the fact
#'  that different respiratory variables are used across CCHS cycles. This
#'  function is for CCHS cycles (2009-2018) that only use COPD and Emphysema as
#'  a combined variable. Asthma is used across CCHS cycles as a separate
#'  variable.
#'
#' @param DHHGAGE_cont continuous age variable.
#'
#' @param CCC_091 variable indicating if respondent has either COPD or Emphysema
#'
#' @param CCC_031 variable indicating if respondent has asthma
#'
#' @return a categorical variable (resp_condition_der) with 3 levels:
#'
#'  \enumerate{
#'  \item respondent is over the age of 35 and has a respiratory condition
#'  \item respondent is under the age of 35 and has a respiratory condition
#'  \item respondent does not have a respiratory condition
#'  }
#'
#' @examples
#' # Using resp_condition_fun1() to create values across CCHS cycles
#' # (2009-2014) resp_condition_fun1() is specified in
#' # variable_details.csv along with the CCHS variables and cycles included.
#'
#' # To transform resp_condition_der, use rec_with_table() for each CCHS cycle
#' # and specify resp_condition_der, along with the various respiratory
#' # variables. Then by using merge_rec_data() you can combine
#' # resp_condition_der across cycles.
#'
#' library(cchsflow)
#'
#' resp2009_2010 <- suppressWarnings(rec_with_table(
#'   cchs2009_2010_p,  c(
#'     "DHHGAGE_cont", "CCC_091", "CCC_031",
#'     "resp_condition_der"
#'   )
#' ))
#'
#' head(resp2009_2010)
#'
#' resp2011_2012 <- suppressWarnings(rec_with_table(
#'   cchs2011_2012_p, c(
#'     "DHHGAGE_cont", "CCC_091", "CCC_031",
#'     "resp_condition_der"
#'   )
#' ))
#'
#' tail(resp2011_2012)
#'
#' combined_resp <-
#'  suppressWarnings(merge_rec_data(resp2009_2010, resp2011_2012))
#'
#' head(combined_resp)
#' tail(combined_resp)
#' @seealso \code{\link{resp_condition_fun2}}, \code{\link{resp_condition_fun3}}
#'
#' @export
resp_condition_fun1 <-
  function(DHH_AGE, CCC_091, CCC_031) {
    resp_condition <-
      if_else2(
        ((DHH_AGE > 0 & DHH_AGE >= 35) &
           (CCC_091 == 1 | CCC_031 == 1)), 1,
        if_else2(
          ((DHH_AGE > 0 & DHH_AGE < 35) &
             (CCC_091 == 1 | CCC_031 == 1)), 2,
          if_else2(
            ((DHH_AGE > 0 & DHH_AGE < 35) &
               (CCC_091 == 2 | CCC_031 == 2)), 3,
            if_else2(
              ((DHH_AGE > 0 & DHH_AGE >= 35) &
                 (CCC_091 == 2 & CCC_031 == 2)), 3,
              if_else2((CCC_091 == "NA(a)" & CCC_031 == "NA(a)"), "NA(a)",
                       "NA(b)")
            )
          )
        )
      )
    return(resp_condition)
  }


#' @title edu_fun
#'
#' @description This function generates a 4 category highest level of education variable 
#' using the same methodology outlined for CCHS derived variable EDUDR04. This circumvents the
#' recategorization of this variable into a 3 category variable seen in newer CCHS cycles 
#' (post-2015)
#'
#' @param EDU_1 variable indicating highest grade of elementary/high school comopleted.
#'
#' @param EDU_2 variable indicating completed high school diploma or its equivalent
#'
#' @param EDU_3 variable indicating if respondent has received any other education that be counted towards a diploma or
#' degree or certificate
#' 
#' @param EDU_4A variable indicating the highest certificate, diploma or degree obtained
#'
#' @return EDU_der a categorical variable (EDU_der) with 4 levels:
#'
#'  \enumerate{
#'  \item Less than high school diploma
#'  \item high school graduation
#'  \item some post-secondary education
#'  \item Post-Secondary graduation
#'  }
#'
#' @examples
#' # Using edu_fun() to create values across CCHS cycles
#' # (2013-2018) edu_fun() is specified in
#' # variable_details.csv along with the CCHS variables and cycles included.
#'
#' # To transform EDU_der, use rec_with_table() for each CCHS cycle
#' # and specify EDU_der, along with the various respiratory
#' # variables. Then by using merge_rec_data() you can combine
#' # resp_condition_der across cycles.
#'
#' library(cchsflow)
#'
#' edu2015_2016 <- suppressWarnings(rec_with_table(
#'   cchs2009_2010_p,  c(
#'     "EDU_1", "EDU_2", "EDU_3",
#'     "EDU_4A", "EDU_der"
#'   )
#' ))
#'
#' head(resp2009_2010)
#'
#' resp2011_2012 <- suppressWarnings(rec_with_table(
#'   cchs2011_2012_p, c(
#'     "EDU_1", "EDU_2", "EDU_3",
#'     "EDU_4A", "EDU_der"
#'   )
#' ))
#'
#' tail(resp2011_2012)
#'
#' combined_resp <-
#'  suppressWarnings(merge_rec_data(resp2009_2010, resp2011_2012))
#'
#' head(combined_resp)
#' tail(combined_resp)
#' @seealso \code{\link{resp_condition_fun2}}, \code{\link{resp_condition_fun3}}
#'
#' @export


#' @title Number of chronic conditions (6 chronic conditions)
#' 
#' @description This function generates a derived variable (number_conditions)
#'  that counts the number of chronic conditions a respondent has. This function
#'  takes 6 CCHS-defined conditions (heart disease, cancer, stroke, bowel
#'  disorder, mood disorder and arthritis), and well one derived variable
#'  (respiratory condition) to count the number of conditions a respondent has.
#'  
#' @param CCC_121 variable indicating if respondent has heart disease (1 = 
#'  respondent has heart disease, 2 = respondent does not have heart disease)
#' 
#' @param CCC_131 variable indicating if respondent has active cancer (1 =
#'  respondent has active cancer, 2 =  respondent does not have active cancer)
#' 
#' @param CCC_151 variable indicating if respondent suffers from the effects
#'  of a stroke (1 = respondent suffers from stroke effects, 2 = respondent
#'  does not suffer from stroke effects)
#' 
#' @param CCC_171 variable indicating if respondent has a bowel disorder (1 =
#'  respondent has bowel disorder, 2 = respondent does not have a bowel
#'  disorder)
#' 
#' @param CCC_280 variable indicating if respondent has a mood disorder (1 =
#'  respondent has a mood disorder, 2 = respondent does not have a mood
#'  disorder. Note, variable was not asked to respondents in the 2001 CCHS
#'  survey cycle.
#' 
#' @param resp_condition_der derived variable indicating if respondent has a
#'  respiratory condition. (1 = respondent is over the age of 35 and has
#'  a respiratory condition, 2 = respondent is under the age of 35 and has a
#'  respiratory conditions, 3 = respondent does not have a respiratory
#'  condition). See \code{\link{resp_condition_fun1}} for
#'  documentation on how variable was derived.
#'
#' @param CCC_051 variable indicating if respondent has arthritis or
#'  rheumatism (1 = respondent has arthritis or rheumatism, 2 = respondent does
#'  not have arthritis or rheumatism)
#'
#' @details mood disorder (CCC_280) was not asked to respondents in the 2001
#'  CCHS survey cycle. This mean respondents in this cycle will only be able to
#'  have a maximum of 6 chronic conditions as opposed to 7 for respondents in
#'  other cycles. \code{\link{multiple_conditions_fun1}} is used for CCHS cycles
#'  from 2003 to 2014.
#' 
#' @return A categorical variable indicating the number of chronic conditions
#'  a respondent has. Respondents with 5 or more conditions are grouped in the
#'  "5+" category. 
#' 
#' @examples 
#'  # Using rec_with_table() to generate multiple_conditions in a CCHS
#'  # cycle.
#'  
#'  # multiple_conditions_fun2() is specified in variable_details.csv along with
#'  # the CCHS variables and cycles included.
#'  
#'  # To generate multiple_conditions, use rec_with_table() and specify the
#'  # multiple_conditions, along with the variables that are derived from it.
#'  # Since resp_condition_der is also a derived variable, you will have to
#'  # specify the variables that are derived from it. In this example, data
#'  # from the 2010 CCHS will be used, so DHHGAGE_cont, CCC_091, and CCC_031
#'  # will be specified along with resp_condition_der.
#'  
#' library(cchsflow)
#'  conditions_2009_2010 <- suppressWarnings(rec_with_table(cchs2009_2010_p,
#'  c("DHHGAGE_cont", "CCC_091",
#'  "CCC_031", "CCC_121","CCC_131","CCC_151", "CCC_171","CCC_280",
#'  "resp_condition_der","CCC_051", "number_conditions")))
#'  
#'  head(conditions_2009_2010)
#'  
#'  # Generating multiple_conditions with user inputted values
#'  # Let's say you are an individual that has heart disease, bowel disorder,
#'  # and arthritis. multiple_conditions_fun2() can be used to count the number
#'  # of chronic conditions you have
#'  
#' library(cchsflow)
#'  num_conditions <- multiple_conditions_fun2(CCC_121 = 1, CCC_131 = 2, 
#'  CCC_151 = 2, CCC_171 = 1, CCC_280 = 2, resp_condition_der = 3, CCC_051 = 1) 
#' 
#' print(num_conditions)
#' 
#' @seealso \code{\link{multiple_conditions_fun1}} 
#' @export
multiple_conditions_fun2 <- 
  function(CCC_121, CCC_131, CCC_151, CCC_171, CCC_280, resp_condition_der,
           CCC_051){
    suppressWarnings({    
      # Convert variables to numeric
      CCC_121 <- as.numeric(CCC_121)
      CCC_131 <- as.numeric(CCC_131)
      CCC_151 <- as.numeric(CCC_151)
      CCC_171 <- as.numeric(CCC_171)
      CCC_280 <- as.numeric(CCC_280)
      resp_condition_der <- as.numeric(resp_condition_der)
      CCC_051 <- as.numeric(CCC_051)
    })

    # set invalid/NA values to 0
    CCC_121 <- if_else2(CCC_121 %in% (1:2), CCC_121, 0)
    CCC_131 <- if_else2(CCC_131 %in% (1:2), CCC_131, 0)
    CCC_151 <- if_else2(CCC_151 %in% (1:2), CCC_151, 0)
    CCC_171 <- if_else2(CCC_171 %in% (1:2), CCC_171, 0)
    CCC_280 <- if_else2(CCC_280 %in% (1:2), CCC_280, 0)
    resp_condition_der <- if_else2(resp_condition_der %in% (1:3),
                                   resp_condition_der, 0)
    CCC_051 <- if_else2(CCC_051 %in% (1:2), CCC_051, 0)
    
    # adjust resp_condition to yes = 1, no = 2
    resp_condition_der <- if_else2(resp_condition_der %in% c(1:2), 1, 2)
    
    # Calculate number of conditions based on yes
    conditions <- 
      (CCC_121%%2) + (CCC_131%%2) + (CCC_151%%2) +(CCC_171%%2) +
                  (CCC_280%%2) + (resp_condition_der%%2) + (CCC_051%%2)
    
    if_else2(conditions>= 5, "5+", conditions)
  }