#'@title CCHS MOCK Function
#'
#' @description This function generates 3 cycles of CCHS mock data using variable
#' availability information from ICES intranet (ONTARIO-specific)
#'
#' @param sample_2013 [numeric] Number of participants to simulate for the 2013-2014 cycle.
#'
#' @param sample_2015 [numeric] Number of participants to simulate for the 2015-2016 cycle.
#'
#' @param sample_2017 [numeric] Number of participants to simulate for the 2017-2018 cycle.
#'
#' @return [named_list] Raw CCHS data from 3 different cycles, returned as a named list of three
#'   tibbles: \code{cchs_2013}, \code{cchs_2015}, and \code{cchs_2017}.
#'
#' @details This function generates the following variables, all of which are integers with numbers correspodning to number of categories (ex. 1 to 5 for 5 category )
#' with corresponding labels
#'            DHH_AGE (dhh_age post-2015) CONTINOUS
#'            DHH_SEX(dhh_sex post-2015) 2 category categorical labelled male and female
#'            DHH_OWN(dhh_own post-2015) 2 category categorical labelled owner and renter
#'            cchs_year(same post-2015) 3 categories (2013-2014, 2015-2016, 2017-2018)
#'            GEN_01(gen_005 post-2015) self preceived health 5 categories: excellent, very good, good, fair, poor
#'            GENGSWL(same post-2015) life satisfaction 5 categories: very satisfied, satisfied, neither satisfied or dissatisfied, dissatisfied, very dissatisfied
#'            GEN_02B(gen_015 post-2015) self preceived mental health 5 categories: excellent, very good, good, fair, poor
#'            GEN_07(gen_020 post-2015) self preceived life stress 5 categories: not at all, not very, a bit, quite a bit, extremely
#'            GEN_09(gen_025 post-2015) self preceived work stress 5 categories: not at all, not very, a bit, quite a bit, extremely
#'            GEN_10(gen_030 post-2015) sense of belonging to local community 4 categories: very strong, somewhat strong, somewhat weak, very weak
#'            CCC_031(ccc_015 post-2015) has asthma categories: yes, no
#'            CCC_051(ccc_050 post-2015) has arthiritis categories: yes, no
#'            CCC_061(ccc_055 post-2015) has back problems categories: yes, no. In 2017-2018, this variable is missing but it is still available as a column but populated completely with 996
#'            CCC_071(ccc_065 post-2015) has HBP categories: yes, no
#'            CCC_091(ccc_030 post-2015) has COPD categories: yes, no
#'            CCC_101(ccc_095 post-2015) has diabetes categories: yes, no
#'            CCC_121(ccc_085 post-2015) has heart disease categories: yes, no
#'            CCC_151(ccc_090 post-2015) has stroke categories: yes, no
#'            CCC_171(ccc_155 post-2015) has bowel disorder categories: yes, no. In 2017-2018, this variable is missing but it is still available as a column but populated completely with 996
#'            CCC_280(ccc_195 post-2015) has mood disorder categories: yes, no
#'            CCC_290(ccc_200 post-2015) has anxiety disorder categories: yes, no
#'            HWTDHTM(hwtdvhtm post-2015) HEIGHT IN cm continous
#'            HWTDWTK(hwtdvwtk post-2015) weight in kg continous
#'            HUPDPAD(no corresponding variables post-2015) categorical 5 categories: no pain, pain does not prevent activity, prevents a few activities, prevents some activities, prevents most activities
#'            PACDEE(derived from paa_045, paa_050, paa_075, paa_080, paadvdys, and paadvvig post-2015) MET (energy expenditure measure) for leisure activities
#'            SMK_01A(no variables post-2015) smoked 100+cigs categorical (2 categories): yes, no
#'            SMK_203(derived from smk_005 and smk_040) age started smoking daily (daily smokers) continous
#'            SMK_204(smk_045 post-2015) number of cig smoked per day (current daily) continous
#'            SMK_207(derived from smk_005 and smk_040 post-2015) age started smoking daily continous
#'            SMK_208(smk_075 post-2015) number of cig smoked per day (former daily) continous
#'            SMK_05B(smk_050 post-2015) number of cig smoked per day (occasional smoker) continous
#'            SMK_09A(smk_080 post-2015) age started smoking daily continous
#'            SMK_09C(smk_090 post-2015) number of YEARS since stopped smoking daily continous
#'            SMKDSTY(smkdvsty post-2015) type of smoker categorical (6 categories): dialy smoker, occasional smoker, always occasional smoker, former daily smoker, former occasional smoker, never smoked
#'            ALCDTTM(alcdvttm post-2015) type of drinker categorical (3 categories): regualr drinker, occasional drinker, did not drink in the last 12 months
#'            ALW_1(alw_005 post-2015) drank alcohol in the past week (2 categories):yes, no
#'            Number of drinks each day of the week (ALW_2A1-ALW_2A7 pre 2015 and alw_010-alw_040 in 005 increments for post 2015 cycles)
#'            ALWDWKY(alwdvwky post-2015) weekly intake of alcohol continous
#'            SDCDCGT(sdcdvcgt post-2015) ethnic identity 13 category: white, black, korean, filipino, japanese, chinese, south asian, south east asian, arab, west asian, latin american, other, multiple origin
#'            EDUDR04(ehg2dvr3 post-2015) individual educational attainment 4 categories pre 2015: less than secondary, secondary grad, some post-secondary, post-secondary grad and 3 categories post 2015: less than secondary school, secondary grad, post-secondary education
#'            CMH_01K(cmh_005 post-2015) consulted a mental health profesional- binary yes, no (1,2). In 2017-2018, this variable is missing but it is still available as a column but populated completely with 996
#'            CMH_01L(cmh_005 post-2015) number of times consulted a mental health profesional last year- continous (only whole numbers). In 2017-2018, this variable is missing but it is still available as a column but populated completely with 996
#'            rural(same for all cycles) rural status binary yes no
#'            material_deprivation(same for all cycles) material deprivation quintile (5 cats corresponding to each quintile)
#'            drgdvlac(only available post 2015) illicit drug use-lifetime (excluding one time marijuana) binary yes no
#'            drgdvyac(only available post 2015) illicit drug use-last year (excluding one time marijuana) binary yes no
#'
#'
#' NA tracking for variables
#'            for continous variables: 996,997,998,or 999 where 996=not applicable and 997,998,999=refusal/DK/missing
#'            for categorical variables: depends on the number of categories
#'               For variables with less than 6 cats--> 6,7,8,9 where 6=not applicable and 7,8,9=refusal/DK/missing
#'               For variables with more than 6 categories--> 96,97,98,99 where 96=not applicable and 97,98,99=refusal/DK/missing
#'                    exception: weight variables use 999.96,999.97,999.98,999.99
#'
#' @examples
#' # Equal sample size across all cycles
#' cchs_mock(100, 100, 100)
#'
#' # Per-cycle sample sizes
#' cchs_mock(sample_2013 = 200, sample_2015 = 300, sample_2017 = 150)
#'
library(dplyr)
library(labelled)

##### NA injection helpers ############

# Categorical <6 valid cats → NA codes 6, 7/8/9
inj_na_s <- function(x, rate_a = 0.050, rate_b = 0.050) {
  n <- length(x)
  x[sample(n, round(n * rate_a))] <- 6L
  x[sample(n, round(n * rate_b))] <- sample(7L:9L, round(n * rate_b), replace = TRUE)
  x
}

# Categorical ≥6 valid cats → NA codes 96, 97/98/99
inj_na_l <- function(x, rate_a = 0.050, rate_b = 0.050) {
  n <- length(x)
  x[sample(n, round(n * rate_a))] <- 96L
  x[sample(n, round(n * rate_b))] <- sample(97L:99L, round(n * rate_b), replace = TRUE)
  x
}

# Continuous → NA codes 996, 997/998/999
inj_na_c <- function(x, rate_a = 0.050, rate_b = 0.050) {
  n <- length(x)
  x[sample(n, round(n * rate_a))] <- 996
  x[sample(n, round(n * rate_b))] <- sample(c(997, 998, 999), round(n * rate_b), replace = TRUE)
  x
}

# Weight → NA codes 999.96, 999.97/999.98/999.99
inj_na_w <- function(x, rate_a = 0.050, rate_b = 0.050) {
  n <- length(x)
  x[sample(n, round(n * rate_a))] <- 999.96
  x[sample(n, round(n * rate_b))] <- sample(c(999.97, 999.98, 999.99),
                                             round(n * rate_b), replace = TRUE)
  x
}

# ── Labelled vector constructors ───────────────────────────────────────────────

make_lbl_s <- function(x, var_lab) {
  labelled(as.integer(x), label = var_lab)
}

make_lbl_l <- function(x, var_lab) {
  labelled(as.integer(x), label = var_lab)
}

make_cont <- function(x, var_lab) {
  labelled(as.double(x), label = var_lab)
}

make_weight <- function(x, var_lab) {
  labelled(as.double(x), label = var_lab)
}

# ── Helper: generate values only for a subgroup; rest → not-applicable code ───
subgroup_cont <- function(n, mask, gen_fn, na_code = 996) {
  out <- rep(as.double(na_code), n)
  k   <- sum(mask)
  if (k > 0) out[mask] <- inj_na_c(gen_fn(k))
  out
}


cchs_mock <- function(sample_2013, sample_2015, sample_2017) {

  # ── Shared raw simulator ─────────────────────────────────────────────────────
  # Returns a list of raw vectors (pre-labelling) common to all cycles.
  .sim_raw <- function(n) {

    smk_raw <- sample(1:6, n, TRUE, c(0.15, 0.07, 0.04, 0.35, 0.13, 0.26))
    alc_raw <- sample(1:3, n, TRUE, c(0.75, 0.21, 0.04))
    cmh_raw <- sample(1:2, n, TRUE, c(0.20, 0.80))

    is_curr_daily  <- smk_raw == 1
    is_curr_occ    <- smk_raw == 2
    is_always_occ  <- smk_raw == 3
    is_form_daily  <- smk_raw == 4
    is_form_occ    <- smk_raw == 5
    is_any_daily   <- is_curr_daily | is_form_daily
    is_any_occ     <- is_curr_occ   | is_always_occ | is_form_occ
    is_drinker     <- alc_raw == 1

    # Weekly drinks (drinkers only)
    drinks_wk <- subgroup_cont(n, is_drinker,
      function(k) pmax(0, round(rnorm(k, 3, 4), 0)))

    # Drinks per day: distribute from weekly total
    safe_wk <- ifelse(is.finite(drinks_wk) & drinks_wk < 990, drinks_wk, 0)
    day_raw <- lapply(1:7, function(d)
      inj_na_c(pmax(0, round(safe_wk * runif(n, 0, 0.35), 0))))

    list(
      # Demographics
      sex    = inj_na_s(sample(1:2, n, TRUE, c(0.43, 0.57))),
      own    = inj_na_s(sample(1:2, n, TRUE, c(0.62, 0.38))),
      age    = inj_na_c(pmax(18, round(rnorm(n, 67, 9), 0))),
      ht_cm  = inj_na_c(pmax(140, round(rnorm(n, 170, 9), 1))),
      wt_kg  = inj_na_w(pmax(35,  round(rnorm(n, 76, 15), 2))),

      # General health
      self_health   = inj_na_s(sample(1:5, n, TRUE, c(0.15, 0.32, 0.31, 0.07, 0.15))),
      life_sat      = inj_na_s(sample(1:5, n, TRUE, c(0.33, 0.49, 0.10, 0.05, 0.02))),
      mental_health = inj_na_s(sample(1:5, n, TRUE, c(0.18, 0.40, 0.30, 0.07, 0.05))),
      life_stress   = inj_na_s(sample(1:5, n, TRUE, c(0.22, 0.31, 0.33, 0.11, 0.025))),
      work_stress   = inj_na_s(sample(1:5, n, TRUE, c(0.22, 0.31, 0.33, 0.11, 0.025))),
      belonging     = inj_na_s(sample(1:4, n, TRUE, c(0.25, 0.45, 0.18, 0.08))),

      # Chronic conditions (1=Yes, 2=No)
      ccc_031 = inj_na_s(sample(1:2, n, TRUE, c(0.11, 0.89))),   # asthma
      ccc_051 = inj_na_s(sample(1:2, n, TRUE, c(0.27, 0.73))),   # arthritis
      ccc_061 = inj_na_s(sample(1:2, n, TRUE, c(0.22, 0.78))),   # back problems
      ccc_071 = inj_na_s(sample(1:2, n, TRUE, c(0.41, 0.59))),   # HBP
      ccc_091 = inj_na_s(sample(1:2, n, TRUE, c(0.08, 0.92))),   # COPD
      ccc_101 = inj_na_s(sample(1:2, n, TRUE, c(0.14, 0.86))),   # diabetes
      ccc_121 = inj_na_s(sample(1:2, n, TRUE, c(0.17, 0.83))),   # heart disease
      ccc_151 = inj_na_s(sample(1:2, n, TRUE, c(0.03, 0.97))),   # stroke
      ccc_171 = inj_na_s(sample(1:2, n, TRUE, c(0.06, 0.94))),   # CCC_171
      ccc_280 = inj_na_s(sample(1:2, n, TRUE, c(0.004, 0.996))), # CCC_280
      ccc_290 = inj_na_s(sample(1:2, n, TRUE, c(0.05, 0.95))),   # CCC_290

      # Functional
      pain_limit = inj_na_s(sample(1:5, n, TRUE, c(0.55, 0.20, 0.12, 0.08, 0.03))),

      # Physical activity
      mets         = inj_na_c(pmax(0, round(rnorm(n, 1.4, 1.5), 1))),
      paa_045      = inj_na_c(pmax(0L, round(rnorm(n, 3,  2),  0))),  # mod freq/wk
      paa_050      = inj_na_c(pmax(0L, round(rnorm(n, 30, 15), 0))),  # mod min/session
      paa_075      = inj_na_c(pmax(0L, round(rnorm(n, 2,  2),  0))),  # vig freq/wk
      paa_080      = inj_na_c(pmax(0L, round(rnorm(n, 25, 15), 0))),  # vig min/session
      paadvdys     = inj_na_c(pmax(0L, round(rnorm(n, 3,  2),  0))),  # days any PA
      paadvvig     = inj_na_c(pmax(0L, round(rnorm(n, 1.5,1.5),0))),  # days vig PA

      # Smoking
      smoked_100       = inj_na_s(as.integer(smk_raw %in% 1:5) + 1L), # 1=Yes 2=No
      smk_type         = inj_na_l(smk_raw),
      age_start_daily  = subgroup_cont(n, is_any_daily,
                           function(k) pmax(10, round(rnorm(k, 17, 4), 0))),
      n_cigs_curr_dly  = subgroup_cont(n, is_curr_daily,
                           function(k) pmax(1, round(rnorm(k, 14, 7), 0))),
      n_cigs_occ       = subgroup_cont(n, is_curr_occ | is_always_occ,
                           function(k) pmax(1, round(rnorm(k, 5, 3), 0))),
      n_cigs_form_dly  = subgroup_cont(n, is_form_daily,
                           function(k) pmax(1, round(rnorm(k, 16, 8), 0))),
      age_start_occ    = subgroup_cont(n, is_any_occ,
                           function(k) pmax(10, round(rnorm(k, 19, 5), 0))),
      yrs_stopped      = subgroup_cont(n, is_form_daily,
                           function(k) pmax(0, round(rnorm(k, 12, 8), 1))),
      yr_start_daily   = subgroup_cont(n, is_any_daily,   # for smk_040 derivation
                           function(k) pmax(1950, round(rnorm(k, 1975, 10), 0))),

      # Alcohol
      alc_type      = inj_na_s(alc_raw),
      drank_last_wk = inj_na_s(ifelse(is_drinker, 1L, 2L)),
      drinks_wk     = drinks_wk,
      day_1 = day_raw[[1]], day_2 = day_raw[[2]], day_3 = day_raw[[3]],
      day_4 = day_raw[[4]], day_5 = day_raw[[5]], day_6 = day_raw[[6]],
      day_7 = day_raw[[7]],

      # Mental health services
      cmh_consult   = inj_na_s(cmh_raw),
      cmh_n_consult = subgroup_cont(n, cmh_raw == 1,
                        function(k) pmax(1, round(rnorm(k, 5, 3), 0))),

      # Socio-demographic
      ethnicity = inj_na_l(sample(1:13, n, TRUE,
        c(0.74, 0.04, 0.01, 0.01, 0.01, 0.07, 0.03, 0.02, 0.01, 0.01, 0.02, 0.02, 0.01))),
      rural     = inj_na_s(sample(1:2, n, TRUE, c(0.15, 0.85))),
      mat_dep   = inj_na_s(sample(1:5, n, TRUE, rep(0.20, 5)))
    )
  }

  # ── 2013-2014: pre-2015 uppercase variable names ─────────────────────────────
  .make_2013 <- function(n) {
    r <- .sim_raw(n)
    tibble(
      cchs_year = "2013-2014",

      # Demographics
      DHH_SEX = make_lbl_s(r$sex, "Sex"),
      DHH_AGE = make_cont(r$age, "Age"),
      DHH_OWN = make_lbl_s(r$own, "Home ownership"),

      # General health
      GEN_01  = make_lbl_s(r$self_health,   "Self-perceived health"),
      GENGSWL = make_lbl_s(r$life_sat,      "Life satisfaction"),
      GEN_02B = make_lbl_s(r$mental_health, "Self-perceived mental health"),
      GEN_07  = make_lbl_s(r$life_stress,   "Self-perceived life stress"),
      GEN_09  = make_lbl_s(r$work_stress,   "Self-perceived work stress"),
      GEN_10  = make_lbl_s(r$belonging,     "Sense of belonging to local community"),

      # Chronic conditions
      CCC_031 = make_lbl_s(r$ccc_031, "Has asthma"),
      CCC_051 = make_lbl_s(r$ccc_051, "Has arthritis"),
      CCC_061 = make_lbl_s(r$ccc_061, "Has back problems"),
      CCC_071 = make_lbl_s(r$ccc_071, "Has high blood pressure"),
      CCC_091 = make_lbl_s(r$ccc_091, "Has COPD"),
      CCC_101 = make_lbl_s(r$ccc_101, "Has diabetes"),
      CCC_121 = make_lbl_s(r$ccc_121, "Has heart disease"),
      CCC_151 = make_lbl_s(r$ccc_151, "Has stroke"),
      CCC_171 = make_lbl_s(r$ccc_171, "Has bowel disorder"),
      CCC_280 = make_lbl_s(r$ccc_280, "Has mood disorder"),
      CCC_290 = make_lbl_s(r$ccc_290, "Has anxiety disorder"),

      # Anthropometrics
      HWTDHTM = make_cont(r$ht_cm,  "Height (cm)"),
      HWTDWTK = make_weight(r$wt_kg,"Weight (kg)"),

      # Functional (pre-2015 only)
      HUPDPAD = make_lbl_s(r$pain_limit, "Activities limited due to pain or discomfort"),

      # Physical activity
      PACDEE  = make_cont(r$mets, "Leisure activity energy expenditure (METs)"),

      # Smoking (pre-2015 specific)
      SMK_01A = make_lbl_s(r$smoked_100, "Smoked 100+ cigarettes in lifetime"),
      SMKDSTY = make_lbl_l(r$smk_type,   "Type of smoker"),
      SMK_203 = make_cont(r$age_start_daily, "Age started smoking daily (current daily smokers)"),
      SMK_204 = make_cont(r$n_cigs_curr_dly, "Number of cigarettes per day (current daily)"),
      SMK_05B = make_cont(r$n_cigs_occ,      "Number of cigarettes per day (occasional)"),
      SMK_207 = make_cont(r$age_start_daily, "Age started smoking daily"),
      SMK_208 = make_cont(r$n_cigs_form_dly, "Number of cigarettes per day (former daily)"),
      SMK_09A = make_cont(r$age_start_occ,   "Age started smoking (occasional)"),
      SMK_09C = make_cont(r$yrs_stopped,     "Years since stopped smoking daily"),

      # Alcohol
      ALCDTTM = make_lbl_s(r$alc_type,      "Type of drinker"),
      ALW_1   = make_lbl_s(r$drank_last_wk, "Drank alcohol in past week"),
      ALW_2A1 = make_cont(r$day_1, "Drinks on Monday"),
      ALW_2A2 = make_cont(r$day_2, "Drinks on Tuesday"),
      ALW_2A3 = make_cont(r$day_3, "Drinks on Wednesday"),
      ALW_2A4 = make_cont(r$day_4, "Drinks on Thursday"),
      ALW_2A5 = make_cont(r$day_5, "Drinks on Friday"),
      ALW_2A6 = make_cont(r$day_6, "Drinks on Saturday"),
      ALW_2A7 = make_cont(r$day_7, "Drinks on Sunday"),
      ALWDWKY = make_cont(r$drinks_wk, "Total drinks last week"),

      # Socio-demographic
      SDCDCGT = make_lbl_l(r$ethnicity, "Ethnic identity"),
      EDUDR04 = make_lbl_s(
                  inj_na_s(sample(1:4, n, TRUE, c(0.29, 0.18, 0.05, 0.46))),
                  "Highest level of education"),
      # Mental health services
      CMH_01K = make_lbl_s(r$cmh_consult,   "Consulted mental health professional in past year"),
      CMH_01L = make_cont(r$cmh_n_consult,  "Number of mental health consultations in past year"),

      rural                = make_lbl_s(r$rural,   "Rural resident"),
      material_deprivation = make_lbl_s(r$mat_dep, "Material deprivation quintile")
    )
  }

  # ── 2015-2016 and 2017-2018: post-2015 lowercase variable names ───────────────
  .make_post2015 <- function(n, year_label) {
    r <- .sim_raw(n)
    tibble(
      cchs_year = year_label,

      # Demographics
      dhh_sex = make_lbl_s(r$sex, "Sex"),
      dhh_age = make_cont(r$age, "Age"),
      dhh_own = make_lbl_s(r$own, "Home ownership"),

      # General health
      gen_005 = make_lbl_s(r$self_health,   "Self-perceived health"),
      GENGSWL = make_lbl_s(r$life_sat,      "Life satisfaction"),
      gen_015 = make_lbl_s(r$mental_health, "Self-perceived mental health"),
      gen_020 = make_lbl_s(r$life_stress,   "Self-perceived life stress"),
      gen_025 = make_lbl_s(r$work_stress,   "Self-perceived work stress"),
      gen_030 = make_lbl_s(r$belonging,     "Sense of belonging to local community"),

      # Chronic conditions
      ccc_015 = make_lbl_s(r$ccc_031, "Has asthma"),
      ccc_050 = make_lbl_s(r$ccc_051, "Has arthritis"),
      ccc_055 = make_lbl_s(if (year_label == "2017-2018") rep(996L, n) else r$ccc_061, "Has back problems"),
      ccc_065 = make_lbl_s(r$ccc_071, "Has high blood pressure"),
      ccc_030 = make_lbl_s(r$ccc_091, "Has COPD"),
      ccc_095 = make_lbl_s(r$ccc_101, "Has diabetes"),
      ccc_085 = make_lbl_s(r$ccc_121, "Has heart disease"),
      ccc_090 = make_lbl_s(r$ccc_151, "Has stroke"),
      ccc_155 = make_lbl_s(if (year_label == "2017-2018") rep(996L, n) else r$ccc_171, "Has bowel disorder"),
      ccc_195 = make_lbl_s(r$ccc_280, "Has mood disorder"),
      ccc_200 = make_lbl_s(r$ccc_290, "Has anxiety disorder"),

      # Anthropometrics (no HUPDPAD post-2015)
      hwtdvhtm = make_cont(r$ht_cm,  "Height (cm)"),
      hwtdvwtk = make_weight(r$wt_kg,"Weight (kg)"),

      # Physical activity source variables (PACDEE derived from these)
      paa_045  = make_cont(r$paa_045,  "Moderate PA: times per week"),
      paa_050  = make_cont(r$paa_050,  "Moderate PA: minutes per session"),
      paa_075  = make_cont(r$paa_075,  "Vigorous PA: times per week"),
      paa_080  = make_cont(r$paa_080,  "Vigorous PA: minutes per session"),
      paadvdys = make_cont(r$paadvdys, "Days of moderate/vigorous PA per week"),
      paadvvig = make_cont(r$paadvvig, "Days of vigorous PA per week"),

      # Smoking (no SMK_01A post-2015)
      smkdvsty = make_lbl_l(r$smk_type,   "Type of smoker"),
      smk_005  = make_cont(r$age_start_daily, "Age started smoking daily"),
      smk_040  = make_cont(r$yr_start_daily,  "Year started smoking daily"),
      smk_045  = make_cont(r$n_cigs_curr_dly, "Number of cigarettes per day (current daily)"),
      smk_050  = make_cont(r$n_cigs_occ,      "Number of cigarettes per day (occasional)"),
      smk_075  = make_cont(r$n_cigs_form_dly, "Number of cigarettes per day (former daily)"),
      smk_080  = make_cont(r$age_start_occ,   "Age started smoking (occasional)"),
      smk_090  = make_cont(r$yrs_stopped,     "Years since stopped smoking daily"),

      # Alcohol
      alcdvttm = make_lbl_s(r$alc_type,      "Type of drinker"),
      alw_005  = make_lbl_s(r$drank_last_wk, "Drank alcohol in past week"),
      alw_010  = make_cont(r$day_1, "Drinks on Monday"),
      alw_015  = make_cont(r$day_2, "Drinks on Tuesday"),
      alw_020  = make_cont(r$day_3, "Drinks on Wednesday"),
      alw_025  = make_cont(r$day_4, "Drinks on Thursday"),
      alw_030  = make_cont(r$day_5, "Drinks on Friday"),
      alw_035  = make_cont(r$day_6, "Drinks on Saturday"),
      alw_040  = make_cont(r$day_7, "Drinks on Sunday"),
      alwdvwky = make_cont(r$drinks_wk, "Total drinks last week"),

      # Socio-demographic
      sdcdvcgt = make_lbl_l(r$ethnicity, "Ethnic identity"),
      ehg2dvr3 = make_lbl_s(
                   inj_na_s(sample(1:3, n, TRUE, c(0.27, 0.21, 0.52))),
                   "Highest level of education"),
      # Mental health services (996-filled in 2017-2018)
      cmh_005 = make_lbl_s(if (year_label == "2017-2018") rep(996L, n) else r$cmh_consult,
                  "Consulted mental health professional in past year"),
      cmh_010 = make_cont(if (year_label == "2017-2018") rep(996, n) else r$cmh_n_consult,
                  "Number of mental health consultations in past year"),

      rural                = make_lbl_s(r$rural,   "Rural resident"),
      material_deprivation = make_lbl_s(r$mat_dep, "Material deprivation quintile"),

      # Post-2015 only: illicit drug use
      drgdvlac = make_lbl_s(
                   inj_na_s(sample(1:2, n, TRUE, c(0.15, 0.85))),
                   "Illicit drug use - lifetime (excl. one-time marijuana)"),
      drgdvyac = make_lbl_s(
                   inj_na_s(sample(1:2, n, TRUE, c(0.07, 0.93))),
                   "Illicit drug use - last year (excl. one-time marijuana)")
    )
  }

  # ── Return ────────────────────────────────────────────────────────────────────
  list(
    cchs_2013 = .make_2013(sample_2013),
    cchs_2015 = .make_post2015(sample_2015, "2015-2016"),
    cchs_2017 = .make_post2015(sample_2017, "2017-2018")
  )
}
