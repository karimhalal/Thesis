#'@title CCHS PUMF-Based Mock
#'
#' @description This function generates 3 cycles of CCHS mock data using variable
#' availability information from ICES intranet (ONTARIO-specific) and using the PUMF data as the guide
#' to replicate intervariable relationships
#'
#' @param data_2013 [df] 2013 PUMF data to be used to inform mock data
#' 
#' @param data_2015 [df] 2015 PUMF data to be used to inform mock data
#' 
#' @param data_2017 [df] 2015-2018 PUMF data to be used to inform mock data
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
#'            Province: not available in actual data (because it is only one province, ontario). GEOGPRV in 2013, and GEO_PRV post-2015, the posibilities are as follows:
#'            10 = Newfoundland and Labrador, 11 = Prince Edward Island, 12 = Nova Scotia, 13 = New Brunswick, 24 = Quebec, 35 = Ontario, 46 = Manitoba, 47 = Saskatchewan, 48 = Alberta
#'            59 = British Columbia, 60 = Yukon, 61 = Northwest Territories, 62 = Nunavut, 96 = Valid skip, 97 = Don't know, 98 = Refusal, 99 = Not stated. Only Ontario partcipants will be used to generate the final data.
#' ]
#'            
#'            DHH_AGE (dhh_age post-2015) CONTINOUS, in pumf, it is DHHGAGE which categorized into 16 groups 01 = Age between 12 and 14
#'            02 = Age between 15 and 17, 03 = Age between 18 and 19, 04 = Age between 20 and 24, 05 = Age between 25 and 29
#'            ,06 = Age between 30 and 34, 07 = Age between 35 and 39, 08 = Age between 40 and 44, 09 = Age between 45 and 49, 10 = Age between 50 and 54, 11 = Age between 55 and 59
#'            12 = Age between 60 and 64, 13 = Age between 65 and 69, 14 = Age between 70 and 74, 15 = Age between 75 and 79, 16 = Age 80 and older. Take the midpoint of each category as the "pseudocontinous" values in the final mock
#'            
#'            DHH_SEX(dhh_sex post-2015) 2 category categorical labelled male and female and coded the same way in pumf
#'            DHH_OWN(dhh_own post-2015) 2 category categorical labelled owner and renter, same in PUMF
#'            cchs_year(same post-2015) 3 categories (2013-2014, 2015-2016, 2017-2018). Not available but should be derived from the dataset definition in the function (ex. cchs_2013==df_name in function arguments)
#'            GEN_01(gen_005 post-2015) self preceived health 5 categories: excellent, very good, good, fair, poor. Coded and named the same way in the PUMF
#'            GEN_02A2 (GEN_010 post-2015) coded the same way in the PUMF, ordinal variable representing a scale (1 (very dissatisfied)-10:very satisfied)
#'            GENGSWL(same post-2015) derived from GEN_02A2: life satisfaction 5 categories: very satisfied, satisfied, neither satisfied or dissatisfied, dissatisfied, very dissatisfied. 
#'            GEN_02B(gen_015 post-2015) self preceived mental health 5 categories: excellent, very good, good, fair, poor. PUMF file has the same coding
#'            GEN_07(gen_020 post-2015) self preceived life stress 5 categories: not at all, not very, a bit, quite a bit, extremely. PUMF file has the same coding
#'            GEN_09(gen_025 post-2015) self preceived work stress 5 categories: not at all, not very, a bit, quite a bit, extremely. PUMF file has the same coding
#'            GEN_10(gen_030 post-2015) sense of belonging to local community 4 categories: very strong, somewhat strong, somewhat weak, very weak. . PUMF file has the same coding
#'            CCC_031(ccc_015 post-2015) has asthma categories: yes, no. . PUMF file has the same coding
#'            CCC_051(ccc_050 post-2015) has arthiritis categories: yes, no. . PUMF file has the same coding
#'            CCC_061(ccc_055 post-2015) has back problems categories: yes, no. In 2017-2018, this variable is missing but it is still available as a column but populated completely with 996 in the real data. In the PUMF, there are many 996s but not all of them are.
#'            CCC_071(ccc_065 post-2015) has HBP categories: yes, no. PUMF file has the same coding
#'            CCC_091(ccc_030 post-2015) has COPD categories: yes, no. PUMF file has the same coding
#'            CCC_101(ccc_095 post-2015) has diabetes categories: yes, no. PUMF file has the same coding
#'            CCC_131(ccc_130 post-2015) has cancer categories: yes, no. PUMF file has the same coding
#'            CCC_121(ccc_085 post-2015) has heart disease categories: yes, no. PUMF file has the same coding
#'            CCC_151(ccc_090 post-2015) has stroke categories: yes, no. PUMF file has the same coding
#'            CCC_171(ccc_155 post-2015) has bowel disorder categories: yes, no. In 2017-2018, this variable is missing but it is still available as a column but populated completely with 996. In the PUMF, there are many 996s but not all of them are.
#'            CCC_071(ccc_065 post-2015) has HBP categories: yes, no. PUMF file has the same coding
#'            CCC_280(ccc_195 post-2015) has mood disorder categories: yes, no. PUMF file has the same coding
#'            CCC_290(ccc_200 post-2015) has anxiety disorder categories: yes, no. PUMF file has the same coding
#'            
#'            HWTDHTM(hwtdvhtm post-2015) HEIGHT IN cm continous. In the PUMF, the equivalent variables are not continous (HWTGHTM 2013, HWTDGHTM post-2015): 0.914 = '0.926 M OR LESS', 0.940 = '0.927 TO 0.952 M', 0.965 = '0.953 TO 0.977 M', 0.991 = '0.978 TO 1.002 M', 1.016 = '1.003 TO 1.028 M'
#'            1.041 = '1.029 TO 1.053 M', 1.067 = '1.054 TO 1.079 M', 1.092 = '1.080 TO 1.104 M', 1.118 = '1.105 TO 1.129 M', 1.143 = '1.130 TO 1.155 M', 1.168 = '1.156 TO 1.180 M', 1.194 = '1.181 TO 1.206 M', 1.219 = '1.207 TO 1.231 M', 1.245 = '1.232 TO 1.256 M', 1.270 = '1.257 TO 1.282 M', 1.295 = '1.283 TO 1.307 M', 1.321 = '1.308 TO 1.333 M', 1.346 = '1.334 TO 1.358 M', 1.372 = '1.359 TO 1.383 M'
#'            1.397 = '1.384 TO 1.409 M', 1.422 = '1.410 TO 1.434 M', 1.448 = '1.435 TO 1.460 M', 1.473 = '1.461 TO 1.485 M', 1.499 = '1.486 TO 1.510 M', 1.524 = '1.511 TO 1.536 M', 1.549 = '1.537 TO 1.561 M', 1.575 = '1.562 TO 1.587 M', 1.600 = '1.588 TO 1.612 M', 1.626 = '1.613 TO 1.637 M', 1.651 = '1.638 TO 1.663 M'
#'            1.676 = '1.664 TO 1.688 M', 1.702 = '1.689 TO 1.714 M', 1.727 = '1.715 TO 1.739 M', 1.753 = '1.740 TO 1.764 M', 1.778 = '1.765 TO 1.790 M', 1.803 = '1.791 TO 1.815 M', 1.829 = '1.816 TO 1.841 M', 1.854 = '1.842 TO 1.866 M', 1.880 = '1.867 TO 1.891 M', 1.905 = '1.892 TO 1.917 M', 1.930 = '1.918 TO 1.942 M'
#'            1.956 = '1.943 TO 1.968 M', 1.981 = '1.969 TO 1.993 M', 2.007 = '1.994 TO 2.018 M', 2.032 = '2.019 TO 2.044 M', 2.057 = '2.045 TO 2.069 M', 2.083 = '2.070 TO 2.095 M', 2.108 = '2.096 TO 2.120 M', 2.134 = '2.121M OR TALLER'. These values will be used as is.
#'
#'            HWTDWTK(hwtdvwtk post-2015) weight in kg continous but only set values in the PUMF, use these values for the mock directly from the PUMF
#'            HUPDPAD(no corresponding variables post-2015) categorical 5 categories: no pain, pain does not prevent activity, prevents a few activities, prevents some activities, prevents most activities
#'            PACDEE(derived from paa_045, paa_050, paa_075, paa_080, paadvdys, and paadvvig post-2015) MET (energy expenditure measure) for leisure activities. Coded the same way in PUMF as well
#'            SMK_01A(no variables post-2015) smoked 100+cigs categorical (2 categories): yes, no. Same coding in the pumf
#'            smk_005(no equivalent variables in 2013) type of smoker presently 3 category (daily, occasionally, not at all). Same coding in the pumf
#'            
#'            SMK_203(derived from smk_005 and the continous smk_040 variable post 2015) age started smoking daily (daily smokers) continous. In the PUMF, smk_005 is codded the same, SMKG040 is the non-continous smk_040 equivalent which is categorized as follows:
#'            01 = Began smoking daily between ages 5 and 11., 02 = Began smoking daily between ages 12 and 14., 03 = Began smoking daily between ages 15 and 17, 04 = Began smoking daily between ages 18 and 19, 05 = Began smoking daily between ages 20 and 24
#'            06 = Began smoking daily between ages 25 and 29, 07 = Began smoking daily between ages 30 and 34, 08 = Began smoking daily between ages 35 and 39, 09 = Began smoking daily between ages 40 and 44, 10 = Began smoking daily between ages 45 and 49. 11 = Began smoking daily at age 50 or older. 
#'            SMKG203 is categorized as follows: 1 = '5 TO 11 YEARS', 2 = '12 TO 14 YEARS', 3 = '15 TO 17 YEARS', 4 = '18 TO 19 YEARS', 5 = '20 TO 24 YEARS', 6 = '25 TO 29 YEARS', 7 = '30 TO 34 YEARS', 8 = '35 TO 39 YEARS', 9 = '40 TO 44 YEARS', 10 = '45 TO 49 YEARS', 11 = '50 YEARS OR MORE'
#'            Take the midpoint of each category to create a pseudo-continous values that are labelled as mentioned at the begining of this blurb. 
#' 
#'            SMK_204(smk_045 post-2015) number of cig smoked per day (current daily) continous. Equivalents in PUMF are SMKG204 and smkg040, both categorized as follows: 
#'            01 = Began smoking daily between ages 5 and 11, 02 = Began smoking daily between ages 12 and 14, 03 = Began smoking daily between ages 15 and 17, 04 = Began smoking daily between ages 18 and 19, 05 = Began smoking daily between ages 20 and 24,
#'            06 = Began smoking daily between ages 25 and 29, 07 = Began smoking daily between ages 30 and 34, 08 = Began smoking daily between ages 35 and 39, 09 = Began smoking daily between ages 40 and 44, 10 = Began smoking daily between ages 45 and 49.
#'            11 = Began smoking daily at age 50 or older. Take the midpoint of each category to create a pseudo-continous value. Out put 50 for the highest category
#'            
#'            SMK_207(derived from smk_005 and smk_040 post-2015) age started smoking daily continous. SMKG207 is the equivalent 2013. Generate the same categories as the ones described in SMKG203.
#'            SMK_208(smk_075 post-2015) number of cig smoked per day (former daily) continous. Same coding in the pumf
#'            SMK_05B(smk_050 post-2015) number of cig smoked per day (occasional smoker) continous. Same coding in the PUMF
#'            SMK_06A (smk_060 post-2015) stopped smoking never daily. 4 categories 
#'                      1 = 'LESS THAN 1 YEAR'
#'                      2 = '1 TO < 2 YEARS'
#'                      3 = '2 TO < 3 YEARS'
#'                      4 = '3 OR MORE YEARS'
#'            Coded the same in the PUMF.
#'            SMK_09A(smk_080 post-2015) stopped smoking daily- when. 5 category variable: 1 = 'LESS THAN 1 YEAR', 2 = '1 TO < 2 YEARS', 3 = '2 TO < 3 YEARS', 4 = '3 OR MORE YEARS'. Same coding the PUMF
#'            SMK_09C(smk_090 post-2015) number of YEARS since stopped smoking daily continous. In the PUMF, the equivalent variables (SMKG09C and SMKG090) categories are as follows: 1 = '3 TO 5 YEARS', 2 = '6 TO YEARS', 3 = '11 OR MORE YEARS'. 
#'            Take the midpoint of each category to make pseudo continous values that has SMK_09C pre 2015 and smk_090.
#'            
#'            SMKDSTP (smkdvstp post-2015)number of years stopped smoking completely continous. PUMF equivalent is a categorical. These are SMKGSTP in 2014 with categories: 
#'                 1 = '< 1 YEAR'
#'                 2 = '1 TO 2 YEARS'
#'                 3 = '3 TO 5 YEARS'
#'                 4 = '6 TO 10 YEARS'
#'                 5 = '11 OR MORE YEARS'0 = Less than 1 year
#'            Post 2015, the equivalent SMKDGSTP categorical is coded as follows:
#'                 0= <1year
#'                 1 = 1 to 2 years
#'                 2 = 3 to 5 years
#'                 3 = 6 to 10 years
#'                 4 = 11 or more years
#'            For both versions of categorical SMKDSTP, take the midpoints to produce psuedo-continuous variables. 
#' 
#'            SMKDSTY(smkdvsty post-2015) type of smoker categorical (6 categories): dialy smoker, occasional smoker, always occasional smoker, former daily smoker, former occasional smoker, never smoked
#'            ALCDTTM(alcdvttm post-2015) type of drinker categorical (3 categories): regualr drinker, occasional drinker, did not drink in the last 12 months. Coded the same in the pumf
#'            ALW_1(alw_005 post-2015) drank alcohol in the past week (2 categories):yes, no. Same coding in the PUMF.
#'            Number of drinks each day of the week (ALW_2A1-ALW_2A7 pre 2015 and alw_010-alw_040 in 005 increments for post 2015 cycles). Same coding for all these variables in the pumf, copy directly
#'            ALWDWKY(alwdvwky post-2015) weekly intake of alcohol continous
#'            SDCDCGT(sdcdvcgt post-2015) ethnic identity 13 category: white, black, korean, filipino, japanese, chinese, south asian, south east asian, arab, west asian, latin american, other, multiple origin. It is SDCGCGT in 2013 pumf and SDCDGCGT in the post 2015 pumf, both which have only 2 categories(white and non white- corresponding to 1 and 2) 
#'            EDUDR04(ehg2dvr3 post-2015) individual educational attainment 4 categories pre 2015: less than secondary, secondary grad, some post-secondary, post-secondary grad and 3 categories post 2015: less than secondary school, secondary grad, post-secondary education. Coded the same in the pumf just copy the values over
#'            CMH_01K(cmh_005 post-2015) consulted a mental health profesional- binary yes, no (1,2). In 2017-2018, this variable is missing but it is still available as a column but populated completely with 996
#'            CMH_01L(cmh_010 post-2015) number of times consulted a mental health profesional last year- continous (only whole numbers). In 2017-2018, this variable is missing but it is still available as a column but populated completely with 996
#'            rural(same for all cycles) rural status binary yes no. Just have random assignment for the mock data, it should be 17% rural and 83% urban. 
#'            material_deprivation(same for all cycles) material deprivation quintile (5 cats corresponding to each quintile). Randomly generate this (there is not equivalent in the pumf)
#'            drgdvlac(only available post 2015) illicit drug use-lifetime (excluding one time marijuana) binary yes no, same coding in the pumf. 
#'            drgdvyac(only available post 2015) illicit drug use-last year (excluding one time marijuana) binary yes no. In the PUMF file, this variable is coded the same way
#'            LBSDWSS(lbfdvwss post 2015) working status last week. Coded the same in the pumf
#'            FSCDHFS2 (fscdvhfs post 2015 ) food security status 5 categories (0-4)
#'            INCDRCA (incdvsca post 2015) 10 category income deciles, coded the same in pumf
#'            INCDVPR (incdvspr post 2015) 10 category income deciles provincial coded the same in pumf
#'            INCDVRRS (incdvsrs post 2015) 10 cateogry income deciles health region level. coded the same in the pumf
#' 
#' NA tracking for variables
#'            for continous variables: 996,997,998,or 999 where 996=not applicable and 997,998,999=refusal/DK/missing
#'            for categorical variables: depends on the number of categories
#'               For variables with less than 6 cats--> 6,7,8,9 where 6=not applicable and 7,8,9=refusal/DK/missing
#'               For variables with more than 6 categories--> 96,97,98,99 where 96=not applicable and 97,98,99=refusal/DK/missing
#'                    exception: weight variables use 999.96,999.97,999.98,999.99
#'
#' @return [list] containing 3 datasets
#' 
#' @examples
#' # Equal sample size across all cycles
#' # pumf_mock(pumf_2013, pumf_2015, pumf_2017, 100, 100, 100)
#' 

library(dplyr)
library(labelled)

# ── Category-to-midpoint helpers ──────────────────────────────────────────────

# Map a categorical vector (NA codes 96-99) to numeric midpoints; NA codes
# are preserved as-is (2-digit or 3-digit, whatever is in the data).
.cat_to_mid <- function(x, midpoints) {
  x      <- as.integer(x)
  result <- as.double(x)
  na_hi  <- x %in% c(96L, 97L, 98L, 99L)
  valid  <- !is.na(x) & !na_hi
  result[valid] <- midpoints[pmin(pmax(x[valid], 1L), length(midpoints))]
  result
}

# same but for larger variable with NA codes: 996-999
.lcat_to_mid <- function(x, midpoints) {
  x      <- as.integer(x)
  result <- as.double(x)
  na_hi  <- x %in% c(996L, 997L, 998L, 999L)
  valid  <- !is.na(x) & !na_hi
  result[valid] <- midpoints[pmin(pmax(x[valid], 1L), length(midpoints))]
  result
}
# Same but for small-category vars (NA codes 6-9); preserved as-is.
.scat_to_mid <- function(x, midpoints) {
  x      <- as.integer(x)
  result <- as.double(x)
  na_lo  <- x %in% c(6L, 7L, 8L, 9L)
  valid  <- !is.na(x) & !na_lo
  result[valid] <- midpoints[pmin(pmax(x[valid], 1L), length(midpoints))]
  result
}

# Convert PUMF height (meters, e.g. 1.702) to cm; preserves 999.9x NA codes.
.ht_to_cm <- function(x) {
  x     <- as.double(x)
  valid <- !is.na(x) & x < 900
  x[valid] <- x[valid]
  x
}

# Expand 2-category PUMF ethnicity (1=white, 2=non-white) to 13-category target.
.expand_eth <- function(x) {
  x <- as.integer(x)
  nw_probs <- c(0.04, 0.01, 0.01, 0.01, 0.07, 0.03, 0.02, 0.01, 0.01, 0.02, 0.02, 0.01)
  nw_probs <- nw_probs / sum(nw_probs)
  is_nw    <- !is.na(x) & x == 2L
  if (any(is_nw)) x[is_nw] <- sample(2L:13L, sum(is_nw), replace = TRUE, prob = nw_probs)
  x
}

# NA introduction for randomly generate 
.inj_na_s <- function(x, rate_a = 0.05, rate_b = 0.05) {
  n <- length(x)
  x[sample(n, round(n * rate_a))] <- 6L
  x[sample(n, round(n * rate_b))] <- sample(7L:9L, round(n * rate_b), replace = TRUE)
  x
}

# Survival date: ~70% June 30 2024, remainder uniform between Jan 2013 and June 29 2024
.gen_survdate <- function(n) {
  n_june30 <- round(n * 0.70)
  n_random  <- n - n_june30
  random_dates <- sample(
    seq(as.Date("2013-01-01"), as.Date("2024-06-29"), by = "day"),
    n_random, replace = TRUE
  )
  sample(c(rep(as.Date("2024-06-30"), n_june30), random_dates))
}

# Event flag: 0=censored (80%), 1=event (5%), 2=competing event (15%)
.gen_event_flag <- function(n) {
  sample(c(0L, 1L, 2L), n, replace = TRUE, prob = c(0.80, 0.05, 0.15))
}

# label assignment functions
.lbl_s  <- function(x, lab) labelled(as.integer(x), label = lab)
.lbl_l  <- function(x, lab) labelled(as.integer(x), label = lab)
.cont   <- function(x, lab) labelled(as.double(x),  label = lab)
.weight <- function(x, lab) labelled(as.double(x),  label = lab)

# ── Filter to Ontario (province == 35) and draw n rows with replacement ───────
.sample_on <- function(df, prov_var, n) {
  on <- df[!is.na(df[[prov_var]]) & as.integer(df[[prov_var]]) == 35L, ]
  if (nrow(on) == 0L)
    stop(sprintf("No Ontario participants found (%s == 35)", prov_var))
  on[sample(nrow(on), n, replace = TRUE), , drop = FALSE]
}

# ── Midpoint lookup tables ────────────────────────────────────────────────────

# DHHGAGE categories 01-16
.AGE_MID <- c(13, 16, 18.5, 22, 27, 32, 37, 42, 47, 52, 57, 62, 67, 72, 77, 82)

# SMKG203 / SMKG207 / smkg040 age-started categories 01-11
.SMK_AGE_MID <- c(8, 13, 16, 18.5, 22, 27, 32, 37, 42, 47, 50)

# SMK_09C / smk_090 years-since-stopped categories 01-03
# 1='3-5yr', 2='6-10yr', 3='11+yr'
.SMK09C_MID <- c(4, 8, 15)

.cmh_01l_mid<-c(1,2,3,4,5,6,7,8,9,10,11,20)

# SMKGSTP (pre-2015, codes 1-5) / SMKDGSTP (post-2015, codes 0-4) years-stopped-completely
# 1/<0>=<1yr, 2/1=1-2yr, 3/2=3-5yr, 4/3=6-10yr, 5/4=11+yr
.SMKDSTP_MID <- c(0.5, 1.5, 4, 8, 15)

# ── Main function ─────────────────────────────────────────────────────────────

pumf_mock <- function(data_2013, data_2015, data_2017,
                      sample_2013, sample_2015, sample_2017) {

  # ── 2013-2014 ──────────────────────────────────────────────────────────────
  .make_2013 <- function(df, n) {
    s <- .sample_on(df, "GEOGPRV", n)

    age_c    <- .cat_to_mid(s[["DHHGAGE"]],  .AGE_MID)
    ht_cm    <- .ht_to_cm(s[["HWTGHTM"]])
    smk203_c <- .cat_to_mid(s[["SMKG203"]], .SMK_AGE_MID)
    smk207_c <- .cat_to_mid(s[["SMKG207"]], .SMK_AGE_MID)
    smk09c_c  <- .scat_to_mid(s[["SMKG09C"]], .SMK09C_MID)
    smkdstp_c <- .cat_to_mid(s[["SMKGSTP"]], .SMKDSTP_MID)
    cmh_01c   <- .cat_to_mid(s[["CMHG01L"]], .cmh_01l_mid)
    eth        <- .expand_eth(s[["SDCGCGT"]])

    tibble(
      cchs_year = "2013-2014",

      DHH_SEX  = .lbl_s(s[["DHH_SEX"]],  "Sex"),
      DHH_AGE  = .cont(age_c,             "Age"),
      CMH_01L = .cont(cmh_01c, "Number of mental health consultations"),
      DHH_OWN  = .lbl_s(s[["DHH_OWN"]],  "Home ownership"),
      DHH_MS   = .lbl_s(s[["DHHGMS"]],   "Marital status"),
      LBSDWSS  = .lbl_s(s[["LBSDWSS"]],  "Working status last week"),
      FSCDHFS2 = .lbl_s(s[["FSCDHFS2"]], "Food security status"),

      GEN_01   = .lbl_s(s[["GEN_01"]],   "Self-perceived health"),
      GEN_02A2 = .lbl_l(s[["GEN_02A2"]], "Life satisfaction scale (1-10)"),
      GENGSWL  = .lbl_s(s[["GENGSWL"]],  "Life satisfaction"),
      GEN_02B  = .lbl_s(s[["GEN_02B"]],  "Self-perceived mental health"),
      GEN_07   = .lbl_s(s[["GEN_07"]],   "Self-perceived life stress"),
      GEN_09   = .lbl_s(s[["GEN_09"]],   "Self-perceived work stress"),
      GEN_10   = .lbl_s(s[["GEN_10"]],   "Sense of belonging to local community"),

      CCC_031  = .lbl_s(s[["CCC_031"]], "Has asthma"),
      CCC_051  = .lbl_s(s[["CCC_051"]], "Has arthritis"),
      CCC_061  = .lbl_s(s[["CCC_061"]], "Has back problems"),
      CCC_071  = .lbl_s(s[["CCC_071"]], "Has high blood pressure"),
      CCC_091  = .lbl_s(s[["CCC_091"]], "Has COPD"),
      CCC_101  = .lbl_s(s[["CCC_101"]], "Has diabetes"),
      CCC_131  = .lbl_s(s[["CCC_131"]], "Has cancer"),
      CCC_121  = .lbl_s(s[["CCC_121"]], "Has heart disease"),
      CCC_151  = .lbl_s(s[["CCC_151"]], "Has stroke"),
      CCC_171  = .lbl_s(s[["CCC_171"]], "Has bowel disorder"),
      CCC_280  = .lbl_s(s[["CCC_280"]], "Has mood disorder"),
      CCC_290  = .lbl_s(s[["CCC_290"]], "Has anxiety disorder"),

      HWTDHTM  = .cont(ht_cm,                       "Height (cm)"),
      HWTDWTK  = .weight(as.double(s[["HWTGWTK"]]), "Weight (kg)"),

      HUPDPAD  = .lbl_s(s[["HUPDPAD"]], "Activities limited due to pain or discomfort"),

      PACDEE   = .cont(as.double(s[["PACDEE"]]), "Leisure activity energy expenditure (METs)"),

      SMK_01A  = .lbl_s(s[["SMK_01A"]],          "Smoked 100+ cigarettes in lifetime"),
      SMKDSTY  = .lbl_l(s[["SMKDSTY"]],          "Type of smoker"),
      SMK_203  = .cont(smk203_c,                  "Age started smoking daily (current daily smokers)"),
      SMK_204  = .cont(as.double(s[["SMK_204"]]), "Number of cigarettes per day (current daily)"),
      SMK_05B  = .cont(as.double(s[["SMK_05B"]]), "Number of cigarettes per day (occasional)"),
      SMK_06A  = .lbl_s(s[["SMK_06A"]],           "Stopped smoking (never daily) - when"),
      SMK_207  = .cont(smk207_c,                  "Age started smoking daily (former daily smokers)"),
      SMK_208  = .cont(as.double(s[["SMK_208"]]), "Number of cigarettes per day (former daily)"),
      SMK_09A  = .lbl_s(s[["SMK_09A"]],           "Stopped smoking daily - when"),
      SMK_09C  = .cont(smk09c_c,                  "Years since stopped smoking daily"),
      SMKDSTP  = .cont(smkdstp_c,                 "Years since stopped smoking completely"),

      ALCDTTM  = .lbl_s(s[["ALCDTTM"]], "Type of drinker"),
      ALW_1    = .lbl_s(s[["ALW_1"]],   "Drank alcohol in past week"),
      ALW_2A1  = .cont(as.double(s[["ALW_2A1"]]), "Drinks on Monday"),
      ALW_2A2  = .cont(as.double(s[["ALW_2A2"]]), "Drinks on Tuesday"),
      ALW_2A3  = .cont(as.double(s[["ALW_2A3"]]), "Drinks on Wednesday"),
      ALW_2A4  = .cont(as.double(s[["ALW_2A4"]]), "Drinks on Thursday"),
      ALW_2A5  = .cont(as.double(s[["ALW_2A5"]]), "Drinks on Friday"),
      ALW_2A6  = .cont(as.double(s[["ALW_2A6"]]), "Drinks on Saturday"),
      ALW_2A7  = .cont(as.double(s[["ALW_2A7"]]), "Drinks on Sunday"),
      ALWDWKY  = .cont(as.double(s[["ALWDWKY"]]), "Total drinks last week"),

      SDCDCGT  = .lbl_l(eth, "Ethnic identity"),
      EDUDR04  = .lbl_s(s[["EDUDR04"]], "Highest level of education"),
      INCDRCA  = .lbl_l(s[["INCDRCA"]], "Household income decile - Canada"),
      INCDRPR  = .lbl_l(s[["INCDRPR"]], "Household income decile - province"),
      INCDRRS  = .lbl_l(s[["INCDRRS"]], "Household income decile - health region"),

      CMH_01K  = .lbl_s(s[["CMH_01K"]],           "Consulted mental health professional in past year"),


      rural                = labelled(.inj_na_s(sample(1:2, n, TRUE, c(0.17, 0.83))),
                                     labels = c(Rural = 1L, Urban = 2L),
                                     label  = "Rural resident"),
      material_deprivation = .lbl_s(.inj_na_s(sample(1:5, n, TRUE, rep(0.20, 5))),  "Material deprivation quintile"),

      survdate   = .gen_survdate(n),
      event_flag = .gen_event_flag(n)
    )
  }

  # ── 2015-2016 and 2017-2018 ────────────────────────────────────────────────
  .make_post2015 <- function(df, n, year_label) {
    s      <- .sample_on(df, "GEO_PRV", n)
    is2017 <- year_label == "2017-2018"

    age_c    <- .cat_to_mid(s[["DHHGAGE"]],   .AGE_MID)
    ht_cm    <- .ht_to_cm(s[["HWTDGHTM"]])
    smk_ag_c <- .cat_to_mid(s[["SMKG040"]], .SMK_AGE_MID)
    smk090_c     <- .scat_to_mid(s[["SMKG090"]], .SMK09C_MID)
    smkdgstp_raw <- as.integer(s[["SMKDGSTP"]])
    smkdgstp_c   <- .cat_to_mid(
      ifelse(smkdgstp_raw %in% c(96L, 97L, 98L, 99L), smkdgstp_raw, smkdgstp_raw + 1L),
      .SMKDSTP_MID)
    cmh_010c     <- .cat_to_mid(s[["CMHG010"]], .cmh_01l_mid)
    eth           <- .expand_eth(s[["SDCDGCGT"]])

    tibble(
      cchs_year = year_label,

      dhh_sex  = .lbl_s(s[["DHH_SEX"]], "Sex"),
      dhh_age  = .cont(age_c,           "Age"),
      cmh_010 = .cont(cmh_010c, "Number of mental health consultations"),
      dhh_own  = .lbl_s(s[["DHH_OWN"]], "Home ownership"),
      dhh_ms   = .lbl_s(s[["DHHGMS"]],  "Marital status"),
      lbfdvwss = .lbl_s(s[["LBFDVWSS"]], "Working status last week"),
      fscdvhfs = .lbl_s(s[["FSCDVHFS"]], "Food security status"),

      gen_005  = .lbl_s(s[["GEN_005"]], "Self-perceived health"),
      gen_010  = .lbl_l(s[["GEN_010"]], "Life satisfaction scale (1-10)"),
      GENGSWL  = .lbl_s(s[["GENDVSWL"]], "Life satisfaction"),
      gen_015  = .lbl_s(s[["GEN_015"]], "Self-perceived mental health"),
      gen_020  = .lbl_s(s[["GEN_020"]], "Self-perceived life stress"),
      gen_025  = .lbl_s(s[["GEN_025"]], "Self-perceived work stress"),
      gen_030  = .lbl_s(s[["GEN_030"]], "Sense of belonging to local community"),

      ccc_015  = .lbl_s(s[["CCC_015"]], "Has asthma"),
      ccc_050  = .lbl_s(s[["CCC_050"]], "Has arthritis"),
      ccc_055  = .lbl_s(if (is2017) rep(996L, n) else as.integer(s[["CCC_055"]]), "Has back problems"),
      ccc_065  = .lbl_s(s[["CCC_065"]], "Has high blood pressure"),
      ccc_030  = .lbl_s(s[["CCC_030"]], "Has COPD"),
      ccc_095  = .lbl_s(s[["CCC_095"]], "Has diabetes"),
      ccc_130  = .lbl_s(s[["CCC_130"]], "Has cancer"),
      ccc_085  = .lbl_s(s[["CCC_085"]], "Has heart disease"),
      ccc_090  = .lbl_s(s[["CCC_090"]], "Has stroke"),
      ccc_155  = .lbl_s(rep(996L, n), "Has bowel disorder"),
      ccc_195  = .lbl_s(s[["CCC_195"]], "Has mood disorder"),
      ccc_200  = .lbl_s(s[["CCC_200"]], "Has anxiety disorder"),

      hwtdvhtm = .cont(ht_cm,                        "Height (cm)"),
      hwtdvwtk = .weight(as.double(s[["HWTDGWTK"]]), "Weight (kg)"),

      paa_045  = .cont(as.double(s[["PAA_045"]]),  "Moderate PA: times per week"),
      paa_050  = .cont(as.double(s[["PAA_050"]]),  "Moderate PA: minutes per session"),
      paa_075  = .cont(as.double(s[["PAA_075"]]),  "Vigorous PA: times per week"),
      paa_080  = .cont(as.double(s[["PAA_080"]]),  "Vigorous PA: minutes per session"),
      paadvdys = .cont(as.double(s[["PAADVDYS"]]), "Days of moderate/vigorous PA per week"),
      paadvvig = .cont(as.double(s[["PAADVVIG"]]), "Days of vigorous PA per week"),

      smkdvsty = .lbl_l(s[["SMKDVSTY"]],           "Type of smoker"),
      smk_005  = .lbl_s(s[["SMK_005"]],            "Type of smoker presently"),
      smk_040  = .cont(smk_ag_c,                   "Age started smoking daily"),
      smk_045  = .cont(as.double(s[["SMK_045"]]),  "Number of cigarettes per day (current daily)"),
      smk_050  = .cont(as.double(s[["SMK_050"]]),  "Number of cigarettes per day (occasional)"),
      smk_060  = .lbl_s(s[["SMK_060"]],            "Stopped smoking (never daily) - when"),
      smk_075  = .cont(as.double(s[["SMK_075"]]),  "Number of cigarettes per day (former daily)"),
      smk_080  = .lbl_s(as.integer(s[["SMK_080"]]), "Stopped smoking daily - when"),
      smk_090  = .cont(smk090_c,                   "Years since stopped smoking daily"),
      smkdvstp = .cont(smkdgstp_c,                 "Years since stopped smoking completely"),

      alcdvttm = .lbl_s(s[["ALCDVTTM"]], "Type of drinker"),
      alw_005  = .lbl_s(s[["ALW_005"]],  "Drank alcohol in past week"),
      alw_010  = .cont(as.double(s[["ALW_010"]]), "Drinks on Monday"),
      alw_015  = .cont(as.double(s[["ALW_015"]]), "Drinks on Tuesday"),
      alw_020  = .cont(as.double(s[["ALW_020"]]), "Drinks on Wednesday"),
      alw_025  = .cont(as.double(s[["ALW_025"]]), "Drinks on Thursday"),
      alw_030  = .cont(as.double(s[["ALW_030"]]), "Drinks on Friday"),
      alw_035  = .cont(as.double(s[["ALW_035"]]), "Drinks on Saturday"),
      alw_040  = .cont(as.double(s[["ALW_040"]]), "Drinks on Sunday"),
      alwdvwky = .cont(as.double(s[["ALWDVWKY"]]), "Total drinks last week"),

      sdcdvcgt = .lbl_l(eth, "Ethnic identity"),
      ehg2dvr3 = .lbl_s(s[["EHG2DVR3"]], "Highest level of education"),
      incdvsca = .lbl_l(s[["INCDVRCA"]], "Household income decile - Canada"),
      incdvspr = .lbl_l(s[["INCDVRPR"]], "Household income decile - province"),
      incdvsrs = .lbl_l(s[["INCDVRRS"]], "Household income decile - health region"),

      cmh_005  = .lbl_s(if (is2017) rep(996L, n) else as.integer(s[["CMH_005"]]),
                   "Consulted mental health professional in past year"),


      rural                = labelled(.inj_na_s(sample(1:2, n, TRUE, c(0.17, 0.83))),
                                     labels = c(Rural = 1L, Urban = 2L),
                                     label  = "Rural resident"),
      material_deprivation = .lbl_s(.inj_na_s(sample(1:5, n, TRUE, rep(0.20, 5))),  "Material deprivation quintile"),

      drgdvlac = .lbl_s(s[["DRGDVLAC"]], "Illicit drug use - lifetime (excl. one-time marijuana)"),
      drgdvyac = .lbl_s(s[["DRGDVYAC"]], "Illicit drug use - last year (excl. one-time marijuana)"),

      survdate   = .gen_survdate(n),
      event_flag = .gen_event_flag(n)
    )
  }

  #LIST RETURNED
  list(
    cchs_2013 = .make_2013(data_2013, sample_2013),
    cchs_2015 = .make_post2015(data_2015, sample_2015, "2015-2016"),
    cchs_2017 = .make_post2015(data_2017, sample_2017, "2017-2018")
  )
}

cchs_full <- NULL  # placeholder; populated by cchs_mock() in the run script