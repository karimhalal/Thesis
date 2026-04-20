library(dplyr)
library(haven)
library(cchsflow)
source(here::here("R", "pumf-mock.R"))
source(here::here("R", "special_functions.R"))

# ── NA helper functions ────────────────────────────────────────────────────────
is_na_a <- function(x) {
  return(x == 6 | x == 96 | is.na(x) & haven::is_tagged_na(x, "a"))
}

is_na_b <- function(x) {
  return(x %in% c(7, 8, 9, 97, 98, 99) | (is.na(x) & haven::is_tagged_na(x, "b")))
}

na_a <- function() haven::tagged_na("a")
na_b <- function() haven::tagged_na("b")
na_c <- function() haven::tagged_na("c")

# Replaces untagged NAs (systematically missing variables) with tagged_na("c")
# For integer-backed labelled vectors, tagged_na (double) can't be assigned directly,
# so fall back to NA_integer_.
fill_na_c <- function(x) {
  untagged <- is.na(x) & !haven::is_tagged_na(x)
  if (!any(untagged)) return(x)
  filler <- if (is.integer(unclass(x))) NA_integer_ else haven::tagged_na("c")
  x[untagged] <- filler
  x
}

# ── 2013-2014 ─────────────────────────────────────────────────────────────────
# Variable names from this cycle are used as the harmonized standard
cchs2013_2014_h <- cchs2013 %>%
  mutate(
    # ── Demographics ────────────────────────────────────────────────────────────
    DHH_SEX = labelled(
      case_when(DHH_SEX == 6 ~ na_a(), DHH_SEX %in% c(7,8,9) ~ na_b(), TRUE ~ DHH_SEX),
      labels = c("Male" = 1, "Female" = 2)
    ),
    DHH_AGE = case_when(
      DHH_AGE == 996 ~ na_a(), DHH_AGE %in% c(997,998,999) ~ na_b(), TRUE ~ DHH_AGE
    ),
    DHH_OWN = labelled(
      case_when(DHH_OWN == 6 ~ na_a(), DHH_OWN %in% c(7,8,9) ~ na_b(), TRUE ~ DHH_OWN),
      labels = c("Owner" = 1, "Renter" = 2)
    ),

    # ── General health ──────────────────────────────────────────────────────────
    GEN_01 = labelled(
      case_when(GEN_01 == 6 ~ na_a(), GEN_01 %in% c(7,8,9) ~ na_b(), TRUE ~ GEN_01),
      labels = c("Excellent" = 1, "Very good" = 2, "Good" = 3, "Fair" = 4, "Poor" = 5)
    ),
    # GENGSWL derived first from raw GEN_02A2 (codes 0-10) before GEN_02A2 is recoded
    GENGSWL = labelled(
      case_when(
        GEN_02A2 %in% c(9,10)    ~ 1L,
        GEN_02A2 %in% c(6,7,8)   ~ 2L,
        GEN_02A2 == 5             ~ 3L,
        GEN_02A2 %in% c(2,3,4)   ~ 4L,
        GEN_02A2 %in% c(0,1)     ~ 5L,
        GEN_02A2 == 96            ~ na_a(),
        GEN_02A2 %in% c(97,98,99) ~ na_b()
      ),
      labels = c("Very satisfied" = 1, "Satisfied" = 2,
                 "Neither satisfied nor dissatisfied" = 3,
                 "Dissatisfied" = 4, "Very dissatisfied" = 5)
    ),
    GEN_02A2 = labelled(
      case_when(GEN_02A2 == 96 ~ na_a(), GEN_02A2 %in% c(97,98,99) ~ na_b(), TRUE ~ GEN_02A2),
      labels = c("0"=0,"1"=1,"2"=2,"3"=3,"4"=4,"5"=5,"6"=6,"7"=7,"8"=8,"9"=9,"10"=10)
    ),
    GEN_02B = labelled(
      case_when(GEN_02B == 6 ~ na_a(), GEN_02B %in% c(7,8,9) ~ na_b(), TRUE ~ GEN_02B),
      labels = c("Excellent" = 1, "Very good" = 2, "Good" = 3, "Fair" = 4, "Poor" = 5)
    ),
    GEN_07 = labelled(
      case_when(GEN_07 == 6 ~ na_a(), GEN_07 %in% c(7,8,9) ~ na_b(), TRUE ~ GEN_07),
      labels = c("Not at all" = 1, "Not very" = 2, "A bit" = 3, "Quite a bit" = 4, "Extremely" = 5)
    ),
    GEN_09 = labelled(
      case_when(GEN_09 == 6 ~ na_a(), GEN_09 %in% c(7,8,9) ~ na_b(), TRUE ~ GEN_09),
      labels = c("Not at all" = 1, "Not very" = 2, "A bit" = 3, "Quite a bit" = 4, "Extremely" = 5)
    ),
    GEN_10 = labelled(
      case_when(GEN_10 == 6 ~ na_a(), GEN_10 %in% c(7,8,9) ~ na_b(), TRUE ~ GEN_10),
      labels = c("Very strong" = 1, "Somewhat strong" = 2, "Somewhat weak" = 3, "Very weak" = 4)
    ),

    # ── Chronic conditions ──────────────────────────────────────────────────────
    CCC_031 = labelled(
      case_when(CCC_031 == 6 ~ na_a(), CCC_031 %in% c(7,8,9) ~ na_b(), TRUE ~ CCC_031),
      labels = c("Yes" = 1, "No" = 2)
    ),
    CCC_051 = labelled(
      case_when(CCC_051 == 6 ~ na_a(), CCC_051 %in% c(7,8,9) ~ na_b(), TRUE ~ CCC_051),
      labels = c("Yes" = 1, "No" = 2)
    ),
    CCC_061 = labelled(
      case_when(CCC_061 == 6 ~ na_a(), CCC_061 %in% c(7,8,9) ~ na_b(), TRUE ~ CCC_061),
      labels = c("Yes" = 1, "No" = 2)
    ),
    CCC_071 = labelled(
      case_when(CCC_071 == 6 ~ na_a(), CCC_071 %in% c(7,8,9) ~ na_b(), TRUE ~ CCC_071),
      labels = c("Yes" = 1, "No" = 2)
    ),
    CCC_091 = labelled(
      case_when(CCC_091 == 6 ~ na_a(), CCC_091 %in% c(7,8,9) ~ na_b(), TRUE ~ CCC_091),
      labels = c("Yes" = 1, "No" = 2)
    ),
    CCC_101 = labelled(
      case_when(CCC_101 == 6 ~ na_a(), CCC_101 %in% c(7,8,9) ~ na_b(), TRUE ~ CCC_101),
      labels = c("Yes" = 1, "No" = 2)
    ),
    CCC_121 = labelled(
      case_when(CCC_121 == 6 ~ na_a(), CCC_121 %in% c(7,8,9) ~ na_b(), TRUE ~ CCC_121),
      labels = c("Yes" = 1, "No" = 2)
    ),
    CCC_151 = labelled(
      case_when(CCC_151 == 6 ~ na_a(), CCC_151 %in% c(7,8,9) ~ na_b(), TRUE ~ CCC_151),
      labels = c("Yes" = 1, "No" = 2)
    ),
    CCC_171 = labelled(
      case_when(CCC_171 == 6 ~ na_a(), CCC_171 %in% c(7,8,9) ~ na_b(), TRUE ~ CCC_171),
      labels = c("Yes" = 1, "No" = 2)
    ),
    CCC_280 = labelled(
      case_when(CCC_280 == 6 ~ na_a(), CCC_280 %in% c(7,8,9) ~ na_b(), TRUE ~ CCC_280),
      labels = c("Yes" = 1, "No" = 2)
    ),
    CCC_290 = labelled(
      case_when(CCC_290 == 6 ~ na_a(), CCC_290 %in% c(7,8,9) ~ na_b(), TRUE ~ CCC_290),
      labels = c("Yes" = 1, "No" = 2)
    ),

    # ── Anthropometrics ─────────────────────────────────────────────────────────
    HWTDHTM = case_when(
      HWTDHTM == 999.96 ~ na_a(), HWTDHTM %in% c(999.97,999.98,999.99) ~ na_b(), TRUE ~ HWTDHTM
    ),
    HWTDWTK = case_when(
      HWTDWTK == 999.96 ~ na_a(), HWTDWTK %in% c(999.97,999.98,999.99) ~ na_b(), TRUE ~ HWTDWTK
    ),

    # ── Pain ────────────────────────────────────────────────────────────────────
    HUPDPAD = labelled(
      case_when(HUPDPAD == 6 ~ na_a(), HUPDPAD %in% c(7,8,9) ~ na_b(), TRUE ~ HUPDPAD),
      labels = c("No pain" = 1, "Pain does not prevent activity" = 2,
                 "Prevents a few activities" = 3, "Prevents some activities" = 4,
                 "Prevents most activities" = 5)
    ),

    # ── Physical activity ───────────────────────────────────────────────────────
    PACDEE = case_when(
      PACDEE == 996 ~ na_a(), PACDEE %in% c(997,998,999) ~ na_b(), TRUE ~ PACDEE
    ),

    # ── Smoking ─────────────────────────────────────────────────────────────────
    SMK_01A = labelled(
      case_when(SMK_01A == 6 ~ na_a(), SMK_01A %in% c(7,8,9) ~ na_b(), TRUE ~ SMK_01A),
      labels = c("Yes" = 1, "No" = 2)
    ),
    SMKDSTY = labelled(
      case_when(SMKDSTY == 96 ~ na_a(), SMKDSTY %in% c(97,98,99) ~ na_b(), TRUE ~ SMKDSTY),
      labels = c("Daily smoker" = 1, "Occasional smoker" = 2, "Always occasional smoker" = 3,
                 "Former daily smoker" = 4, "Former occasional smoker" = 5, "Never smoked" = 6)
    ),
    SMK_203 = case_when(
      SMK_203 == 996 ~ na_a(), SMK_203 %in% c(997,998,999) ~ na_b(), TRUE ~ SMK_203
    ),
    SMK_204 = case_when(
      SMK_204 == 996 ~ na_a(), SMK_204 %in% c(997,998,999) ~ na_b(), TRUE ~ SMK_204
    ),
    SMK_05B = case_when(
      SMK_05B == 996 ~ na_a(), SMK_05B %in% c(997,998,999) ~ na_b(), TRUE ~ SMK_05B
    ),
    SMK_06A = labelled(
      case_when(SMK_06A == 6 ~ na_a(), SMK_06A %in% c(7,8,9) ~ na_b(), TRUE ~ SMK_06A),
      labels = c("Less than 1 year" = 1, "1 to < 2 years" = 2,
                 "2 to < 3 years" = 3, "3 or more years" = 4)
    ),
    SMK_207 = case_when(
      SMK_207 == 996 ~ na_a(), SMK_207 %in% c(997,998,999) ~ na_b(), TRUE ~ SMK_207
    ),
    SMK_208 = case_when(
      SMK_208 == 996 ~ na_a(), SMK_208 %in% c(997,998,999) ~ na_b(), TRUE ~ SMK_208
    ),
    SMK_09A = labelled(
      case_when(SMK_09A == 6 ~ na_a(), SMK_09A %in% c(7,8,9) ~ na_b(), TRUE ~ SMK_09A),
      labels = c("Less than 1 year" = 1, "1 to < 2 years" = 2,
                 "2 to < 3 years" = 3, "3 or more years" = 4)
    ),
    SMK_09C = case_when(
      SMK_09C == 996 ~ na_a(), SMK_09C %in% c(997,998,999) ~ na_b(), TRUE ~ SMK_09C
    ),
    SMKDSTP = case_when(
      SMKDSTP == 996 ~ na_a(), SMKDSTP %in% c(997,998,999) ~ na_b(), TRUE ~ SMKDSTP
    ),

    # ── Alcohol ─────────────────────────────────────────────────────────────────
    ALCDTTM = labelled(
      case_when(ALCDTTM == 6 ~ na_a(), ALCDTTM %in% c(7,8,9) ~ na_b(), TRUE ~ ALCDTTM),
      labels = c("Regular drinker" = 1, "Occasional drinker" = 2,
                 "Did not drink in last 12 months" = 3)
    ),
    ALW_1 = labelled(
      case_when(ALW_1 == 6 ~ na_a(), ALW_1 %in% c(7,8,9) ~ na_b(), TRUE ~ ALW_1),
      labels = c("Yes" = 1, "No" = 2)
    ),
    ALW_2A1 = case_when(ALW_2A1 == 996 ~ na_a(), ALW_2A1 %in% c(997,998,999) ~ na_b(), TRUE ~ ALW_2A1),
    ALW_2A2 = case_when(ALW_2A2 == 996 ~ na_a(), ALW_2A2 %in% c(997,998,999) ~ na_b(), TRUE ~ ALW_2A2),
    ALW_2A3 = case_when(ALW_2A3 == 996 ~ na_a(), ALW_2A3 %in% c(997,998,999) ~ na_b(), TRUE ~ ALW_2A3),
    ALW_2A4 = case_when(ALW_2A4 == 996 ~ na_a(), ALW_2A4 %in% c(997,998,999) ~ na_b(), TRUE ~ ALW_2A4),
    ALW_2A5 = case_when(ALW_2A5 == 996 ~ na_a(), ALW_2A5 %in% c(997,998,999) ~ na_b(), TRUE ~ ALW_2A5),
    ALW_2A6 = case_when(ALW_2A6 == 996 ~ na_a(), ALW_2A6 %in% c(997,998,999) ~ na_b(), TRUE ~ ALW_2A6),
    ALW_2A7 = case_when(ALW_2A7 == 996 ~ na_a(), ALW_2A7 %in% c(997,998,999) ~ na_b(), TRUE ~ ALW_2A7),
    ALWDWKY = case_when(ALWDWKY == 996 ~ na_a(), ALWDWKY %in% c(997,998,999) ~ na_b(), TRUE ~ ALWDWKY),

    # ── Sociodemographic ────────────────────────────────────────────────────────
    SDCDCGT = labelled(
      case_when(SDCDCGT == 96 ~ na_a(), SDCDCGT %in% c(97,98,99) ~ na_b(), TRUE ~ SDCDCGT),
      labels = c("White" = 1, "Black" = 2, "Korean" = 3, "Filipino" = 4, "Japanese" = 5,
                 "Chinese" = 6, "South Asian" = 7, "Southeast Asian" = 8, "Arab" = 9,
                 "West Asian" = 10, "Latin American" = 11, "Other" = 12, "Multiple origins" = 13)
    ),
    # Collapse cats 3 (some post-secondary) and 4 (post-secondary grad) into 3
    EDUDR03 = labelled(
      case_when(
        EDUDR04 %in% c(3,4)   ~ 3L,
        EDUDR04 == 6           ~ na_a(),
        EDUDR04 %in% c(7,8,9) ~ na_b(),
        TRUE                   ~ EDUDR04
      ),
      labels = c("Less than secondary" = 1, "Secondary graduation" = 2,
                 "Post-secondary education" = 3)
    ),
    # Pre-2015 LBSDWSS has 4 categories; collapse 3 and 4 to 3 to match post-2015 coding
    LBSDWSS = labelled(
      case_when(
        LBSDWSS %in% c(3,4)   ~ 3L,
        LBSDWSS == 6           ~ na_a(),
        LBSDWSS %in% c(7,8,9) ~ na_b(),
        TRUE                   ~ LBSDWSS
      ),
      labels = c("Worked at job or business last week" = 1,
                 "Absent from work/business" = 2,
                 "Did not have a job last week" = 3)
    ),
    FSCDHFS2 = labelled(
      case_when(
        FSCDHFS2 == 6           ~ na_a(),
        FSCDHFS2 %in% c(7,8,9) ~ na_b(),
        TRUE                    ~ FSCDHFS2
      ),
      labels = c("Food secure" = 0, "Food insecure without hunger" = 1,
                 "Food insecure with moderate hunger" = 2,
                 "Food insecure with severe hunger" = 3,
                 "Severely food insecure" = 4)
    ),
    INCDRCA = labelled(
      case_when(INCDRCA == 96 ~ na_a(), INCDRCA %in% c(97,98,99) ~ na_b(), TRUE ~ INCDRCA),
      labels = c("1st decile"=1,"2nd decile"=2,"3rd decile"=3,"4th decile"=4,"5th decile"=5,
                 "6th decile"=6,"7th decile"=7,"8th decile"=8,"9th decile"=9,"10th decile"=10)
    ),
    INCDRPR = labelled(
      case_when(INCDRPR == 96 ~ na_a(), INCDRPR %in% c(97,98,99) ~ na_b(), TRUE ~ INCDRPR),
      labels = c("1st decile"=1,"2nd decile"=2,"3rd decile"=3,"4th decile"=4,"5th decile"=5,
                 "6th decile"=6,"7th decile"=7,"8th decile"=8,"9th decile"=9,"10th decile"=10)
    ),
    INCDRRS = labelled(
      case_when(INCDRRS == 96 ~ na_a(), INCDRRS %in% c(97,98,99) ~ na_b(), TRUE ~ INCDRRS),
      labels = c("1st decile"=1,"2nd decile"=2,"3rd decile"=3,"4th decile"=4,"5th decile"=5,
                 "6th decile"=6,"7th decile"=7,"8th decile"=8,"9th decile"=9,"10th decile"=10)
    ),

    # ── Mental health services ──────────────────────────────────────────────────
    CMH_01K = labelled(
      case_when(CMH_01K == 6 ~ na_a(), CMH_01K %in% c(7,8,9) ~ na_b(), TRUE ~ CMH_01K),
      labels = c("Yes" = 1, "No" = 2)
    ),
    CMH_01L = case_when(
      CMH_01L == 996 ~ na_a(), CMH_01L %in% c(997,998,999) ~ na_b(), TRUE ~ CMH_01L
    ),

    # ── Area-level characteristics ───────────────────────────────────────────────
    rural = labelled(
      case_when(rural == 6 ~ na_a(), rural %in% c(7,8,9) ~ na_b(), TRUE ~ rural),
      labels = c("Rural" = 1, "Urban" = 2)
    ),
    material_deprivation = labelled(
      case_when(material_deprivation == 6           ~ na_a(),
                material_deprivation %in% c(7,8,9) ~ na_b(),
                TRUE                                ~ material_deprivation),
      labels = c("1st quintile (least deprived)" = 1, "2nd quintile" = 2, "3rd quintile" = 3,
                 "4th quintile" = 4, "5th quintile (most deprived)" = 5)
    )
  )

# ── Combine 2015 and 2017 raw data ─────────────────────────────────────────────
cchs2015_2018 <- bind_rows_keep_labels(df1 = cchs2015, df2 = cchs2017, prefer = c("df1", "df2"))

# ── 2015-2018: recode and harmonize variable names to 2013 convention ──────────
cchs2015_2018_h <- cchs2015_2018 %>%
  mutate(
    # ── Derive PACDEE from physical activity components ──────────────────────────
    PACDEE = calculate_energy_expenditure_18plus(paa_045, paa_050, paa_075, paa_080,
                                                 paadvdys, paadvvig),

    # ── Demographics ────────────────────────────────────────────────────────────
    DHH_SEX = labelled(
      case_when(dhh_sex == 6 ~ na_a(), dhh_sex %in% c(7,8,9) ~ na_b(), TRUE ~ dhh_sex),
      labels = c("Male" = 1, "Female" = 2)
    ),
    DHH_AGE = case_when(
      dhh_age == 996 ~ na_a(), dhh_age %in% c(997,998,999) ~ na_b(), TRUE ~ dhh_age
    ),
    DHH_OWN = labelled(
      case_when(dhh_own == 6 ~ na_a(), dhh_own %in% c(7,8,9) ~ na_b(), TRUE ~ dhh_own),
      labels = c("Owner" = 1, "Renter" = 2)
    ),

    # ── General health ──────────────────────────────────────────────────────────
    GEN_01 = labelled(
      case_when(gen_005 == 6 ~ na_a(), gen_005 %in% c(7,8,9) ~ na_b(), TRUE ~ gen_005),
      labels = c("Excellent" = 1, "Very good" = 2, "Good" = 3, "Fair" = 4, "Poor" = 5)
    ),
    # GENGSWL derived first from raw gen_010 before it is recoded to GEN_02A2
    GENGSWL = labelled(
      case_when(
        gen_010 %in% c(9,10)    ~ 1L,
        gen_010 %in% c(6,7,8)   ~ 2L,
        gen_010 == 5             ~ 3L,
        gen_010 %in% c(2,3,4)   ~ 4L,
        gen_010 %in% c(0,1)     ~ 5L,
        gen_010 == 96            ~ na_a(),
        gen_010 %in% c(97,98,99) ~ na_b()
      ),
      labels = c("Very satisfied" = 1, "Satisfied" = 2,
                 "Neither satisfied nor dissatisfied" = 3,
                 "Dissatisfied" = 4, "Very dissatisfied" = 5)
    ),
    GEN_02A2 = labelled(
      case_when(gen_010 == 96 ~ na_a(), gen_010 %in% c(97,98,99) ~ na_b(), TRUE ~ gen_010),
      labels = c("0"=0,"1"=1,"2"=2,"3"=3,"4"=4,"5"=5,"6"=6,"7"=7,"8"=8,"9"=9,"10"=10)
    ),
    GEN_02B = labelled(
      case_when(gen_015 == 6 ~ na_a(), gen_015 %in% c(7,8,9) ~ na_b(), TRUE ~ gen_015),
      labels = c("Excellent" = 1, "Very good" = 2, "Good" = 3, "Fair" = 4, "Poor" = 5)
    ),
    GEN_07 = labelled(
      case_when(gen_020 == 6 ~ na_a(), gen_020 %in% c(7,8,9) ~ na_b(), TRUE ~ gen_020),
      labels = c("Not at all" = 1, "Not very" = 2, "A bit" = 3, "Quite a bit" = 4, "Extremely" = 5)
    ),
    GEN_09 = labelled(
      case_when(gen_025 == 6 ~ na_a(), gen_025 %in% c(7,8,9) ~ na_b(), TRUE ~ gen_025),
      labels = c("Not at all" = 1, "Not very" = 2, "A bit" = 3, "Quite a bit" = 4, "Extremely" = 5)
    ),
    GEN_10 = labelled(
      case_when(gen_030 == 6 ~ na_a(), gen_030 %in% c(7,8,9) ~ na_b(), TRUE ~ gen_030),
      labels = c("Very strong" = 1, "Somewhat strong" = 2, "Somewhat weak" = 3, "Very weak" = 4)
    ),

    # ── Chronic conditions ──────────────────────────────────────────────────────
    CCC_031 = labelled(
      case_when(ccc_015 == 6 ~ na_a(), ccc_015 %in% c(7,8,9) ~ na_b(), TRUE ~ ccc_015),
      labels = c("Yes" = 1, "No" = 2)
    ),
    CCC_051 = labelled(
      case_when(ccc_050 == 6 ~ na_a(), ccc_050 %in% c(7,8,9) ~ na_b(), TRUE ~ ccc_050),
      labels = c("Yes" = 1, "No" = 2)
    ),
    # ccc_055 is all 996 in 2017-2018 (question not asked); handle both standard and fill NA codes
    CCC_061 = labelled(
      case_when(
        ccc_055 == 6             ~ na_a(),
        ccc_055 %in% c(7,8,9)   ~ na_b(),
        ccc_055 == 996           ~ na_a(),
        ccc_055 %in% c(997,998,999) ~ na_b(),
        TRUE                     ~ ccc_055
      ),
      labels = c("Yes" = 1, "No" = 2)
    ),
    CCC_071 = labelled(
      case_when(ccc_065 == 6 ~ na_a(), ccc_065 %in% c(7,8,9) ~ na_b(), TRUE ~ ccc_065),
      labels = c("Yes" = 1, "No" = 2)
    ),
    CCC_091 = labelled(
      case_when(ccc_030 == 6 ~ na_a(), ccc_030 %in% c(7,8,9) ~ na_b(), TRUE ~ ccc_030),
      labels = c("Yes" = 1, "No" = 2)
    ),
    CCC_101 = labelled(
      case_when(ccc_095 == 6 ~ na_a(), ccc_095 %in% c(7,8,9) ~ na_b(), TRUE ~ ccc_095),
      labels = c("Yes" = 1, "No" = 2)
    ),
    CCC_121 = labelled(
      case_when(ccc_085 == 6 ~ na_a(), ccc_085 %in% c(7,8,9) ~ na_b(), TRUE ~ ccc_085),
      labels = c("Yes" = 1, "No" = 2)
    ),
    CCC_151 = labelled(
      case_when(ccc_090 == 6 ~ na_a(), ccc_090 %in% c(7,8,9) ~ na_b(), TRUE ~ ccc_090),
      labels = c("Yes" = 1, "No" = 2)
    ),
    # ccc_155 is all 996 in 2017-2018 (question not asked)
    CCC_171 = labelled(
      case_when(
        ccc_155 == 6             ~ na_a(),
        ccc_155 %in% c(7,8,9)   ~ na_b(),
        ccc_155 == 996           ~ na_a(),
        ccc_155 %in% c(997,998,999) ~ na_b(),
        TRUE                     ~ ccc_155
      ),
      labels = c("Yes" = 1, "No" = 2)
    ),
    CCC_280 = labelled(
      case_when(ccc_195 == 6 ~ na_a(), ccc_195 %in% c(7,8,9) ~ na_b(), TRUE ~ ccc_195),
      labels = c("Yes" = 1, "No" = 2)
    ),
    CCC_290 = labelled(
      case_when(ccc_200 == 6 ~ na_a(), ccc_200 %in% c(7,8,9) ~ na_b(), TRUE ~ ccc_200),
      labels = c("Yes" = 1, "No" = 2)
    ),

    # ── Anthropometrics ─────────────────────────────────────────────────────────
    HWTDHTM = case_when(
      hwtdvhtm == 999.96 ~ na_a(), hwtdvhtm %in% c(999.97,999.98,999.99) ~ na_b(), TRUE ~ hwtdvhtm
    ),
    HWTDWTK = case_when(
      hwtdvwtk == 999.96 ~ na_a(), hwtdvwtk %in% c(999.97,999.98,999.99) ~ na_b(), TRUE ~ hwtdvwtk
    ),

    # ── Physical activity ───────────────────────────────────────────────────────
    PACDEE = case_when(
      PACDEE == 996 ~ na_a(), PACDEE %in% c(997,998,999) ~ na_b(), TRUE ~ PACDEE
    ),

    # ── Smoking ─────────────────────────────────────────────────────────────────
    # smk_005 has no pre-2015 equivalent; retained under post-2015 name
    smk_005 = labelled(
      case_when(smk_005 == 6 ~ na_a(), smk_005 %in% c(7,8,9) ~ na_b(), TRUE ~ smk_005),
      labels = c("Daily" = 1, "Occasionally" = 2, "Not at all" = 3)
    ),
    SMKDSTY = labelled(
      case_when(smkdvsty == 96 ~ na_a(), smkdvsty %in% c(97,98,99) ~ na_b(), TRUE ~ smkdvsty),
      labels = c("Daily smoker" = 1, "Occasional smoker" = 2, "Always occasional smoker" = 3,
                 "Former daily smoker" = 4, "Former occasional smoker" = 5, "Never smoked" = 6)
    ),
    # smk_040 is the single age-started-smoking-daily variable post-2015;
    # maps to both SMK_203 (current daily) and SMK_207 (former daily) from 2013
    SMK_203 = case_when(
      smk_040 == 996 ~ na_a(), smk_040 %in% c(997,998,999) ~ na_b(), TRUE ~ smk_040
    ),
    SMK_207 = case_when(
      smk_040 == 996 ~ na_a(), smk_040 %in% c(997,998,999) ~ na_b(), TRUE ~ smk_040
    ),
    SMK_204 = case_when(
      smk_045 == 996 ~ na_a(), smk_045 %in% c(997,998,999) ~ na_b(), TRUE ~ smk_045
    ),
    SMK_05B = case_when(
      smk_050 == 996 ~ na_a(), smk_050 %in% c(997,998,999) ~ na_b(), TRUE ~ smk_050
    ),
    SMK_06A = labelled(
      case_when(smk_060 == 6 ~ na_a(), smk_060 %in% c(7,8,9) ~ na_b(), TRUE ~ smk_060),
      labels = c("Less than 1 year" = 1, "1 to < 2 years" = 2,
                 "2 to < 3 years" = 3, "3 or more years" = 4)
    ),
    SMK_208 = case_when(
      smk_075 == 996 ~ na_a(), smk_075 %in% c(997,998,999) ~ na_b(), TRUE ~ smk_075
    ),
    SMK_09A = labelled(
      case_when(smk_080 == 6 ~ na_a(), smk_080 %in% c(7,8,9) ~ na_b(), TRUE ~ smk_080),
      labels = c("Less than 1 year" = 1, "1 to < 2 years" = 2,
                 "2 to < 3 years" = 3, "3 or more years" = 4)
    ),
    SMK_09C = case_when(
      smk_090 == 996 ~ na_a(), smk_090 %in% c(997,998,999) ~ na_b(), TRUE ~ smk_090
    ),
    SMKDSTP = case_when(
      smkdvstp == 996 ~ na_a(), smkdvstp %in% c(997,998,999) ~ na_b(), TRUE ~ smkdvstp
    ),

    # ── Alcohol ─────────────────────────────────────────────────────────────────
    ALCDTTM = labelled(
      case_when(alcdvttm == 6 ~ na_a(), alcdvttm %in% c(7,8,9) ~ na_b(), TRUE ~ alcdvttm),
      labels = c("Regular drinker" = 1, "Occasional drinker" = 2,
                 "Did not drink in last 12 months" = 3)
    ),
    ALW_1 = labelled(
      case_when(alw_005 == 6 ~ na_a(), alw_005 %in% c(7,8,9) ~ na_b(), TRUE ~ alw_005),
      labels = c("Yes" = 1, "No" = 2)
    ),
    ALW_2A1 = case_when(alw_010 == 996 ~ na_a(), alw_010 %in% c(997,998,999) ~ na_b(), TRUE ~ alw_010),
    ALW_2A2 = case_when(alw_015 == 996 ~ na_a(), alw_015 %in% c(997,998,999) ~ na_b(), TRUE ~ alw_015),
    ALW_2A3 = case_when(alw_020 == 996 ~ na_a(), alw_020 %in% c(997,998,999) ~ na_b(), TRUE ~ alw_020),
    ALW_2A4 = case_when(alw_025 == 996 ~ na_a(), alw_025 %in% c(997,998,999) ~ na_b(), TRUE ~ alw_025),
    ALW_2A5 = case_when(alw_030 == 996 ~ na_a(), alw_030 %in% c(997,998,999) ~ na_b(), TRUE ~ alw_030),
    ALW_2A6 = case_when(alw_035 == 996 ~ na_a(), alw_035 %in% c(997,998,999) ~ na_b(), TRUE ~ alw_035),
    ALW_2A7 = case_when(alw_040 == 996 ~ na_a(), alw_040 %in% c(997,998,999) ~ na_b(), TRUE ~ alw_040),
    ALWDWKY = case_when(
      alwdvwky == 996 ~ na_a(), alwdvwky %in% c(997,998,999) ~ na_b(), TRUE ~ alwdvwky
    ),

    # ── Sociodemographic ────────────────────────────────────────────────────────
    SDCDCGT = labelled(
      case_when(sdcdvcgt == 96 ~ na_a(), sdcdvcgt %in% c(97,98,99) ~ na_b(), TRUE ~ sdcdvcgt),
      labels = c("White" = 1, "Black" = 2, "Korean" = 3, "Filipino" = 4, "Japanese" = 5,
                 "Chinese" = 6, "South Asian" = 7, "Southeast Asian" = 8, "Arab" = 9,
                 "West Asian" = 10, "Latin American" = 11, "Other" = 12, "Multiple origins" = 13)
    ),
    EDUDR03 = labelled(
      case_when(ehg2dvr3 == 6 ~ na_a(), ehg2dvr3 %in% c(7,8,9) ~ na_b(), TRUE ~ ehg2dvr3),
      labels = c("Less than secondary" = 1, "Secondary graduation" = 2,
                 "Post-secondary education" = 3)
    ),
    LBSDWSS = labelled(
      case_when(
        lbfdvwss == 6           ~ na_a(),
        lbfdvwss %in% c(7,8,9) ~ na_b(),
        TRUE                    ~ lbfdvwss
      ),
      labels = c("Worked at job or business last week" = 1,
                 "Absent from work/business" = 2,
                 "Did not have a job last week" = 3)
    ),
    # Post-2015 fscdvhfs has 4 categories (0-3); collapse 1 and 2 into 1, 3 becomes 2
    FSCDHFS2 = labelled(
      case_when(
        fscdvhfs == 0           ~ 0L,
        fscdvhfs %in% c(1,2)   ~ 1L,
        fscdvhfs == 3           ~ 2L,
        fscdvhfs == 6           ~ na_a(),
        fscdvhfs %in% c(7,8,9) ~ na_b()
      ),
      labels = c("Food secure" = 0, "Moderately food insecure" = 1,
                 "Severely food insecure" = 2)
    ),
    INCDRCA = labelled(
      case_when(incdvsca == 96 ~ na_a(), incdvsca %in% c(97,98,99) ~ na_b(), TRUE ~ incdvsca),
      labels = c("1st decile"=1,"2nd decile"=2,"3rd decile"=3,"4th decile"=4,"5th decile"=5,
                 "6th decile"=6,"7th decile"=7,"8th decile"=8,"9th decile"=9,"10th decile"=10)
    ),
    INCDRPR = labelled(
      case_when(incdvspr == 96 ~ na_a(), incdvspr %in% c(97,98,99) ~ na_b(), TRUE ~ incdvspr),
      labels = c("1st decile"=1,"2nd decile"=2,"3rd decile"=3,"4th decile"=4,"5th decile"=5,
                 "6th decile"=6,"7th decile"=7,"8th decile"=8,"9th decile"=9,"10th decile"=10)
    ),
    INCDRRS = labelled(
      case_when(incdvsrs == 96 ~ na_a(), incdvsrs %in% c(97,98,99) ~ na_b(), TRUE ~ incdvsrs),
      labels = c("1st decile"=1,"2nd decile"=2,"3rd decile"=3,"4th decile"=4,"5th decile"=5,
                 "6th decile"=6,"7th decile"=7,"8th decile"=8,"9th decile"=9,"10th decile"=10)
    ),

    # ── Mental health services ──────────────────────────────────────────────────
    # cmh_005 and cmh_010 are all 996 in 2017-2018 (question not asked)
    CMH_01K = labelled(
      case_when(
        cmh_005 == 6              ~ na_a(),
        cmh_005 %in% c(7,8,9)    ~ na_b(),
        cmh_005 == 996            ~ na_a(),
        cmh_005 %in% c(997,998,999) ~ na_b(),
        TRUE                      ~ cmh_005
      ),
      labels = c("Yes" = 1, "No" = 2)
    ),
    CMH_01L = case_when(
      cmh_010 == 996              ~ na_a(),
      cmh_010 %in% c(997,998,999) ~ na_b(),
      TRUE                        ~ cmh_010
    ),

    # ── Area-level characteristics ───────────────────────────────────────────────
    rural = labelled(
      case_when(rural == 6 ~ na_a(), rural %in% c(7,8,9) ~ na_b(), TRUE ~ rural),
      labels = c("Rural" = 1, "Urban" = 2)
    ),
    material_deprivation = labelled(
      case_when(material_deprivation == 6           ~ na_a(),
                material_deprivation %in% c(7,8,9) ~ na_b(),
                TRUE                                ~ material_deprivation),
      labels = c("1st quintile (least deprived)" = 1, "2nd quintile" = 2, "3rd quintile" = 3,
                 "4th quintile" = 4, "5th quintile (most deprived)" = 5)
    ),

    # ── Post-2015 only (no pre-2015 equivalent; retain post-2015 names) ──────────
    drgdvlac = labelled(
      case_when(drgdvlac == 6 ~ na_a(), drgdvlac %in% c(7,8,9) ~ na_b(), TRUE ~ drgdvlac),
      labels = c("Yes" = 1, "No" = 2)
    ),
    drgdvyac = labelled(
      case_when(drgdvyac == 6 ~ na_a(), drgdvyac %in% c(7,8,9) ~ na_b(), TRUE ~ drgdvyac),
      labels = c("Yes" = 1, "No" = 2)
    )
  )

# ── Combine all cycles (2013 labels preferred) ────────────────────────────────
cchs_all_h <- bind_rows_keep_labels(
  df1    = cchs2013_2014_h,
  df2    = cchs2015_2018_h,
  prefer = c("df1", "df2")
)

# ── Fill systematically missing variables with tagged_na("c") ─────────────────
# Untagged NAs arise from variables not collected in a given cycle (e.g. HUPDPAD
# post-2015, SMK_01A post-2015, drgdvlac pre-2015). These will be labelled as
# "Question not asked in survey" in downstream table outputs.
cchs_all_h <- cchs_all_h %>%
  mutate(across(everything(), fill_na_c))
