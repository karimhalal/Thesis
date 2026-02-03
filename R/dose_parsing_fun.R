# load relevant source code

source(here::here("R", "DIN_utils.R"))

#' @title Dose Parsing Function
#'
#' @description This function creates replaces missing dosages using din identifers and creates 3 derived 
#' variables to standardize dosage for use in subsequent funcitons. This will breakdown each dose to the
#' number component (the actual dose fo the active ingredient only), and the dose units and output each into
#' seperate columns
#' 
#' @details there are a number of dosage forms that have different patterns of expressing their dose
#'        
#'        **Tablets and capsules dose**
#'         -- Defined as: tab_cap_forms<-paste(c("cap", "cr cap", "er cap", "sr cap", "tab", "chew tab",
#'            "cr tab", "er tab", "er tab chewable", "sr tab", "tab (chewable)", "sl tab", "sup", 
#'            "rect sup", "buccal soluble fil"), collapse = "|")
#'         -- These doses are expressed as (xmg or xmcg), where x is a 1 to 3 digit number that can also be
#'            a decimal (anywhere between 0.2 to 200)
#'         -- The dose function will extract the numeric component of the input
#' 
#'         **Liquid/gel doses**
#'          -- Defined as:  oral_liquid_form<-paste(c("o/l", "o/l 500ml", "oral sol", "oral drops", "susp",
#'             "oral concentrate", "er pd for sol", "rect gel*"), collapse = "|")
#'         -- These doses are expressed as xmg/ml or xmg/xml, where x is a 1 to 3 digit number that can also be
#'            a decimal (anywhere between 0.2 to 200)
#'         -- The dose function will determine whether the form is xmg/ml or xmg/xml. If the dose is xmg/ml extract
#'            only the numeric component. If the dose is in xmg/xml, the function will parse the two numeeric values
#'            and extract their ratio which will then be used in subsequent dose calculations
#' 
#'        **Transdermal patches dose**
#'         -- Defined as: transdermal_forms<-paste(c("trans patch"), collapse = "|")
#'         -- These doses are expressed as xmcg/hr, where x is a 1 to 3 digit number that can only be a whole number
#'         -- The dose function will extract the numeric component of the input
#' 
#'        **Multiple dosage and improperely coded dosages**
#'         -- Any of the above dosage forms can include some dosages that include multiple entries
#'         -- These doses are expressed as dose a(can be any of the above mentioned form) & dose b(same format)
#'                --Example: 32mg & 200mg
#'         -- Some doses are improperely coded (will provide dosage in non standard form or the dosage of no-narcotic component)       
#'         -- In either case, the dose function will use the DIN to map each perscription back to the din_list where these special
#'           cases will be identified as any DIN corresponding to a non-NA "Active.Ingredient.Dose" column. 
#' 
#' Quality checks will be incorporated to make sure the expected numeric dosage are generated
#'
#'
#' @param dosage_form charcter or character vector 
#'
#' @param STRENGTH dose of each individual unit of perscription drug
#'
#' @param din_list_comb list containing multiple dataframes with all perscription details for each drug (unique din)
#'
#' 
#' @return dataframe containing two additional columns, one containing numeric dose and the other 
#' containing [character ] dose unit
#' 
#'           **Process:**
#'          1. Match DIN to conversion factor from reference table
#'          2. Extract days supply and quantity (# of units) corresponding to perscription
#'          3. Apply MEQ formula using matched conversion factor
#'          4. Return daily MEQ value
#'
#'        
#'          **Missing Data Handling:**
#'          - Perscriptions with missing dosage will also be matched back to the DIN_list to extract
#'          
#'
#' 
#' @example
#' 
#apply function to full dataset
nms_data%>%
  rowwise()%>%
  mutate()
#' @export 

dose_parse_fun<-function(data, din = NULL, dosage_form = NULL, STRENGTH = NULL, din_list_comb = NULL){

  #load mapping datasets if none have been previously loaded
  if(is.null(din_list_comb)){
    din_list_comb<-get_din_list_combined()
  }

  #load relvant vectors and tables
  din_list<-din_list_comb[["din_list"]]
  din_vec_tab_cap<-din_list_comb[["din_vec_tab_cap"]]
  din_vec_transdermal<-din_list_comb[["din_vec_transdermal"]]
  din_vec_liquid<-din_list_comb[["din_vec_liquid"]]
  din_list_cor<-get_dinlist_cor(din_list)

  #check if input is a dataframe or vectors

  if(is.data.frame(data)){
    #dataframe input: use existing columns
    df <- data
  } else {
    #vector input: create dataframe from vectors
    df <- data.frame(din = data, dosage_form = dosage_form, STRENGTH = STRENGTH, stringsAsFactors = FALSE)
  }

  #fill in missing STRENGTH and correct multiple/improper dosages
  df<-df%>%
    mutate(STRENGTH_harmonized= case_when(
      STRENGTH == "" | is.na(STRENGTH) ~ din_list$STRENGTH[match(din, din_list$DIN.PIN)],
      din %in% din_list_cor$DIN.PIN ~ din_list_cor$Active.Ingredient.Dose[match(din, din_list_cor$DIN.PIN)],
      TRUE ~ STRENGTH
    ),
      dose_group= case_when(
        din%in%din_vec_tab_cap~"tab/cap",
        din%in%din_vec_transdermal~ "transdermal",
        din%in%din_vec_liquid~"liquid",
        TRUE~"other"
    ))

  #regex patterns for liquid forms
  liquid_pattern_perml<-"(\\d{1,3}(?:\\.\\d+)?(mg|mcg)/ml)"
  liquid_pattern_ratio<-"(\\d{1,3}(?:\\.\\d+)?(mg|mcg)/(\\d+)ml)"

  #extract the numeric using regular expressions
  df<-df%>%
    mutate(dose_num= case_when(
      dose_group=="tab/cap"~as.numeric(str_extract(STRENGTH_harmonized, "\\d{1,3}(?:\\.\\d+)?(?=(mg|mcg))")),
      dose_group=="transdermal"~as.numeric(str_extract(STRENGTH_harmonized, "\\d{1,3}(?:\\.\\d+)?(?=(mcg/hr))")),
      dose_group=="liquid" & stringr::str_detect(STRENGTH_harmonized, liquid_pattern_perml)~as.numeric(str_extract(STRENGTH_harmonized, "\\d{1,3}(?:\\.\\d+)?(?=(mg|mcg))")),
      dose_group=="liquid" & stringr::str_detect(STRENGTH_harmonized, liquid_pattern_ratio)~as.numeric(str_extract(STRENGTH_harmonized, "\\d{1,3}(?:\\.\\d+)?(?=(mg|mcg))"))/as.numeric(str_extract(STRENGTH_harmonized, "(?<=/)\\d+(?=ml)")),
      TRUE~NA_real_
    ),
          dose_unit= case_when(
      dose_group=="tab/cap"~as.character(str_extract(STRENGTH_harmonized, "(?<=\\d)(mg|mcg)")),
      dose_group=="transdermal"~as.character(str_extract(STRENGTH_harmonized, "(?<=\\d)(mcg/hr)")),
      dose_group=="liquid" ~"mg/ml",
      TRUE~ NA_character_
        ))

  return(df)

}
