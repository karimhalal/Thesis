#' @title Compute average cumlative dose daily
#'
#' @description This function uses previously calculated milligram equivalents to compute total milligram equivalents exposure to both benzodiazepines 
#' and opioids during the period of initial perscription. This provides an estimate of the strength of initial exposure to narcotics 
#' for each individual.
#'
#' This function also flattens perscription data, originally assigned one row per perscription to individual level data
#' to generate a datset that in which each row corresponds to a unique individual identified by their ICES Knowledge Number (IKN).
#' 
#' @param din_list [dataframe] the mapping list used to determine the type of medication on the basis of its 
#' 
#' 
#' @param MEQ [numeric] A numeric representing the daily standardized milligram equivalents
#' (see oral_eq_fun and transpatch_eq_fun).
#' 
#'
#' @param DIN [character] Drug identification number used to map to DIN_list to determine drug class
#' for appropriate category mapping
#' 
#' @param DAYSSUPL  [integer] the number of days the drug is supposed to be used for. The intended length
#' of the perscription
#' 
#' @param DT_OF_SERV_TS [POSIXct] the date of the initial opioid/BZD perscription
#' 
#' @param IKN [character] unique person identifier used to link multiple different datasets. Will be used as the flattening variable.
#' IKN is a unique 10 digit identifier used to link different datasets
#' 
#' @param DIN_DESC [character] description of each perscription. To be included in the final
#' 
#' @return A data frame with one row per individual (IKN) containing the following additional variables:
#'   \item{avg_daily_bzd_meq}-Average daily BZD milligram equivalents during initial period
#'   \item{avg_daily_opioid_meq}-Average daily opioid milligram equivalents during initial period

#'
#' @details This function will take into account the period directly following the first perscription of opioids or bzds. This
#' period will cover the length of the first perscription. The function uses the days suuply and perscription date variable. The function
#' handles NAs as follows:
#'
#'          **Missing Data Codes:**
#'          - Propagates tagged NAs from the input `meq`.
#'          - returns NA(a) for invalid DIN. 
#'  @examples
#' # Scalar usage: Single prescription, no summation required
#' avg_dose(IKN, MEQ, DAYSSUPL, DT_OF_SERV_TS, DIN)
#' avg_dose("0123456789",45, 10, "2013-05-20", "02245284")
#' #returns avg_daily_bzd_meq= and avg_daily_opioid_meq=0 (DIN does not correspond to opioid medication)
#'
#' # Multiple prescriptions of the same type, some within the same time period and others outside the previously established cutoffs
#' categorize_MEQ_risk(DIN= c(02245284, 02245284,),MEQ= c(45, 75, 25))
#' # Returns: 
#'
#' #' # Multiple prescriptions of the same typ, some
#' categorize_MEQ_risk(DIN= c(02245284, 02245284,),daily _meq= c(45, 75, 25))
#' # Returns: 
#' 
#' 
#' # Database usage
#' library(dplyr)
#' # nms_data %>%
#' #  group_by ( = categorize_MEQ_risk(meq))
#' @export
avg_dose_baseline<-function(IKN, MEQ, DAYSSUPL, DT_OF_SERV_TS, DIN, DIN_DESC, MANUFACTURER_CD, STRENGTH){
  #load the existing DIN list if the argument is NULL
  if(!exists("din_list")){
    config<-yaml::yaml.load_file("config.yml")
    din_list<-read.csv(config$default$variable$din_list,
    fileEncoding="UTF-8-BOM",
    stringsAsFactors=FALSE)
  }

  #create long data from the input vectors
  per_data<-tibble::tibble(
    IKN=IKN,
    MEQ= MEQ,
    DAYSSUPL=DAYSSUPL,
    DT_OF_SERV_TS=DT_OF_SERV_TS,
    DIN=DIN,
    DIN_DESC=DIN_DESC,
    MANUFACTURER_CD=MANUFACTURER_CD,
    STRENGTH=STRENGTH
  )
  
  #opioid/bzd classification or each individuals perscription
  per_data<-per_data%>%
    dplyr::mutate(
      drug_class=dplyr::case_when(
        grepl("opioid", din_list$Active.Ingredients.Class.and.Use[match(DIN,din_list$DIN.PIN)])~"opioid",
        grepl("bzd", din_list$Active.Ingredient.Class.and.Use[match(DIN, din_list$DIN.PIN)])~"BZD",
        TRUE~"other"
      )
    )
  result<-per_data%>%
    #group the dataset by IKN and establish chronological order of perscriptions for each individual
    dplyr::group_by(IKN)%>%
    dplyr::arrange(DT_OF_SERV_TS, .by_group = TRUE)%>%
    #use summarise function to generate one summary per IKN corresponding
    dplyr::summarise({      
      #identify the first relevant (opioid/bzd) perscription and the duration of that perscription
      first_per<-min(DT_OF_SERV_TS, na.rm = TRUE)
      first_per_day<-DAYSSUPL[DT_OF_SERV_TS==first_per]
      
      #generate observation period. The observation period will be the duration of the first perscription
      period_days<-first_per_day
      cutoff_period<-first_per_day+lubridate::days(period_days)
      
      #idenitfy all perscriptions that occur within the dates previously established as the observation period
      within_period<-DT_OF_SERV_TS<=cutoff_period & DT_OF_SERV_TS>=first_per

      #compute total opioid equivalents
      total_opioid_eq<-sum(ifelse(within_period && drug_class=="opioid",
      MEQ*DAYSSUPL,
      0),
      na.rm=T)
      
      #compute total BZD equivalents
      total_bzd_eq<-sum(ifelse(within_period&drug_class=="bzd",
      MEQ*DAYSSUPL,
      0),
      na.rm=T)

      #compute average opioid equivalents
      avg_bzd_eq<-total_bzd_eq/period_days
      avg_opioid_eq<-total_opioid_eq/period_days
    }

    )
}