###NMS DATA DESCRIPTION###

#DIN- 8 digit number used to identify specific drugs. The complete list of DINs is provided in DIN.PIN column

#####DAYSSUPL-######
#  3 digit numeric variable signifying the (NAs are represented ) number of days for which the perscriprtion is expected to last (some missing values that could be coded as empty cells or NAs)
#randomly assign these numbers using a normal distribution (the rsample function)

##### DIN_DESC- ######
# [character] DESCRIPTION OF THE DIN being used fomatted as the following: [BRAND NAME] [GENERIC NAME] [STRENGTH] [DOSAGE FORM] [ROUTE]
#EXAMPLE: CO FENTANYL MATRIX PATCH 25MCG/H
# the different paired DINs and descriptions available are provided in the master DIN list used for mapping
#It is possible that some DIN and DIN Descriptions may be mismatched (do not match the pairings in the mapping sheet)
#There are no standardized spacing convention between the elements of this variable could be a regular space, could be hyphenated, or could be missing a space sometimes

####DOSAGE_FORM####
# the dosage form of each drug perscription
# these are the dosage forms: [Tab], [Cap], [O/L], [Sup], [Inj], [Inj-1ml Pk], [Inj Sol], [Inj Sol-1ml Pk], [Rect Sup],[Oral Drops], [ER Tab], [INJ AMP], [Oral Sol], [SR Tab], [Susp], [SR Cap], [INJ AMP-2ML PK],
# [Tropical Sol], [Inj Sol Amp], [ER Cap], [SL Tab], [CR Cap], [CR Tab], [Rect Gel-2x 5mg Pk], [Nas Spray], [Trans Patch], [Buccal Soluble Fil], [Soluble Film Foil Pk.], [ER Inj Sol-Pref Syr], [Tab (Chewable)],
# [Oral Concentrate (Unflavoured)], [Soluble Film], [ER Tab Chewable], [O/L 500mL], [Rect Gel-2x10mg Pk], [Rect Gel-2x15mg Pk]
#

####CURR_STAT####
# Three category character variable (A (0.8), C(p=0.1), V(p=0.1))

####DT_OF_SERV_TS####
#Datetime of perscription, the format is a POSIXCT style (but no specific time just a date) with the baseline day being January first 1960 (01-01-1960)
#the dates should be spaced equally throughout the period of the study
#Applicable date range: June2012-December2021

###Quantity###
#Num8 variable (numeric variable displayed using 8 variables)- indicates the number of units dipensed in the prescription



####MANUFACTURER_CD####
#3 Letter identifier for the manufacturer (also found in DIN list)

####IKN####
#UNIQUE IDENTIFIER THAT CAN BE USED, SET A SEED FOR REPORODUCIBILITY SO THAT THE IKN can be used for example matching with other generated datasets

####LICENSING COLLEGE OF PERSCRIBER####
###the following are the variable details: 01 = The College of Physicians & Surgeons of Ontario
#02 = Royal College of Dental Surgeons of Ontario
#03 = College of Chiropodists of Ontario
#05 = Out of Province
#08 = College of Midwives of Ontario
#09 = Ontario College of Pharmacists
#43 = College of Optometrists of Ontario
#44 = College of Nurses of Ontario
#99 = Other
#N0 = College of Naturopaths of Ontario

# ============================================================================
# MOCK NMS DATA GENERATION FUNCTION
# ============================================================================

#' Generate Mock NMS (National Medication Survey) Data
#'
#' @param n_records Number of prescription records to generate
#' @param n_patients Number of unique patients (IKN)
#' @param din_list_path Path to the DIN list CSV file
#' @param seed Random seed for reproducibility
#' @return A data frame with mock NMS data
#'
generate_mock_nms_data <- function(
    n_records = 10000,
    n_patients = 2000,
    din_list_path = "DIN _list.csv",
    seed = 42
) {

  # Load required libraries
  library(dplyr)
  library(lubridate)

  # Set seed for reproducibility
  set.seed(seed)

  # Read DIN list
  din_master <- read.csv(din_list_path, stringsAsFactors = FALSE)

  # Clean column names (remove dots from imported column names)
  names(din_master) <- gsub("\\.", "_", names(din_master))

  # ============================================================================
  # 1. Generate IKN (Patient Identifiers)
  # ============================================================================
  # Create unique patient IDs with seed for reproducibility
  set.seed(seed)
  unique_ikn <- sprintf("IKN%07d", sample(1000000:9999999, n_patients, replace = FALSE))

  # Assign IKNs to records (patients can have multiple prescriptions), this function code "long" raw data
  set.seed(seed + 1)
  ikn <- sample(unique_ikn, n_records, replace = TRUE)

##################
  # Generate DIN (Drug Identification Numbers)
##################

  # Sample DINs from the master list. This will represent
  set.seed(seed + 2)
  sampled_dins <- sample(1:nrow(din_master), n_records, replace = TRUE)
  din <- din_master$DIN_PIN[sampled_dins]

  # Introduce data quality issues:
  # 10% junk/invalid DINs, 3% missing, 2% improperly formatted
  set.seed(seed + 2.1)
  din_issue_type <- sample(
    c("valid", "junk", "missing", "malformed"),
    n_records,
    replace = TRUE,
    prob = c(0.85, 0.10, 0.03, 0.02)
  )

  set.seed(seed + 2.2)
  for (i in 1:n_records) {
    if (din_issue_type[i] == "junk") {
      # Generate junk DINs (not in the master list)
      junk_type <- sample(1:3, 1)
      if (junk_type == 1) {
        # Random 8-digit number not in master list
        din[i] <- as.character(sample(90000000:99999999, 1))
      } else if (junk_type == 2) {
        # 7-digit number (too short)
        din[i] <- as.character(sample(1000000:9999999, 1))
      } else {
        # 9-digit number (too long)
        din[i] <- as.character(sample(100000000:999999999, 1))
      }
    } else if (din_issue_type[i] == "missing") {
      # Missing DIN
      din[i] <- NA
    } else if (din_issue_type[i] == "malformed") {
      # Improperly formatted DINs
      malform_type <- sample(1:5, 1)
      if (malform_type == 1) {
        # Leading zeros removed
        din[i] <- gsub("^0+", "", din[i])
      } else if (malform_type == 2) {
        # Extra leading zeros
        din[i] <- paste0("00", din[i])
      } else if (malform_type == 3) {
        # Contains spaces or hyphens
        din[i] <- paste0(substr(din[i], 1, 4), "-", substr(din[i], 5, 8))
      } else {
        # Partial DIN (incomplete)
        din[i] <- substr(din[i], 1, sample(4:6, 1))
      }
    }
  }

  # 
  # 3. Generate MANUFACTURER_CD
  # 
  # Extract from DIN master list based on sampled DINs
  manufacturer_cd <- din_master$Manufacturer_Code[sampled_dins]

  #for invalid DINs, randomly assign the manufacturer code
  for (i in 1:n_records){
    if(din_issue_type[i]!="valid"){
      manufacturer_cd[i]<-sample(c("NOV", "VAL", "HLR", "PFP","PMS", "RPH"),1)
    }
  }

  # 
  # 4. Generate DOSAGE_FORM
  # 
  # Extract from DIN master list for all the valid DINs. This might
  dosage_form <- din_master$Dosage_Form[sampled_dins]

  #for invalid DIN, randomize the most common types
  for(i in 1:n_records){
    if(din_issue_type[i]!="valid"){
      dosage_form[i]<-sample(c(
  "buccal soluble fil",
  "cap",
  "chew tab",
  "cr cap",
  "cr tab",
  "er cap",
  "er pd for sol",
  "er tab",
  "er tab chewable",
  "o/l",
  "o/l 500ml",
  "oral concentrate (cherry flavour)",
  "oral concentrate (unflavoured)",
  "oral drops",
  "oral sol",
  "rect gel",
  "rect gel-2x 5mg pk",
  "rect gel-2x10mg pk",
  "rect gel-2x15mg pk",
  "rect sup",
  "sl tab",
  "soluble film",
  "soluble film foil pk.",
  "sr cap",
  "sr tab",
  "sup",
  "susp",
  "tab",
  "tab (chewable)",
  "topical sol"
), size = 1)
    }
  }


  #
  # 5. STRENGTH (Description)
  # 
  # Dosage character variable including the dosage quantity (2 or 3 digit number) followed
  #assign dosage for correctly identified DIN
  STRENGTH<-din_master$Strength[sampled_dins]

  #Generate random doses for invalid DINs based on Dosage forms
  for(i in 1:n_records){
    if (din_issue_type[i] != "valid" & grepl("cap", dosage_form[i])){
      STRENGTH[i]<-as.character(paste(sample(0.25:150, 1), "mg"))
    } else if (din_issue_type[i]!="valid"&grepl("Trans Patch", dosage_form[i])){
      STRENGTH[i]<-as.character(paste(sample(c(12, 25, 50, 75, 100, 125), 1), "mcg/hr"))
    } else if (din_issue_type[i]!="valid"&grepl("o/l", dosage_form[i])){
      STRENGTH[i]<-as.character(paste(sample(c(1,2,10,25,30, 50, 60, 75), 1), "mg/ml"))
    } else if (din_issue_type[i]!="valid"&grepl("inj", dosage_form[i])){
      STRENGTH[i]<-as.character(paste(sample(c(1,2,10,25,30, 50, 60, 75, 100, 200), 1), "mg/ml"))
    }
  }


  # 
  # 6. Generate DIN_DESC (Description)
  # 
  # Description of DIN including the following: BRAND NAME] [GENERIC NAME] [STRENGTH] [DOSAGE FORM] [ROUTE]
  set.seed(seed + 3)

  din_desc <- mapply(function(brand, strength, form, idx) {
    # Introduce random spacing variations
    spacing_type <- sample(1:3, 1)

    if (spacing_type == 1) {
      # Normal spacing
      paste(toupper(brand), strength, form, sep = " ")
    } else if (spacing_type == 2) {
      # Hyphenated
      paste(toupper(brand), strength, form, sep = "-")
    } else {
      # Sometimes missing space
      if (runif(1) > 0.5) {
        paste0(toupper(brand), " ", strength, form)
      } else {
        paste(toupper(brand), strength, form, sep = " ")
      }
    }
  },
  din_master$Brand_Name[sampled_dins],
  din_master$Strength[sampled_dins],
  din_master$Dosage_Form[sampled_dins],
  1:n_records)

  # Introduce some mismatches (5% of records)
  set.seed(seed + 4)
  mismatch_indices <- sample(1:n_records, round(0.05 * n_records))
  if (length(mismatch_indices) > 0) {
    random_dins <- sample(1:nrow(din_master), length(mismatch_indices), replace = TRUE)
    din_desc[mismatch_indices] <- toupper(paste(
      din_master$Brand_Name[random_dins],
      din_master$Strength[random_dins],
      din_master$Dosage_Form[random_dins]
    ))
  }

  # 
  # 6. Generate DAYSSUPL (Days Supply)
  # 
  # Normal distribution with mean ~30 days, SD ~15 days
  set.seed(seed + 5)
  dayssupl <- round(rnorm(n_records, mean = 30, sd = 15))
  # Constrain to reasonable values (1-365 days)
  dayssupl <- pmin(pmax(dayssupl, 1), 365)

  # Introduce missing values (approximately 5%)
  set.seed(seed + 6)
  na_indices <- sample(1:n_records, round(0.05 * n_records))
  dayssupl[na_indices] <- NA

  # 
  # 7. Generate CURR_STAT (Current Status)
  # 
  # The overwhelming majority of perscriptions will not be reversed (this should be cleaned by the analyst) A = 90%, C = 5%, V = 5%
  set.seed(seed + 7)
  curr_stat <- sample(
    c("A", "C", "V"),
    n_records,
    replace = TRUE,
    prob = c(0.9, 0.05, 0.05)
  )

  # ============================================================================
  # 8. Generate DT_OF_SERV_TS (Date of Service)
  # ============================================================================
  # Date range: June 2012 to December 2021
  start_date <- as.POSIXct("2012-06-01", tz = "UTC")
  end_date <- as.POSIXct("2021-12-31", tz = "UTC")

  # Generate equally spaced dates with some random variation
  set.seed(seed + 8)
  dt_of_serv_ts <- seq(start_date, end_date, length.out = n_records) +
    runif(n_records, -86400, 86400) # Add random ±1 day variation

  # Convert to date only (no time component)
  dt_of_serv_ts <- as.POSIXct(format(dt_of_serv_ts, "%Y-%m-%d"), tz = "UTC")

  # ============================================================================
  # 9. Generate QUANTITY (Number of Units Dispensed)
  # ============================================================================
  # Typically follows a distribution based on dosage form
  set.seed(seed + 9)

  quantity <- sapply(dosage_form, function(form) {
    if (grepl("Trans Patch|Patch", form, ignore.case = TRUE)) {
      # Patches: typically 3 units
      round(rnorm(1, mean = 10, sd = 2))
    } else if (grepl("Tab|Cap", form, ignore.case = TRUE)) {
      # Tablets/Capsules: typically 30-90 units
      round(rnorm(1, mean = 60, sd = 30))
    } else if (grepl("O/L|Oral|Syrup|Sol", form, ignore.case = TRUE)) {
      # Liquids: typically 1-3 bottles
      round(rnorm(1, mean = 2, sd = 1))
    } else if (grepl("Inj", form, ignore.case = TRUE)) {
      # Injections: typically 1-10 units
      round(rnorm(1, mean = 3, sd = 2))
    } else {
      # Default
      round(rnorm(1, mean = 30, sd = 15))
    }
  })

  # Constrain to positive values
  quantity <- pmax(quantity, 1)

  # ============================================================================
  # 10. Generate LICENSING_COLLEGE_PRESCRIBER
  # ============================================================================
  # Weighted distribution based on typical prescriber types
  set.seed(seed + 10)
  licensing_college <- sample(
    c("01", "02", "03", "05", "08", "09", "43", "44", "99", "N0"),
    n_records,
    replace = TRUE,
    prob = c(0.75, 0.02, 0.01, 0.05, 0.01, 0.05, 0.02, 0.05, 0.03, 0.01)
  )

  # ============================================================================
  # Create Final Data Frame
  # ============================================================================
  nms_mock_data <- data.frame(
    IKN = ikn,
    DIN = din,
    DIN_DESC = din_desc,
    MANUFACTURER_CD = manufacturer_cd,
    STRENGTH=STRENGTH,
    DOSAGE_FORM = dosage_form,
    DAYSSUPL = dayssupl,
    QUANTITY = quantity,
    DT_OF_SERV_TS = dt_of_serv_ts,
    CURR_STAT = curr_stat,
    LICENSING_COLLEGE_PRESCRIBER = licensing_college,
    stringsAsFactors = FALSE
  )

  # Sort by IKN and date
  nms_mock_data <- nms_mock_data %>%
    arrange(IKN, DT_OF_SERV_TS)

  return(nms_mock_data)
}

# ============================================================================
# Example Usage
# ============================================================================

# Generate mock data
# mock_nms <- generate_mock_nms_data(
#   n_records = 10000,
#   n_patients = 2000,
#   din_list_path = "DIN _list.csv",
#   seed = 42
# )
#
# # View first few records
# head(mock_nms)
#
# # Check structure
# str(mock_nms)
#
# # Summary statistics
# summary(mock_nms)
#
# # Save to CSV
# write.csv(mock_nms, "mock_nms_data.csv", row.names = FALSE)
