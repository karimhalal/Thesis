#' @title Generate Mock NMS Data for Testing and Validation
#'
#' @description This function generates realistic mock prescription data that mimics the structure and data quality issues
#' found in real-world National Medication Survey (NMS) datasets used in pharmaceutical research and health administrative
#' data analysis. The function creates synthetic prescription records with intentional data quality issues including invalid
#' DINs, missing values, formatting inconsistencies, and mismatched descriptions to simulate real-world data cleaning challenges.
#'
#' The generated dataset includes multiple prescriptions per patient (IKN) and incorporates realistic distributions
#' for prescription timing, drug quantities, dosage forms, and prescriber types. This mock data is designed for
#' testing data cleaning pipelines, validation functions, and analytical workflows without requiring access to
#' protected health information. The function ensures reproducibility through seed control and creates "long" format
#' data where each row represents a single prescription record.
#' 
#' @param n_records [integer] The total number of prescription records to generate. Determines the size of the
#' output dataset. Default is 10000.
#'
#' @param n_patients [integer] The number of unique patients (IKN values) to create. Each patient may have
#' multiple prescription records. Must be less than or equal to n_records. Default is 2000.
#'
#' @param din_list_path [character] File path to the master DIN list CSV file containing valid drug identification
#' numbers and associated metadata (brand names, strengths, dosage forms, manufacturer codes). This file is used
#' as the reference for generating realistic prescription data. Default is "DIN _list.csv".
#'
#' @param seed [numeric] Random seed for reproducibility. Setting the same seed will generate identical mock
#' datasets, which is essential for reproducible testing and validation. Default is 42.
#'
#' @return A data frame with n_records rows containing the following variables:
#'   \item{IKN}{[character] Unique 10-digit patient identifier in format "IKNxxxxxxx"}
#'   \item{DIN}{[character] 8-digit Drug Identification Number (may contain invalid/malformed entries)}
#'   \item{DIN_DESC}{[character] Drug description including brand name, strength, dosage form with variable spacing}
#'   \item{MANUFACTURER_CD}{[character] 3-letter manufacturer code}
#'   \item{STRENGTH}{[character] Drug strength with units (e.g., "25mg", "50mcg/hr")}
#'   \item{DOSAGE_FORM}{[character] Pharmaceutical dosage form (e.g., "tab", "cap", "trans patch")}
#'   \item{DAYSSUPL}{[integer] Number of days the prescription is intended to last (may contain NAs)}
#'   \item{QUANTITY}{[numeric] Number of units dispensed in the prescription}
#'   \item{DT_OF_SERV_TS}{[POSIXct] Date of service/prescription (ranges from June 2012 to December 2021)}
#'   \item{CURR_STAT}{[character] Current status code: "A" (active), "C" (cancelled), or "V" (void)}
#'   \item{LICENSING_COLLEGE_PRESCRIBER}{[character] Two-digit/character code for prescriber's licensing college}
#'
#' @details This function generates mock data with realistic data quality issues to simulate real-world datasets. The function
#' uses a master DIN list as reference and introduces controlled variations and errors to create testing scenarios.
#'
#'          **Variable Descriptions and Data Generation:**
#'
#'          *IKN (ICES Key Number):*
#'          - Unique patient identifier that can be used for record linkage across datasets
#'          - Format: "IKNxxxxxxx" (7 random digits with "IKN" prefix)
#'          - Generated with reproducible seed to enable consistent cross-dataset matching
#'          - Each patient (IKN) can have multiple prescription records
#'
#'          *DIN (Drug Identification Number):*
#'          - 8-digit number used to identify specific drugs in Canada
#'          - Complete list of valid DINs provided in the DIN_PIN column of the master DIN list
#'          - **Data quality issues intentionally introduced:**
#'            - 85% valid DINs from master list
#'            - 10% junk/invalid values (wrong length, not in master list)
#'            - 3% missing values (NA)
#'            - 2% malformed (hyphens, extra zeros, partial DINs)
#'
#'          *DIN_DESC (Drug Description):*
#'          - Character string formatted as: [BRAND NAME] [GENERIC NAME] [STRENGTH] [DOSAGE FORM] [ROUTE]
#'          - Example: "CO FENTANYL MATRIX PATCH 25MCG/H"
#'          - Paired with DIN values from the master DIN mapping list
#'          - **Spacing variations:** normal spaces, hyphens, or missing spaces (no standardization)
#'          - 5% of records have intentionally mismatched DIN/DIN_DESC pairings
#'
#'          *DOSAGE_FORM:*
#'          - Pharmaceutical dosage form of each prescription
#'          - Available forms include: Tab, Cap, O/L, Sup, Inj, Inj-1ml Pk, Inj Sol, Inj Sol-1ml Pk, Rect Sup,
#'            Oral Drops, ER Tab, INJ AMP, Oral Sol, SR Tab, Susp, SR Cap, INJ AMP-2ML PK, Topical Sol,
#'            Inj Sol Amp, ER Cap, SL Tab, CR Cap, CR Tab, Rect Gel-2x 5mg Pk, Nas Spray, Trans Patch,
#'            Buccal Soluble Fil, Soluble Film Foil Pk., ER Inj Sol-Pref Syr, Tab (Chewable),
#'            Oral Concentrate (Unflavoured), Soluble Film, ER Tab Chewable, O/L 500mL,
#'            Rect Gel-2x10mg Pk, Rect Gel-2x15mg Pk
#'          - Extracted from master DIN list for valid DINs; randomly assigned for invalid DINs
#'
#'          *STRENGTH:*
#'          - Drug strength with units (e.g., "25mg", "50mcg/hr", "10mg/ml")
#'          - Format varies by dosage form (mg for oral, mcg/hr for patches, mg/ml for liquids)
#'          - Extracted from master DIN list for valid DINs; randomly generated for invalid DINs
#'
#'          *DAYSSUPL (Days Supply):*
#'          - 3-digit numeric variable indicating number of days prescription is expected to last
#'          - Generated using normal distribution: mean = 30 days, sd = 15 days
#'          - Constrained to range 1-365 days
#'          - **Missing values:** 5% of records coded as NA (empty cells)
#'
#'          *QUANTITY:*
#'          - Numeric variable (8-digit display format) indicating number of units dispensed
#'          - Distribution varies by dosage form:
#'            - Transdermal patches: mean = 10, sd = 2
#'            - Tablets/capsules: mean = 60, sd = 30
#'            - Oral liquids/syrups: mean = 2, sd = 1 (representing bottles)
#'            - Injections: mean = 3, sd = 2
#'            - Default: mean = 30, sd = 15
#'
#'          *DT_OF_SERV_TS (Date of Service - Timestamp):*
#'          - POSIXct datetime format (date only, no time component)
#'          - Baseline: January 1, 1960 (01-01-1960)
#'          - **Study period:** June 2012 to December 2021
#'          - Dates distributed approximately equally throughout period with ±1 day random variation
#'
#'          *CURR_STAT (Current Status):*
#'          - Three-category character variable indicating prescription status
#'          - "A" (Active): 90% - valid, dispensed prescriptions
#'          - "C" (Cancelled): 5% - cancelled before dispensing
#'          - "V" (Void): 5% - voided/reversed prescriptions
#'          - Note: C and V records should typically be filtered out during analysis
#'
#'          *MANUFACTURER_CD (Manufacturer Code):*
#'          - 3-letter identifier for pharmaceutical manufacturer
#'          - Extracted from master DIN list (matched to DIN)
#'          - Examples: "NOV", "VAL", "HLR", "PFP", "PMS", "RPH"
#'          - Randomly assigned for invalid DINs
#'
#'          *LICENSING_COLLEGE_PRESCRIBER:*
#'          - Two-character code identifying prescriber's regulatory college
#'          - Distribution:
#'            - "01" = College of Physicians & Surgeons of Ontario (75%)
#'            - "02" = Royal College of Dental Surgeons of Ontario (2%)
#'            - "03" = College of Chiropodists of Ontario (1%)
#'            - "05" = Out of Province (5%)
#'            - "08" = College of Midwives of Ontario (1%)
#'            - "09" = Ontario College of Pharmacists (5%)
#'            - "43" = College of Optometrists of Ontario (2%)
#'            - "44" = College of Nurses of Ontario (5%)
#'            - "99" = Other (3%)
#'            - "N0" = College of Naturopaths of Ontario (1%)
#'
#'          **Data Structure:**
#'          - Output is in "long" format: one row per prescription record
#'          - Multiple prescriptions per patient (IKN) are represented as separate rows
#'          - Records sorted by IKN and DT_OF_SERV_TS (chronological order within each patient)
#'
#'          **Reproducibility:**
#'          - Multiple seeds used throughout generation (seed, seed+1, seed+2, etc.)
#'          - Setting same seed value ensures identical output for testing and validation
#'          - Critical for creating matched mock datasets across different data sources
#'
#'  @examples
#' # Generate mock data with specified parameters
#' mock_nms <- generate_mock_nms_data(
#'   n_records = 10000,
#'   n_patients = 2000,
#'   din_list_path = "DIN _list.csv",
#'   seed = 42
#' )
#'
#' # View first few records
#' head(mock_nms)
#'
#' # Check structure
#' str(mock_nms)
#'
#' # Summary statistics
#' summary(mock_nms)
#'
#' # Save to CSV
#' write.csv(mock_nms, "mock_nms_data.csv", row.names = FALSE)
#'
#' # Generate smaller dataset for testing and validation
#' test_data <- generate_mock_nms_data(
#'   n_records = 1000,
#'   n_patients = 200,
#'   din_list_path = "DIN _list.csv",
#'   seed = 123
#' )
#'
#' # Generate large dataset with custom seed
#' large_data <- generate_mock_nms_data(
#'   n_records = 50000,
#'   n_patients = 10000,
#'   seed = 2024
#' )
#'
#' # Database usage with dplyr
#' library(dplyr)
#' mock_nms <- generate_mock_nms_data(n_records = 5000, n_patients = 1000)
#'
#' # Filter to valid prescriptions only (active status, no missing values)
#' valid_prescriptions <- mock_nms %>%
#'   filter(CURR_STAT == "A", !is.na(DIN), !is.na(DAYSSUPL))
#'
#' # Count prescriptions per patient
#' patient_summary <- mock_nms %>%
#'   group_by(IKN) %>%
#'   summarise(
#'     n_prescriptions = n(),
#'     first_prescription = min(DT_OF_SERV_TS),
#'     last_prescription = max(DT_OF_SERV_TS)
#'   )
#'
#' # Examine data quality issues
#' data_quality <- mock_nms %>%
#'   summarise(
#'     total_records = n(),
#'     missing_din = sum(is.na(DIN)),
#'     missing_dayssupl = sum(is.na(DAYSSUPL)),
#'     cancelled_void = sum(CURR_STAT %in% c("C", "V"))
#'   )
#'
#' # Basic usage with default parameters
#' mock_data <- generate_mock_nms_data()
#' head(mock_data)
#'
#' # Generate smaller dataset for testing
#' test_data <- generate_mock_nms_data(
#'   n_records = 1000,
#'   n_patients = 200,
#'   din_list_path = "DIN _list.csv",
#'   seed = 123
#' )
#'
#' # Generate large dataset with custom seed
#' large_data <- generate_mock_nms_data(
#'   n_records = 50000,
#'   n_patients = 10000,
#'   seed = 2024
#' )
#'
#' # Database usage with dplyr
#' library(dplyr)
#' mock_nms <- generate_mock_nms_data(n_records = 5000, n_patients = 1000)
#'
#' # Filter to valid prescriptions only
#' valid_prescriptions <- mock_nms %>%
#'   filter(CURR_STAT == "A", !is.na(DIN), !is.na(DAYSSUPL))
#'
#' # Count prescriptions per patient
#' patient_summary <- mock_nms %>%
#'   group_by(IKN) %>%
#'   summarise(n_prescriptions = n())
#'
#' @export
generate_mock_nms_data <- function(
    n_records = 10000,
    n_patients = 2000,
    din_list_path = NULL,
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

  # 
  # 1. Generate IKN (Patient Identifiers)
  #
  # Create unique patient IDs with seed for reproducibility
  set.seed(seed)
  unique_ikn <- sample(100000000:999999999, n_patients, replace = FALSE)

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

  #
  # 8. Generate DT_OF_SERV_TS (Date of Service)
  #
  # Date range: June 2012 to December 2021
  start_date <- as.POSIXct("2012-06-01", tz = "UTC")
  end_date <- as.POSIXct("2021-12-31", tz = "UTC")

  # Generate equally spaced dates with some random variation
  set.seed(seed + 8)
  dt_of_serv_ts <- seq(start_date, end_date, length.out = n_records) +
    runif(n_records, -86400, 86400) # Add random ±1 day variation

  # Convert to date only (no time component)
  dt_of_serv_ts <- as.POSIXct(format(dt_of_serv_ts, "%Y-%m-%d"), tz = "UTC")

  # 
  # 9. Generate QUANTITY (Number of Units Dispensed)
  # 
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
