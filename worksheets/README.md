# Worksheets Directory Organization

This directory contains sheets outlining the the structure of variables used in all analysis. The files are organized into subdirectories based on their data source and purpose.

## Directory Structure

```
worksheets/
├── NMS_sheets/          #  Narcotics Measures System (NMS) related data
├── cchs_sheets/         # Canadian Community Health Survey (CCHS) variable mappings
└── deps_tbl.csv         # Dependencies table for ICES environment setup
```

## Subdirectories

### NMS_sheets/
Contains variable and variable details sheets outlining NMS variables

**Files:**
- `DIN _list.csv` - General DIN list with medication information
- `din_list_oral.csv` - Oral medication DIN list with conversion factors
- `din_list_transdermal.csv` - Transdermal medication DIN list
- `DIN_list2.csv` - Additional DIN reference data- will be excluded later
- `NMS_Datasheet.csv` - Main NMS dataset variables sheet
- `bzd.csv` - Benzodiazepine-specific medication data (will be removed)

**DIN List Structure:**
Each DIN list contains the following information:
- DIN/PIN (Drug Identification Number)
- Brand Name
- Manufacturer Code
- Dosage Form (tablet, capsule, etc.)
- Strength (mg)
- Effective Date and End Date
- Active Ingredients
- Active Ingredient Class and Use
- Conversion factors (where applicable)

### cchs_sheets/
Contains variable mapping and metadata for Canadian Community Health Survey (CCHS) data.

**Files:**
- `variables_sheet.csv` - Comprehensive variable definitions and mappings across CCHS cycles
- `od_variables.csv` - Overdose-related variables from CCHS
- `cchsflow_variables_details1.csv` - Detailed variable specifications and metadata for CCHSflow variables
- `od_variable_details.csv` - Overdose-related variable specifications and metadata

**Variable Sheet Structure:**
The variable mapping files contain:
- **variable**: Variable name/code
- **role**: Variable role (e.g., predictor, intermediate, imputation-variable, table-1-a)
- **label**: Short variable label
- **labelLong**: Descriptive long label
- **section**: Thematic section (e.g., Health status, Demographics)
- **subject**: Specific subject area (e.g., ADL, Chronic condition, Alcohol)
- **variableType**: Type of variable (Categorical, Continuous)
- **units**: Units of measurement
- **databaseStart**: CCHS cycles where variable appears
- **variableStart**: Original variable names across cycles
- **description**: Additional notes and transformations

## Root Files

### deps_tbl.csv
A dependencies table listing R packages and their dependencies used in the project for import into ICES.

**Structure:**
- Package name
- All package dependencies

This file is useful for:
- Tracking package requirements
- Ensuring reproducibility within different environments
- Managing package installations

## Usage Notes

1. **DIN Lists**: Used primarily by functions in `R/DIN_utils.R` and related medication conversion scripts
2. **CCHS Variable Sheets**: Referenced by `R/loadData.R` and variable harmonization functions
3. **Dependencies Table**: Can be used with package management and reproducibility scripts

## Data Standards

- All CSV files use UTF-8 encoding
- Date formats follow ISO 8601 (YYYY-MM-DD) where applicable
- Missing values are represented as tagged `NA` (NA(a) and NA(b))
- Column headers are preserved exactly as shown to maintain compatibility with existing R scripts

## Updates and Maintenance

When updating these reference files:
1. Maintain the existing column structure
2. Document any changes to variable definitions
3. Update corresponding R scripts if variable names change
4. Keep backup copies of previous versions for reproducibility

## Related Code

The following R scripts interact with these worksheets:
- [R/loadData.R](../R/loadData.R) - Loads CCHS data using variable mappings
- [R/DIN_utils.R](../R/DIN_utils.R) - Utilities for DIN list processing
- [R/conversion_factor_assignment.R](../R/conversion_factor_assignment.R) - Medication dose conversions factor assignment
- [R/oral_eq_fun.R](../R/oral_eq_fun.R) - Oral equivalence calculations
