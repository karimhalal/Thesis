# Trends and Variations in Associations Between Survey-Derived Individual Characteristics and Opioid-Related Adverse Events in Community-Dwelling Ontarians: 2013-2024

### 1. Project Goal

This project aims to develop a predictive algorithm for the risk of drug overdose following the prescription of narcotics. It utilizes a survival-analysis approach, drawing on administrative and survey-based predictors from the Canadian Community Health Survey (CCHS) and the Narcotics Monitoring System (NMS).

### 2. Project Organization

The project is organized into the following directories:

-   **Data/**: Contains the raw CCHS data for different survey cycles (2013-2018).
-   **R/**: Houses all R scripts for data loading, processing, analysis, medication dose calculations, and utility data sheet functions.
-   **worksheets/**: Contains metadata sheets for CCHS variables (CCHSflow-style) and NMS medication data (DIN lists, conversion factors).
-   **renv/**: R package environment management for reproducibility.
-   **papers/**: Research protocol documentation and eventually thesis writeup markdown.
-   **test/**: Testing directory (currently empty, will be updated with tests once functions are finalized).

```         
/Users/karimhalal/Desktop/The worlds greatest thesis/Thesis/
├── .gitignore
├── .Rprofile
├── config.yml
├── README.md
├── Thesis.Rproj
├── renv.lock
├── test_renv_compatibility.R
├── Data/
│   ├── cchs2013_2014.RData
│   ├── cchs2015_2016.RData
│   └── cchs2017_2018.RData
├── R/
│   ├── average_dose_cumulative.R
│   ├── conversion_factor_assignment.R
│   ├── create-study-data.R
│   ├── dependency_table.R
│   ├── din_data_builtin.R
│   ├── DIN_utils.R
│   ├── dose_cat_fun.R
│   ├── dose_parsing_fun.R
│   ├── generate_din_builtin.R
│   ├── get-dependency-tree.R
│   ├── get-desc-data.R
│   ├── get-start-var.R
│   ├── harmonized.R
│   ├── impute-data.R
│   ├── list-utils.R
│   ├── loadData.R
│   ├── mock_data_NMS.R
│   ├── oral_eq_fun.R
│   ├── roles-sheet.R
│   ├── sample_time_variance.R
│   ├── special_functions.R
│   ├── table-1-a.R
│   ├── tanspatch_eq_fun.R
│   ├── transformation-type.R
│   ├── truncate-data.R
│   ├── variable-details-sheet-utils.R
│   ├── variable-start-utils\.R
│   └── variables-sheet-utils.R
├── worksheets/
│   ├── README.md
│   ├── deps_tbl.csv
│   ├── din_list2up.csv
│   ├── demportdev-variables-2.numbers
│   ├── cchs_sheets/
│   │   ├── README.md
│   │   ├── variable_roles_README.md
│   │   ├── CCHSFLOW_VARIABLE_DETAILS.csv
│   │   ├── od_variables.csv
│   │   └── variables_sheet.csv
│   └── NMS_sheets/
│       ├── DIN _list.csv
│       ├── DIN_list2.csv
│       ├── bzd.csv
│       ├── din_list_oral.csv
│       ├── din_list_transdermal.csv
│       └── NMS_Datasheet.csv
├── renv/
│   ├── activate.R
│   ├── settings.json
│   ├── .gitignore
│   └── library/
├── papers/
│   └── Protocol-Updated.docx
└── test/
```

### 3. File Descriptions

### Root Directory

-   [.gitignore](.gitignore): Specifies files and directories to be ignored by Git.
-   [.Rprofile](.Rprofile): R environment initialization file for the project.
-   [config.yml](config.yml): The main configuration file that defines paths to data and variable sheets (both CCHS and NMS), ensuring a centralized and easily manageable setup. This file will be populated by dummy paths so as to preserve source ID privacy.
-   [README.md](README.md): This file provides an overview of the project structure and organization.
-   [Thesis.Rproj](Thesis.Rproj): RStudio project file that helps manage the project context and settings.
-   [renv.lock](renv.lock): Lock file for R package versions to ensure reproducibility across environments.
-   [test_renv_compatibility.R](test_renv_compatibility.R): Script for testing compatibility of the renv environment setup. Will later be

### `Data/` Directory

This directory stores the CCHS datasets for three different survey cycles: - [cchs2013_2014.RData](Data/cchs2013_2014.RData): CCHS PUMF data for 2013-2014 cycle - [cchs2015_2016.RData](Data/cchs2015_2016.RData): CCHS PUMF data for 2015-2016 cycle - [cchs2017_2018.RData](Data/cchs2017_2018.RData): CCHS PUMF data for 2017-2018 cycle. This directory is not included in ICES-ready release.

### `R/` Directory

This directory contains all the core R scripts organized by functionality:

#### Data Loading & Harmonization

-   [loadData.R](R/loadData.R): Main data handling script that reads configuration from `config.yml`, loads CCHS data, and orchestrates the harmonization process using `cchsflow` and `recodeflow` packages.
-   [harmonized.R](R/harmonized.R): Scripts for creating and manipulating the harmonized dataset. This is a legact script, that will later be excluded
-   [create-study-data.R](R/create-study-data.R): Creates the final study dataset from harmonized CCHS data.

#### Medication & Dose Calculations (NMS)

-   [DIN_utils.R](R/DIN_utils.R): Utilities for processing Drug Identification Number (DIN) lists, parsing medication information, and validating DIN data.
-   [din_data_builtin.R](R/din_data_builtin.R): Auto-generated file containing a built-in DIN dataset as hardcoded R vectors, enabling DIN lookups without external CSV dependencies.
-   [generate_din_builtin.R](R/generate_din_builtin.R): Generator script that reads DIN_list2.csv and produces the din_data_builtin.R file with embedded DIN data.
-   [dose_parsing_fun.R](R/dose_parsing_fun.R): Functions for parsing medication dose information from prescription records.
-   [oral_eq_fun.R](R/oral_eq_fun.R): Calculates oral morphine equivalent doses for oral opioid medications.
-   [tanspatch_eq_fun.R](R/tanspatch_eq_fun.R): Calculates oral morphine equivalent doses for transdermal (patch) opioid medications.
-   [conversion_factor_assignment.R](R/conversion_factor_assignment.R): Assigns conversion factors to medications according ODPRN guidelines and papers outlined in study protcol. These are used for standardizing dose calculations
-   [average_dose_cumulative.R](R/average_dose_cumulative.R): Calculates average and cumulative medication doses over time periods. Inidcates total drug exposure over period of initial perscription
-   [dose_cat_fun.R](R/dose_cat_fun.R): Categorizes medication doses into clinically meaningful groups. (high dose, moderate dose, low dose)

#### Variable & Sheet Utilities- CCHSFLOW Sheets

-   [variables-sheet-utils.R](R/variables-sheet-utils.R): Utility functions for working with CCHSFLOW variable and variable details sheets.
-   [variable-details-sheet-utils.R](R/variable-details-sheet-utils.R): Utility functions for working with variable details metadata.
-   [variable-start-utils.R](R/variable-start-utils.R): Utilities for handling variable start information across CCHS cycles.
-   [roles-sheet.R](R/roles-sheet.R): Functions for managing variable role assignments (predictor, outcome, etc.).
-   [transformation-type.R](R/transformation-type.R): Defines and applies variable transformation types.
-   [get-dependency-tree.R](R/get-dependency-tree.R): Recursively resolves variable dependency trees from the variables sheet, tracing derived variables back to their source columns.
-   [get-start-var.R](R/get-start-var.R): Resolves the starting (source) variable for predictors, handling RCS, centered, and interaction variable types.

#### Data Generation & Testing

-   [mock_data_NMS.R](R/mock_data_NMS.R): Generates mock NMS (Narcotics Monitoring System) data for testing and development purposes.
-   [impute-data.R](R/impute-data.R): Performs multiple imputation using the MICE library to handle missing data in the study dataset, with validation that all predictors have corresponding imputation variables.
-   [sample_time_variance.R](R/sample_time_variance.R): Functions for handling temporal variance in sampling. Also legacy code, will be removed.
-   [truncate-data.R](R/truncate-data.R): Data truncation utilities for handling extreme values or time periods, truncation rules are specified in variable/variable details sheet

#### Analysis & Reporting

-   [table-1-a.R](R/table-1-a.R): Generates summary descriptive statistics table (Table 1) of dataset characteristics.
-   [get-desc-data.R](R/get-desc-data.R): Retrieves and prepares data for descriptive analysis.
-   [special_functions.R](R/special_functions.R): Collection of custom R functions for specific data transformations and derivations.

#### Project Utilities

-   [dependency_table.R](R/dependency_table.R): Generates a table of all package dependencies for reproducibility.
-   [list-utils.R](R/list-utils.R): Helper functions for list manipulation used across other scripts.

### `worksheets/` Directory

This directory contains metadata sheets and reference data organized into subdirectories. See [worksheets/README.md](worksheets/README.md) for detailed documentation.

#### CCHS Sheets ([worksheets/cchs_sheets/](worksheets/cchs_sheets/))

-   [CCHSFLOW_VARIABLE_DETAILS.csv](worksheets/cchs_sheets/CCHSFLOW_VARIABLE_DETAILS.csv): Comprehensive variable metadata for CCHSflow harmonized variables.
-   [variables_sheet.csv](worksheets/cchs_sheets/variables_sheet.csv): Variable definitions and mappings across CCHS cycles.
-   [README.md](worksheets/cchs_sheets/README.md): Documentation for CCHS variable sheets including variable roles.
-   [variable_roles_README.md](worksheets/cchs_sheets/variable_roles_README.md): Detailed documentation of variable roles and their purposes.

#### NMS Sheets ([worksheets/NMS_sheets/](worksheets/NMS_sheets/))

-   [din_list_oral.csv](worksheets/NMS_sheets/din_list_oral.csv): DIN list for oral opioid medications with conversion factors.
-   [din_list_transdermal.csv](worksheets/NMS_sheets/din_list_transdermal.csv): DIN list for transdermal (patch) opioid medications.
-   [DIN_list2.csv](worksheets/NMS_sheets/DIN_list2.csv): Mapping din_list including corrective dosages (for improprely coded doses), conversion factors
-   [DIN \_list.csv](worksheets/NMS_sheets/DIN%20_list.csv): Raw DIN_list from NMS information sheets
-   [NMS_Datasheet.csv](worksheets/NMS_sheets/NMS_Datasheet.csv): Main NMS dataset variable definitions.
-   [bzd.csv](worksheets/NMS_sheets/bzd.csv): Benzodiazepine medication data (to be excluded in future versions).

#### Root Worksheets Files

-   [deps_tbl.csv](worksheets/deps_tbl.csv): Package dependencies table for ICES environment setup.
-   [din_list2up.csv](worksheets/din_list2up.csv): Older DIN list with incorrect TRAMADOL conversion factors

### `renv/` Directory

R package environment management directory for ensuring reproducibility: - [activate.R](renv/activate.R): Activation script for the renv environment. - [settings.json](renv/settings.json): Configuration settings for renv. - **library/**: Contains installed R packages specific to this project.

### `papers/` Directory

Contains research documentation: - [Protocol-Updated.docx](papers/Protocol-Updated.docx): Updated research protocol document.

### `test/` Directory

Testing directory (currently empty, reserved for future test scripts).

### 4. Architecture and Workflow

The project follows a modular, configuration-driven architecture that promotes clarity and reproducibility. It integrates two main data sources: the Canadian Community Health Survey (CCHS) for individual characteristics and the Narcotics Monitoring System (NMS) for narcotics prescription data.

### Workflow:

1.  **Configuration**: The [config.yml](config.yml) file acts as the single source of truth for all file paths and parameters, including:
    -   CCHS data files for different survey cycles
    -   Variable definition sheets (variables_sheet.csv, CCHSFLOW_VARIABLE_DETAILS.csv)
    -   NMS medication reference data (DIN lists for oral and transdermal medications)
2.  **CCHS Data Loading & Harmonization**:
    -   [loadData.R](R/loadData.R) reads the configuration and orchestrates the CCHS data loading and harmonization process
    -   Iterates through the specified CCHS datasets (2013-2014, 2015-2016, 2017-2018)
    -   Applies transformations using `cchsflow`, `recodeflow` packages and custom functions from [special_functions.R](R/special_functions.R)
    -   Combines data into a single harmonized dataset with consistent variable definitions across cycles
    -   [create-study-data.R](R/create-study-data.R) creates the final study-ready dataset with derived variables
3.  **NMS Data Processing & Medication Dose Calculations**:
    -   [DIN_utils.R](R/DIN_utils.R) loads and validates Drug Identification Number (DIN) reference lists
    -   [dose_parsing_fun.R](R/dose_parsing_fun.R) parses medication dose information from prescription records
    -   [conversion_factor_assignment.R](R/conversion_factor_assignment.R) assigns appropriate conversion factors based on medication formulation
    -   [oral_eq_fun.R](R/oral_eq_fun.R) and [tanspatch_eq_fun.R](R/tanspatch_eq_fun.R) calculate oral morphine equivalent doses for different formulations
    -   [average_dose_cumulative.R](R/average_dose_cumulative.R) computes average and cumulative exposure metrics
    -   [dose_cat_fun.R](R/dose_cat_fun.R) categorizes doses into clinically meaningful groups
4.  **Data Linkage**:
    -   CCHS survey data is linked with NMS prescription data to create a unified analytical dataset
    -   Temporal alignment ensures prescription data corresponds to survey periods
5.  **Analysis & Reporting**:
    -   [table-1-a.R](R/table-1-a.R) generates descriptive statistics tables (Table 1)
    -   [get-desc-data.R](R/get-desc-data.R) prepares data for descriptive analyses
    -   Additional analysis scripts perform survival analysis and predictive modeling
6.  **Utilities & Quality Assurance**:
    -   [dependency_table.R](R/dependency_table.R) generates package dependency tables for reproducibility and validation of package availability
    -   [truncate-data.R](R/truncate-data.R) handles data cleaning and outlier management
    -   Variable sheet utilities ensure consistent variable definitions and roles across the project

### Key Design Principles:

-   **Configuration-Driven**: All file paths and parameters centralized in config.yml
-   **Modular Architecture**: Separate scripts for distinct functionalities (data loading, dose calculations, analysis)
-   **Reproducibility**: Package management via renv, documented dependencies, version-controlled reference data
-   **Metadata-Driven**: Variable roles, transformations, and details defined in external sheets rather than hardcoded
-   **Separation of Concerns**: CCHS harmonization separate from NMS dose calculations, allowing independent updates

This structured approach ensures the analysis is easy to understand, modify, and reproduce across different computing environments.

### 5. Getting Started

#### Prerequisites

-   R version 4.4.2 or higher
-   RStudio (recommended)
-   Access to CCHS data files
-   Access to NMS prescription data (if applicable)

#### Setup Instructions

1.  **Clone or Open the Repository**:

    -   Open the [Thesis.Rproj](Thesis.Rproj) file in RStudio

2.  **Restore R Package Environment**:

    ``` r
    # renv will automatically activate when you open the project
    # To restore all packages to their recorded versions:
    renv::restore()
    ```

3.  **Verify Configuration**:

    -   Check that [config.yml](config.yml) points to the correct data file locations
    -   Ensure all worksheet files are present in the `worksheets/` subdirectories

4.  **Run Data Harmonization**:

    ``` r
    source("R/loadData.R")
    ```

5.  **Generate Study Dataset**:

    ``` r
    source("R/create-study-data.R")
    ```

#### Project Structure Navigation

-   Start with [config.yml](config.yml) to understand data sources and file paths
-   Review [worksheets/README.md](worksheets/README.md) for variable definitions and metadata
-   Explore [worksheets/cchs_sheets/variable_roles_README.md](worksheets/cchs_sheets/variable_roles_README.md) for variable role documentation
-   Review [R/DIN_utils.R](R/DIN_utils.R) for medication data processing

#### For More Information

-   **CCHS Variable Documentation**: See [worksheets/cchs_sheets/README.md](worksheets/cchs_sheets/README.md)
-   **NMS Data Documentation**: See [worksheets/README.md](worksheets/README.md) under NMS_sheets section
-   **Research Protocol**: See [papers/Protocol-Updated.docx](papers/Protocol-Updated.docx)

#### Licenses
- **CCHS Variable Documentation**: see [CCHSFLOW_DOCUMENTATION](https://big-life-lab.github.io/cchsflow/)
- **NMS Data Documentation**: 
    - Drug information is collected from Health Canada [Drug Product Database(DPD)](https://health-products.canada.ca/dpd-bdpp/)
    - Opioid conversion factors extracted from [Gomes et. al.](https://odprn.ca/wp-content/uploads/2020/11/Opioid-Milligrams-of-Morphine-Equivalents_FINAL.pdf)
    - BZD conversion factors extracted from [Borelli et. al.](https://pmc.ncbi.nlm.nih.gov/articles/PMC10373022/)
    - Stimulant medication coversion factors [Farhat et al.](https://pmc.ncbi.nlm.nih.gov/articles/PMC10373022/)
- **Source Code**: Copyright <2026> <Abdul Karim Halal>

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.