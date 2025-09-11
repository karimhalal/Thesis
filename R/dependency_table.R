#load relevant packages
library(tibble)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(packrat)


pkgs <- c(
  "tidyverse", "recodeflow", "mice", "gtsummary", "labelled",
  "here", "fastDummies", "rms", "Hmisc", "tidylog", 
  "cowplot", "glue", "janitor", "readxl", "writexl", "betareg", "tidymodels",
  "cchsflow", "cmprsk", "survival", "dials", "config", "epitools", "flextable", "ftExtra",
  "ggplot2", "haven", "jsonlite", "kableExtra", "logger", "magrittr", "memoise", "devtools",
  "tibble", "purrr", "quarto", "pkgdown", "lintr", "stylr", "osfr", "parsnip", "pmsampsize",
  "ranger", "rsample", "stringr", "targets", "utils", "xgboost", "yardstick", "DescTools", "survminer", "patchwork",
  "tune", "dplyr", "reshape", "riskRegression", "pec", "timeROC", "tableone", "gbm"

)


# Instead of unlisting -> unique -> sort, stop after mapping
deps_list <- pkgs |>
  purrr::map(\(x) tools::package_dependencies(x, recursive = TRUE)) 

# Build a table: one row per package with all its deps
deps_tbl <- tibble(
  package = pkgs,
  dependencies = map_chr(deps_list, ~ paste(sort(unique(.x[[1]])), collapse = ", "))
)

write.csv(deps_tbl, file="/Users/karimhalal/Desktop/The worlds greatest thesis/Thesis/worksheets/deps_tbl.csv")


