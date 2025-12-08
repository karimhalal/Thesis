
#' Load combined DIN list
#' @keywords internal
get_din_list_combined <- function() {
  
  # Check if cached
  if (exists("cached_din_list_all", envir = nms.globals)) {
    return(nms.globals$cached_din_list_all)
  }
  
  # Load config
  config <- yaml::yaml.load_file("config.yml", eval.expr = TRUE)
  
  # Load all DIN lists
  din_oral <- read.csv(
    config$default$variable$din_list_oral,
    fileEncoding = "UTF-8-BOM",
    stringsAsFactors = FALSE
  )
  
  din_transdermal <- read.csv(
    config$default$variable$din_list_transdermal,
    fileEncoding = "UTF-8-BOM",
    stringsAsFactors = FALSE
  )
  
  # Load the combined DIN list
  din_combined <- read.csv(
    config$default$variable$
  )
  
  # Remove duplicates (keep first occurrence)
  din_combined <- din_combined %>%
    dplyr::distinct(DIN.PIN, .keep_all = TRUE)
  
  # Cache it
  nms.globals$cached_din_list_all <- din_combined
  
  message(sprintf(
    "Loaded combined DIN list: %d entries (%d oral, %d transdermal)",
    nrow(din_combined),
    nrow(din_oral),
    nrow(din_transdermal)
  ))
  return(din_oral)
  return(din_transdermal)
  return(din_combined)
}