is_part_of_group <- function(roles_sheet_row) {
  return(nchar(trimws(roles_sheet_row[1,]$roleGroup)) != 0)
}