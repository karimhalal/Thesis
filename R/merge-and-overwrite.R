merge_and_overwrite <- function(x, y, by) {
  merged_df_duplicate_columns <- base::merge(x, y, by = by, all.x = TRUE, all.y = TRUE)

  merged_df_duplicate_columns %>%
    dplyr::select(-dplyr::ends_with(".x")) %>%
    dplyr::rename_with(~ gsub("\\.y", "", .), dplyr::ends_with(".y"))
}
