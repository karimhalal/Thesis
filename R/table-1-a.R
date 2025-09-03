library(dplyr)
library(labelled)
library(stringr)
library(purrr)

library(dplyr)
library(labelled)
library(stringr)
library(purrr)

to_labelled_from_attrs <- function(x) {
  labs_long <- attr(x, "labels_long", exact = TRUE)
  if (is.null(labs_long)) return(x)

  var_lab <- attr(x, "label_long", exact = TRUE)
  if (is.null(var_lab)) var_lab <- attr(x, "label", exact = TRUE)

  # Build value labels: keep only numeric codes from labels_long (skip NA(a)/NA(b) in labels)
  codes_chr <- unname(labs_long)
  names_chr <- names(labs_long)
  num_ok <- suppressWarnings(!is.na(as.numeric(codes_chr)))
  val_labs <- as.numeric(codes_chr[num_ok])
  names(val_labs) <- names_chr[num_ok]

  # Coerce data to numeric + tagged NA where present
  x_chr <- as.character(x)

  # positions that are tagged NA strings like "NA(a)"
  is_tag <- stringr::str_detect(x_chr, "^NA\\([a-z]\\)$")
  is_tag[is.na(is_tag)] <- FALSE

  # numeric where possible
  x_out <- suppressWarnings(as.numeric(x_chr))

  if (any(is_tag)) {
    tags <- stringr::str_match(x_chr[is_tag], "^NA\\(([a-z])\\)$")[, 2]
    # tags might still have NA; keep only non-NA tags
    keep <- !is.na(tags)
    idx <- which(is_tag)
    idx <- idx[keep]
    if (length(idx)) x_out[idx] <- tagged_na(tags[keep])
  }

  labelled(x = x_out, labels = val_labs, label = var_lab)
}



# Option B: automatically target any column that has labels_long
harmonized_data1 <- harmonized_data %>%
  mutate(across(where(~ !is.null(attr(., "labels_long"))), to_labelled_from_attrs))

my_tbl<-harmonized_data%>%
  tbl_summary(statistic = list(all_continuous() ~ "{median} ({p25}, {p75})", all_categorical() ~
    "{n} ({p}%)"),
  include=c(ALCDTTM,DHHGAGE_D,DHH_SEX,ALWDWKY, SDCGCGT, SurveyCycle, CCC_031,CCC_071, CCC_091, CCC_121, CCC_131, CCC_280, CCC_290, DHHGMS, EDUDR03, FSCDHFS2, GEN_07, GEN_09, GEN_10, HUPDPAD, smoke_simple ))

library(dplyr)
library(gtsummary)

# Loop through variables in the table that have labels_long
vars_with_labels <- names(select(harmonized_data, where(~ !is.null(attr(., "labels_long")))))

for (var in vars_with_labels) {
  labs_map <- setNames(
    names(attr(harmonized_data[[var]], "labels_long")),   # display names
    unname(attr(harmonized_data[[var]], "labels_long"))   # current codes in table
  )
  
  my_tbl <- my_tbl %>%
    modify_table_body(
      ~ .x %>%
        mutate(label = ifelse(
          variable == var & label %in% names(labs_map),
          labs_map[label],
          label
        ))
    )
}



