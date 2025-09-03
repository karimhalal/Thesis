
install.packages("Hmisc")
library(Hmisc)
#Create the study
harmonized_data <- harmonized_data %>%
 mutate(Age_Group = case_when(DHHGAGE_D == 1 ~ "<20",
                               DHHGAGE_D == 2 ~ "20-29",
                               DHHGAGE_D == 3 ~ "30-39", 
                               DHHGAGE_D == 4 ~ "40-49",
                               DHHGAGE_D == 5 ~ "50-59",
                               DHHGAGE_D == 6 ~ "60-69",
                               DHHGAGE_D == 7 ~ "70-79",
                               DHHGAGE_D == 8 ~ "80+")
                              )%>%
  mutate(ALCDTTM = case_when())
str(harmonized_data$ALCDTTM)
library(dplyr)
library(labelled)

harmonized_data <- harmonized_data %>%
  mutate(
    ALCDTTM = set_value_labels(
      ALCDTTM,
      "Regular" = 1,
      "Occasional" = 2,
      "No drink in last 12 months" = 3,
      "Not applicable (new label)" = tagged_na("b")   # relabel tagged NA(b)
    )
  )
