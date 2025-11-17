
#' @title Conversion factor assignment
#' 

#load din_list and necessary libraries
din_list<-read.csv("/Users/karimhalal/Desktop/The worlds greatest thesis/Thesis/worksheets/DIN _list.csv")
library(dplyr)
library(stringr)

#COLUMNS PROCESSING (ADDING LEADING 0S AND CORRECTING SPELLING MISTKAKES)
din_list<-din_list%>%
  mutate(Active.Ingredients=tolower(Active.Ingredients))%>%
  mutate(Active.Ingredient.Class.and.Use=tolower(Active.Ingredient.Class.and.Use))%>%
  mutate(DIN.PIN=str_pad(DIN.PIN, width = 8, side = "left", pad = "0"))%>%
  mutate(Active.Ingredients=case_when(
   Active.Ingredients=="hydrormorphone hydrochloride"~"hydromorphone hydrochloride",
    Active.Ingredients=="butalbital + acetylsalicylic acid + caffeine"~"butalbital + acetylsalicylic acid + caffeine+codeine",
    TRUE~Active.Ingredients
  ))

#Assign convesion factors to drugs in the opioid class based on active ingredient (see Adams et. al (2025) and Gomes et. al (2022))
din_list<-din_list%>%
  mutate(conversion_factor = case_when(
    grepl("opioid", Active.Ingredient.Class.and.Use) & grepl("codeine", Active.Ingredients) ~ 0.15,
    grepl("opioid", Active.Ingredient.Class.and.Use) & grepl("morphine", Active.Ingredients) ~ 1,
    grepl("opioid", Active.Ingredient.Class.and.Use) & grepl("oxycodone", Active.Ingredients) ~ 1.5,
    grepl("opioid", Active.Ingredient.Class.and.Use) & grepl("hydromorphone", Active.Ingredients) ~ 5,
    grepl("opioid", Active.Ingredient.Class.and.Use) & grepl("hydrocodone", Active.Ingredients) ~ 1,
    grepl("opioid", Active.Ingredient.Class.and.Use) & grepl("meperidine", Active.Ingredients) ~ 0.1,
    grepl("opioid", Active.Ingredient.Class.and.Use) & grepl("tramadol", Active.Ingredients) ~ 1,
    grepl("opioid", Active.Ingredient.Class.and.Use) & grepl("dihydrocodeine", Active.Ingredients) ~ 0.25,
    grepl("opioid", Active.Ingredient.Class.and.Use) & grepl("pentazocine", Active.Ingredients) ~ 0.25,
    grepl("opioid", Active.Ingredient.Class.and.Use) & grepl("tapentadol", Active.Ingredients) ~ 0.3,
    grepl("opioid", Active.Ingredient.Class.and.Use) & grepl("oxymorphone", Active.Ingredients) ~ 3,
    grepl("opioid", Active.Ingredient.Class.and.Use) & grepl("methadone", Active.Ingredients) ~ 4.7,
    grepl("opioid", Active.Ingredient.Class.and.Use) & grepl("fentanyl", Active.Ingredients)& grepl("Trans Patch", Dosage.Form) ~ 3.8,
    TRUE ~ NA_real_
  ))%>%
  #Exclude medications for which the strength of evidence for conversion is weak. This includes butorphanol and diphenoxylate (methadone and buprenorphine)
  filter(!grepl("diphenoxylate", Active.Ingredients))%>%
  filter(!grepl("butorphanol", Active.Ingredients))
  #filter out the injectible

#check on the missing conversion factors for opioids