
#' @title Conversion factor assignment
#' 

#load din_list and necessary libraries
din_list<-read.csv("/Users/karimhalal/Desktop/The worlds greatest thesis/Thesis/worksheets/DIN _list.csv")
library(dplyr)
library(stringr)

#get rid of empty rows (artifact from CSV export)
din_list<-din_list%>%
  filter(!is.na(DIN.PIN))

write.csv(din_list,"worksheets/DIN _list.csv",row.names=FALSE)

#write edits to new csv file
# #COLUMNS PROCESSING (ADDING LEADING 0S AND CORRECTING SPELLING MISTKAKES)
din_list<-din_list%>%
  mutate(Active.Ingredients=tolower(Active.Ingredients))%>%
  mutate(Active.Ingredient.Class.and.Use=tolower(Active.Ingredient.Class.and.Use))%>%
  mutate(Dosage.Form=tolower(Dosage.Form))%>%
  mutate(DIN.PIN=str_pad(DIN.PIN, width = 8, side = "left", pad = "0"))%>%
  mutate(Active.Ingredients=case_when(
   Active.Ingredients=="hydrormorphone hydrochloride"~"hydromorphone hydrochloride",
   Active.Ingredients=="hydromophone hydrochloride"~"hydromorphone hydrochloride",
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
    grepl("opioid", Active.Ingredient.Class.and.Use) & grepl("buprenorphine", Active.Ingredients) & grepl("tab", Dosage.Form)~ 38.8,
    grepl("opioid", Active.Ingredient.Class.and.Use) & grepl("buprenorphine", Active.Ingredients) & grepl("trans patch", Dosage.Form)~ 2.2,
    grepl("opioid", Active.Ingredient.Class.and.Use) & grepl("buprenorphine", Active.Ingredients) & grepl("film", Dosage.Form)~ 0.039,
    grepl("opioid", Active.Ingredient.Class.and.Use) & grepl("fentanyl", Active.Ingredients)& grepl("trans patch", Dosage.Form) ~ 4.2,
    grepl("opioid", Active.Ingredient.Class.and.Use) & grepl("fentanyl", Active.Ingredients)& grepl("buccal soluble fil|tab", Dosage.Form) ~ 0.13,
    TRUE ~ NA_real_
  ))%>%
  #Exclude medications for which the strength of evidence for conversion is weak. This includes butorphanol and diphenoxylate (methadone and buprenorphine)
  filter(!grepl("diphenoxylate", Active.Ingredients))%>%
  filter(!grepl("butorphanol", Active.Ingredients))%>%
  #filter out the injectible drugs, these are short acting drugs usually used in clinical settings
  filter(!grepl("inj", Dosage.Form))%>%

#assign diazepam equivalents conversion factor to relevent BZD
  mutate(conversion_factor=case_when(
    grepl("bzd", Active.Ingredient.Class.and.Use)&grepl("alprazolam",Active.Ingredients)&grepl("cap|tab",Dosage.Form)~10,
    grepl("bzd", Active.Ingredient.Class.and.Use)&grepl("chlordiazepoxide",Active.Ingredients)&grepl("cap|tab",Dosage.Form)~0.4,
    grepl("bzd", Active.Ingredient.Class.and.Use)&grepl("clobazam",Active.Ingredients)&grepl("cap|tab",Dosage.Form)~0.5,
    grepl("bzd", Active.Ingredient.Class.and.Use)&grepl("clonazepam",Active.Ingredients)&grepl("cap|tab",Dosage.Form)~10,
    grepl("bzd", Active.Ingredient.Class.and.Use)&grepl("clorazepate",Active.Ingredients)&grepl("cap|tab",Dosage.Form)~0.67,
    grepl("bzd", Active.Ingredient.Class.and.Use)&grepl("diazepam",Active.Ingredients)&grepl("cap|tab",Dosage.Form)~1,
    grepl("bzd", Active.Ingredient.Class.and.Use)&grepl("flurazepam",Active.Ingredients)&grepl("cap|tab",Dosage.Form)~0.33,
    grepl("bzd", Active.Ingredient.Class.and.Use)&grepl("lorazepam",Active.Ingredients)&grepl("cap|tab",Dosage.Form)~5,
    grepl("bzd", Active.Ingredient.Class.and.Use)&grepl("oxazepam",Active.Ingredients)&grepl("cap|tab",Dosage.Form)~0.33,
    grepl("bzd", Active.Ingredient.Class.and.Use)&grepl("temazepam",Active.Ingredients)&grepl("cap|tab",Dosage.Form)~0.5,
    grepl("bzd", Active.Ingredient.Class.and.Use)&grepl("triazolam",Active.Ingredients)&grepl("cap|tab",Dosage.Form)~20,
    grepl("bzd", Active.Ingredient.Class.and.Use)&grepl("nitrazepam",Active.Ingredients)&grepl("cap|tab",Dosage.Form)~1,
    TRUE~conversion_factor
  ))

write.csv(din_list, "worksheets/DIN_list2.csv", row.names=FALSE)

din_list_oral<-din_list%>%
  filter(!grepl("inj", Dosage.Form)&!grepl("trans patch", Dosage.Form))
write.csv(din_list_oral, "worksheets/din_list_oral.csv", row.names=FALSE)

din_list_transderm<-din_list%>%
  filter(grepl("trans patch", Dosage.Form))%>%
  write.csv("worksheets/din_list_transdermal.csv", row.names=FALSE)