install.packages("dagitty")
library(dagitty)

#dag
dag <- dagitty('dag {
"Sense of belonging to community" [exposure,pos="-1.982,1.430"]
"chronic conditions (self report)" [pos="-1.949,-0.381"]
"drug-related harm" [outcome,pos="1.082,1.361"]
"food insecurity" [pos="-1.159,0.355"]
"illicit drug abuse" [pos="0.034,1.442"]
"marital status" [pos="-1.361,0.609"]
"material deprivation" [pos="-1.863,0.710"]
"perscription drug abuse" [pos="1.203,-1.364"]
"prescription characteristics" [pos="0.701,-0.131"]
"self-report mental health disorder" [pos="-0.869,-1.044"]
Age [pos="-2.065,-1.302"]
education [pos="-0.818,-0.092"]
income [pos="-1.487,-0.092"]
pain [pos="0.252,-0.111"]
"Sense of belonging to community" -> "illicit drug abuse"
"Sense of belonging to community" -> "perscription drug abuse"
"chronic conditions (self report)" -> "Sense of belonging to community"
"chronic conditions (self report)" -> "perscription drug abuse"
"chronic conditions (self report)" -> pain
"food insecurity" -> "illicit drug abuse"
"food insecurity" -> "perscription drug abuse"
"food insecurity" -> "self-report mental health disorder"
"illicit drug abuse" -> "drug-related harm"
"marital status" -> "Sense of belonging to community"
"marital status" -> "illicit drug abuse"
"material deprivation" -> "Sense of belonging to community"
"material deprivation" -> "illicit drug abuse"
"perscription drug abuse" -> "drug-related harm"
"prescription characteristics" -> "perscription drug abuse"
"self-report mental health disorder" -> "Sense of belonging to community"
"self-report mental health disorder" -> "illicit drug abuse"
"self-report mental health disorder" -> "perscription drug abuse"
Age -> "Sense of belonging to community"
Age -> "chronic conditions (self report)"
Age -> "illicit drug abuse"
Age -> "marital status"
Age -> "perscription drug abuse"
Age -> "self-report mental health disorder"
Age -> education
Age -> income
education -> income
income -> "food insecurity"
income -> "material deprivation"
income -> "self-report mental health disorder"
pain -> "Sense of belonging to community"
pain -> "illicit drug abuse"
pain -> "perscription drug abuse"
}
'
)


dag_perscription_opioid<-dagitty::dagitty('dag {
"chronic conditions (self report)" [adjusted,pos="-0.898,-1.056"]
"drug-related harm" [outcome,pos="0.949,1.144"]
"illicit drug abuse" [pos="-0.000,1.206"]
"material deprivation" [adjusted,pos="-1.747,-0.161"]
"opioid perscription initiation/dose" [exposure,pos="-1.847,1.096"]
"perscription drug abuse" [pos="0.924,-1.304"]
"self-report drug use" [adjusted,pos="-1.352,-0.770"]
Age [adjusted,pos="-2.077,-1.231"]
income [pos="-1.855,-0.605"]
pain [adjusted,pos="0.151,-0.054"]
sex [adjusted,pos="-0.669,-0.374"]
"chronic conditions (self report)" -> "opioid perscription initiation/dose"
"chronic conditions (self report)" -> "perscription drug abuse"
"chronic conditions (self report)" -> pain
"illicit drug abuse" -> "drug-related harm"
"material deprivation" -> "illicit drug abuse"
"material deprivation" -> "opioid perscription initiation/dose"
"material deprivation" -> "perscription drug abuse"
"opioid perscription initiation/dose" -> "illicit drug abuse"
"opioid perscription initiation/dose" -> "perscription drug abuse"
"perscription drug abuse" -> "drug-related harm"
"self-report drug use" -> "illicit drug abuse"
"self-report drug use" -> "opioid perscription initiation/dose"
"self-report drug use" -> "perscription drug abuse"
Age -> "chronic conditions (self report)"
Age -> "illicit drug abuse"
Age -> "opioid perscription initiation/dose"
Age -> "perscription drug abuse"
Age -> "self-report drug use"
Age -> income
income -> "material deprivation"
pain -> "illicit drug abuse"
pain -> "opioid perscription initiation/dose"
pain -> "perscription drug abuse"
sex -> "illicit drug abuse"
sex -> "opioid perscription initiation/dose"
sex -> "perscription drug abuse"
sex -> "self-report drug use"
}')

dag_perscription_BZD<-dagitty('dag {
"BZD perscription initiation/dose" [exposure,pos="-1.847,1.096"]
"drug-related harm" [outcome,pos="0.949,1.144"]
"illicit drug abuse" [pos="0.341,1.223"]
"material deprivation" [adjusted,pos="-1.811,-0.116"]
"mental health conditions" [adjusted,pos="0.678,-0.109"]
"perscription drug abuse" [pos="0.924,-1.304"]
"self-report drug use" [adjusted,pos="-1.391,-0.746"]
"self-reported stress" [adjusted,pos="-0.652,1.006"]
"sense of belonging/isolation" [pos="-1.564,0.538"]
Age [adjusted,pos="-2.077,-1.231"]
income [pos="-1.855,-0.605"]
rural [pos="-1.159,-0.112"]
sex [adjusted,pos="-0.669,-0.374"]
"BZD perscription initiation/dose" -> "illicit drug abuse"
"BZD perscription initiation/dose" -> "perscription drug abuse"
"illicit drug abuse" -> "drug-related harm"
"material deprivation" -> "BZD perscription initiation/dose"
"material deprivation" -> "illicit drug abuse"
"material deprivation" -> "perscription drug abuse"
"material deprivation" -> "self-reported stress"
"material deprivation" -> "sense of belonging/isolation"
"mental health conditions" -> "BZD perscription initiation/dose"
"mental health conditions" -> "illicit drug abuse"
"mental health conditions" -> "perscription drug abuse"
"perscription drug abuse" -> "drug-related harm"
"self-report drug use" -> "BZD perscription initiation/dose"
"self-report drug use" -> "illicit drug abuse"
"self-report drug use" -> "perscription drug abuse"
"self-reported stress" -> "BZD perscription initiation/dose"
"self-reported stress" -> "illicit drug abuse"
"self-reported stress" -> "perscription drug abuse"
"sense of belonging/isolation" -> "self-reported stress"
Age -> "BZD perscription initiation/dose"
Age -> "illicit drug abuse"
Age -> "mental health conditions"
Age -> "perscription drug abuse"
Age -> "self-report drug use"
Age -> income
income -> "material deprivation"
income -> rural
rural -> "material deprivation"
rural -> "self-reported stress"
sex -> "BZD perscription initiation/dose"
sex -> "illicit drug abuse"
sex -> "mental health conditions"
sex -> "perscription drug abuse"
sex -> "self-report drug use"
sex -> "self-reported stress"
}
')

dag_mental_health<-dagitty::dagitty('dag {
"drug-related harm" [outcome,pos="0.665,1.104"]
"illicit drug abuse" [latent,pos="0.131,1.119"]
"marital status" [adjusted,pos="0.385,-0.079"]
"material deprivation" [adjusted,pos="-1.600,0.395"]
"metal health conditions (anxiety/depression) self-report" [exposure,pos="-1.815,0.948"]
"multiple chronic conditions" [adjusted,pos="-1.576,-1.080"]
"perscription drug abuse" [latent,pos="0.694,-1.253"]
"self-reported stress" [adjusted,pos="-0.889,0.917"]
Age [adjusted,pos="-1.916,-1.203"]
Alcohol [adjusted,pos="-0.373,-0.049"]
income [pos="-1.541,-0.319"]
rural [pos="-1.070,-0.918"]
sex [adjusted,pos="-0.463,-0.874"]
"illicit drug abuse" -> "drug-related harm"
"marital status" -> "illicit drug abuse"
"marital status" -> "self-reported stress"
"material deprivation" -> "illicit drug abuse"
"material deprivation" -> "metal health conditions (anxiety/depression) self-report"
"material deprivation" -> Alcohol
"metal health conditions (anxiety/depression) self-report" -> "illicit drug abuse"
"metal health conditions (anxiety/depression) self-report" -> "perscription drug abuse"
"metal health conditions (anxiety/depression) self-report" <-> Alcohol
"multiple chronic conditions" -> "metal health conditions (anxiety/depression) self-report"
"multiple chronic conditions" -> "perscription drug abuse"
"perscription drug abuse" -> "drug-related harm"
"self-reported stress" -> "illicit drug abuse"
"self-reported stress" -> "metal health conditions (anxiety/depression) self-report"
"self-reported stress" -> "perscription drug abuse"
Age -> "illicit drug abuse"
Age -> "metal health conditions (anxiety/depression) self-report"
Age -> "multiple chronic conditions"
Age -> "perscription drug abuse"
Age -> income
Alcohol -> "illicit drug abuse"
Alcohol -> "perscription drug abuse"
income -> "material deprivation"
rural -> "material deprivation"
rural -> "self-reported stress"
rural -> income
sex -> "illicit drug abuse"
sex -> "metal health conditions (anxiety/depression) self-report"
sex -> "perscription drug abuse"
sex -> "self-reported stress"
sex -> income
}
')

#find adjustment sets
dagitty::adjustmentSets(dag)
dagitty::adjustmentSets(dag_perscription_opioid)
dagitty::adjustmentSets(dag_perscription_BZD)

###OUTPUT minimal adjustment sets
#for sense of belong to local community: {SurveyC Age, chronic conditions (self report), marital status, material deprivation, pain, self-report mental health disorder }
#For opioid perscription: { Age, chronic conditions (self report), material deprivation, pain, self-report drug use, sex }
#For BZD perscription { Age, material deprivation, mental health conditions, self-report drug use, self-reported stress, sex }
#for mental health: { Age, Alcohol, marital status, material deprivation, multiple chronic conditions, self-reported stress, sex }