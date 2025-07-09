#list of functions to derive variables of interest
adl_fun <- cchsflow::adl_fun
adl_score_5_fun <- cchsflow::adl_score_5_fun
binge_drinker_fun <- cchsflow::binge_drinker_fun
if_else2 <- cchsflow::if_else2
low_drink_short_fun <- cchsflow::low_drink_short_fun
low_drink_long_fun <- cchsflow::low_drink_long_fun
age_cat_fun <- cchsflow::age_cat_fun
low_drink_score_fun <- cchsflow::low_drink_score_fun
low_drink_score_fun1 <- cchsflow::low_drink_score_fun1
SMKG040_fun <- cchsflow::SMKG040_fun
time_quit_smoking_fun <- cchsflow::time_quit_smoking_fun
smoke_simple_fun <- cchsflow::smoke_simple_fun
surveycycle_fun<-cchsflow::surveycycle_fun


#change name of the survey cycle to simplify it
surveycycle_fun<- function (data_name){
switch(data_name, cchs2013_2014= {return("2013-2014")},
cchs2015_2016= {return("2015-2016")},
cchs2017_2018= {return("2017-2018")},)
}