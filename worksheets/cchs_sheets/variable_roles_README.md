# Variable Roles

## predictor
Variables used as independent variables in statistical models.
| Variable Name | Variable Description |
|---|---|
| DHH_AGE_C_rcs1 | Centered age first RCS variable |
| DHH_AGE_C_rcs2 | Centered age second RCS variable |
| DHH_AGE_C_rcs3 | Centered age third RCS variable |
| DHH_AGE_C_rcs4 | Centered age fourth RCS variable |
| DHH_SEX_cat1_c | Centered dummy sex variable: Male |
| DHH_MS_cat2_C | Marital status centered dummy variable: Common-law |
| DHH_MS_cat3_C | Marital status centered dummy variable: Widow/sep/div |
| DHH_MS_cat4_C | Marital status centered dummy variable: Single/Never mar. |
| ALCDTTM_cat1_C | Centered drinker type dummy variable: Regular |
| ALCDTTM_cat2_C | Centered drinker type dummy variable: Occasional |
| ALWDVLTR_der_cat1_C | Centered dummy long term drinking risk: Yes |
| ALWDVSTR_der_cat1_C | Centered dummy short term drinking risk: Yes |
| CCC_031_cat1_c | Centered dummy asthma variable: Yes |
| CCC_061_cat1_c | Centered dummy back problems variable: Yes |
| CCC_071_cat1_c | Centered dummy hypertension variable: Yes |
| CCC_091_cat1_c | Centered dummy COPD/Emphysema/Bronchitis variable: Yes |
| CCC_121_cat1_c | Centered dummy heart disease variable: Yes |
| CCC_131_cat1_c | Centered dummy active cancer variable: Yes |
| CCC_151_cat1_c | Centered dummy stroke variable: Yes |
| CCC_280_cat1_c | Centered dummy mood disorder variable: Yes |
| CCC_290_cat1_c | Centered dummy anxiety disorder variable: Yes |
| EDU_der_cat1 | Centered dummy highest level of education: Less than high school |
| EDU_der_cat2 | Centered dummy highest level of education: High school graduate |
| EDU_der_cat3 | Centered dummy highest level of education: Some post-secondary education |
| FSCDHFS_cat1_C | HC food security centered dummy variable: Mod. Food insec |
| FSCDHFS_cat2_C | HC food security centered dummy variable: Sev. Food insec |
| GEN_01_cat2_C | Self-perceived health centered dummy variable: Very good |
| GEN_01_cat3_C | Self-perceived health centered dummy variable: Good |
| GEN_01_cat4_C | Self-perceived health centered dummy variable: Fair |
| GEN_01_cat5_C | Self-perceived health centered dummy variable: Poor |
| GEN_02B_cat2_C | Self-perceived mental health centered dummy variable: Very good |
| GEN_02B_cat3_C | Self-perceived mental health centered dummy variable: Good |
| GEN_02B_cat4_C | Self-perceived mental health centered dummy variable: Fair |
| GEN_02B_cat5_C | Self-perceived mental health centered dummy variable: Poor |
| GEN_07_cat2_C | Self-perceived life stress centered dummy variable: Not very stressful |
| GEN_07_cat3_C | Self-perceived life stress centered dummy variable: A bit stressful |
| GEN_07_cat4_C | Self-perceived life stress centered dummy variable: Quite a bit stressful |
| GEN_07_cat5_C | Self-perceived life stress centered dummy variable: Extremely stressful |
| GEN_10_cat2_C | Centered dummy sense of belonging: somewhat strong |
| GEN_10_cat3_C | Centered dummy sense of belonging: Somwhat weak |
| GEN_10_cat4_C | Centered dummy sense of belonging: Very weak |
| HUPDPAD_cat2_C | Centered ummy HUI pain: pain prevents no activities |
| HUPDPAD_cat3_C | Centered Dummy HUI pain: pain prevents a few activities |
| HUPDPAD_cat4_C | Centered Dummy HUI pain: pain prevents some activities |
| HUPDPAD_cat5_C | Centered Dummy HUI pain: pain prevents most activities |
| IDGFLA_cat1_C | Centered ummy illicit drug use-ever variable: Yes |
| IDGFLAC_cat1_C | Centered dummy Illicit drug use- ever (exclude one time cannabis): Yes |
| IDGFYA_cat1_C | Centered dummy Illicit drug use- 12 mo: Yes |
| IDGFYAC_cat1_C | Centered dummy illicit drug use- 12 (excluding one time cannabis): Yes |
| INCDRCA_cat2_C | Centered Dummy household income distribution, decile 2 |
| INCDRCA_cat3_C | Centered Dummy household income distribution, decile 3 |
| LBFCDWSS | working status |
| SDCGCGT_cat2_C | Centered dummy ethnicity variable: Non-white |
| smoke_simple | Simple smoking status |
| SurveyCycle_cat1_C | Cnetered Dummied survey year: 2013-2014 |
| SurveyCycle_cat2_C | Centered Dummied survey year: 2015-2016 |
| DHH_AGE_C_X_ALCDTTM_cat1_C | interaction between "Centered Age dummy variable" and "Centered drinker type dummy variable: Regular" |
| DHH_AGE_C_X_ALCDTTM_cat2_C | interaction between "Centered Age dummy variable" and "Centered drinker type dummy variable: Occasional" |
| DHH_AGE_C_X_ALWDWKY_C | interaction between "Centered Age dummy variable" and "Centered drinks last week" |
| DHH_AGE_C_X_CCC_071_cat1_C | interaction between "Centered Age dummy variable" and "Centered dummy hypertension variable: Yes" |
| DHH_AGE_C_X_CCC_091_cat1_C | interaction between "Centered Age dummy variable" and "Centered respiratory condition dummy variable: Has respiratory condition" |
| DHH_AGE_C_X_CCC_121_cat1_C | interaction between "Centered Age dummy variable" and "Centered dummy heart disease variable: Yes" |
| DHH_AGE_C_X_CCC_151_cat1_C | interaction between "Centered Age dummy variable" and "Centered dummy stroke variable: Yes" |
| DHH_AGE_C_X_CCC_280_cat1_C | interaction between "Centered Age dummy variable" and "Centered Dummy mood disorder variable: Yes" |
| DHH_AGE_C_X_HWTGBMI_der_C | interaction between "Centered Age dummy variable" and "Centered Derived BMI" |
| DHHGAGE_cont_C_X_smoke_simple_cat1_C | interaction between "Centered Age dummy variable" and "Simple smoking status centered dummy variable: Current smoker" |
| DHHGAGE_cont_C_X_smoke_simple_cat2_C | interaction between "Centered Age dummy variable" and "Simple smoking status centered dummy variable: Former daily smoker quit less than 5 years or former occasional smoker" |
| DHHGAGE_cont_C_X_smoke_simple_cat3_C | interaction between "Centered Age dummy variable" and "Simple smoking status centered dummy variable: Former daily smoker quit >5 years" |

## intermediate
Variables that are used to derive other variables.
| Variable Name | Variable Description |
|---|---|
| ADL_01 | Help preparing meals |
| ADL_02 | Help appointments/errands |
| ADL_03 | Help housework |
| ADL_04 | Help personal care |
| ADL_05 | Help move inside house |
| ADL_der | Derived help tasks |
| ADL_score_5 | ADL score  |
| DHH_AGE | Continous age |
| DHH_AGE_C | Centered continous age variable |
| DHH_AGE_C_rcs1 | Centered age first RCS variable |
| DHH_AGE_C_rcs2 | Centered age second RCS variable |
| DHH_AGE_C_rcs3 | Centered age third RCS variable |
| DHH_AGE_C_rcs4 | Centered age fourth RCS variable |
| DHH_SEX | Sex |
| DHH_SEX_cat1 | Dummry sex variable: Male |
| DHH_SEX_cat1_c | Centered dummy sex variable: Male |
| DHH_MS | Marital status |
| DHH_MS_cat2 | Marital status dummy variable: Common-law |
| DHH_MS_cat2_C | Marital status centered dummy variable: Common-law |
| DHH_MS_cat3 | Marital status dummy variable: Widow/sep/div |
| DHH_MS_cat3_C | Marital status centered dummy variable: Widow/sep/div |
| DHH_MS_cat4 | Marital status dummy variable: Single/Never mar. |
| DHH_MS_cat4_C | Marital status centered dummy variable: Single/Never mar. |
| ALCDTTM | Drinker type (last 12 months) |
| ALCDTTM_cat1 | Drinker type dummy variable: Regular |
| ALCDTTM_cat1_C | Centered drinker type dummy variable: Regular |
| ALCDTTM_cat2 | Drinker type dummy variable: Occasional |
| ALCDTTM_cat2_C | Centered drinker type dummy variable: Occasional |
| ALC_1 | Any alcohol- 12 months |
| ALC_005 | Any alcohol- Lifetime |
| ALW_1 | Any alcohol past week |
| ALW_2A1 | # of drinks - Sunday |
| ALW_2A2 | # of drinks - Monday |
| ALW_2A3 | # of drinks - Tuesday |
| ALW_2A4 | # of drinks - Wednesday |
| ALW_2A5 | # of drinks - Thursday |
| ALW_2A6 | # of drinks - Friday |
| ALW_2A7 | # of drinks - Saturday |
| binge_drinker | Binge Drinker |
| ALWDVLTR_der | long term drinking risk |
| ALWDVLTR_der_cat1 | Dummy long term drinking risk variable: Yes |
| ALWDVLTR_der_cat1_C | Centered dummy long term drinking risk: Yes |
| ALWDVSTR_der | Short term drinking risk |
| ALWDVSTR_der_cat1 | Dummy short term drinking risk variable: Yes |
| ALWDVSTR_der_cat1_C | Centered dummy short term drinking risk: Yes |
| ALWDWKY | Drinks last week |
| CCC_031 | Asthma |
| CCC_031_cat1 | Dummy asthma variable: Yes |
| CCC_031_cat1_c | Centered dummy asthma variable: Yes |
| CCC_061 | Back Problems |
| CCC_061_cat1 | Dummy back problems variable: Yes |
| CCC_061_cat1_c | Centered dummy back problems variable: Yes |
| CCC_071 | Hypertension |
| CCC_071_cat1 | Dummy hypertension variable: Yes |
| CCC_071_cat1_c | Centered dummy hypertension variable: Yes |
| CCC_091 | COPD/Emphysema/Bronchitis |
| CCC_091_cat1 | Dummy COPD/Emphysema/Bronchitis variable: Yes |
| CCC_091_cat1_c | Centered dummy COPD/Emphysema/Bronchitis variable: Yes |
| CCC_121 | Heart Disease |
| CCC_121_cat1 | Dummy Heart Disease variable: Yes |
| CCC_121_cat1_c | Centered dummy heart disease variable: Yes |
| CCC_131 | Active Cancer |
| CCC_131_cat1 | Dummy active cancervariable: Yes |
| CCC_131_cat1_c | Centered dummy active cancer variable: Yes |
| CCC_151 | Stroke |
| CCC_151_cat1 | Dummy stroke variable: Yes |
| CCC_151_cat1_c | Centered dummy stroke variable: Yes |
| CCC_280 | Mood disorder |
| CCC_280_cat1 | Dummy mood disordervariable: Yes |
| CCC_280_cat1_c | Centered dummy mood disorder variable: Yes |
| CCC_290 | Anxiety Disorder |
| CCC_290_cat1 | Dummy anxiety disorder variable: Yes |
| CCC_290_cat1_c | Centered dummy anxiety disorder variable: Yes |
| resp_condition | Respiratory condition |
| EDUDR04 | Highest level education |
| EDUDR03 | Highest level education |
| EDU_1 | Highest grade completed |
| EDU_2 | Graduated from high school |
| EDU_3 | Any other education |
| EDU_4A | Highest Degree completed  |
| EDU_der | Highest level education |
| EDU_der_cat1 | Dummy highest level of education: Less than high school |
| EDU_der_cat2 | Dummy highest level of education: High school graduate |
| EDU_der_cat3 | Dummy highest level of education: Some post-secondary education |
| FSCDHFS2 | HC food security |
| FSCDHFS_cat1 | HC food security dummy variable: Mod. Food insec |
| FSCDHFS_cat1_C | HC food security centered dummy variable: Mod. Food insec |
| FSCDHFS_cat2 | HC food security dummy variable: Sev. Food insec |
| FSCDHFS_cat2_C | HC food security centered dummy variable: Sev. Food insec |
| GEN_01 | Self-perceived health |
| GEN_01_cat2 | Self-perceived health dummy variable: Very good |
| GEN_01_cat3 | Self-perceived health dummy variable: Good |
| GEN_01_cat4 | Self-perceived health dummy variable: Fair |
| GEN_01_cat5 | Self-perceived health dummy variable: Poor |
| GEN_01_cat2_C | Self-perceived health centered dummy variable: Very good |
| GEN_01_cat3_C | Self-perceived health centered dummy variable: Good |
| GEN_01_cat4_C | Self-perceived health centered dummy variable: Fair |
| GEN_01_cat5_C | Self-perceived health centered dummy variable: Poor |
| GEN_02B | Self-perceived mental health |
| GEN_02B_cat2 | Self-perceived mental health dummy variable: Very good |
| GEN_02B_cat3 | Self-perceived mental health dummy variable: Good |
| GEN_02B_cat4 | Self-perceived mental health dummy variable: Fair |
| GEN_02B_cat5 | Self-perceived mental health dummy variable: Poor |
| GEN_02B_cat2_C | Self-perceived mental health centered dummy variable: Very good |
| GEN_02B_cat3_C | Self-perceived mental health centered dummy variable: Good |
| GEN_02B_cat4_C | Self-perceived mental health centered dummy variable: Fair |
| GEN_02B_cat5_C | Self-perceived mental health centered dummy variable: Poor |
| GEN_07 | Self-perceived life stress |
| GEN_07_cat2 | Self-perceived life stress dummy variable: Not very stressful |
| GEN_07_cat3 | Self-perceived life stress dummy variable: A bit stressful |
| GEN_07_cat4 | Self-perceived life stress dummy variable: Quite a bit stressful |
| GEN_07_cat5 | Self-perceived life stress dummy variable: Extremely stressful |
| GEN_07_cat2_C | Self-perceived life stress centered dummy variable: Not very stressful |
| GEN_07_cat3_C | Self-perceived life stress centered dummy variable: A bit stressful |
| GEN_07_cat4_C | Self-perceived life stress centered dummy variable: Quite a bit stressful |
| GEN_07_cat5_C | Self-perceived life stress centered dummy variable: Extremely stressful |
| GEN_10 | Sense of belonging |
| GEN_10_cat2 | Dummy sense of belonging: Somewhat strong |
| GEN_10_cat3 | Dummy sense of belonging: Somwhat weak |
| GEN_10_cat4 | Dummy sense of belonging: Very weak |
| GEN_10_cat2_C | Centered dummy sense of belonging: somewhat strong |
| GEN_10_cat3_C | Centered dummy sense of belonging: Somwhat weak |
| GEN_10_cat4_C | Centered dummy sense of belonging: Very weak |
| HWTDHTM | Height |
| HWTDWTK | Weight |
| HWTDBMI_der | Derived BMI |
| HUPDPAD | HUI Pain |
| HUPDPAD_cat2 | Dummy HUI pain: pain prevents no activities |
| HUPDPAD_cat3 | Dummy HUI pain: pain prevents a few activities |
| HUPDPAD_cat4 | Dummy HUI pain: pain prevents some activities |
| HUPDPAD_cat5 | Dummy HUI pain: pain prevents most activities |
| HUPDPAD_cat2_C | Centered ummy HUI pain: pain prevents no activities |
| HUPDPAD_cat3_C | Centered Dummy HUI pain: pain prevents a few activities |
| HUPDPAD_cat4_C | Centered Dummy HUI pain: pain prevents some activities |
| HUPDPAD_cat5_C | Centered Dummy HUI pain: pain prevents most activities |
| INCDRCA | Household income distribution |
| INCDRCA_cat2 | Dummy household income distribution, decile 2 |
| INCDRCA_cat3 | Dummy household income distribution, decile 3 |
| INCDRCA_cat2_C | Centered Dummy household income distribution, decile 2 |
| INCDRCA_cat3_C | Centered Dummy household income distribution, decile 3 |
| IDGFLA | Illicit drug use-ever |
| IDGFLA_cat1 | Dummy illicit drug use-ever variable: Yes |
| IDGFLA_cat1_C | Centered ummy illicit drug use-ever variable: Yes |
| IDGFLAC | Illicit drug use- ever (exclude one time cannabis) |
| IDGFLAC_cat1 | Dummy Illicit drug use- ever (exclude one time cannabis): Yes |
| IDGFLAC_cat1_C | Centered dummy Illicit drug use- ever (exclude one time cannabis): Yes |
| IDGFYA | Illicit drug use- 12 mo |
| IDGFYA_cat1 | Dummy illicit drug use- 12 mo: Yes |
| IDGFYA_cat1_C | Centered dummy Illicit drug use- 12 mo: Yes |
| IDGFYAC | Illicit drug use- 12 (excluding one time cannabis) |
| IDGFYAC_cat1 | Dummy illicit drug use- 12 (excluding one time cannabis): Yes |
| IDGFYAC_cat1_C | Centered dummy illicit drug use- 12 (excluding one time cannabis): Yes |
| SDCGCGT | Ethnicity |
| SDCGCGT_cat2 | Dummy variable for ethnicity: Non-white |
| SDCGCGT_cat2_C | Centered dummy ethnicity variable: Non-white |
| SMK_01A | In lifetime, smoked 100 or more cigarettes |
| SMK_05B | # of cigarettes smoked daily - occasional smoker |
| SMK_05C | Number of days - smoked 1 cigarette or more (occ. smoker) |
| SMK_09A_B | When did you stop smoking daily - former daily |
| SMK_204 | # of cigarettes smoked daily - daily smoker |
| SMK_208 | # of cigarettes smoke each day - former daily |
| SMKDSTY | Smoking status |
| SMKG09C | Years since stopped smoking daily - former daily |
| SMKG203_cont | Age started to smoke daily - daily smoker (G) |
| SMK207 | Age started to smoke daily - former daily smoker |
| smoke_simple | Simple smoking status |
| time_quit_smoking | Time since quit |
| SurveyCycle | Survey Year |
| SurveyCycle_cat1 | Dummied survey year: 2013-2014 |
| SurveyCycle_cat2 | Dummied survey year: 2015-2016 |
| SurveyCycle_cat1_C | Cnetered Dummied survey year: 2013-2014 |
| SurveyCycle_cat2_C | Centered Dummied survey year: 2015-2016 |

## imputation-variable
Variables used in the multiple imputation process.
| Variable Name | Variable Description |
|---|---|
| age_X_arthritis | Age and arthritis interaction |
| age_X_bowel_disorder | Age and bowel disorder interaction |
| age_X_cancer | Age and cancer interaction |
| age_X_diabetes | Age and diabetes interaction |
| age_X_drinker_type | Age and drinker type interaction |
| age_X_drinks_last_week | Age and # of drinks last week interaction |
| age_X_education | Age and education level interaction |
| age_X_hbp | Age and high blood pressure interaction |
| age_X_heart_disease | age and heart disease interaction |
| age_X_mood_disorder | age and mood disorder interaction |
| age_X_COPD | Age and COPD interaction |
| age_X_smoking_status | Age and smoking status interaction |
| age_X_stroke | Age and stroke interaction |
| age_X_anxiety | Age and anxiety disorder interaction |
| age_X_number_conditions | Age and number of conditions interation |
| DHH_AGE | Continous age |
| DHH_SEX | Sex |
| ALCDTTM | Drinker type (last 12 months) |
| ALWDVLTR_der | long term drinking risk |
| ALWDVSTR_der | Short term drinking risk |
| CCC_031 | Asthma |
| CCC_051 | Arthritis/Rheumatism |
| CCC_061 | Back Problems |
| CCC_071 | Hypertension |
| CCC_091 | COPD/Emphysema/Bronchitis |
| CCC_101 | Diabetes |
| CCC_121 | Heart Disease |
| CCC_131 | Active Cancer |
| CCC_151 | Stroke |
| CCC_171 | Bowel disorder |
| EDU_der | Highest level education |
| GEN_01 | Self-perceived health |
| GEN_02B | Self-perceived mental health |
| GEN_09 | Self-perceived work stress |
| GEN_10 | Sense of belonging |
| HWTDBMI_der | Derived BMI |
| DHH_OWN | Home ownership |
| DHHDHSZ | Household Size |
| INCDHH | Household income |
| INCDRCA | Household income distribution |
| number_conditions | Number of conditions |
| SDCGCGT | Ethnicity |
| SurveyCycle | Survey Year |

## table-1-a
Variables to be included in descriptive Table 1.
| Variable Name | Variable Description |
|---|---|
| DHHGAGE | Age |
| DHH_MS | Marital status |
| GEN_10 | Sense of belonging|
| GEN_
| SDCGCGT | Ethnicity |

## weight
Survey sampling weights.
| Variable Name | Variable Description |
|---|---|
| WTS_i | Weight |
