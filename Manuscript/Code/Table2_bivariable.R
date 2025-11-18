#STATISTICAL ANALYSIS

#First defining a function to fit regression models and create tables

univariate <- function(exp, outcome){
  
  formula <- as.formula(paste("Surv(permth_int, ", outcome, ") ~ ", exp))
  
  svycoxph(formula, design = nhanes_design) %>% 
    tbl_regression(exponentiate = T)
  
}
######################################################
#TABLE 1: Scaled exposures, main outcome


tbl_stack(
  tbls = list(
    #Exposures
    univariate('PFOA_scaled', "MAINOUTCOME"),
    univariate('PFOS_scaled', "MAINOUTCOME"),
    univariate('PFNA_scaled', "MAINOUTCOME"),
    univariate('PFHxS_scaled', "MAINOUTCOME"),
    
    #Covariates
    univariate('Year', "MAINOUTCOME"),
    univariate('Age', "MAINOUTCOME"),
    univariate('Gender', "MAINOUTCOME"),
    univariate('Ethnicity', "MAINOUTCOME"),
    univariate('EducationHighest', "MAINOUTCOME"),
    univariate('Povertytoincomeratio', "MAINOUTCOME"),
    univariate('Occupation2', "MAINOUTCOME"),
    univariate('Diet', "MAINOUTCOME"),
    univariate('Diabetes', "MAINOUTCOME"),
    univariate('BMI_class', "MAINOUTCOME"),
    univariate('Hypertension', "MAINOUTCOME"),
    univariate('ChronicKidneyDisease', "MAINOUTCOME"),
    univariate('Hyperlipidemia', 'MAINOUTCOME')
  )
) %>% 
  modify_column_hide(columns = p.value) %>% 
  as_gt() %>%
  tab_footnote(
    "NOTE: exposures are log-transformed and z-score standardised"
  ) %>% 
  tab_footnote(
    "Estimates are survey-feature modified"
  ) %>% 
  tab_header(
    "Unadjusted bivariate estimates from survey-featured Cox regression of each covariate on non-communicable disease mortality: NHANES cycles 2003/04 to 2017/18"
  ) %>% 
  gtsave('outputs/Table/REGRESSIONTABLES/Scaled/univariate_NCD.docx')

######################################################
#TABLE 2: Scaled exposures, secondary outcome

tbl_stack(
  tbls = list(
    #Exposures
    univariate('PFOA_scaled', "Allcausemortality"),
    univariate('PFOS_scaled', "Allcausemortality"),
    univariate('PFNA_scaled', "Allcausemortality"),
    univariate('PFHxS_scaled', "Allcausemortality"),
    
    #Covariates
    univariate('Year', "Allcausemortality"),
    univariate('Age', "Allcausemortality"),
    univariate('Gender', "Allcausemortality"),
    univariate('Ethnicity', "Allcausemortality"),
    univariate('EducationHighest', "Allcausemortality"),
    univariate('Povertytoincomeratio', "Allcausemortality"),
    univariate('Occupation2', "Allcausemortality"),
    univariate('Diet', "Allcausemortality"),
    univariate('Diabetes', "Allcausemortality"),
    univariate('BMI_class', "Allcausemortality"),
    univariate('Hypertension', "Allcausemortality"),
    univariate('ChronicKidneyDisease', "Allcausemortality"),
    univariate('Hyperlipidemia', 'Allcausemortality')
  )
) %>% 
  modify_column_hide(columns = p.value) %>% 
  as_gt() %>%
  tab_footnote(
    "NOTE: exposures are log-transformed and z-score standardised"
  ) %>% 
  tab_footnote(
    "Estimates are survey-feature modified"
  ) %>% 
  tab_header(
    "Unadjusted bivariate estimates from survey-featured Cox regression of each covariate on all-cause mortality: NHANES cycles 2003/04 to 2017/18"
  ) %>% 
  gtsave('outputs/Table/REGRESSIONTABLES/Scaled/univariate_ACM.docx')
