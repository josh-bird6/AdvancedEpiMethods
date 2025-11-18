#COMPLETE CASE ANALYSIS

#Function for extracting data from Cox models

multivariate <- function(exp, outcome, designobj){
  
  formula <- as.formula(paste("Surv(permth_int, ", outcome, ") ~ ", exp, "+ Year + rms::rcs(Age, 4) + Gender + Ethnicity + EducationHighest + Povertytoincomeratio + Occupation2 + Diet + Diabetes + BMI_class + Hypertension + ChronicKidneyDisease + Hyperlipidemia"))
  
   svycoxph(formula, design = designobj) %>% 
     tbl_regression(exponentiate = T,
                    include = exp) 
  
}
###################################################
#Model 1: scaled exposures, main outcome


tbl_stack(tbls = list(
  multivariate('PFOA_scaled', "MAINOUTCOME", nhanes_design),
  multivariate('PFOS_scaled', "MAINOUTCOME", nhanes_design),
  multivariate('PFNA_scaled', "MAINOUTCOME", nhanes_design),
  multivariate('PFHxS_scaled', "MAINOUTCOME", nhanes_design)
)) %>% 
  as_gt() %>%
  tab_footnote(
    "Adjusted for: survey cycle, age (with restricted cubic spline), ethnicity, gender, education, diabetes, diet, employment status, poverty to income ratio, BMI, hypertension, chronic kidney disease and hyperlipidemia"
  ) %>% 
  tab_header(
    "Adjusted estimates from survey-featured Cox regression of PFAS exposure on non-communicable disease mortality: NHANES cycles 2003/04 to 2017/18"
  ) %>% 
  gtsave('outputs/Table/REGRESSIONTABLES/Scaled/adjusted_NCD.docx')


######################################################
#Model 2: Scaled exposures, secondary outcome

tbl_stack(tbls = list(
  multivariate('PFOA_scaled', "Allcausemortality", nhanes_design),
  multivariate('PFOS_scaled', "Allcausemortality", nhanes_design),
  multivariate('PFNA_scaled', "Allcausemortality", nhanes_design),
  multivariate('PFHxS_scaled', "Allcausemortality", nhanes_design)
))%>% 
  as_gt() %>%
  tab_footnote(
    "Adjusted for: survey cycle, age (with restricted cubic spline), ethnicity, gender, education, diabetes, diet, employment status, poverty to income ratio, BMI, hypertension, chronic kidney disease and hyperlipidemia"
  ) %>% 
  tab_header(
    "Adjusted estimates from survey-featured Cox regression of PFAS exposure on all-cause mortality: NHANES cycles 2003/04 to 2017/18"
  ) %>% 
  gtsave('outputs/Table/REGRESSIONTABLES/Scaled/adjusted_ACM.docx')


######################################################
######################################################
######################################################
######################################################
######################################################
######################################################

#Function for extracting data from Cox models - WITH INTERACTION TERMS

multivariate_interaction <- function(exp, outcome, designobj, PFAS1, PFAS2, PFAS3){
  
  formula <- as.formula(paste("Surv(permth_int, ", outcome, ") ~ ", exp, " + Year + rms::rcs(Age, 4) + Gender + Ethnicity + EducationHighest + Povertytoincomeratio + Occupation2 + Diet + Diabetes + BMI_class + Hypertension + ChronicKidneyDisease + Hyperlipidemia +",
                              exp, "*", PFAS1, "+",  exp, "*", PFAS2, "+",  exp, "*", PFAS3))
  
  svycoxph(formula, design = designobj) %>% 
    tbl_regression(exponentiate = T,
                   include = exp) 
  
}


######################################################
#TABLE 1: Scaled exposures, primary outcome

tbl_stack(
  tbls = list(
    multivariate_interaction('PFOA_scaled', "MAINOUTCOME", nhanes_design, "PFOS_scaled", "PFNA_scaled", "PFHxS_scaled"),
    
    multivariate_interaction('PFOS_scaled', "MAINOUTCOME", nhanes_design, "PFOA_scaled", "PFNA_scaled", "PFHxS_scaled"),
    
    multivariate_interaction('PFNA_scaled', "MAINOUTCOME", nhanes_design, "PFOA_scaled", "PFOS_scaled", "PFHxS_scaled"),
    
    multivariate_interaction('PFHxS_scaled', "MAINOUTCOME", nhanes_design, "PFOA_scaled", "PFOS_scaled", "PFNA_scaled")
  )
) %>% 
  as_gt() %>%
  tab_footnote(
    "Adjusted for: survey cycle, age (with restricted cubic spline), ethnicity, gender, education, diabetes, diet, employment status, poverty to income ratio, smoking status, BMI, hypertension and heart disease"
  ) %>% 
  tab_footnote(
    "Includes PFAS*PFAS interaction terms"
    ) %>% 
  tab_header(
    "Adjusted estimates from survey-featured Cox regression of PFAS exposure on non-communicable disease mortality: NHANES cycles 2003/04 to 2017/18"
  ) %>% 
  gtsave('outputs/Table/REGRESSIONTABLES/Scaled/adjusted_interaction_NCD.docx')

######################################################
#TABLE 2: Scaled exposures, secondary outcome

tbl_stack(
  tbls = list(
    multivariate_interaction('PFOA_scaled', "Allcausemortality", nhanes_design, "PFOS_scaled", "PFNA_scaled", "PFHxS_scaled"),
    
    multivariate_interaction('PFOS_scaled', "Allcausemortality", nhanes_design, "PFOA_scaled", "PFNA_scaled", "PFHxS_scaled"),
    
    multivariate_interaction('PFNA_scaled', "Allcausemortality", nhanes_design, "PFOA_scaled", "PFOS_scaled", "PFHxS_scaled"),
    
    multivariate_interaction('PFHxS_scaled', "Allcausemortality", nhanes_design, "PFOA_scaled", "PFOS_scaled", "PFNA_scaled")
  )
) %>% 
  as_gt() %>%
  tab_footnote(
    "Adjusted for: survey cycle, age (with restricted cubic spline), ethnicity, gender, education, diabetes, diet, employment status and poverty to income ratio"
  ) %>% 
  tab_footnote(
    "Includes PFAS*PFAS interaction terms"
  ) %>% 
  tab_header(
    "Adjusted estimates from survey-featured Cox regression of PFAS exposure on all-cause mortality: NHANES cycles 2003/04 to 2017/18"
  ) %>% 
  gtsave('outputs/Table/REGRESSIONTABLES/Scaled/adjusted_interaction_ACM.docx')
