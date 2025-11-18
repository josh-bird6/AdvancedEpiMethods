#MICE ANALYSIS

#First creating a function to fit regression model for EACH IMPUTATION
multivariate_MICE <- function(exp, outcome) {
  with(design_analytic, 
    svycoxph(as.formula(
    paste(
      "Surv(permth_int, ",
      outcome,
      ") ~",
      exp,
      "+ Year + rms::rcs(Age, 4) + Gender + Ethnicity + EducationHighest + Povertytoincomeratio + Occupation2 + Diet + Diabetes + BMI_class + Hypertension + ChronicKidneyDisease + Hyperlipidemia"
    ))
  ))
}

#Next, a function for formatting results
pool_and_format_results <- function(fit_mira) {
  # 'MIcombine' applies Rubin's Rules to pool the estimates from each imputed dataset.
  pooled <- mitools::MIcombine(fit_mira)
  
  # Exponentiate the coefficients and confidence intervals to get HRs.
  HR_estimates <- exp(coef(pooled))
  ci_estimates <- exp(confint(pooled))
  
  # Create and return a final data frame, rounding the results for clarity and adding asterisk for p < 0.05.
  results_df <- data.frame(
    HR = round(HR_estimates, 2),
    `2.5%` = round(ci_estimates[, 1], 2),
    `97.5%` = round(ci_estimates[, 2], 2)
  ) %>% 
    mutate(`HR (95% CI)` = paste0(HR, " (", X2.5., ", ", X97.5., ")"),
           sig = case_when(X2.5. > 1 & X97.5. > 1 | X2.5. < 1 & X97.5. < 1 ~ "*",
                                     T ~ "")) %>% 
    rename(lower = X2.5.,
           upper = X97.5.) %>% 
    slice(1) %>%
    rownames_to_column('Characteristic') 
  
  return(results_df)
}

##################################################################
#Model 1: scaled exposures, main outcome

bind_rows(
  pool_and_format_results(multivariate_MICE('PFOA_scaled', "MAINOUTCOME")),
  
  pool_and_format_results(multivariate_MICE('PFOS_scaled', "MAINOUTCOME")),
  
  pool_and_format_results(multivariate_MICE('PFNA_scaled', "MAINOUTCOME")),
  
  pool_and_format_results(multivariate_MICE('PFHxS_scaled', "MAINOUTCOME"))
  ) %>% 
  gt() %>% 
  tab_footnote(
    "Adjusted for: survey cycle, age (with restricted cubic spline), ethnicity, gender, education, diabetes, diet, employment status, poverty to income ratio, BMI, hypertension, chronic kidney disease and hyperlipidemia"
  ) %>% 
  tab_header(
    "Adjusted estimates from survey-featured Cox regression of PFAS exposure on non-communicable disease mortality: NHANES cycles 2003/04 to 2017/18"
  ) %>% 
  gtsave('outputs/Table/REGRESSIONTABLES/Scaled/adjusted_NCD_MICE.docx')

##################################################################
#Model 2: scaled exposures, secondary outcome

bind_rows(
  pool_and_format_results(multivariate_MICE('PFOA_scaled', "Allcausemortality")),
  
  pool_and_format_results(multivariate_MICE('PFOS_scaled', "Allcausemortality")),
  
  pool_and_format_results(multivariate_MICE('PFNA_scaled', "Allcausemortality")),
  
  pool_and_format_results(multivariate_MICE('PFHxS_scaled', "Allcausemortality"))
) %>% 
  gt() %>% 
  tab_footnote(
    "Adjusted for: survey cycle, age (with restricted cubic spline), ethnicity, gender, education, diabetes, diet, employment status, poverty to income ratio, BMI, hypertension, chronic kidney disease and hyperlipidemia"
  ) %>% 
  tab_header(
    "Adjusted estimates from survey-featured Cox regression of PFAS exposure on all-cause mortality: NHANES cycles 2003/04 to 2017/18"
  ) %>% 
  gtsave('outputs/Table/REGRESSIONTABLES/Scaled/adjusted_ACM_MICE.docx')

######################################################
######################################################
######################################################
######################################################
######################################################
######################################################

multivariate_MICE_interaction <- function(exp, outcome, PFAS1, PFAS2, PFAS3) {
  with(design_analytic, 
       svycoxph(as.formula(
         paste(
           "Surv(permth_int, ",
           outcome,
           ") ~",
           exp,
           "+ Year + rms::rcs(Age, 4) + Gender + Ethnicity + EducationHighest + Povertytoincomeratio + Occupation2 + Diet + Diabetes + BMI_class + Hypertension + ChronicKidneyDisease + Hyperlipidemia +",
           exp, "*", PFAS1, "+",  exp, "*", PFAS2, "+",  exp, "*", PFAS3
         ))
       ))
}

######################################################
#TABLE 1: Scaled exposures, primary outcome

bind_rows(
  pool_and_format_results(multivariate_MICE_interaction('PFOA_scaled', "MAINOUTCOME", "PFOS_scaled", "PFNA_scaled", "PFHxS_scaled")),
  
  pool_and_format_results(multivariate_MICE_interaction('PFOS_scaled', "MAINOUTCOME", "PFOA_scaled", "PFNA_scaled", "PFHxS_scaled")),
  
  pool_and_format_results(multivariate_MICE_interaction('PFNA_scaled', "MAINOUTCOME", "PFOA_scaled", "PFOS_scaled", "PFHxS_scaled")),
  
  pool_and_format_results(multivariate_MICE_interaction('PFHxS_scaled', "MAINOUTCOME", "PFOA_scaled", "PFOS_scaled", "PFNA_scaled"))
) %>% 
  gt() %>% 
  tab_footnote(
    "Adjusted for: survey cycle, age (with restricted cubic spline), ethnicity, gender, education, diabetes, diet, employment status, poverty to income ratio, BMI, hypertension, chronic kidney disease and hyperlipidemia"
  ) %>% 
  tab_footnote(
    "Includes PFAS*PFAS interaction terms"
  ) %>% 
  tab_header(
    "Adjusted estimates from survey-featured Cox regression of PFAS exposure on non-communicable disease mortality: NHANES cycles 2003/04 to 2017/18"
  ) %>% 
  gtsave('outputs/Table/REGRESSIONTABLES/Scaled/adjusted_interaction_NCD_MICE.docx')
  
######################################################
#TABLE 2: Scaled exposures, secondary outcome

bind_rows(
  pool_and_format_results(multivariate_MICE_interaction('PFOA_scaled', "Allcausemortality", "PFOS_scaled", "PFNA_scaled", "PFHxS_scaled")),
  
  pool_and_format_results(multivariate_MICE_interaction('PFOS_scaled', "Allcausemortality", "PFOA_scaled", "PFNA_scaled", "PFHxS_scaled")),
  
  pool_and_format_results(multivariate_MICE_interaction('PFNA_scaled', "Allcausemortality", "PFOA_scaled", "PFOS_scaled", "PFHxS_scaled")),
  
  pool_and_format_results(multivariate_MICE_interaction('PFHxS_scaled', "Allcausemortality", "PFOA_scaled", "PFOS_scaled", "PFNA_scaled"))
) %>% 
  gt() %>% 
  tab_footnote(
    "Adjusted for: survey cycle, age (with restricted cubic spline), ethnicity, gender, education, diabetes, diet, employment status, poverty to income ratio, BMI, hypertension, chronic kidney disease and hyperlipidemia"
  ) %>% 
  tab_footnote(
    "Includes PFAS*PFAS interaction terms"
  ) %>% 
  tab_header(
    "Adjusted estimates from survey-featured Cox regression of PFAS exposure on all-cause mortality: NHANES cycles 2003/04 to 2017/18"
  ) %>% 
  gtsave('outputs/Table/REGRESSIONTABLES/Scaled/adjusted_interaction_ACM_MICE.docx')
