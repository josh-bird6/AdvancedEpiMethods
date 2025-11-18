#Subgroup analysis -AGE

#Manually creating models for MICE
PFOA_subgroup <- mitools::MIcombine(with(
  design_analytic,
  svycoxph(
    Surv(permth_int, MAINOUTCOME) ~ PFOA_scaled + Year + Age_category * PFOA_scaled + Gender + Ethnicity + EducationHighest + Povertytoincomeratio + Occupation2 + Diet + Diabetes + BMI_class + Hypertension + ChronicKidneyDisease + Hyperlipidemia
  )
))

PFOS_subgroup <- mitools::MIcombine(with(
  design_analytic,
  svycoxph(
    Surv(permth_int, MAINOUTCOME) ~ PFOS_scaled + Year + Age_category * PFOS_scaled + Gender + Ethnicity + EducationHighest + Povertytoincomeratio + Occupation2 + Diet + Diabetes + BMI_class + Hypertension + ChronicKidneyDisease + Hyperlipidemia
  )
))

PFNA_subgroup <- mitools::MIcombine(with(
  design_analytic,
  svycoxph(
    Surv(permth_int, MAINOUTCOME) ~ PFNA_scaled + Year + Age_category * PFNA_scaled + Gender + Ethnicity + EducationHighest + Povertytoincomeratio + Occupation2 + Diet + Diabetes + BMI_class + Hypertension + ChronicKidneyDisease + Hyperlipidemia
  )
))

PFHxS_subgroup <- mitools::MIcombine(with(
  design_analytic,
  svycoxph(
    Surv(permth_int, MAINOUTCOME) ~ PFHxS_scaled + Year + Age_category * PFHxS_scaled + Gender + Ethnicity + EducationHighest + Povertytoincomeratio + Occupation2 + Diet + Diabetes + BMI_class + Hypertension + ChronicKidneyDisease + Hyperlipidemia
  )
))


################################################


subgroup_analysis <- bind_rows(
  
  #Complete case analysis
  bind_rows(
    
    #PFOA
    bind_cols((exp(coef(
      svycoxph(
        Surv(permth_int, MAINOUTCOME) ~ PFOA_scaled + SEQN + Year + PFOA_scaled *
          Age_category + Gender + Ethnicity + EducationHighest + Povertytoincomeratio + Occupation2 + Diet + Diabetes + BMI_class + Hypertension + ChronicKidneyDisease + Hyperlipidemia,
        design = nhanes_design
      )
    )) %>%
      data.frame() %>%
      rownames_to_column(var = "var")), exp(confint(
        svycoxph(
          Surv(permth_int, MAINOUTCOME) ~ PFOA_scaled + SEQN + Year + PFOA_scaled *
            Age_category + Gender + Ethnicity + EducationHighest + Povertytoincomeratio + Occupation2 + Diet + Diabetes + BMI_class + Hypertension + ChronicKidneyDisease + Hyperlipidemia,
          design = nhanes_design
        )
      )))%>% 
      slice(27:29),
    
    #PFOS
    bind_cols((exp(coef(
      svycoxph(
        Surv(permth_int, MAINOUTCOME) ~ PFOS_scaled + SEQN + Year + PFOS_scaled *
          Age_category + Gender + Ethnicity + EducationHighest + Povertytoincomeratio + Occupation2 + Diet + Diabetes + BMI_class + Hypertension + ChronicKidneyDisease + Hyperlipidemia,
        design = nhanes_design
      )
    )) %>%
      data.frame() %>%
      rownames_to_column(var = "var")), exp(confint(
        svycoxph(
          Surv(permth_int, MAINOUTCOME) ~ PFOS_scaled + SEQN + Year + PFOS_scaled *
            Age_category + Gender + Ethnicity + EducationHighest + Povertytoincomeratio + Occupation2 + Diet + Diabetes + BMI_class + Hypertension + ChronicKidneyDisease + Hyperlipidemia,
          design = nhanes_design
        )
      )))%>% 
      slice(27:29),
    
    #PFNA
    bind_cols((exp(coef(
      svycoxph(
        Surv(permth_int, MAINOUTCOME) ~ PFNA_scaled + SEQN + Year + PFNA_scaled *
          Age_category + Gender + Ethnicity + EducationHighest + Povertytoincomeratio + Occupation2 + Diet + Diabetes + BMI_class + Hypertension + ChronicKidneyDisease + Hyperlipidemia,
        design = nhanes_design
      )
    )) %>%
      data.frame() %>%
      rownames_to_column(var = "var")), exp(confint(
        svycoxph(
          Surv(permth_int, MAINOUTCOME) ~ PFNA_scaled + SEQN + Year + PFNA_scaled *
            Age_category + Gender + Ethnicity + EducationHighest + Povertytoincomeratio + Occupation2 + Diet + Diabetes + BMI_class + Hypertension + ChronicKidneyDisease + Hyperlipidemia,
          design = nhanes_design
        )
      )))%>% 
      slice(27:29),
    
    #PFHxS
    bind_cols((exp(coef(
      svycoxph(
        Surv(permth_int, MAINOUTCOME) ~ PFHxS_scaled + SEQN + Year + PFHxS_scaled *
          Age_category + Gender + Ethnicity + EducationHighest + Povertytoincomeratio + Occupation2 + Diet + Diabetes + BMI_class + Hypertension + ChronicKidneyDisease + Hyperlipidemia,
        design = nhanes_design
      )
    )) %>%
      data.frame() %>%
      rownames_to_column(var = "var")), exp(confint(
        svycoxph(
          Surv(permth_int, MAINOUTCOME) ~ PFHxS_scaled + SEQN + Year + PFHxS_scaled *
            Age_category + Gender + Ethnicity + EducationHighest + Povertytoincomeratio + Occupation2 + Diet + Diabetes + BMI_class + Hypertension + ChronicKidneyDisease + Hyperlipidemia,
          design = nhanes_design
        )
      )))%>% 
      slice(27:29)
  ) %>% 
    separate(var, into = c('PFAS', 'Characteristic'), sep = ":") %>% 
    mutate(PFAS = str_sub(PFAS, 1, -8),
           cat = 'Complete case') %>% 
    rename(HR = ".",
           lower = `2.5 %`,
           upper = `97.5 %`),
  
  #########################
  #MICE
    bind_rows(
    #PFOS
    bind_cols(exp(coef(PFOS_subgroup)) %>%
                data.frame(), 
              exp(confint(PFOS_subgroup)) %>%
                data.frame()) %>% 
      slice(26:28),
    
    #PFOA
    bind_cols(exp(coef(PFOA_subgroup)) %>%
                data.frame(), 
              exp(confint(PFOA_subgroup)) %>%
                data.frame()) %>% 
      slice(26:28),
    
    #PFNA
    bind_cols(exp(coef(PFNA_subgroup)) %>%
                data.frame(), 
              exp(confint(PFNA_subgroup)) %>%
                data.frame()) %>% 
      slice(26:28),
    
    #PFHxS
    bind_cols(exp(coef(PFHxS_subgroup)) %>%
                data.frame(), 
              exp(confint(PFHxS_subgroup)) %>%
                data.frame()) %>% 
      slice(26:28)
    
  ) %>% 
    rownames_to_column(var = 'var') %>% 
    separate(var, into = c('PFAS', 'Characteristic'), sep = ":") %>% 
    mutate(PFAS = str_sub(PFAS, 1, -8),
           cat = 'MICE') %>% 
    rename(HR = ".",
           lower = `X2.5..`,
           upper = `X97.5..`)
)

write_csv(subgroup_analysis, 'outputs/Table/REGRESSIONTABLES/Scaled/subgroup_analysis.csv')


################################################################
################################################################
################################################################
################################################################
################################################################
################################################################

#Sex subgroup analysis
#Note: This uses gender_subgroup rather than Gender (accounting for pre- and post-menopausal individuals)
bind_rows(

bind_cols((exp(coef(
  svycoxph(
    Surv(permth_int, MAINOUTCOME) ~ PFOA_scaled + SEQN + Year + PFOA_scaled * gender_subgroup +
    rms::rcs(Age, 4)  + Ethnicity + EducationHighest + Povertytoincomeratio + Occupation2 + Diet + Diabetes + BMI_class + Hypertension + ChronicKidneyDisease + Hyperlipidemia,
    design = nhanes_design
  )
)) %>%
  data.frame() %>%
  rownames_to_column(var = "var")), exp(confint(
    svycoxph(
      Surv(permth_int, MAINOUTCOME) ~ PFOA_scaled + SEQN + Year + PFOA_scaled * gender_subgroup + 
        rms::rcs(Age, 4)+ Ethnicity + EducationHighest + Povertytoincomeratio + Occupation2 + Diet + Diabetes + BMI_class + Hypertension + ChronicKidneyDisease + Hyperlipidemia,
      design = nhanes_design
    )
  ))) %>% 
  slice(28:29),

bind_cols((exp(coef(
  svycoxph(
    Surv(permth_int, MAINOUTCOME) ~ PFOS_scaled + SEQN + Year + PFOS_scaled * gender_subgroup +
      rms::rcs(Age, 4)  + Ethnicity + EducationHighest + Povertytoincomeratio + Occupation2 + Diet + Diabetes + BMI_class + Hypertension + ChronicKidneyDisease + Hyperlipidemia,
    design = nhanes_design
  )
)) %>%
  data.frame() %>%
  rownames_to_column(var = "var")), exp(confint(
    svycoxph(
      Surv(permth_int, MAINOUTCOME) ~ PFOS_scaled + SEQN + Year + PFOS_scaled * gender_subgroup + 
        rms::rcs(Age, 4)+ Ethnicity + EducationHighest + Povertytoincomeratio + Occupation2 + Diet + Diabetes + BMI_class + Hypertension + ChronicKidneyDisease + Hyperlipidemia,
      design = nhanes_design
    )
  )))%>% 
  slice(28:29),

bind_cols((exp(coef(
  svycoxph(
    Surv(permth_int, MAINOUTCOME) ~ PFNA_scaled + SEQN + Year + PFNA_scaled * gender_subgroup +
      rms::rcs(Age, 4)  + Ethnicity + EducationHighest + Povertytoincomeratio + Occupation2 + Diet + Diabetes + BMI_class + Hypertension + ChronicKidneyDisease + Hyperlipidemia,
    design = nhanes_design
  )
)) %>%
  data.frame() %>%
  rownames_to_column(var = "var")), exp(confint(
    svycoxph(
      Surv(permth_int, MAINOUTCOME) ~ PFNA_scaled + SEQN + Year + PFNA_scaled * gender_subgroup + 
        rms::rcs(Age, 4)+ Ethnicity + EducationHighest + Povertytoincomeratio + Occupation2 + Diet + Diabetes + BMI_class + Hypertension + ChronicKidneyDisease + Hyperlipidemia,
      design = nhanes_design
    )
  )))%>% 
  slice(28:29),

bind_cols((exp(coef(
  svycoxph(
    Surv(permth_int, MAINOUTCOME) ~ PFHxS_scaled + SEQN + Year + PFHxS_scaled * gender_subgroup+
      rms::rcs(Age, 4)  + Ethnicity + EducationHighest + Povertytoincomeratio + Occupation2 + Diet + Diabetes + BMI_class + Hypertension + ChronicKidneyDisease + Hyperlipidemia,
    design = nhanes_design
  )
)) %>%
  data.frame() %>%
  rownames_to_column(var = "var")), exp(confint(
    svycoxph(
      Surv(permth_int, MAINOUTCOME) ~ PFHxS_scaled + SEQN + Year + PFHxS_scaled * gender_subgroup + 
        rms::rcs(Age, 4)+ Ethnicity + EducationHighest + Povertytoincomeratio + Occupation2 + Diet + Diabetes + BMI_class + Hypertension + ChronicKidneyDisease + Hyperlipidemia,
      design = nhanes_design
    )
  )))%>% 
  slice(28:29),
) %>% 
  write_csv('outputs/Table/REGRESSIONTABLES/Scaled/subgroup_analysis_gender.csv')
