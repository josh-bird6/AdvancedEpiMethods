####FOREST PLOTS

#First for a scaled variable

#Function for extracting non-MICE models

extraction_function_models <- function(df, category, spec_outcome){
  df %>% 
    as.data.frame() %>% 
    separate_wider_delim(`**95% CI**`, ", ", names = c("lower", "upper")) %>% 
    rename(Characteristic = `**Characteristic**`,
           HR = `**HR**`) %>% 
    mutate(cat = category,
           outcome = spec_outcome,           
           HR = as.numeric(HR),
           lower = as.numeric(lower),
           upper = as.numeric(upper),
           sig = case_when(`**p-value**` <.05 ~ "*",
                           T ~ "")) %>% 
    select(-5)
}

#Function for extracting MICE models
extraction_function_models_MICE <- function(df, category, spec_outcome) {
  df %>% 
    tibble() %>% 

    mutate(cat = category,
           outcome = spec_outcome) %>% 
    select(Characteristic, HR, lower, upper, cat, outcome, sig)  
} 

#######

#Extracting data from scaled exposure
scaled_PLOT <- bind_rows(
  #FIRST - MAIN OUTCOME
  
  #univariate models
  extraction_function_models(univariate('PFOA_scaled', "MAINOUTCOME"), 'Univariate', 'NCD mortality'),
  extraction_function_models(univariate('PFOS_scaled', "MAINOUTCOME"), 'Univariate', 'NCD mortality'),
  extraction_function_models(univariate('PFNA_scaled', "MAINOUTCOME"), 'Univariate', 'NCD mortality'),
  extraction_function_models(univariate('PFHxS_scaled', "MAINOUTCOME"), 'Univariate', 'NCD mortality'),
  
  #multivariable adjusted 
  extraction_function_models(multivariate('PFOA_scaled', "MAINOUTCOME", nhanes_design), 'Multivariate adjusted complete case', 'NCD mortality'),
  extraction_function_models(multivariate('PFOS_scaled', "MAINOUTCOME", nhanes_design), 'Multivariate adjusted complete case', 'NCD mortality'),
  extraction_function_models(multivariate('PFNA_scaled', "MAINOUTCOME", nhanes_design), 'Multivariate adjusted complete case', 'NCD mortality'),
  extraction_function_models(multivariate('PFHxS_scaled', "MAINOUTCOME", nhanes_design), 'Multivariate adjusted complete case', 'NCD mortality'),
  
  #multivariable interaction
  extraction_function_models(multivariate_interaction('PFOA_scaled', "MAINOUTCOME", nhanes_design, "PFOS_scaled", "PFNA_scaled", "PFHxS_scaled"), 'Multivariate interaction complete case', 'NCD mortality'),
  extraction_function_models(multivariate_interaction('PFOS_scaled', "MAINOUTCOME", nhanes_design, "PFOA_scaled", "PFNA_scaled", "PFHxS_scaled"), 'Multivariate interaction complete case', 'NCD mortality'),
  extraction_function_models(multivariate_interaction('PFNA_scaled', "MAINOUTCOME", nhanes_design, "PFOA_scaled", "PFOS_scaled", "PFHxS_scaled"), 'Multivariate interaction complete case', 'NCD mortality'),
  extraction_function_models(multivariate_interaction('PFHxS_scaled', "MAINOUTCOME", nhanes_design, "PFOS_scaled", "PFNA_scaled", "PFNA_scaled"), 'Multivariate interaction complete case', 'NCD mortality'),
  
  #multivariable adjusted MICE
  extraction_function_models_MICE(pool_and_format_results(multivariate_MICE('PFOA_scaled', "MAINOUTCOME")), 'Multivariate adjusted MICE', 'NCD mortality'),
  extraction_function_models_MICE(pool_and_format_results(multivariate_MICE('PFOS_scaled', "MAINOUTCOME")), 'Multivariate adjusted MICE', 'NCD mortality'),
  extraction_function_models_MICE(pool_and_format_results(multivariate_MICE('PFNA_scaled', "MAINOUTCOME")), 'Multivariate adjusted MICE', 'NCD mortality'),
  extraction_function_models_MICE(pool_and_format_results(multivariate_MICE('PFHxS_scaled', "MAINOUTCOME")), 'Multivariate adjusted MICE', 'NCD mortality'),
  
  #multivariable interaction MICE
  
  extraction_function_models_MICE(pool_and_format_results(multivariate_MICE_interaction('PFOA_scaled', "MAINOUTCOME", "PFOS_scaled", "PFNA_scaled", "PFHxS_scaled")), 'Multivariate interaction MICE', 'NCD mortality'),
  extraction_function_models_MICE(pool_and_format_results(multivariate_MICE_interaction('PFOS_scaled', "MAINOUTCOME", "PFOA_scaled", "PFNA_scaled", "PFHxS_scaled")), 'Multivariate interaction MICE', 'NCD mortality'),
  extraction_function_models_MICE(pool_and_format_results(multivariate_MICE_interaction('PFNA_scaled', "MAINOUTCOME", "PFOA_scaled", "PFOS_scaled", "PFHxS_scaled")), 'Multivariate interaction MICE', 'NCD mortality'),
  extraction_function_models_MICE(pool_and_format_results(multivariate_MICE_interaction('PFHxS_scaled', "MAINOUTCOME", "PFOA_scaled", "PFOS_scaled", "PFNA_scaled")), 'Multivariate interaction MICE', 'NCD mortality'),
  
  #############################################################################################
  #NOW SECONDARY OUTCOME - ALL CAUSE MORTALITY
  extraction_function_models(univariate('PFOA_scaled', "Allcausemortality"), 'Univariate', 'All cause mortality'),
  extraction_function_models(univariate('PFOS_scaled', "Allcausemortality"), 'Univariate', 'All cause mortality'),
  extraction_function_models(univariate('PFNA_scaled', "Allcausemortality"), 'Univariate', 'All cause mortality'),
  extraction_function_models(univariate('PFHxS_scaled', "Allcausemortality"), 'Univariate', 'All cause mortality'),
  
  #multivariable adjusted 
  extraction_function_models(multivariate('PFOA_scaled', "Allcausemortality", nhanes_design), 'Multivariate adjusted complete case', 'All cause mortality'),
  extraction_function_models(multivariate('PFOS_scaled', "Allcausemortality", nhanes_design), 'Multivariate adjusted complete case', 'All cause mortality'),
  extraction_function_models(multivariate('PFNA_scaled', "Allcausemortality", nhanes_design), 'Multivariate adjusted complete case', 'All cause mortality'),
  extraction_function_models(multivariate('PFHxS_scaled', "Allcausemortality", nhanes_design), 'Multivariate adjusted complete case', 'All cause mortality'),
  
  #multivariable interaction
  extraction_function_models(multivariate_interaction('PFOA_scaled', "Allcausemortality", nhanes_design, "PFOS_scaled", "PFNA_scaled", "PFHxS_scaled"), 'Multivariate interaction complete case', 'All cause mortality'),
  extraction_function_models(multivariate_interaction('PFOS_scaled', "Allcausemortality", nhanes_design, "PFOA_scaled", "PFNA_scaled", "PFHxS_scaled"), 'Multivariate interaction complete case', 'All cause mortality'),
  extraction_function_models(multivariate_interaction('PFNA_scaled', "Allcausemortality", nhanes_design, "PFOA_scaled", "PFOS_scaled", "PFHxS_scaled"), 'Multivariate interaction complete case', 'All cause mortality'),
  extraction_function_models(multivariate_interaction('PFHxS_scaled', "Allcausemortality", nhanes_design, "PFOS_scaled", "PFNA_scaled", "PFNA_scaled"), 'Multivariate interaction complete case', 'All cause mortality'),
  
  #multivariable adjusted MICE
  extraction_function_models_MICE(pool_and_format_results(multivariate_MICE('PFOA_scaled', "Allcausemortality")), 'Multivariate adjusted MICE', 'All cause mortality'),
  extraction_function_models_MICE(pool_and_format_results(multivariate_MICE('PFOS_scaled', "Allcausemortality")), 'Multivariate adjusted MICE', 'All cause mortality'),
  extraction_function_models_MICE(pool_and_format_results(multivariate_MICE('PFNA_scaled', "Allcausemortality")), 'Multivariate adjusted MICE', 'All cause mortality'),
  extraction_function_models_MICE(pool_and_format_results(multivariate_MICE('PFHxS_scaled', "Allcausemortality")), 'Multivariate adjusted MICE', 'All cause mortality'),
  
  #multivariable interaction MICE
  
  extraction_function_models_MICE(pool_and_format_results(multivariate_MICE_interaction('PFOA_scaled', "Allcausemortality", "PFOS_scaled", "PFNA_scaled", "PFHxS_scaled")), 'Multivariate interaction MICE', 'All cause mortality'),
  extraction_function_models_MICE(pool_and_format_results(multivariate_MICE_interaction('PFOS_scaled', "Allcausemortality", "PFOA_scaled", "PFNA_scaled", "PFHxS_scaled")), 'Multivariate interaction MICE', 'All cause mortality'),
  extraction_function_models_MICE(pool_and_format_results(multivariate_MICE_interaction('PFNA_scaled', "Allcausemortality", "PFOA_scaled", "PFOS_scaled", "PFHxS_scaled")), 'Multivariate interaction MICE', 'All cause mortality'),
  extraction_function_models_MICE(pool_and_format_results(multivariate_MICE_interaction('PFHxS_scaled', "Allcausemortality", "PFOA_scaled", "PFOS_scaled", "PFNA_scaled")), 'Multivariate interaction MICE', 'All cause mortality')
  
) %>% 
  mutate(cat = as.factor(cat),
         cat = fct_relevel(cat, c('Multivariate interaction MICE',
                                  'Multivariate adjusted MICE',
                                  'Multivariate interaction complete case',
                                  'Multivariate adjusted complete case',
                                  "Univariate")),
         sig = case_when(Characteristic == "PFHxS_scaled"  & upper >1 & lower <1 ~ " ", 
                         T ~ sig),
         Characteristic = str_replace(Characteristic, "_scaled", ""),
         `Missingness treatment` = case_when(str_detect(cat, "MICE") ~ "MICE \n(n = 13,798)",
                                             str_detect(cat, 'complete case') ~ "Complete case \n(n = 10,630)",
                                             T ~ "Univariate analysis \n(n = 13,798)"),
         cat3 = case_when(str_detect(cat, "Multivariate adjusted") ~ "Multivariate adjusted",
                          T ~ "Multivariate adjusted + interaction")) %>% 
  filter(Characteristic == 'PFOA' | Characteristic == 'PFOS' | Characteristic == 'PFNA' | Characteristic == 'PFHxS')



#exporting
ggsave('outputs/Vis/NCD.png', 
       forestplot_fun(scaled_PLOT, 'NCD mortality'), 
       width = 8,
       height = 8,
       units = 'in',
       dpi = 300)

ggsave('outputs/Vis/ACM.png', 
       forestplot_fun(scaled_PLOT, 'All cause mortality'), 
       width = 8,
       height = 8,
       units = 'in',
       dpi = 300)
