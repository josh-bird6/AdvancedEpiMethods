#Creating tables

#EXPOSURE TABLE (from Functions.R)
#Extracts and formats exposure data for each analyte

write.csv(bind_rows(
  
  exposure_extraction_fun(FINAL_BASEDATASET_regression$PFOA, "PFOA"),
  
  exposure_extraction_fun(FINAL_BASEDATASET_regression$PFOS, "PFOS"),
  
  exposure_extraction_fun(FINAL_BASEDATASET_regression$PFNA, "PFNA"),
  
  exposure_extraction_fun(FINAL_BASEDATASET_regression$PFHxS, "PFHxS")
  
) %>% 
  remove_rownames(), 'Data/test.csv')
#############################################################

#Table 1, stratified by outcome (and including exposure information)
#First specifying variables of interest
varsofinterest <- c('PFOA', 'PFOS', 'PFNA', 'PFHxS', 'Year', "Age", "Gender", "Ethnicity", "EducationHighest", "Povertytoincomeratio", 'Occupation2', 'Diet', 'Diabetes', 'BMI_class', 'Hypertension', 'ChronicKidneyDisease', 'Hyperlipidemia')

svyTable1::svytable1(design = nhanes_design,
                     strata_var = "MAINOUTCOME",
                     table_vars = 'Age_category')



#UNWEIGHTED (these are counts)
write.csv(print(
  CreateTableOne(
    vars = varsofinterest,
    strata = 'MAINOUTCOME',
    data = FINAL_BASEDATASET_regression,
    test = F,
    addOverall = T,
    includeNA = T
  ),
  showAllLevels = F,
  print = T,
  format = "f",
  nonnormal = c('PFOA', "PFOS", 'PFNA', 'PFHxS')
),
'outputs/Table/Outcomestable_COUNTS.csv')

#WEIGHTED (these are %)
write.csv(
  print(
    svyCreateTableOne(
      vars = varsofinterest,
      strata = 'MAINOUTCOME',
      data = nhanes_design,
      test = F,
      includeNA = T,
      addOverall = T
    ),
    print = T,
    format = 'p',
    nonnormal = c('PFOA', "PFOS", 'PFNA', 'PFHxS')
  ),
  'outputs/Table/Outcomestable_PROP.csv'
)



#Survey weighted median and standard error of median - NOT TO USE

#PFOA
# svyquantile(
#   x = ~FINAL_BASEDATASET$PFOA,
#   design = nhanes_design,
#   quantiles = 0.5,
#   ci = T)
# 
# #PFOS
# svyquantile(
#   x = ~FINAL_BASEDATASET$PFOS,
#   design = nhanes_design,
#   quantiles = 0.5,
#   ci = T
# )
# 
# #PFNA
# svyquantile(
#   x = ~FINAL_BASEDATASET$PFNA,
#   design = nhanes_design,
#   quantiles = 0.5,
#   ci = T
# )
# 
# #PFHxS
# svyquantile(
#   x = ~FINAL_BASEDATASET$PFHxS,
#   design = nhanes_design,
#   quantiles = 0.5,
#   ci = T
# )

#Tables for each exposure (that will be concatenated in Excel)
#PFOA
# write.csv(
#   print(CreateTableOne(vars = varsofinterest, strata = 'PFOA', data = FINAL_BASEDATASET, test = F, addOverall = T), 
#         showAllLevels = F,
#         print = F), 'outputs/Table/PFOA.csv')
# 
# #PFOS
# write.csv(
#   print(CreateTableOne(vars = varsofinterest, strata = 'PFOS_tertile', data = FINAL_BASEDATASET, test = F, addOverall = T), 
#         showAllLevels = F,
#         print = F), 'outputs/Table/PFOS.csv')
# 
# #PFNA
# write.csv(
#   print(CreateTableOne(vars = varsofinterest, strata = 'PFNA_tertile', data = FINAL_BASEDATASET, test = F, addOverall = T), 
#         showAllLevels = F,
#         print = F), 'outputs/Table/PFNA.csv')
# 
# #PFHxS
# write.csv(
#   print(CreateTableOne(vars = varsofinterest, strata = 'PFHxS_tertile', data = FINAL_BASEDATASET, test = F, addOverall = T), 
#         showAllLevels = F,
#         print = F), 'outputs/Table/PFHxS.csv')

############################################################

