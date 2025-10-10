
#Be sure to run Functions.R script first

#################################################
#Reading in mortality data by year
data_mort_0304 <- readfun('2003_2004')
data_mort_0506 <- readfun('2005_2006')
data_mort_0708 <- readfun('2007_2008')
data_mort_0910 <- readfun('2009_2010')
data_mort_1112 <- readfun('2011_2012')
data_mort_1314 <- readfun('2013_2014')
data_mort_1516 <- readfun('2015_2016')
data_mort_1718 <- readfun('2017_2018')

#################################################
#Extracting data for for 2003-04 through 2011-12
`2003-04` <- extractfun('L24PFC_C', 'DEMO_C', 'DIQ_C', 'BMX_C', 'SMQ_C', 'BPQ_C', 'RHQ_C', 'MCQ_C', 'L40_C', "OCQ_C", "DR1TOT_C", 'WTSA2YR', "2003-04", data_mort_0304) 

`2005-06` <- extractfun('PFC_D', 'DEMO_D', 'DIQ_D', 'BMX_D', 'SMQ_D', 'BPQ_D', 'RHQ_D', 'MCQ_D', 'BIOPRO_D', "OCQ_D", 'DR1TOT_D', 'WTSA2YR', '2005-06', data_mort_0506)

`2007-08` <- extractfun('PFC_E', 'DEMO_E', 'DIQ_E', 'BMX_E', 'SMQ_E', 'BPQ_E', 'RHQ_E', 'MCQ_E', 'BIOPRO_E', "OCQ_E", 'DR1TOT_F', "WTSC2YR", '2007-08', data_mort_0708)

`2009-10` <- extractfun('PFC_F', 'DEMO_F', 'DIQ_F', 'BMX_F', 'SMQ_F', 'BPQ_F', 'RHQ_F', 'MCQ_F', 'BIOPRO_F', "OCQ_F", 'DR1TOT_G', "WTSC2YR", '2009-10', data_mort_0910)

`2011-12` <- extractfun('PFC_G', 'DEMO_G', 'DIQ_G', 'BMX_G', 'SMQ_G', 'BPQ_G', 'RHQ_G', 'MCQ_G', 'BIOPRO_G', "OCQ_G", 'DR1TOT_H', "WTSA2YR", '2011-12', data_mort_1112)

######################
######################
######################
######################
######################
######################
#2013/14 is where it gets very sketchy

#first we need to grab the PFAS data for the four chemicals, which are located in two different survey modules
#Then combine measurements for *PFOA and PFOS ONLY*

`2013-14` <- left_join(
  #First for PFOA and PFOS
  (nhanes('SSPFAS_H') %>% 
     mutate(PFOA = SSNPFOA + SSBPFOA,
            PFOS = SSNPFOS + SSMPFOS) %>% 
     select(SEQN, WTSSBH2Y, PFOA, PFOS)),
  #Then for PFNA, PFHxS, which are single measurements (do not require combining)
  (nhanes('PFAS_H') %>%
     rename(PFNA = LBXPFNA,
            PFHxS = LBXPFHS) %>% 
     select(SEQN, PFNA, PFHxS)),
  by = 'SEQN'
) %>% 
  left_join(nhanes('DEMO_H'), by = 'SEQN') %>%        #joining sociodemographic data
  left_join(nhanes('DIQ_H'), by = 'SEQN') %>%         #joining diabetes data (questionnaire)
  left_join(nhanes('BMX_H'), by = 'SEQN') %>%         #joining BMI data (exam)
  left_join(nhanes('SMQ_H'), by = 'SEQN') %>%         #joining smoking data (exam)
  left_join(nhanes('BPQ_H'), by = 'SEQN') %>%         #joining hypertension data (questionnaire)
  left_join(nhanes('RHQ_H'), by = 'SEQN') %>%         #joining menstrual cycle data (questionnaire)
  left_join(nhanes('MCQ_H'), by = 'SEQN') %>%         #joining heart disease data (questionnaire)
  left_join(nhanes('BIOPRO_H'), by = 'SEQN') %>%      #joining kidney data (laboratory data)
  left_join(nhanes('OCQ_H'), by = 'SEQN') %>%       #joining occupation data (exam)
  left_join(nhanes('DR1TOT_H'), by = 'SEQN') %>%             #joining diet information (exam)
  rename(Weight = WTSSBH2Y,
         Gender = RIAGENDR,
         Age = RIDAGEYR,
         Ethnicity = RIDRETH1,
         Education = DMDEDUC2,
         Income = INDFMPIR,
         Diabetes = DIQ010,
         BMI = BMXBMI, 
         Smoking = SMQ040,
         Hypertension = BPQ020,
         Menopause = RHQ031,
         Heartdisease = MCQ160C,
         Kidney = LBXSCR,
         Occupation = OCD150,
         Diet = DRD360,
         Pregnancy = RIDEXPRG,
         PseudoPSU = SDMVPSU,
         PseudoStratum = SDMVSTRA) %>% 
  mutate(Year = '2013-14') %>% 
  select(Year, SEQN, PFOA, PFOS, PFNA, PFHxS, Gender, Age, Ethnicity, Education, Income, Diabetes, BMI, Smoking, Hypertension, Menopause, Heartdisease, Kidney, Pregnancy, Occupation, Diet,  Weight, PseudoPSU, PseudoStratum) %>% 
  left_join(data_mort_1314, by = 'SEQN')
######################
######################
######################
######################
######################
######################

#2015/16 and 2017/18

`2015-16` <- extractfun_latter('PFAS_I', 'DEMO_I', 'DIQ_I', 'BMX_I', 'SMQ_I', 'BPQ_I', 'RHQ_I', 'MCQ_I', 'BIOPRO_I', "OCQ_I", 'DR1TOT_I', "2015-16", data_mort_1516)

`2017-18` <- extractfun_latter('PFAS_J', 'DEMO_J', 'DIQ_J', 'BMX_J', 'SMQ_J', 'BPQ_J', 'RHQ_J', 'MCQ_J', 'BIOPRO_J', "OCQ_J", "DR1TOT_J", "2017-18", data_mort_1718)


##################################################
 #NOW THE FINAL MERGE!

FINAL_BASEDATASET <- bind_rows(
  `2003-04`,
  `2005-06`,
  `2007-08`,
  `2009-10`,
  `2011-12`,
  `2013-14`,
  `2015-16`,
  `2017-18`
  ) %>%                           #(n = 17,851)
  #defining causes of death (i.e. outcome)
  mutate(
    `Cause of death` = case_when(
      ucod_leading == 1 ~ "Diseases of heart",
      ucod_leading == 2 ~ "Malignant neoplasms",
      ucod_leading == 3 ~ "Chronic lower respiratory diseases",
      ucod_leading == 4 ~ "Accidents (unintentional injuries)",
      ucod_leading == 5 ~ "Cerebrovascular diseases",
      ucod_leading == 6 ~ "Alzheimer’s disease",
      ucod_leading == 7 ~ "Diabetes mellitus",
      ucod_leading == 8 ~ "Influenza and pneumonia",
      ucod_leading == 9 ~ "Nephritis, nephrotic syndrome and nephrosis",
      ucod_leading == 10 ~ "All other causes"
    ),
    MAINOUTCOME = case_when(ucod_leading == 1 | ucod_leading == 2 | ucod_leading == 3 | ucod_leading == 5 | ucod_leading == 6 | ucod_leading == 7 | ucod_leading == 9 ~ 1,
                            T ~ 0),
    #Calibrating weight based on number of survey cycles
    Weight_pool = Weight/8,
    #Creating follow-up time variable
    fu_time = (Age * 12) + permth_int,
    
    #NOW FOR SOME WRANGLING
    #First we are going to transform the exposure scales. They are heavily right-skewed, so going to log transform and then scale (mean = 0, SD = 1)
    PFOA_scaled = as.numeric(scale(log(PFOA))),
    PFOS_scaled = as.numeric(scale(log(PFOS))),
    PFNA_scaled = as.numeric(scale(log(PFNA))),
    PFHxS_scaled = as.numeric(scale(log(PFHxS))),
    #Miscellaneous variable formatting
    Ethnicity = case_when(Ethnicity == 'Other Hispanic' | Ethnicity == 'Mexican American' ~ 'Hispanic',
                          T ~ Ethnicity), 
    Education = str_to_lower(Education),
    Education = case_when(
      Education %in% c(
        "9-11th grade (includes 12th grade with no diploma)",
        "less than 9th grade"
      ) ~ 'Some high school or below',
      Education %in% c(
        'high school graduate/ged or equivalent',
        'high school grad/ged or equivalent'
      ) ~ 'High School/GED or equivalent',
      T ~ Education
    ), 
    Education = str_to_title(Education),
    BMI_class = case_when(BMI < 25 ~ 'Normal weight',
                          BMI >= 25 & BMI < 30 ~ 'Overweight',
                          BMI >= 30 ~ 'Obese'), 
    Menopause2 = case_when(Menopause == 'No' | Gender == 'Female' & Age >= 60 ~ "Yes", 
                           T ~ "No"), 
    Occupation2 = case_when(Occupation == 'Working at a job or business,' ~ 'Employed', Occupation == 'With a job or business but not at work,' | Occupation == "Looking for work, or" | Occupation == 'Not working at a job or business?' ~ 'Not employed',
                            T ~ Occupation), 
           #Finally, creating a binary flag for all cause mortality (to be explored in secondary analysis)
    Allcausemortality = case_when(!is.na(`Cause of death`) ~ 1,
                                         T ~ 0)
    ) %>% 
  #some people do not have weights applied, so they are removed
  filter(!is.na(Weight_pool)) %>% 
  rename(`Povertytoincomeratio` = Income) %>% 
  select(MAINOUTCOME, Year, SEQN, PFOA, PFOA_scaled, PFOS, PFOS_scaled, PFNA, PFNA_scaled, PFHxS, PFHxS_scaled, everything(), -diabetes, -hyperten, -eligstat, -Menopause)

#checking missingness
unlist(lapply(FINAL_BASEDATASET , function(x) sum(is.na(x))))/nrow(FINAL_BASEDATASET) 



