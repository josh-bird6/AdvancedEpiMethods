
#Be sure to run Functions.R script first

#################################################
#Reading in data by year
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
`2003-04` <- extractfun('L24PFC_C', 'DEMO_C', 'WTSA2YR', "2003-04", data_mort_0304) 

`2005-06` <- extractfun('PFC_D', 'DEMO_D', 'WTSA2YR', '2005-06', data_mort_0506)

`2007-08` <- extractfun('PFC_E', 'DEMO_E', "WTSC2YR", '2007-08', data_mort_0708)

`2009-10` <- extractfun('PFC_F', 'DEMO_F', "WTSC2YR", '2009-10', data_mort_0910)

`2011-12` <- extractfun('PFC_G', 'DEMO_G', "WTSA2YR", '2011-12', data_mort_1112)

######################
######################
######################
######################
######################
######################
#2013/14 is where it gets very sketchy

#first we need to grab the PFAS data for the four chemicals from both datasets (2 from each)
#Then combine measurements for PFOA and PFOS ONLY

`2013-14` <- left_join(
  #First for PFOA and PFOS
  (nhanes('SSPFAS_H') %>% 
     mutate(PFOA = SSNPFOA + SSBPFOA,
            PFOS = SSNPFOS + SSMPFOS) %>% 
     select(SEQN, WTSSBH2Y, PFOA, PFOS)),
  #Then for PFNA, PFHxS
  (nhanes('PFAS_H') %>%
     rename(PFNA = LBXPFNA,
            PFHxS = LBXPFHS) %>% 
     select(SEQN, PFNA, PFHxS)),
  by = 'SEQN'
  ) %>% 
  #now joining sociodemographic data
  left_join(nhanes('DEMO_H'), by = 'SEQN') %>% 
  rename(Weight = WTSSBH2Y,
         Gender = RIAGENDR,
         Age = RIDAGEYR,
         Ethnicity = RIDRETH1,
         Education = DMDEDUC2,
         Pregnancy = RIDEXPRG,
         PseudoPSU = SDMVPSU,
         PseudoStratum = SDMVSTRA) %>% 
  mutate(Year = '2013-14') %>% 
  select(Year, SEQN, PFOA, PFOS, PFNA, PFHxS, Gender, Age, Ethnicity, Education, Pregnancy, Weight, PseudoPSU, PseudoStratum) %>% 
  left_join(data_mort_1314, by = 'SEQN')

######################
######################
######################
######################
######################
######################

#2015/16 and 2017/18

`2015-16` <- extractfun_latter('PFAS_I', 'DEMO_I', "2015-16", data_mort_1516)

`2017-18` <- extractfun_latter('PFAS_J', 'DEMO_J', "2017-18", data_mort_1718)


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
  ) %>% 
  #exclusion criteria - filtering out children
  #also filtering missing exposure/outcome data
  filter(Age >= 18,
         !is.na(PFOA),
         !is.na(mortstat)) %>% 
  #defining causes of death
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
    )
  )

#Creating tertiles to categorise each of four exposures into low, medium and high
FINAL_BASEDATASET <- FINAL_BASEDATASET %>%
  mutate(PFOA_tertile = cut(PFOA,
                            (quantile(FINAL_BASEDATASET$PFOA, c(0:3/3))),
                            include.lowest = T,
                            labels = c("Low", "Medium", "High")),
         PFOS_tertile = cut(PFOS,
                            (quantile(FINAL_BASEDATASET$PFOS, c(0:3/3))),
                            include.lowest = T,
                            labels = c("Low", "Medium", "High")),
         PFNA_tertile = cut(PFNA,
                            (quantile(FINAL_BASEDATASET$PFNA, c(0:3/3))),
                            include.lowest = T,
                            labels = c("Low", "Medium", "High")),
         PFHxS_tertile = cut(PFHxS,
                             (quantile(FINAL_BASEDATASET$PFHxS, c(0:3/3))),
                             include.lowest = T,
                             labels = c("Low", "Medium", "High"))
  ) %>% 
  select(Year, SEQN, PFOA, PFOA_tertile, PFOS, PFOS_tertile, PFNA, PFNA_tertile, PFHxS, PFHxS_tertile, everything())

#checking missingness
unlist(lapply(FINAL_BASEDATASET , function(x) sum(is.na(x))))/nrow(FINAL_BASEDATASET) 

#visualising
FINAL_BASEDATASET %>% 
  filter(!is.na(`Cause of death`)) %>% 
  group_by(`Cause of death`) %>% 
  summarise(total = n(),
            prop = (total/(table(FINAL_BASEDATASET$`Cause of death`) %>% 
                             sum()))*100) %>% 
  mutate(label = paste0(total, " (",round(prop,1), "%)")) %>% 
  ggplot(aes(x = reorder(`Cause of death`, prop), y = total)) +
  geom_col()+
  coord_flip() + 
  theme_bw() +
  geom_text(aes(label = label), hjust = -.1) +
  labs(x = "",
       title = 'Causes of death (N= 1,660)',
       y = 'Deaths') +
  scale_y_continuous(limits = c(0,600))

