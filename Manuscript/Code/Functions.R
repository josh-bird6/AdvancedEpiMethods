#Script for functions

library(tidyverse)
library(nhanesA)
library(table1)

#Creating function to read in all the mortality data
#Downloaded directly from the CDC website: https://ftp.cdc.gov/pub/Health_Statistics/NCHS/datalinkage/linked_mortality/
readfun <- function(df) {
  read_fwf(
    paste0('Data/NHANES_', df, '_MORT_2019_PUBLIC.dat'),
    col_types = 'iiiiiiii',
    fwf_cols(
      SEQN = c(1, 6),
      eligstat = c(15, 15),
      mortstat = c(16, 16),
      ucod_leading = c(17, 19),
      diabetes = c(20, 20),
      hyperten = c(21, 21),
      permth_int = c(43, 45),
      permth_exm = c(46, 48)
    ),
    na = c('', '.')
  )
}
######################################################
#Function for extracting data ONLY FOR FIRST 5 WAVES OF TIME SERIES (2013/14 to 2011/12)

extractfun <- function(lab, demo, weight, year, mortdat){
  left_join(nhanes(lab), nhanes(demo), by = 'SEQN') %>% 
    rename(Weight = weight,
           PFOA = LBXPFOA,
           PFOS = LBXPFOS,
           PFNA = LBXPFNA,
           PFHxS = LBXPFHS,
           Gender = RIAGENDR,
           Age = RIDAGEYR,
           Ethnicity = RIDRETH1,
           Education = DMDEDUC2,
           Pregnancy = RIDEXPRG,
           PseudoPSU = SDMVPSU,
           PseudoStratum = SDMVSTRA) %>% 
    mutate(Year = year) %>% 
    select(Year, SEQN, PFOA, PFOS, PFNA, PFHxS, Gender, Age, Ethnicity, Education, Pregnancy, Weight, PseudoPSU, PseudoStratum) %>% 
    left_join(mortdat, by = 'SEQN')
}
######################################################
#2013/14 is a faff - see Base_wranglingR.R script

#2015/16 and 2017/18 are more straightforward, but require a seaprate function (because of course is not formatted the exact same)

extractfun_latter <- function(lab, demo, year, mortdata) {
  
  left_join(nhanes(lab), nhanes(demo), by = 'SEQN') %>% 
    mutate(PFOA = LBXNFOA + LBXBFOA,
           PFOS = LBXNFOS + LBXMFOS,
           Year = year) %>% 
    rename(Weight = WTSB2YR,
           PFNA = LBXPFNA,
           PFHxS = LBXPFHS,
           Gender = RIAGENDR,
           Age = RIDAGEYR,
           Ethnicity = RIDRETH1,
           Education = DMDEDUC2,
           Pregnancy = RIDEXPRG,
           PseudoPSU = SDMVPSU,
           PseudoStratum = SDMVSTRA) %>% 
    select(Year, SEQN, PFOA, PFOS, PFNA, PFHxS, Gender, Age, Ethnicity, Education, Pregnancy, Weight, PseudoPSU, PseudoStratum) %>% 
    left_join(mortdata, by = 'SEQN') 
}
