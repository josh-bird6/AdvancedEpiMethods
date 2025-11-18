


#missingness
unlist(lapply(FINAL_BASEDATASET_regression, function(x) sum(is.na(x))))/nrow(FINAL_BASEDATASET_regression) 
  
#assessing aatterns of missingness
md.pattern(FINAL_BASEDATASET_regression)

#Barplot (missingness is negligible, save for PIR and Diet)
FINAL_BASEDATASET_regression %>% 
  select(-PFHxS_scaled, -PFOA_scaled, -PFNA_scaled, -PFOS_scaled, -Age_category, -Age_binary, -permth_int, -mortstat, -SEQN, -Hypertension, -PseudoStratum, -PseudoPSU, -Weight_pool) %>%
  rename(`All cause mortality` = Allcausemortality,
         `Main outcome` = MAINOUTCOME,
         `Occupation status` = Occupation2,
         BMI = BMI_class,
         `Poverty to income ratio` = Povertytoincomeratio,
         `Chronic kidney disease` = ChronicKidneyDisease) %>% 
  select(`Poverty to income ratio`, Diet) %>% 
  DataExplorer::plot_missing() +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13.5),
        plot.caption = element_text(size = 13.5)) +
  labs(x = "") +
  scale_y_continuous(limits = c(0, 2100),
                     labels = scales::comma)


###########################################
#imputation

#Excluding SEQN from analysis and also calculating follow-up time using Nelson-Aalen
FINAL_BASEDATASET_regression_MICE <- FINAL_BASEDATASET_regression %>%
         mutate(EducationHighest = as.factor(EducationHighest),
                Occupation2 = as.factor(Occupation2),
                BMI_class = as.factor(BMI_class),
                cumhaz = nelsonaalen(., permth_int, MAINOUTCOME)) %>% 
  select(-mortstat, -SEQN, -permth_int)


#Reproducibility
set.seed(11)

#Running imputation, creating 22 datasets (based on overall missingness %) and iterating 5 times
imputation <- mice(FINAL_BASEDATASET_regression_MICE, m = 22, maxit = 5)

#verifying that convergence was achieved -- this is very important!
#No visible pattern
plot(imputation)
#PIR is the only numeric variable
bwplot(imputation, Povertytoincomeratio ~ .imp)

#Long form dataset
impdata <- complete(imputation, action = "long")

#INclude original follow up time in the complete long form imputed dataset
impdata$permth_int <- rep(FINAL_BASEDATASET_regression$permth_int, imputation$m) 
impdata$eligible <- 1

#Confirming missingness is GONE
DataExplorer::plot_missing(subset(impdata, subset = .imp == 1))

###########################################################################
#CREATING SURVEY DESIGN OBJECT

#First grabbing all the people who are ineligible
dat_ineligible <- FINAL_BASEDATASET %>% 
  filter(miss == 1) %>% 
  select(Weight_pool, PseudoPSU, PseudoStratum, SEQN)

#Replicating the ineligible dataset 22 times (corresponding to m in mice call above)
ineligible_list <- lapply(1:22, function(i) {
  df <- dat_ineligible
  df$.imp <- i
  return(df)
})
ineligible_stacked <- do.call(rbind, ineligible_list)

#Finding which columns exist in imputed data but not in ineligible data
cols_to_add <- setdiff(names(impdata), names(ineligible_stacked))

#Add these missing columns to the ineligible data, filling with NA.
ineligible_stacked[, cols_to_add] <- NA

#Set eligiblity criteria for this group to nil
ineligible_stacked$eligible <- 0

#Force the ineligible data to have the exact same column names and order as the imputed data
ineligible_final <- ineligible_stacked[, names(impdata)]

#Combine
impdata_FULL <- rbind(impdata, ineligible_final)

########################################
#Create survey design object
design_full <- svydesign(
  id = ~ PseudoPSU,
  strata = ~ PseudoStratum,
  weights = ~ Weight_pool,
  data = mitools::imputationList(split(impdata_FULL, impdata_FULL$.imp)),
  nest = T
)

#Subsetting by eligible people
design_analytic <- subset(design_full, eligible == 1)

