#STATISTICAL ANALYSIS

#CRUDE ANALYSIS, ONE FOR EACH EXPOSURE
#These models are fit and then concentenated, with some additional formatting of the table


crudetable <- bind_rows(
#PFOA
(svycoxph(
  Surv(fu_time, MAINOUTCOME) ~ PFOA_scaled,
  design = nhanes_design) %>% 
  broom::tidy(conf.int = T, exponentiate = T)),
#PFOS
(svycoxph(
  Surv(fu_time, MAINOUTCOME) ~ PFOS_scaled,
  design = nhanes_design) %>% 
  broom::tidy(conf.int = T, exponentiate = T)),
#PFNA
(svycoxph(
  Surv(fu_time, MAINOUTCOME) ~ PFNA_scaled,
  design = nhanes_design) %>% 
  broom::tidy(conf.int = T, exponentiate = T)),
#PFHxS
(svycoxph(
  Surv(fu_time, MAINOUTCOME) ~ PFHxS_scaled,
  design = nhanes_design) %>% 
  broom::tidy(conf.int = T, exponentiate = T))
) %>% 
  mutate(`HR (95%CI)` = paste0(round(estimate,3), " (", round(conf.low,2), ", ", round(conf.high,2), ")"),
         p.value = round(p.value, 3),
         robust.se = round(robust.se, 3),
         term = str_replace(term, "_scaled", " ")) %>% 
  rename(`P-value` = p.value,
         `PFAS subtype` = term,
         `Robust SE` = robust.se) %>% 
  select(`PFAS subtype`, `HR (95%CI)`, `Robust SE`, `P-value`) %>% 
  flextable() %>% 
  # hline(i = c(2,4,6)) %>% 
  width(j = c(1,2), width = 2)

save_as_docx(crudetable, path = "outputs/Table/crudetable_continuous.docx")


###########################################################

#ADJUSTED for year, age, ethnicity and gender
#adding restricted cubic spline for age (four knots, as per literature)

adjusted_table <- bind_rows(
#PFOA
  (svycoxph(
    Surv(fu_time, MAINOUTCOME) ~ PFOA_scaled + Year + rcs(Age, 4) + Ethnicity + Gender,
    design = nhanes_design
  ) %>%
    broom::tidy(conf.int = T, exponentiate = T) %>% 
    slice(1)
),

#PFOS
(svycoxph(
    Surv(fu_time, MAINOUTCOME) ~ PFOS_scaled + Year + rcs(Age, 4) + Ethnicity + Gender,
    design = nhanes_design
  ) %>%
    broom::tidy(conf.int = T, exponentiate = T) %>% 
    slice(1)
) ,

#PFNA
(
  svycoxph(
    Surv(fu_time, MAINOUTCOME) ~ PFNA_scaled + Year + rcs(Age, 4) + Ethnicity + Gender,
    design = nhanes_design
  ) %>%
    broom::tidy(conf.int = T, exponentiate = T) %>% 
    slice(1)
) ,

#PFHxS
(
  svycoxph(
    Surv(fu_time, MAINOUTCOME) ~ PFHxS_scaled + Year + rcs(Age, 4) + Ethnicity + Gender,
    design = nhanes_design
  ) %>%
    broom::tidy(conf.int = T, exponentiate = T) %>% 
    slice(1)
) 
) %>% 
  mutate(`HR (95%CI)` = paste0(round(estimate,2), " (", round(conf.low,2), ", ", round(conf.high,2), ")"),
         p.value = round(p.value, 3),
         robust.se = round(robust.se, 3),
         term = str_replace(term, "_scaled", " ")) %>% 
  rename(`P-value` = p.value,
         `PFAS subtype` = term,
         `Robust SE` = robust.se) %>% 
  select(`PFAS subtype`, `HR (95%CI)`, `Robust SE`, `P-value`) %>% 
  flextable() %>% 
  # hline(i = c(2,4,6)) %>% 
  width(j = c(1,2), width = 2) %>% 
  add_footer_row(values = "Adjusted for survey year, age, sex and ethnicity", colwidths = 4)
  
save_as_docx(adjusted_table, path = "outputs/Table/adjustedtable_continuous.docx")
