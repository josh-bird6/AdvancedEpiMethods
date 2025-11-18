#visualising cause of death
  bind_rows(
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
                            T ~ 0)) %>% 
  filter(!is.na(`Cause of death`),
         MAINOUTCOME == 1,
         Age >= 18,               #filtering out all observations under 18y/o (n = 14,007),        
         !is.na(PFOA),            #filtering out all observations with missing exposure data (individuals missing one measurement are missing them all, n = 12,968) 
         !is.na(mortstat)) %>% 
  group_by(`Cause of death`) %>% 
  summarise(total = n(),
            prop = (total/sum(FINAL_BASEDATASET_regression$MAINOUTCOME))*100) %>% 
  mutate(label = paste0(total, " (",round(prop,1), "%)")) %>% 
  ggplot(aes(x = reorder(`Cause of death`, prop), y = total)) +
  geom_col()+
  coord_flip() + 
  theme_bw() +
  geom_text(aes(label = label, fontface = 'bold'), hjust = -.1, size = 5) +
  labs(x = "",
       caption = 'n = 1,116',
       y = 'Deaths') +
  scale_y_continuous(limits = c(0,600)) +
  scale_x_discrete(labels = scales::label_wrap(22)) +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13.5),
        plot.caption = element_text(size = 13.5))

##################################################################

#Ad hoc chart of publications by year
# readxl::read_excel('Data/PubMed_Timeline_Results_by_Year.xlsx') %>% 
read_csv('Data/PubMed_Timeline_Results_by_Year.csv') %>% 
  ggplot(aes(x = Year, y = Count)) +
  geom_col() +
  theme_bw() +
  labs(y = 'Number of peer-reviewed publications') +
  scale_x_continuous(breaks = seq(2000,2025, 2)) +
    theme(legend.position = 'bottom',
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 13),
          legend.text = element_text(size = 12))
  
  ggsave("outputs/Vis/Timeseries2.png", dpi = 300, height = 5, width = 8, units = 'in')
  



#Population-level PFAS concentrations have definitely been declining over time
  bind_rows(
    `2003-04`,
    `2005-06`,
    `2007-08`,
    `2009-10`,
    `2011-12`,
    `2013-14`,
    `2015-16`,
    `2017-18`
  ) %>% 
    filter(!is.na(PFOA)) %>% 
    group_by(Year) %>% 
    summarise(
  PFOA = median(PFOA),
  PFOS = median(PFOS),
  PFNA = median(PFNA),
  PFHxS = median(PFHxS)
) %>% 
  pivot_longer(2:5, names_to = 'Analyte', values_to = 'median concentration') %>% 
  ggplot(aes(x = Year, y = `median concentration`, group = Analyte, color = Analyte)) +
  geom_point(size = 3) +
  geom_line(size = 1.5) +
  labs(y = 'Median PFAS concentration (µg/mL)',
       x = 'Survey year') +
    theme_bw()+
  theme(legend.position = 'bottom',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 12))
  
  ggsave("outputs/Vis/Timeseries.png", dpi = 300, height = 5, width = 8, units = 'in')
