#Some initial visualisation for exploratory data purposes

#visualising cause of death
FINAL_BASEDATASET %>% 
  filter(!is.na(`Cause of death`),
         MAINOUTCOME == 1) %>% 
  group_by(`Cause of death`) %>% 
  summarise(total = n(),
            prop = (total/sum(FINAL_BASEDATASET$MAINOUTCOME))*100) %>% 
  mutate(label = paste0(total, " (",round(prop,1), "%)")) %>% 
  ggplot(aes(x = reorder(`Cause of death`, prop), y = total)) +
  geom_col()+
  coord_flip() + 
  theme_bw() +
  geom_text(aes(label = label), hjust = -.1) +
  labs(x = "",
       caption = 'n = 1,116',
       y = 'Deaths') +
  scale_y_continuous(limits = c(0,600)) +
  scale_x_discrete(labels = scales::label_wrap(22)) +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13.5))

##################################################################

#Ad hoc chart of publications by year
readxl::read_excel('Data/PubMed_Timeline_Results_by_Year.xlsx') %>% 
  ggplot(aes(x = Year, y = Count)) +
  geom_col() +
  theme_bw() +
  labs(y = 'Number of peer-reviewed publications') +
  scale_x_continuous(breaks = seq(2000,2025, 2)) +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13.5))

##################################################################
#Combined KM chart (along with creating new follow up time variable in years)


survfit2(Surv(time, MAINOUTCOME) ~ Gender, data = (FINAL_BASEDATASET %>%
                                                mutate(time = (Age * 12) + permth_int, time = time / 12))) %>%
  ggsurvfit() +
  labs(x = 'Follow-up time (years)', y = 'Survival probability') +
  theme(legend.position = 'none') +
  add_confidence_interval()



# km_vis <- function(chemical) {
#   
#   FINAL_BASEDATASET %>% 
#     mutate(time = (Age *12)+permth_int,
#            time = time/12) %>% 
#     select(chemical, time, MAINOUTCOME)
# }
# 
# #plotting using cowplot package
# plot_grid((survfit2(Surv(time, MAINOUTCOME) ~ 1, data = km_vis('PFOA')) %>% 
#              ggsurvfit() +
#              labs(x = '',
#                   y = 'Survival probability',
#                   title = "PFOA") +
#              theme(legend.position = 'none') +
#              add_confidence_interval()),
#           (survfit2(Surv(time, MAINOUTCOME) ~ 1, data = km_vis('PFOS')) %>% 
#              ggsurvfit() +
#              labs(x = '',
#                   y = '',
#                   title = "PFOS") +
#              theme(legend.position = 'none') +
#              add_confidence_interval()),
#           (survfit2(Surv(time, MAINOUTCOME) ~ 1, data = km_vis('PFNA')) %>% 
#              ggsurvfit() +
#              labs(x = 'Years',
#                   y = 'Survival probability',
#                   title = "PFNA") +
#              add_confidence_interval()),
#           (survfit2(Surv(time, MAINOUTCOME) ~ 1, data = km_vis('PFHxS')) %>% 
#              ggsurvfit() +
#              add_confidence_interval() +
#              labs(title = "PFHxS",
#                   y = "",
#                   x = 'Years')),
#           nrow = 2)


#Definitely been decreasing over time.
FINAL_BASEDATASET %>% group_by(Year) %>% summarise(
  PFOA = median(PFOA),
  PFOS = median(PFOS),
  PFNA = median(PFNA),
  PFHxS = median(PFHxS)
) %>% 
  pivot_longer(2:5, names_to = 'Analyte', values_to = 'median concentration') %>% 
  ggplot(aes(x = Year, y = `median concentration`, group = Analyte, color = Analyte)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(y = 'Median concentration (µg/mL)',
       x = 'Survey year') +
  theme(legend.position = 'bottom')

