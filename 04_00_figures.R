# Plot O3 and temp
pO3_temp_all <- 
  fit %>% 
  filter(ap == 'o3',
         int, 
         spline_df == 0) %>% 
  unnest(lm) %>% 
  filter(str_detect(term, 'mean')) %>% 
  mutate(term = case_when(str_sub(term, 1, 4) == 'temp' & !str_detect(term, ':') ~ 'temp',
                          str_sub(term, 1, 2) == 'o3'   ~ 'o3',
                          T ~ 'int') %>% 
           factor(levels = c('o3', 'temp', 'int'),
                  labels = c('O3', 'Temperature', 'Interaction'),
                  ordered = T),
         t = factor(paste0(t, 'h')),
         model = factor(model,
                        levels = c('m0', 'm1', 'm2'),
                        labels = c('Site', 
                                   '+ year, month, weekday',
                                   '+ age, sex, smoking, waist-hip ratio')),
         outc = factor(outc, 
                       levels = c('sbp', 'dbp', 'pp', 'pulse'),
                       labels = c('SBP', 'DBP', 'PP', 'Pulse rate'),
                       ordered = T)) %>% 
  ggplot(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high, colour = model)) +
  geom_hline(aes(yintercept = 0), colour = 'black', linetype = 'dotted') +
  geom_point(position = position_dodge(width = .5)) +
  geom_errorbar(position = position_dodge(width = .5), width = .4) +
  scale_colour_manual(values = c('grey70', 'grey40', 'grey10')) +
  scale_y_continuous(breaks = c(-2, -1.5, -1, -.5, 0, .5, 1), limits = c(-2.2, 1.2)) +
  labs(x = NULL,
       y = '\u03b2 estimate (mmHg / bpm), 95% CI',
       colour = 'Adjustment model') +
  facet_grid(outc ~ t) +
  theme_bw() +
  theme(legend.position = 'bottom',
        legend.direction = 'vertical',
        axis.text.x = element_text(angle = 15, hjust = 1))

pO3_temp_sex <- 
  fit_m2_sex %>% 
  unnest(lm) %>% 
  filter(str_detect(term, 'mean')) %>% 
  mutate(term = case_when(str_sub(term, 1, 4) == 'temp' & !str_detect(term, ':') ~ 'temp',
                          str_sub(term, 1, 2) == 'o3'   ~ 'o3',
                          T ~ 'int') %>% 
           factor(levels = c('o3', 'temp', 'int'),
                  labels = c('O3', 'Temperature', 'Interaction'),
                  ordered = T),
         t = factor(paste0(t, 'h')),
         outc = factor(outc, 
                       levels = c('sbp', 'dbp', 'pp', 'pulse'),
                       labels = c('SBP', 'DBP', 'PP', 'Pulse rate'),
                       ordered = T),
         subset = factor(subset,
                         levels = c('sex == \'MALE\'', 'sex == \'FEMALE\''),
                         labels = c('Men', 'Women'),
                         ordered = T)) %>% 
  ggplot(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high, colour = subset)) +
  geom_hline(aes(yintercept = 0), colour = 'black', linetype = 'dotted') +
  geom_point(position = position_dodge(width = .5)) +
  geom_errorbar(position = position_dodge(width = .5), width = .4) +
  scale_y_continuous(breaks = c(-2, -1.5, -1, -.5, 0, .5, 1), limits = c(-2.2, 1.2)) +
  scale_colour_manual(values = c('grey70', 'grey10')) +
  labs(x = NULL,
       y = NULL,
       colour = 'Stratification') +
  facet_grid(outc ~ .) +
  theme_bw() +
  theme(legend.position = 'bottom',
        legend.direction = 'vertical',
        axis.text.x = element_text(angle = 30, hjust = 1))

pO3_temp_age <- 
  fit_m2_age %>% 
  unnest(lm) %>% 
  filter(str_detect(term, 'mean')) %>% 
  mutate(term = case_when(str_sub(term, 1, 4) == 'temp' & !str_detect(term, ':') ~ 'temp',
                          str_sub(term, 1, 2) == 'o3'   ~ 'o3',
                          T ~ 'int') %>% 
           factor(levels = c('o3', 'temp', 'int'),
                  labels = c('O3', 'Temperature', 'Interaction'),
                  ordered = T),
         t = factor(paste0(t, 'h')),
         outc = factor(outc, 
                       levels = c('sbp', 'dbp', 'pp', 'pulse'),
                       labels = c('SBP', 'DBP', 'PP', 'Pulse rate'),
                       ordered = T),
         subset = factor(subset,
                         levels = c('age <55', 
                                    'age >= 55 & age < 60',
                                    'age >= 60'),
                         labels = c('<55 years', '55-60 years', '\u226560 years'),
                         ordered = T)) %>% 
  ggplot(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high, colour = subset)) +
  geom_hline(aes(yintercept = 0), colour = 'black', linetype = 'dotted') +
  geom_point(position = position_dodge(width = .5)) +
  geom_errorbar(position = position_dodge(width = .5), width = .4) +
  scale_y_continuous(breaks = c(-2, -1.5, -1, -.5, 0, .5, 1), limits = c(-2.2, 1.2)) +
  scale_colour_manual(values = c('grey70', 'grey40', 'grey10')) +
  labs(x = NULL,
       y = NULL,
       colour = 'Stratification') +
  facet_grid(outc ~ .) +
  theme_bw() +
  theme(legend.position = 'bottom',
        legend.direction = 'vertical',
        axis.text.x = element_text(angle = 30, hjust = 1))

pO3_temp_site <- 
  fit_m2_site %>% 
  unnest(lm) %>% 
  filter(str_detect(term, 'mean')) %>% 
  mutate(term = case_when(str_sub(term, 1, 4) == 'temp' & !str_detect(term, ':') ~ 'temp',
                          str_sub(term, 1, 2) == 'o3'   ~ 'o3',
                          T ~ 'int') %>% 
           factor(levels = c('o3', 'temp', 'int'),
                  labels = c('O3', 'Temperature', 'Interaction'),
                  ordered = T),
         t = factor(paste0(t, 'h')),
         outc = factor(outc, 
                       levels = c('sbp', 'dbp', 'pp', 'pulse'),
                       labels = c('SBP', 'DBP', 'PP', 'Pulse rate'),
                       ordered = T),
         subset = factor(subset,
                         levels = c('site == \'mal\'', 
                                    'site == \'got\'', 
                                    'site == \'sth\''),
                         labels = c('Malm\u00f6', 'Gothenburg', 'Stockholm'),
                         ordered = T)) %>% 
  ggplot(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high, colour = subset)) +
  geom_hline(aes(yintercept = 0), colour = 'black', linetype = 'dotted') +
  geom_point(position = position_dodge(width = .5)) +
  geom_errorbar(position = position_dodge(width = .5), width = .4) +
  scale_y_continuous(breaks = c(-2, -1.5, -1, -.5, 0, .5, 1), limits = c(-2.2, 1.2)) +
  scale_colour_manual(values = c('grey70', 'grey40', 'grey10')) +
  labs(x = NULL,
       y = NULL,
       colour = 'Stratification') +
  facet_grid(outc ~ .) +
  theme_bw() +
  theme(legend.position = 'bottom',
        legend.direction = 'vertical',
        axis.text.x = element_text(angle = 30, hjust = 1))

pO3_temp_ndvi <- 
  fit_m2_ndvi %>% 
  unnest(lm) %>% 
  filter(str_detect(term, 'mean')) %>% 
  mutate(term = case_when(str_sub(term, 1, 4) == 'temp' & !str_detect(term, ':') ~ 'temp',
                          str_sub(term, 1, 2) == 'o3'   ~ 'o3',
                          T ~ 'int') %>% 
           factor(levels = c('o3', 'temp', 'int'),
                  labels = c('O3', 'Temperature', 'Interaction'),
                  ordered = T),
         t = factor(paste0(t, 'h')),
         outc = factor(outc, 
                       levels = c('sbp', 'dbp', 'pp', 'pulse'),
                       labels = c('SBP', 'DBP', 'PP', 'Pulse rate'),
                       ordered = T),
         subset = factor(subset,
                         levels = c('ndvi < -.3022024',
                                    'ndvi >= -.3022024 & ndvi < .4915824', 
                                    'ndvi >= .4915824'),
                         labels = c('NDVI lower tertile', 'NDVI middle tertile', 'NDVI upper tertile'),
                         ordered = T)) %>% 
  ggplot(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high, colour = subset)) +
  geom_hline(aes(yintercept = 0), colour = 'black', linetype = 'dotted') +
  geom_point(position = position_dodge(width = .5)) +
  geom_errorbar(position = position_dodge(width = .5), width = .4) +
  scale_y_continuous(breaks = c(-2, -1.5, -1, -.5, 0, .5, 1), limits = c(-2.2, 1.2)) +
  scale_colour_manual(values = c('grey70', 'grey40', 'grey10')) +
  labs(x = NULL,
       y = NULL,
       colour = 'Stratification') +
  facet_grid(outc ~ .) +
  theme_bw() +
  theme(legend.position = 'bottom',
        legend.direction = 'vertical',
        axis.text.x = element_text(angle = 30, hjust = 1))

pO3_temp <- cowplot::plot_grid(
  pO3_temp_all,
  pO3_temp_sex,
  pO3_temp_age,
  pO3_temp_site,
  pO3_temp_ndvi,
  nrow = 1,
  align = 'h',
  axis = 'lrbt',
  rel_widths = c(1, .25, .25, .25, .25)
)
ggsave( here::here('figures', paste0(Sys.Date(), '_main_res_ap_con.svg')), pO3_temp, width = 12, height = 10)


pO3_temp_extr <- 
  fit_m1_extr %>% 
  unnest(lm) %>% 
  filter(str_detect(term, 'mean')) %>% 
  mutate(term = case_when(str_sub(term, 1, 4) == 'temp' & !str_detect(term, ':') ~ 'temp',
                          str_sub(term, 1, 2) == 'o3'   ~ 'o3',
                          T ~ 'int') %>% 
           factor(levels = c('o3', 'temp', 'int'),
                  labels = c('O3', 'Temperature', 'Interaction'),
                  ordered = T),
         t = factor(paste0(t, 'h')),
         outc = factor(outc, 
                       levels = c('sbp', 'dbp', 'pp', 'pulse'),
                       labels = c('SBP', 'DBP', 'PP', 'Pulse rate'),
                       ordered = T)) %>% 
  ggplot(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(aes(yintercept = 0), colour = 'black', linetype = 'dotted') +
  geom_point(position = position_dodge(width = .5)) +
  geom_errorbar(position = position_dodge(width = .5), width = .4) +
  scale_y_continuous(breaks = c(-2, -1.5, -1, -.5, 0, .5, 1), limits = c(-2.2, 1.2)) +
  scale_colour_manual(values = c('grey70', 'grey40', 'grey10')) +
  labs(x = NULL,
       y = NULL,
       colour = 'Stratification') +
  facet_grid(outc ~ t) +
  theme_bw() +
  theme(legend.position = 'bottom',
        legend.direction = 'vertical',
        axis.text.x = element_text(angle = 30, hjust = 1))
