fit_pm10 <-
  expand_grid(outc = c('sbp', 'dbp', 'pp', 'pulse'),
              ap = 'o3',
              m = 'mean',
              t = c(24, 72, 168),
              spline_df = 0,
              int = F) %>% 
  group_by(outc, ap, m, t, spline_df, int) %>% 
  nest() %>% 
  mutate(lm = list(lm_SVBP(df = df_1, 
                           outc = outc, 
                           expo1 = 'temp', 
                           expo2 = ap, 
                           m = m,
                           t = t, 
                           spline_df = spline_df, 
                           int = int,
                           model = 
                             c('year', 'month', 'wday', paste0('pm10_mean', t, 'h')))))

fit_pm10 %>% 
  mutate(lm = map(lm, ~ tidy(.x, conf.int = T))) %>% 
  filter(!int, 
         spline_df == 0) %>% 
  unnest(lm) %>% 
  ungroup() %>% 
  filter(str_detect(term, 'mean')) %>% 
  mutate(term = case_when(str_sub(term, 1, 4) == 'temp' & !str_detect(term, ':') ~ 'temp',
                          str_sub(term, 1, 2) == 'o3'   ~ 'o3',
                          str_sub(term, 1, 4) == 'pm10' ~ 'pm10',
                          T ~ 'int') %>% 
           factor(levels = c('o3', 'temp', 'pm10'),
                  labels = c('O3', 'Temperature', 'PM10'),
                  ordered = T),
         t = factor(paste0(t, 'h')),
         #model = factor(model,
         #               levels = c('m0', 'm1', 'm2'),
         #               labels = c('Site', 
         #                          '+ year, month, weekday',
         #                          '+ age, sex, smoking, waist-hip ratio')),
         outc = factor(outc, 
                       levels = c('sbp', 'dbp', 'pp', 'pulse'),
                       labels = c('SBP', 'DBP', 'PP', 'Pulse rate'),
                       ordered = T),
         res = paste0(sprintf('%.2f', estimate), ' (', 
                      sprintf('%.2f', conf.low), ', ',
                      sprintf('%.2f', conf.high), ')')) %>% 
  pivot_wider(id_cols = c('outc', 't'), names_from = 'term', values_from = 'res') %>% view()
