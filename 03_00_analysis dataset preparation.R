library('tidyverse')

df_1 <- readRDS('/safe/data/Research projects/SCAPIS/Karl/SCAPIS enviBP/data SCAPIS/df_outc.rds') %>% 
  filter(site %in% c('mal', 'got', 'sth')) %>% 
  left_join(
    readRDS(here::here('data expo', 'expo_merged.rds')) %>% 
      select(-hour),
    by = c('date', 'site')
  ) %>% 
  left_join(
    readRDS('/safe/data/Research projects/SCAPIS/Originaldata/SCAPIS_master_20230605.rds') %>% 
      select(id = scapis_id, date = anthropometrycollectiondate, contains('ndvi500m')) %>% 
      pivot_longer(cols = -c(date, id), names_to = 'year', values_to = 'ndvi', names_pattern = 'ndvi500m(.*)_as01') %>% 
      filter(year == str_sub(date, 1, 4)) %>% 
      select(id, ndvi), 
    by = 'id'
  ) %>% 
  mutate(diab = case_match(diab,
                           'NORMOGLYCEMIA' ~ 'no',
                           'NEW_DM' ~ 'yes',
                           'KNOWN_DM' ~ 'yes',
                           'IFG' ~ 'no',
                           'ELEV_HBA1C' ~ 'no',
                           '' ~ NA_character_))

df_2 <- df_1 %>% 
  mutate(across(c('ndvi', contains('temp'), contains('o3'), contains('pm10'), contains('no2')),
                ~(.x - mean(.x, na.rm = T)) / sd(.x, na.rm = T)),
         across(c('year'),
                ~ .x - min(.x, na.rm = T)),
         year = factor(year, ordered = F),
         month = factor(lubridate::month(date, label = T), ordered = F),
         wday = factor(lubridate::wday(date, label = T), ordered = F),
         score2_uncal = 1 - (if_else(sex == 'MALE', .9695, .9776) ^ exp( 
           if_else(sex == 'MALE', .3742, .4648) * ((age - 60) / 5) +
             if_else(sex == 'MALE', .6012, .7744) * (smo == 'CURRENT') +
             if_else(sex == 'MALE', .2777, .3131) * (sbp - 120) / sbp +
             if_else(sex == 'MALE', .1458, .1002) * (tchol - 6) +
             if_else(sex == 'MALE', -.2698, -.2606) * (hdl - 1.3) / .5 +
             if_else(sex == 'MALE', -.0755, -.1088) * (smo == 'CURRENT') * ((age - 60) / 5) +
             if_else(sex == 'MALE', -.0255, -.0277) * ((sbp - 120) / sbp) * ((age - 60) / 5) +
             if_else(sex == 'MALE', -.0281, -.0226) * (tchol - 6) * ((age - 60) / 5) +
             if_else(sex == 'MALE', .0426, .0613) * ((hdl - 1.3) / .5) * ((age - 60) / 5)
         )
         ),
         score2_cal = (1 - exp(-exp(if_else(sex == 'MALE', -.1565, -.3143) +
                                      if_else(sex == 'MALE', .8009, .7701) *
                                      log(-log(1 - score2_uncal))))) * 100
  )

