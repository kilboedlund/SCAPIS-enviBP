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
  )

df_2 <- df_1 %>% 
  mutate(across(c('ndvi', contains('temp'), contains('o3'), contains('pm10'), contains('no2')),
                ~(.x - mean(.x, na.rm = T)) / sd(.x, na.rm = T)),
         across(c('year'),
                ~ .x - min(.x, na.rm = T)),
         year = factor(year, ordered = F),
         month = factor(lubridate::month(date, label = T), ordered = F),
         wday = factor(lubridate::wday(date, label = T), ordered = F),
         yday = yday(date) / if_else(leap_year(date), 366, 355),
         site = factor(site),
         season = if_else(month(date) %in% 4:9, 'W', 'C')
  )

