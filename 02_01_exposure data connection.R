library('tidyverse')

source(here::here('00_00_functions.R'))
source(here::here('01_01_temp data import.R'))
source(here::here('01_02_air data import.R'))
source(here::here('01_03_humidity data import.R'))
source(here::here('01_04_wind data import.R'))

df_expo_1 <- df_air_1 %>%
  select(date, hour, exposure, stn, site, pollutant) %>%
  pivot_wider(id_cols = c('date', 'hour', 'site'),
              names_from = 'pollutant',
  values_from = 'exposure') %>%
  left_join(df_temp_1 %>%
              mutate(hour = hour(time)) %>%
              select(date, hour, temp, site),
            by = c('date', 'hour', 'site')) %>%
  left_join(df_humid_1 %>%
              mutate(hour = hour(time)) %>%
              select(date, hour, humi, site),
            by = c('date', 'hour', 'site')) %>%
  left_join(df_wind_1 %>%
              mutate(hour = hour(time)) %>%
              select(date, hour, wind_dir, wind_spe, site),
            by = c('date', 'hour', 'site'))

df_expo_2 <- df_expo_1 %>%
  ungroup %>%
  mutate(year = year(date)) %>%
  filter(
    (site == 'got' & year %in% 2013:2018) |
      (site == 'mal' & year %in% 2014:2018) |
      (site == 'sth' & year %in% 2015:2018) |
      (site == 'ume' & year %in% 2016:2018) |
      (site == 'upp' & year %in% 2015:2018)
  )

df_impute_1 <- df_expo_2 %>%
  mutate(datetime = as.POSIXct(paste(date, hour), format="%Y-%m-%d %H") %>% as.integer()) %>%
  filter(!is.na(datetime)) %>%
  select(-c(date, hour)) %>%
  as.data.frame() %>%
  Amelia::amelia(ts = 'datetime',
         cs = 'site',
         m = 5)

saveRDS(df_expo_2, here::here('data expo', 'expo.rds'))
saveRDS(df_impute_1, here::here('data expo', 'expo_imp.rds'))