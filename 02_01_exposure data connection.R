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
              select(date, hour, wind_spe, site),
            by = c('date', 'hour', 'site')) %>% 
  mutate(
    temp_ap = 
      round(temp 
            + .33 * (.01 * humi * 6.105 * exp((17.27 * temp) / (237.7 + temp))) 
            - .7 * wind_spe - 4,
            1),
    temp_wc =
      round(13.12 + 0.6215*temp 
            - 11.37*(3.6 * wind_spe)^.16 
            + .3965*temp*(3.6 * wind_spe)^.16,
            1)
  ) %>% 
  select(date, hour, site, o3, temp, temp_ap, temp_wc, pm10, no2)
  
df_expo_2 <- df_expo_1 %>%
  ungroup %>%
  mutate(year = year(date)) %>%
  filter(
    (site == 'got' & year %in% 2013:2018) |
      (site == 'mal' & year %in% 2014:2018) |
      (site == 'sth' & year %in% 2015:2018)
  )

df_impute_1 <- df_expo_2 %>%
  left_join(df_humid_1 %>%
              mutate(hour = hour(time)) %>%
              select(date, hour, humi, site),
            by = c('date', 'hour', 'site')) %>%
  left_join(df_wind_1 %>%
              mutate(hour = hour(time),
                     wind_dir_sin = sin(wind_dir),
                     wind_dir_cos = cos(wind_dir)) %>%
              select(date, hour, wind_dir_sin, wind_dir_cos, wind_spe, site),
            by = c('date', 'hour', 'site')) %>% 
  mutate(datetime = as.POSIXct(paste(date, hour), format="%Y-%m-%d %H") %>% as.integer(),
         wday = wday(date, label = T, locale = "EN-us"),
         month = month(date, label = T, locale = "EN-us")) %>%
  filter(!is.na(datetime)) %>%
  select(-c(date, hour)) %>%
  as.data.frame() %>%
  Amelia::amelia(ts = 'datetime',
         cs = 'site',
         m = 5,
         logs = c('pm10', 'o3', 'no2'),
         noms = c('wday', 'month', 'year'))

saveRDS(df_expo_2, here::here('data expo', 'expo.rds'))
saveRDS(df_impute_1, here::here('data expo', 'expo_imp.rds'))

# Calculate LCC for double Gothenburg stations

ccc_data <- left_join(
  air_data(9346, 'o3', 9) %>% 
    transmute(Start,
              Slut,
              o3_mol = `O3 (1188)`),
  air_data(8577, 'o3', 9) %>% 
    transmute(Start,
              Slut, 
              o3_got = `O3 (86)`)) 

epiR::epi.ccc(
    x = ccc_data$o3_mol,
    y = ccc_data$o3_got
  )
