library('tidyverse')

df_expo <- readRDS(here::here('data expo', 'expo.rds')) %>% 
  group_by(site) %>% 
  arrange(date, hour) %>% 
  mutate(o3_mean24h   = zoo::rollmean(o3  , 24, align = 'right', fill = NA, na.rm = T),
         temp_mean24h = zoo::rollmean(temp_ap, 24, align = 'right', fill = NA, na.rm = T),
         temp_db_mean24h = zoo::rollmean(temp, 24, align = 'right', fill = NA, na.rm = T),
         pm10_mean24h = zoo::rollmean(pm10, 24, align = 'right', fill = NA, na.rm = T),
         no2_mean24h = zoo::rollmean(no2, 24, align = 'right', fill = NA, na.rm = T),
         
         o3_miss24h   = zoo::rollmean(!is.na(o3)  , 24, align = 'right', fill = NA),
         temp_miss24h = zoo::rollmean(!is.na(temp_ap), 24, align = 'right', fill = NA),
         temp_db_miss24h = zoo::rollmean(!is.na(temp), 24, align = 'right', fill = NA),
         pm10_miss24h = zoo::rollmean(!is.na(pm10), 24, align = 'right', fill = NA),
         no2_miss24h = zoo::rollmean(!is.na(no2), 24, align = 'right', fill = NA),
         
         o3_mean168h   = zoo::rollmean(o3  , 168, align = 'right', fill = NA, na.rm = T),
         temp_mean168h = zoo::rollmean(temp_ap, 168, align = 'right', fill = NA, na.rm = T),
         pm10_mean168h = zoo::rollmean(pm10, 168, align = 'right', fill = NA, na.rm = T),
         no2_mean168h = zoo::rollmean(no2, 168, align = 'right', fill = NA, na.rm = T),
          
         o3_miss168h   = zoo::rollmean(!is.na(o3)  , 168, align = 'right', fill = NA),
         temp_miss168h = zoo::rollmean(!is.na(temp_ap), 168, align = 'right', fill = NA),
         pm10_miss168h = zoo::rollmean(!is.na(pm10), 168, align = 'right', fill = NA),
         no2_miss168h = zoo::rollmean(!is.na(no2), 168, align = 'right', fill = NA)
  ) %>% 
  filter(hour == 8) %>% 
  mutate(o3_24h = if_else(o3_miss24h < .8, NA, o3_mean24h),
         temp_24h = if_else(temp_miss24h < .8, NA, temp_mean24h),
         temp_db_24h = if_else(temp_db_miss24h < .8, NA, temp_db_mean24h),
         pm10_24h = if_else(pm10_miss24h < .8, NA, pm10_mean24h),
         no2_24h = if_else(no2_miss24h < .8, NA, no2_mean24h),
         
         o3_168h = if_else(o3_miss168h < .8, NA, o3_mean168h),
         temp_168h = if_else(temp_miss168h < .8, NA, temp_mean168h),
         pm10_168h = if_else(pm10_miss168h < .8, NA, pm10_mean168h),
         no2_168h = if_else(no2_miss168h < .8, NA, no2_mean168h)
  ) %>% 
  select(-contains('miss'), -contains('mean'))

saveRDS(df_expo, here::here('data expo', 'expo_merged.rds'))

# In relation to WHO limits
readRDS(here::here('data expo', 'expo.rds')) %>% 
  group_by(site) %>% 
  arrange(date, hour) %>% 
  mutate(o3_mean8h    = zoo::rollmean(o3  ,  8, align = 'right', fill = NA, na.rm = F)) %>% 
  group_by(site, year) %>% 
  summarise(s = sum(o3_mean8h > 100, na.rm = T),
            p = mean(o3_mean8h > 100, na.rm = T)) %>% 
  view()
