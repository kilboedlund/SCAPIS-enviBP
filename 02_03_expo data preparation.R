library('tidyverse')

cv <- function(x) {
  return(sd(x, na.rm = T) / mean(x, na.rm = T))
}

rollcv <- function(x, k, align, fill) {
  zoo::rollapply(x, k, FUN = cv, align = align, fill = fill)
}

df_expo_1 <- readRDS(here::here('data expo', 'expo.rds'))

df_expo_2 <- df_expo_1 %>% 
  group_by(site) %>% 
  arrange(date, hour) %>% 
  mutate(#pm25_mean12h = zoo::rollmean(pm25, 12, align = 'right', fill = NA, na.rm = T),
         #pm10_mean12h = zoo::rollmean(pm10, 12, align = 'right', fill = NA, na.rm = T),
         #no2_mean12h  = zoo::rollmean(no2 , 12, align = 'right', fill = NA, na.rm = T),
         #o3_mean12h   = zoo::rollmean(o3  , 12, align = 'right', fill = NA, na.rm = T),
         #temp_mean12h = zoo::rollmean(temp, 12, align = 'right', fill = NA, na.rm = T),
         #pm25_cv12h = rollcv(pm25, 12, align = 'right', fill = NA),
         #pm10_cv12h = rollcv(pm10, 12, align = 'right', fill = NA),
         #no2_cv12h  = rollcv(no2 , 12, align = 'right', fill = NA),
         #o3_cv12h   = rollcv(o3  , 12, align = 'right', fill = NA),
         #temp_cv12h = rollcv(temp, 12, align = 'right', fill = NA),
         #pm25_miss12h = zoo::rollmean(!is.na(pm25), 12, align = 'right', fill = NA),
         #pm10_miss12h = zoo::rollmean(!is.na(pm10), 12, align = 'right', fill = NA),
         #no2_miss12h  = zoo::rollmean(!is.na(no2) , 12, align = 'right', fill = NA),
         #o3_miss12h   = zoo::rollmean(!is.na(o3)  , 12, align = 'right', fill = NA),
         #temp_miss12h = zoo::rollmean(!is.na(temp), 12, align = 'right', fill = NA),
         pm25_mean24h = zoo::rollmean(pm25, 24, align = 'right', fill = NA, na.rm = T),
         pm10_mean24h = zoo::rollmean(pm10, 24, align = 'right', fill = NA, na.rm = T),
         no2_mean24h  = zoo::rollmean(no2 , 24, align = 'right', fill = NA, na.rm = T),
         o3_mean24h   = zoo::rollmean(o3  , 24, align = 'right', fill = NA, na.rm = T),
         temp_mean24h = zoo::rollmean(temp, 24, align = 'right', fill = NA, na.rm = T),
         #pm25_cv24h = rollcv(pm25, 24, align = 'right', fill = NA),
         #pm10_cv24h = rollcv(pm10, 24, align = 'right', fill = NA),
         #no2_cv24h  = rollcv(no2 , 24, align = 'right', fill = NA),
         #o3_cv24h   = rollcv(o3  , 24, align = 'right', fill = NA),
         #temp_cv24h = rollcv(temp, 24, align = 'right', fill = NA),
         pm25_miss24h = zoo::rollmean(!is.na(pm25), 24, align = 'right', fill = NA),
         pm10_miss24h = zoo::rollmean(!is.na(pm10), 24, align = 'right', fill = NA),
         no2_miss24h  = zoo::rollmean(!is.na(no2) , 24, align = 'right', fill = NA),
         o3_miss24h   = zoo::rollmean(!is.na(o3)  , 24, align = 'right', fill = NA),
         temp_miss24h = zoo::rollmean(!is.na(temp), 24, align = 'right', fill = NA),
         #pm25_mean48h = zoo::rollmean(pm25, 48, align = 'right', fill = NA, na.rm = T),
         #pm10_mean48h = zoo::rollmean(pm10, 48, align = 'right', fill = NA, na.rm = T),
         #no2_mean48h  = zoo::rollmean(no2 , 48, align = 'right', fill = NA, na.rm = T),
         #3_mean48h   = zoo::rollmean(o3  , 48, align = 'right', fill = NA, na.rm = T),
         #temp_mean48h = zoo::rollmean(temp, 48, align = 'right', fill = NA, na.rm = T),
         #pm25_cv48h = rollcv(pm25, 48, align = 'right', fill = NA),
         #pm10_cv48h = rollcv(pm10, 48, align = 'right', fill = NA),
         #no2_cv48h  = rollcv(no2 , 48, align = 'right', fill = NA),
         #o3_cv48h   = rollcv(o3  , 48, align = 'right', fill = NA),
         #temp_cv48h = rollcv(temp, 48, align = 'right', fill = NA),
         #pm25_miss48h = zoo::rollmean(!is.na(pm25), 48, align = 'right', fill = NA),
         #pm10_miss48h = zoo::rollmean(!is.na(pm10), 48, align = 'right', fill = NA),
         #no2_miss48h  = zoo::rollmean(!is.na(no2) , 48, align = 'right', fill = NA),
         #o3_miss48h   = zoo::rollmean(!is.na(o3)  , 48, align = 'right', fill = NA),
         #temp_miss48h = zoo::rollmean(!is.na(temp), 48, align = 'right', fill = NA),
         pm25_mean72h = zoo::rollmean(pm25, 72, align = 'right', fill = NA, na.rm = T),
         pm10_mean72h = zoo::rollmean(pm10, 72, align = 'right', fill = NA, na.rm = T),
         no2_mean72h  = zoo::rollmean(no2 , 72, align = 'right', fill = NA, na.rm = T),
         o3_mean72h   = zoo::rollmean(o3  , 72, align = 'right', fill = NA, na.rm = T),
         temp_mean72h = zoo::rollmean(temp, 72, align = 'right', fill = NA, na.rm = T),
         #pm25_cv72h = rollcv(pm25, 72, align = 'right', fill = NA),
         #pm10_cv72h = rollcv(pm10, 72, align = 'right', fill = NA),
         #no2_cv72h  = rollcv(no2 , 72, align = 'right', fill = NA),
         #o3_cv72h   = rollcv(o3  , 72, align = 'right', fill = NA),
         #temp_cv72h = rollcv(temp, 72, align = 'right', fill = NA),
         pm25_miss72h = zoo::rollmean(!is.na(pm25), 72, align = 'right', fill = NA),
         pm10_miss72h = zoo::rollmean(!is.na(pm10), 72, align = 'right', fill = NA),
         no2_miss72h  = zoo::rollmean(!is.na(no2) , 72, align = 'right', fill = NA),
         o3_miss72h   = zoo::rollmean(!is.na(o3)  , 72, align = 'right', fill = NA),
         temp_miss72h = zoo::rollmean(!is.na(temp), 72, align = 'right', fill = NA),
         pm25_mean168h = zoo::rollmean(pm25, 168, align = 'right', fill = NA, na.rm = T),
         pm10_mean168h = zoo::rollmean(pm10, 168, align = 'right', fill = NA, na.rm = T),
         no2_mean168h  = zoo::rollmean(no2 , 168, align = 'right', fill = NA, na.rm = T),
         o3_mean168h   = zoo::rollmean(o3  , 168, align = 'right', fill = NA, na.rm = T),
         temp_mean168h = zoo::rollmean(temp, 168, align = 'right', fill = NA, na.rm = T),
         #pm25_cv168h = rollcv(pm25, 168, align = 'right', fill = NA),
         #pm10_cv168h = rollcv(pm10, 168, align = 'right', fill = NA),
         #no2_cv168h  = rollcv(no2 , 168, align = 'right', fill = NA),
         #o3_cv168h   = rollcv(o3  , 168, align = 'right', fill = NA),
         #temp_cv168h = rollcv(temp, 168, align = 'right', fill = NA),
         pm25_miss168h = zoo::rollmean(!is.na(pm25), 168, align = 'right', fill = NA),
         pm10_miss168h = zoo::rollmean(!is.na(pm10), 168, align = 'right', fill = NA),
         no2_miss168h  = zoo::rollmean(!is.na(no2) , 168, align = 'right', fill = NA),
         o3_miss168h   = zoo::rollmean(!is.na(o3)  , 168, align = 'right', fill = NA),
         temp_miss168h = zoo::rollmean(!is.na(temp), 168, align = 'right', fill = NA)
  ) %>% 
  filter(hour == 8) %>% 
  mutate(#pm25_mean12h = if_else(pm25_miss12h < .8, NA, pm25_mean12h),
         #pm10_mean12h = if_else(pm10_miss12h < .8, NA, pm10_mean12h),
         #no2_mean12h = if_else(no2_miss12h < .8, NA, no2_mean12h),
         #o3_mean12h = if_else(o3_miss12h < .8, NA, o3_mean12h),
         #temp_mean12h = if_else(temp_miss12h < .8, NA, temp_mean12h),
         #pm25_cv12h = if_else(pm25_miss12h < .8, NA, pm25_cv12h),
         #pm10_cv12h = if_else(pm10_miss12h < .8, NA, pm10_cv12h),
         #no2_cv12h = if_else(no2_miss12h < .8, NA, no2_cv12h),
         #o3_cv12h = if_else(o3_miss12h < .8, NA, o3_cv12h),
         #temp_cv12h = if_else(temp_miss12h < .8, NA, temp_cv12h),
         pm25_mean24h = if_else(pm25_miss24h < .8, NA, pm25_mean24h),
         pm10_mean24h = if_else(pm10_miss24h < .8, NA, pm10_mean24h),
         no2_mean24h = if_else(no2_miss24h < .8, NA, no2_mean24h),
         o3_mean24h = if_else(o3_miss24h < .8, NA, o3_mean24h),
         temp_mean24h = if_else(temp_miss24h < .8, NA, temp_mean24h),
         #pm25_cv24h = if_else(pm25_miss24h < .8, NA, pm25_cv24h),
         #pm10_cv24h = if_else(pm10_miss24h < .8, NA, pm10_cv24h),
         #no2_cv24h = if_else(no2_miss24h < .8, NA, no2_cv24h),
         #o3_cv24h = if_else(o3_miss24h < .8, NA, o3_cv24h),
         #temp_cv24h = if_else(temp_miss24h < .8, NA, temp_cv24h),
         #pm25_mean48h = if_else(pm25_miss48h < .8, NA, pm25_mean48h),
         #pm10_mean48h = if_else(pm10_miss48h < .8, NA, pm10_mean48h),
         #no2_mean48h = if_else(no2_miss48h < .8, NA, no2_mean48h),
         #o3_mean48h = if_else(o3_miss48h < .8, NA, o3_mean48h),
         #temp_mean48h = if_else(temp_miss48h < .8, NA, temp_mean48h),
         #pm25_cv48h = if_else(pm25_miss48h < .8, NA, pm25_cv48h),
         #pm10_cv48h = if_else(pm10_miss48h < .8, NA, pm10_cv48h),
         #no2_cv48h = if_else(no2_miss48h < .8, NA, no2_cv48h),
         #o3_cv48h = if_else(o3_miss48h < .8, NA, o3_cv48h),
         #temp_cv48h = if_else(temp_miss48h < .8, NA, temp_cv48h),
         pm25_mean72h = if_else(pm25_miss72h < .8, NA, pm25_mean72h),
         pm10_mean72h = if_else(pm10_miss72h < .8, NA, pm10_mean72h),
         no2_mean72h = if_else(no2_miss72h < .8, NA, no2_mean72h),
         o3_mean72h = if_else(o3_miss72h < .8, NA, o3_mean72h),
         temp_mean72h = if_else(temp_miss72h < .8, NA, temp_mean72h),
         #pm25_cv72h = if_else(pm25_miss72h < .8, NA, pm25_cv72h),
         #pm10_cv72h = if_else(pm10_miss72h < .8, NA, pm10_cv72h),
         #no2_cv72h = if_else(no2_miss72h < .8, NA, no2_cv72h),
         #o3_cv72h = if_else(o3_miss72h < .8, NA, o3_cv72h),
         #temp_cv72h = if_else(temp_miss168h < .8, NA, temp_cv168h),
         pm25_mean168h = if_else(pm25_miss168h < .8, NA, pm25_mean168h),
         pm10_mean168h = if_else(pm10_miss168h < .8, NA, pm10_mean168h),
         no2_mean168h = if_else(no2_miss168h < .8, NA, no2_mean168h),
         o3_mean168h = if_else(o3_miss168h < .8, NA, o3_mean168h),
         temp_mean168h = if_else(temp_miss168h < .8, NA, temp_mean168h),
         #pm25_cv168h = if_else(pm25_miss168h < .8, NA, pm25_cv168h),
         #pm10_cv168h = if_else(pm10_miss168h < .8, NA, pm10_cv168h),
         #no2_cv168h = if_else(no2_miss168h < .8, NA, no2_cv168h),
         #o3_cv168h = if_else(o3_miss168h < .8, NA, o3_cv168h),
         #temp_cv168h = if_else(temp_miss168h < .8, NA, temp_cv168h)
  )

saveRDS(df_expo_2, here::here('data expo', 'expo_2.rds'))

#df_expo_2 <- readRDS(here::here('data expo', 'expo_2.rds'))

df_ndvi <- readRDS('/safe/data/Research projects/SCAPIS/Originaldata/SCAPIS_master_20230605.rds') %>% 
  select(id = scapis_id, date = anthropometrycollectiondate, contains('ndvi500m')) %>% 
  pivot_longer(cols = -c(date, id), names_to = 'year', values_to = 'ndvi', names_pattern = 'ndvi500m(.*)_as01') %>% 
  filter(year == str_sub(date, 1, 4)) %>% 
  select(id, ndvi)
