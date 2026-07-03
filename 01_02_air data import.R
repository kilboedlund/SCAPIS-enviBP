library('tidyverse')
source(here::here('00_00_functions.R'))

# Read data files ------------------------------------------------------------------------------------------------------
df_air_1 <- list(
  pm25 = list(
    sth_8781 = air_data(8781, 'pm25', 9),
    got_8577 = air_data(8577, 'pm25', 9),
    mal_8773 = air_data(8773, 'pm25', 9)
    #upp_34398 = air_data(34398, 'pm25', 9),
    #upp_159404 = air_data(159404, 'pm25', 9),
    #ume_13532 = air_data(13532, 'pm25', 9)
  ),
  pm10 = list(
    sth_8781 = air_data(8781, 'pm10', 9),
    got_8577 = air_data(8577, 'pm10', 9),
    mal_8773 = air_data(8773, 'pm10', 9)
    #upp_34398 = air_data(34398, 'pm10', 9),
    #upp_159404 = air_data(159404, 'pm10', 9),
    #ume_13532 = air_data(13532, 'pm10', 9)
  ),
  no2 = list(
    sth_8781 = air_data(8781, 'no2', 9),
    got_8577 = air_data(8577, 'no2', 9),
    mal_8773 = air_data(8773, 'no2', 9)
    #upp_34398 = air_data(34398, 'no2', 9),
    #upp_159404 = air_data(159404, 'no2', 9),
    #ume_13532 = air_data(13532, 'no2', 9)
  ),
  o3 = list(
    sth_8781 = air_data(8781, 'o3', 9),
    #got_9346 = air_data(9346, 'o3', 9),
    got_8577 = air_data(8577, 'o3', 9) %>% select(Start, Slut, 'O3 (86)'),
    mal_8773 = air_data(8773, 'o3', 9)
  )
) %>%
  map2(names(.),
       ~map2(.x, names(.x),
             ~set_names(.x, c('start', 'stop', 'exposure')) %>%
               mutate(stn = .y, site = str_sub(.y, 1, 3))) %>%
         bind_rows() %>%
         mutate(pollutant = .y)) %>%
  bind_rows() %>%
  mutate(date = date(start),
         year = year(start),
         month = month(start),
         day = day(start),
         hour = hour(start))

saveRDS(df_air_1, here::here('data expo', 'air_1.rds'))