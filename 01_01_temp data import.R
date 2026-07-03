library('tidyverse')
source(here::here('00_00_functions.R'))

# Read data files ------------------------------------------------------------------------------------------------------
df_temp_1 <- list(
  #lin = temp_data(85240, 9, '1'),
  got = temp_data(71420, 13, '1'),
  sth = temp_data(98230, 9, '1'),
  #upp = temp_data(97510, 9, '1'),
  #ume = temp_data(140480, 10, '1')),
  mal = temp_data(52350, 9, '1')
) %>%
  map2(names(.),
       ~.x %>% mutate(site = .y)) %>%
  bind_rows() %>%
  rename('date' = 'Datum',
         'time' = 'Tid (UTC)',
         'temp' = 'Lufttemperatur',
         'qual' = 'Kvalitet') %>%
  mutate(qual = qual == 'G',
         temp = if_else(qual, temp, NA_real_))

# Calculate means and variability ----------------------------------------------
df_temp_2 <- df_temp_1 %>%
  group_by(date, site) %>%
  summarise(min = min(temp, na.rm = T),
            max = max(temp, na.rm = T),
            mean = mean(temp, na.rm = T),
            #qrange = quantile(temp, probs = c(.05, .95), na.rm = T) %>% diff(),
            #sd = sd(temp, na.rm = T)
            ) %>%
  group_by(site) %>%
  mutate(year = year(date),
         month = month(date, label = T),
         change = mean - lag(mean, 1)) %>%
  ungroup()
