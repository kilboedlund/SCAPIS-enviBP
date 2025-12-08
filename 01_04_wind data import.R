library('tidyverse')
source(here::here('00_00_functions.R'))

# Read data files ------------------------------------------------------------------------------------------------------
df_wind_1 <- list(
  lin = wind_data(85240, 9),
  got = wind_data(71420, 14),
  sth = wind_data(98210, 10),
  upp = wind_data(97510, 10),
  ume = wind_data(140480, 11),
  mal = wind_data(52350, 10)
) %>%
  map2(names(.),
       ~ .x %>% mutate(site = .y)) %>%
  bind_rows() %>%
  rename('date' = 'Datum',
         'time' = 'Tid (UTC)',
         'wind_dir' = 'Vindriktning',
         'qual_dir' = 'Kvalitet...4',
         'wind_spe' = 'Vindhastighet',
         'qual_spe' = 'Kvalitet...6') %>%
  mutate(across(contains('qual'), ~ .x == 'G'))

## Proof of concept graph ----------------------------------------------------------------------------------------------
df_wind_1 %>%
  group_by(site) %>%
  filter(qual_dir, wind_spe > 3) %>%
  mutate(wind_dir = round(wind_dir, -1)) %>%
  count(wind_dir) %>%
  ggplot(aes(x = wind_dir, y = n, fill = site)) +
  geom_col() +
  coord_polar() +
  scale_x_continuous(expand = c(0,0)) +
  facet_wrap(vars(site))

df_wind_2 <- df_wind_1 %>%
  mutate(year = year(date),
         month = month(date, label = T)) %>%
  group_by(year, month, date, site) %>%
  summarise(wind_spe = mean(wind_spe))