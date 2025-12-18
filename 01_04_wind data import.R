library('tidyverse')
source(here::here('00_00_functions.R'))

# Read data files ------------------------------------------------------------------------------------------------------
df_wind_1 <- list(
  lin = wind_data(85240, 9),
  got = wind_data(71420, 14),
  sth = expand.grid(Datum = as.Date.character('2013-01-01'):as.Date.character('2018-12-31') %>% as.Date(),
                    `Tid (UTC)` = 0:23) %>%
    left_join(wind_data(98210, 10) %>%
                mutate(`Tid (UTC)` = as.integer(hour(`Tid (UTC)`)))) %>%
    arrange(Datum, `Tid (UTC)`) %>%
    mutate(theta = ((270 - Vindriktning) %% 360) * (pi / 180),
           x = (Vindhastighet * cos(theta)) %>% zoo::na.approx(na.rm = F),
           y = (Vindhastighet * sin(theta)) %>% zoo::na.approx(na.rm = F),
           Vindhastighet = sqrt(x^2 + y^2),
           Vindriktning = (270 - atan2(y, x) * (180 / pi)) %% 360,
           `Tid (UTC)` = as.difftime(hm(paste0(`Tid (UTC)`, ':00')))) %>%
    select(Datum,
           `Tid (UTC)`, Vindriktning, Vindhastighet),
  upp = wind_data(97510, 10),
  ume = wind_data(140480, 11),
  mal = wind_data(52350, 10)
) %>%
  map2(names(.),
       ~.x %>% mutate(site = .y)) %>%
  bind_rows() %>%
  rename('date' = 'Datum',
         'time' = 'Tid (UTC)',
         'wind_dir' = 'Vindriktning',
         'qual_dir' = 'Kvalitet...4',
         'wind_spe' = 'Vindhastighet',
         'qual_spe' = 'Kvalitet...6') %>%
  mutate(across(contains('qual'), ~.x == 'G')) %>%
  distinct()