library('tidyverse')
source(here::here('00_00_functions.R'))

# Read data files ------------------------------------------------------------------------------------------------------
df_humid_1 <- list(
  lin = humid_data(85240, 9),
  got = humid_data(71420, 13),
  sth = humid_data(98230, 9),
  upp = humid_data(97510, 9),
  ume = humid_data(140480, 10),
  mal = humid_data(52350, 9)
) %>%
  map2(names(.),
       ~ .x %>% mutate(site = .y)) %>%
  bind_rows() %>%
  rename('date' = 'Datum',
         'time' = 'Tid (UTC)',
         'humi' = 'Relativ Luftfuktighet',
         'qual' = 'Kvalitet') %>%
  mutate(qual = qual == 'G')

## Proof of concept graph ----------------------------------------------------------------------------------------------
df_humid_1 %>%
  mutate(year = year(date)) %>%
  group_by(site, year, date) %>%
  summarise(humi = mean(humi)) %>%
  ggplot(aes(x = date, y = humi, colour = site)) +
  geom_line() +
  theme_classic() +
  facet_wrap(vars(year), scales = 'free_x')