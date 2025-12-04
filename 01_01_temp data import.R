library('tidyverse')
source(here::here('00_00_functions.R'))

# Read data files ------------------------------------------------------------------------------------------------------
df_temp_1 <- list(
  lin = temp_data(85240, 9),
  got = temp_data(71420, 13),
  sth = temp_data(98230, 9),
  upp = temp_data(97510, 9),
  ume = temp_data(140480, 10),
  mal = temp_data(52350, 9)
) %>%
  map2(names(.),
       ~ .x %>% mutate(site = .y)) %>%
  bind_rows() %>%
  rename('date' = 'Datum',
         'time' = 'Tid (UTC)',
         'temp' = 'Lufttemperatur',
         'qual' = 'Kvalitet') %>%
  mutate(qual = qual == 'G')

## Proof of concept graph ----------------------------------------------------------------------------------------------
df_temp_1 %>%
  mutate(year = year(date)) %>%
  group_by(site, year, date) %>%
  summarise(temp = mean(temp)) %>%
  ggplot(aes(x = date, y = temp, colour = site)) +
  geom_line() +
  theme_classic() +
  facet_wrap(vars(year), scales = 'free_x')

# Calculate variability ------------------------------------------------------------------------------------------------
df_temp_2 <- df_temp_1 %>%
  group_by(date, site) %>%
  summarise(min = min(temp),
            max = max(temp),
            mean = mean(temp),
            qrange = quantile(temp, probs = c(.05, .95)) %>% diff(),
            sd = sd(temp)) %>%
  mutate(year = year(date),
         month = month(date, label = T))

## Proof of concept graphs ---------------------------------------------------------------------------------------------
df_temp_2  %>%
  group_by(month, site) %>%
  summarise(qrange = median(qrange)) %>%
  ggplot(aes(x = month, y = qrange, fill = site)) +
  geom_col() +
  facet_grid(vars(site)) +
  theme_classic() +
  ggtitle('Temp qrange, seasonal variation')
ggsave(here::here('figures', paste(Sys.Date(), '_temp_qrange_seasonal variation.svg')), width = 5, height = 5)

df_temp_2  %>%
  group_by(year, month, site) %>%
  summarise(mean = mean(mean)) %>%
  ggplot(aes(x = month, y = mean, fill = site)) +
  geom_col() +
  facet_grid(site ~ year) +
  theme_classic() +
  ggtitle('Temp mean, seasonal variation')
ggsave(here::here('figures', paste(Sys.Date(), '_temp_mean_seasonal variation.svg')), width = 5, height = 5)

## Linear regression ---------------------------------------------------------------------------------------------------
lm(qrange ~ site + as.character(month), data = df_temp_2) %>%
  summary()
lm(mean ~ site + as.character(month), data = df_temp_2) %>%
  summary()
