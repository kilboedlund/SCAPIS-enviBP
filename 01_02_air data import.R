library('tidyverse')
source(here::here('00_00_functions.R'))

# Read data files ------------------------------------------------------------------------------------------------------
df_air_1 <- list(
  pm25 = list(
    sth_8781 = air_data(8781, 'pm25', 9),
    got_8577 = air_data(8577, 'pm25', 9),
    mal_8773 = air_data(8773, 'pm25', 9),
    upp_34398 = air_data(34398, 'pm25', 9),
    upp_159404 = air_data(159404, 'pm25', 9)
  ),
  pm10 = list(
    sth_8781 = air_data(8781, 'pm10', 9),
    got_8577 = air_data(8577, 'pm10', 9),
    mal_8773 = air_data(8773, 'pm10', 9),
    upp_34398 = air_data(34398, 'pm10', 9),
    upp_159404 = air_data(159404, 'pm10', 9)
  ),
  no2 = list(
    sth_8781 = air_data(8781, 'no2', 9),
    got_8577 = air_data(8577, 'no2', 9),
    mal_8773 = air_data(8773, 'no2', 9),
    upp_34398 = air_data(34398, 'no2', 9),
    upp_159404 = air_data(159404, 'no2', 9)
  ),
  o3 = list(
    sth_8781 = air_data(8781, 'o3', 9),
    got_9346 = air_data(9346, 'o3', 9),
    mal_8773 = air_data(8773, 'o3', 9),
    mal_8963 = air_data(8963, 'o3', 9)
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

## Missingness ---------------------------------------------------------------------------------------------------------
df_air_1 %>%
  group_by(site, year, pollutant) %>%
  summarise(missingness_proportion = mean(is.na(exposure))) %>%
  ggplot(aes(x = year,
             y = site,
             fill = missingness_proportion,
             label = round(100 * missingness_proportion, 1))) +
  geom_tile() +
  geom_text() +
  theme_classic() +
  theme(legend.position = 'none') +
  facet_wrap(vars(pollutant))

# Calculate variability ------------------------------------------------------------------------------------------------
df_air_2 <- df_air_1 %>%
  group_by(date, site, stn, pollutant) %>%
  summarise(min = min(exposure),
            max = max(exposure),
            mean = mean(exposure),
            qrange = quantile(exposure, probs = c(.05, .95), na.rm = T) %>% diff(),
            cv = sd(exposure, na.rm = T)/mean(exposure, na.rm = T)) %>%
  mutate(month = month(date, label = T))

## Proof of concept graphs ---------------------------------------------------------------------------------------------
df_air_2  %>%
  group_by(month, stn, pollutant) %>%
  summarise(cv = median(cv, na.rm = T)) %>%
  ggplot(aes(x = month, y = cv, fill = pollutant)) +
  geom_col() +
  facet_grid(vars(stn, pollutant)) +
  theme_classic() +
  ggtitle('AP coefficient of variation, seasonal variation')
ggsave(here::here('figures', paste(Sys.Date(), '_AP_CV_seasonal variation.svg')), width = 5, height = 5)

df_air_2  %>%
  group_by(stn, month, pollutant) %>%
  summarise(cv = median(cv, na.rm = T)) %>%
  ggplot(aes(x = stn, y = month, fill = cv, label = round(cv * 100, 1))) +
  geom_tile() +
  geom_text() +
  facet_grid(vars(pollutant)) +
  theme_classic() +
  ggtitle('AP coefficient of variation')
ggsave(here::here('figures', paste(Sys.Date(), '_AP_CV_heatmap.svg')), width = 5, height = 5)

## Linear regression ---------------------------------------------------------------------------------------------------
lm(cv ~ site:pollutant + as.character(month):pollutant + pollutant, data = df_air_2) %>%
  summary()
lm(mean ~ site + as.character(month), data = df_temp_2) %>%
  summary()
