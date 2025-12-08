library('tidyverse')
source(here::here('00_00_functions.R'))

# Read data files ------------------------------------------------------------------------------------------------------
df_air_1 <- list(
  pm25 = list(
    sth_8781 = air_data(8781, 'pm25', 9),
    got_8577 = air_data(8577, 'pm25', 9),
    mal_8773 = air_data(8773, 'pm25', 9),
    upp_34398 = air_data(34398, 'pm25', 9),
    upp_159404 = air_data(159404, 'pm25', 9),
    ume_13532 = air_data(13532, 'pm25', 9)
  ),
  pm10 = list(
    sth_8781 = air_data(8781, 'pm10', 9),
    got_8577 = air_data(8577, 'pm10', 9),
    mal_8773 = air_data(8773, 'pm10', 9),
    upp_34398 = air_data(34398, 'pm10', 9),
    upp_159404 = air_data(159404, 'pm10', 9),
    ume_13532 = air_data(13532, 'pm10', 9)
  ),
  no2 = list(
    sth_8781 = air_data(8781, 'no2', 9),
    got_8577 = air_data(8577, 'no2', 9),
    mal_8773 = air_data(8773, 'no2', 9),
    upp_34398 = air_data(34398, 'no2', 9),
    upp_159404 = air_data(159404, 'no2', 9),
    ume_13532 = air_data(13532, 'no2', 9)
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

## Missingness per year ------------------------------------------------------------------------------------------------
df_air_1 %>%
  group_by(site, year, pollutant = str_to_upper(pollutant)) %>%
  summarise(missingness_proportion = mean(is.na(exposure))) %>%
  ggplot(aes(x = year,
             y = site,
             fill = missingness_proportion,
             label = round(100 * missingness_proportion, 1))) +
  geom_tile() +
  geom_text() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA)) +
  facet_wrap(vars(pollutant))

# Calculate variability ------------------------------------------------------------------------------------------------
df_air_2 <- df_air_1 %>%
  group_by(date, site, stn, pollutant) %>%
  summarise(min = min(exposure),
            max = max(exposure),
            mean = mean(exposure),
            iqr = IQR(exposure),
            iqr_d = IQR(exposure[hour(start) >= 8 & hour(start) <  20]),
            iqr_n = IQR(exposure[hour(start) <  8 | hour(start) >= 20])) %>%
  mutate(month = month(date, label = T))

## Missingness per month
df_air_2 %>%
  mutate(ym = floor_date(date, "month")) %>%
  group_by(ym, site, pollutant = str_to_upper(pollutant)) %>%
  summarise(p = mean(is.na(mean))) %>%
  filter(p != 1) %>%
  ggplot() +
  geom_tile(aes(x = site, y = ym, height = 32, fill = p)) +
  geom_tile(data = readxl::read_xlsx(path = here::here('data SCAPIS', 'SCAPIS_per_month.xlsx')) %>%
    pivot_longer(cols = -date, names_to = 'site', values_to = 'n_SCAPIS') %>%
    mutate(date = as.Date(date)) %>%
    filter(!is.na(n_SCAPIS)),
            aes(x = site, y = date, width = n_SCAPIS / 500, height = 32), fill = '#f33c3a') +
  scale_y_continuous(expand = c(0, 0),
                     breaks = as.Date.character(paste0(2013:2018, '-01-01')),
                     labels = paste(2013:2018, 'Jan')) +
  scale_fill_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(x = 'Site', y = NULL, fill = 'Missingness') +
  scale_x_discrete(expand = c(0, 0)) +
  facet_wrap(vars(pollutant)) +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA)) +
  ggtitle('Monthly missingness in hourly air pollution measurements per site, overlayed with SCAPIS recruitment')
ggsave(here::here('figures', paste0(Sys.Date(), '_AP_monthly_missingness.svg')), width = 10, height = 10)

## Proof of concept graphs ---------------------------------------------------------------------------------------------
df_air_2  %>%
  group_by(month, stn, site, pollutant) %>%
  summarise(cv = median(cv, na.rm = T)) %>%
  ggplot(aes(x = month, y = cv, group = stn, fill = site)) +
  geom_col(position = 'dodge') +
  geom_abline(aes(intercept = 0, slope = 0)) +
  geom_text(aes(label = str_to_upper(pollutant), x = 1, y = .75), fontface = "bold") +
  scale_y_continuous(expand = c(0,0), limits = c(0,.82)) +
  labs(y = 'Monthly median coefficient of variation', x = NULL) +
  facet_grid(vars(pollutant)) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text = element_blank()
  ) +
  ggtitle('(Lack of) Seasonal variation in the coefficient of variation for four air pollutants')
ggsave(here::here('figures', paste(Sys.Date(), '_AP_CV_seasonal variation.svg')), width = 10, height = 10)

df_air_2  %>%
  group_by(stn, month, pollutant) %>%
  summarise(cv = median(cv, na.rm = T)) %>%
  ggplot(aes(x = stn,
             y = month,
             fill = cv,
             label = sprintf('%.1f', round(cv * 100, 1)))) +
  geom_tile() +
  geom_text(color = 'white') +
  labs(x = NULL, y = NULL) +
  facet_grid(vars(pollutant)) +
  theme_classic() +
  theme(legend.position = 'none') +
  ggtitle('Coefficient of variation for air pollution measurements, per monitoring station')
ggsave(here::here('figures', paste(Sys.Date(), '_AP_CV_heatmap.svg')), width = 10, height = 10)

## Linear regression ---------------------------------------------------------------------------------------------------
lm(cv ~ site:pollutant + as.character(month):pollutant + pollutant, data = df_air_2) %>%
  summary()
lm(mean ~ site + as.character(month), data = df_temp_2) %>%
  summary()
