library('tidyverse')

source(here::here('00_00_functions.R'))
source(here::here('01_01_temp data import.R'))
source(here::here('01_02_air data import.R'))
source(here::here('01_03_humidity data import.R'))

## Prepare a combined dataset for EDA ----------------------------------------------------------------------------------
df_eda_1 <- left_join(df_temp_1 %>% filter(qual) %>% select(-qual),
                      df_humid_1 %>% filter(qual) %>% select(-qual),
                      by = c('date', 'time', 'site')) %>%
  left_join(df_air_1 %>%
              mutate(time = hms::as_hms(start)) %>%
              group_by(date, time, site, pollutant) %>%
              summarise(exposure = mean(as.double(exposure), na.rm = T)) %>%
              pivot_wider(id_cols = c('date', 'time', 'site'),
                          names_from = 'pollutant',
                          values_from = 'exposure'),
            by = c('date', 'time', 'site'))

## Correlation table ---------------------------------------------------------------------------------------------------
df_eda_1 %>%
  select(temp, humi, no2, o3, pm10, pm25) %>%
  cor(use = 'pairwise.complete.obs') %>%
  as.data.frame() %>%
  rownames_to_column('var1') %>%
  pivot_longer(cols = -var1, names_to = 'var2', values_to = 'r') %>%
  ggplot(aes(x = var1, y = var2, fill = r, label = round(r, 2))) +
  geom_tile() +
  geom_text() +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(legend.position = 'none') +
  ggtitle('Correlations between hourly environmental exposures')
ggsave(here::here('figures', paste0(Sys.Date(), '_cor.svg')), width = 5, height = 5)

df_eda_2 <- df_eda_1 %>%
  group_by(date, site) %>%
  summarise(across(c(humi, no2, o3, pm10, pm25), ~mean(.x, na.rm = T))) %>%
  left_join(df_wind_2, by = c('date', 'site'))

## Descriptive analyses of air pollution measurement -------------------------------------------------------------------
# Calculate summary exposure metrics
# this relies on a custom iqr function, bc base::IQR disrupts calculations when encountering an NA value
df_air_2 <- df_air_1 %>%
  group_by(date, site, stn, pollutant) %>%
  summarise(min = min(exposure),
            max = max(exposure),
            mean = mean(exposure),
            mean_d = mean(exposure[hour(start) >= 8 & hour(start) < 20]),
            mean_n = mean(exposure[hour(start) < 8 | hour(start) >= 20]),
            median = median(exposure),
            cv = cv(exposure),
            cv_d = cv(exposure[hour(start) >= 8 & hour(start) < 20]),
            cv_n = cv(exposure[hour(start) < 8 | hour(start) >= 20])) %>%
  mutate(month = month(date, label = T))

# Missingness per year
df_air_1 %>%
  group_by(site, year, pollutant = str_to_upper(pollutant)) %>%
  summarise(p = mean(!is.na(exposure))) %>%
  filter(p != 0) %>%
  ggplot(aes(x = year,
             y = site,
             fill = p,
             label = sprintf('%.1f', round(100 * p, 1)))) +
  geom_tile() +
  geom_text(colour = 'white') +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_gradient(low = 'white', high = 'darkblue', limits = c(0, 1), labels = scales::percent) +
  labs(x = 'Year', y = 'Site', fill = 'Measurement\ncompleteness') +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA)) +
  facet_wrap(vars(pollutant)) +
  ggtitle('Yearly completeness in hourly air pollution measurements per pollutant and site')
ggsave(here::here('figures', paste0(Sys.Date(), '_AP_yearly_completeness.svg')), width = 10, height = 10)

# Missingness per month
df_air_2 %>%
  mutate(ym = floor_date(date, "month")) %>%
  group_by(ym, site, pollutant = str_to_upper(pollutant)) %>%
  summarise(p = mean(!is.na(mean))) %>%
  filter(p != 0) %>%
  ggplot() +
  geom_tile(aes(x = site, y = ym, height = 32, fill = p)) +
  geom_tile(data = readxl::read_xlsx(path = here::here('data SCAPIS', 'SCAPIS_per_month.xlsx')) %>%
    pivot_longer(cols = -date, names_to = 'site', values_to = 'n_SCAPIS') %>%
    mutate(date = as.Date(date)) %>%
    filter(!is.na(n_SCAPIS)),
            aes(x = site, y = date, width = n_SCAPIS / 500, height = 32), fill = 'red') +
  scale_y_continuous(expand = c(0, 0),
                     breaks = as.Date.character(paste0(2013:2018, '-01-01')),
                     labels = paste(2013:2018, 'Jan')) +
  labs(x = 'Site', y = NULL, fill = 'Measurement\ncompleteness') +
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_gradient(low = 'white', high = 'darkblue', limits = c(0, 1), labels = scales::percent) +
  facet_wrap(vars(pollutant)) +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA)) +
  ggtitle('Monthly completeness in hourly air pollution measurements per site, overlayed with SCAPIS recruitment')
ggsave(here::here('figures', paste0(Sys.Date(), '_AP_monthly_completeness.svg')), width = 10, height = 10)

# Seasonal variation
df_air_2 %>%
  group_by(month, stn, site, pollutant) %>%
  summarise(cv = median(cv, na.rm = T)) %>%
  ggplot(aes(x = month, y = cv, group = stn, fill = site)) +
  geom_col(position = 'dodge') +
  geom_abline(aes(intercept = 0, slope = 0)) +
  geom_text(aes(label = str_to_upper(pollutant), x = 1, y = .75), fontface = "bold") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, .82)) +
  labs(y = 'Monthly median coefficient of variation', x = NULL) +
  facet_grid(vars(pollutant)) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text = element_blank()
  ) +
  ggtitle('Seasonal variation in the coefficient of variation for four air pollutants')
ggsave(here::here('figures', paste(Sys.Date(), '_AP_CV_seasonal variation.svg')), width = 10, height = 10)

# Coefficients of variation
df_air_2 %>%
  group_by(stn, month, pollutant) %>%
  summarise(cv = median(cv, na.rm = T)) %>%
  mutate(pollutant = str_to_upper(pollutant)) %>%
  ggplot(aes(x = stn,
             y = month,
             fill = cv,
             label = sprintf('%.1f', round(cv * 100, 1)))) +
  geom_tile() +
  geom_text(color = 'white') +
  scale_fill_gradient2(limits = c(0, 1),
                       low = 'darkred',
                       mid = 'white',
                       high = 'darkblue',
                       midpoint = .5,
                       labels = scales::percent) +
  labs(x = NULL, y = NULL, fill = 'Coefficient of\nvariation') +
  facet_grid(vars(pollutant)) +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA)) +
  ggtitle('Coefficient of variation for air pollution measurements, per monitoring station')
ggsave(here::here('figures', paste(Sys.Date(), '_AP_CV_heatmap.svg')), width = 10, height = 10)

# Correlations between exposure metrics
df_air_2 %>%
  ungroup() %>%
  group_by(pollutant) %>%
  select(pollutant, min, max, mean, mean_d, mean_n, median, cv, cv_d, cv_n) %>%
  summarise(across(everything(),
                   .fns = list(
                     cor_mean = ~cor(mean, .x, use = 'pairwise.complete.obs'),
                     cor_mean_d = ~cor(mean_d, .x, use = 'pairwise.complete.obs'),
                     cor_mean_n = ~cor(mean_n, .x, use = 'pairwise.complete.obs'),
                     cor_min = ~cor(min, .x, use = 'pairwise.complete.obs'),
                     cor_max = ~cor(max, .x, use = 'pairwise.complete.obs'),
                     cor_median = ~cor(median, .x, use = 'pairwise.complete.obs'),
                     cor_cv = ~cor(cv, .x, use = 'pairwise.complete.obs'),
                     cor_cv_d = ~cor(cv_d, .x, use = 'pairwise.complete.obs'),
                     cor_cv_n = ~cor(cv_n, .x, use = 'pairwise.complete.obs')),
                   .names = '{.fn}.{.col}')) %>%
  pivot_longer(-pollutant, names_to = 'var', values_to = 'r', names_prefix = 'cor_') %>%
  separate_wider_delim(var, names = c('var1', 'var2'), delim = '.') %>%
  mutate(pollutant = str_to_upper(pollutant)) %>%
  ggplot(aes(x = var1,
             y = var2,
             fill = r,
             label = sprintf('%.2f', round(r, 2)))) +
  geom_tile() +
  geom_text() +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_gradient2(limits = c(-1, 1),
                       low = 'darkred',
                       mid = 'white',
                       high = 'darkblue') +
  labs(x = NULL, y = NULL, fill = 'Pearson\'s r') +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA)) +
  facet_wrap(vars(pollutant)) +
  ggtitle('Correlations between various summary metrics of air pollution')
ggsave(here::here('figures', paste0(Sys.Date(), '_cor_AP.svg')), width = 10, height = 10)

# Lag correlations
df_air_1 %>%
  mutate(season = 'All') %>%
  bind_rows(df_air_1 %>%
              mutate(season = if_else((month > 6 | (month == 6 & day >= 21)) &
                                        (month < 9 | (month == 9 & day < 22)),
                                      true = 'Summertime (spring to autumn equinox)',
                                      false = 'Wintertime (autumn to spring equinox)'))) %>%
  mutate(daytime = if_else(hour >= 8 & hour < 20, 'Daytime (8 am to 7 pm)', 'Nighttime (8 pm to 7 am)')) %>%
  group_by(stn, pollutant, season) %>%
  do(data.frame(., setNames(data.table::shift(.$exposure, -72:72), paste0('lag', -72:72)))) %>%
  group_by(pollutant, season, daytime) %>%
  summarise(
    across(
      contains("lag"),
      ~cor(.x, exposure, use = "pairwise.complete.obs"),
      .names = "cor_{.col}"
    )
  ) %>%
  pivot_longer(-c(pollutant, season, daytime),
               names_to = 'lag',
               names_prefix = 'cor_lag',
               values_to = 'r') %>%
  mutate(lag = str_replace(lag, '\\.', '-') %>% as.integer,
         pollutant = str_to_upper(pollutant)) %>%
  ggplot(aes(x = lag, y = r, colour = season)) +
  geom_line(size = 1) +
  geom_hline(aes(yintercept = 0), colour = 'black') +
  geom_segment(data = . %>% filter(season == 'All'), aes(x = lag, xend = lag, y = 0, yend = r)) +
  facet_wrap(vars(pollutant), ncol = 1) +
  scale_y_continuous(expand = c(0, 0), breaks = (1:4) / 4, labels = scales::percent) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_x_continuous(expand = c(0, 0), breaks = -2:3 * 24) +
  scale_colour_manual(values = c('black', 'darkred', 'darkblue')) +
  labs(y = 'Pearson\'s r', x = 'Lag (hours)', colour = 'Season') +
  facet_grid(pollutant ~ daytime) +
  theme_classic() +
  theme(
    legend.position = 'bottom',
    panel.border = element_rect(fill = NA),
    panel.grid.major = element_line(linetype = 'dashed')
  ) +
  ggtitle('Seasonal hourly lag correlations for four air pollutants')
ggsave(here::here('figures', paste0(Sys.Date(), '_cor_AP.svg')), width = 10, height = 10)

# Linear regression
lm(cv ~ site:pollutant +
  as.character(month):pollutant +
  pollutant, data = df_air_2) %>%
  summary()
lm(mean ~ site + as.character(month), data = df_temp_2) %>%
  summary()

## Temperature ---------------------------------------------------------------------------------------------------------
df_temp_1 %>%
  mutate(year = year(date)) %>%
  group_by(site, year, date) %>%
  summarise(temp = mean(temp)) %>%
  ggplot(aes(x = date, y = temp, colour = site)) +
  geom_line() +
  theme_classic() +
  facet_wrap(vars(year), scales = 'free_x')

df_temp_2 %>%
  group_by(month, site) %>%
  summarise(qrange = median(qrange)) %>%
  ggplot(aes(x = month, y = qrange, fill = site)) +
  geom_col() +
  facet_grid(vars(site)) +
  theme_classic() +
  ggtitle('Temp qrange, seasonal variation')
ggsave(here::here('figures', paste(Sys.Date(), '_temp_qrange_seasonal variation.svg')), width = 5, height = 5)

df_temp_2 %>%
  group_by(year, month, site) %>%
  summarise(mean = mean(mean)) %>%
  ggplot(aes(x = month, y = mean, fill = site)) +
  geom_col() +
  facet_grid(site ~ year) +
  theme_classic() +
  ggtitle('Temp mean, seasonal variation')
ggsave(here::here('figures', paste(Sys.Date(), '_temp_mean_seasonal variation.svg')), width = 5, height = 5)

# Linear regression
lm(qrange ~ site + as.character(month), data = df_temp_2) %>%
  summary()
lm(mean ~ site + as.character(month), data = df_temp_2) %>%
  summary()

## Humidity ------------------------------------------------------------------------------------------------------------
df_humid_1 %>%
  mutate(year = year(date)) %>%
  group_by(site, year, date) %>%
  summarise(humi = mean(humi)) %>%
  ggplot(aes(x = date, y = humi, colour = site)) +
  geom_line() +
  theme_classic() +
  facet_wrap(vars(year), scales = 'free_x')

## Wind ----------------------------------------------------------------------------------------------------------------
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