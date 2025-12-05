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