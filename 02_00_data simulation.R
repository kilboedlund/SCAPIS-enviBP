library('tidyverse')

source(here::here('01_01_temp data import.R'))
source(here::here('01_02_air data import.R'))
source(here::here('01_03_humidity data import.R'))

# Simple data simulator to construct a dataset with temperature, AP, and blood pressure with predetermined correlations
set.seed(2439)
N <- 30154
df_sim_1 <- data.frame(id = 1:N) %>%
  mutate(site = sample(c('got', 'sth', 'mal', 'lin', 'ume', 'upp'),
                       size = N,
                       replace = T),
         sex = sample(c('M', 'F'),
                      size = N,
                      replace = T),
         date = runif(n = N,
                      min = as.numeric(as.Date.character('2013-01-02', format = '%Y-%m-%d')),
                      max = as.numeric(as.Date.character('2018-12-30', format = '%Y-%m-%d'))) %>%
           round() %>%
           as.Date(),
         temp_mean = sample(df_temp_2$mean,
                            size = N,
                            replace = T),
         ap_pm25_mean = sample(df_air_2 %>%
                                 filter(pollutant == 'pm25', !is.na(mean)) %>%
                                 pull(mean),
                               size = N,
                               replace = T),
         ap_pm25_qrange = sample(df_air_2 %>%
                                   filter(pollutant == 'pm25', !is.na(qrange)) %>%
                                   pull(qrange),
                                 size = N,
                                 replace = T),
         ap_no2_mean = sample(df_air_2 %>%
                                filter(pollutant == 'no2', !is.na(mean)) %>%
                                pull(mean),
                              size = N,
                              replace = T),
         sbp = 125 +
           rnorm(n = N, mean = 0, sd = 15) +
           10 * (sex == 'M') +
           .19 * ap_pm25_mean +
           .14 * ap_no2_mean,
         dbp = 75 +
           5 * (sex == 'F') +
           .8 * (sbp - mean(sbp)) +
           .2 * rnorm(n = N, 0, 3),
         ht_med = rbinom(n = N, 3, .05 + .5 * (sbp > 140 | dbp > 90)),
         sbp_obs = sbp -
           10 * ht_med -
           .98 * (temp_mean - mean(temp_mean)) +
           .24 * ap_pm25_qrange,
         dbp_obs = dbp - 5 * ht_med,
         pp_obs = sbp_obs - dbp_obs)

df_sim_1 %>%
  pivot_longer(cols = contains('_obs'),
               names_to = 'reading',
               values_to = 'pressure') %>%
  ggplot(aes(x = bp, fill = reading)) +
  geom_histogram(binwidth = 5) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 7500)) +
  facet_wrap(vars(reading), ncol = 1, scales = 'free_x') +
  theme_classic() +
  theme(panel.grid.major.x = element_line())