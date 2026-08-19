library('tidyverse')
library('broom.mixed')

table1 <- 
  list(
    df_3 %>% 
      summarise(n = n(),
                sex. = mean(sex == 'FEMALE', na.rm = T) * 100,
                age = mean(age, na.rm = T),
                diab = mean(diab == 'yes', na.rm = T) * 100,
                hpt = mean(hpt == 'yes', na.rm = T) * 100,
                whr = mean((sex == 'MALE' & whr > .9) |
                             (sex == 'FEMALE' & whr > .85), na.rm = T) * 100,
                smo0 = mean(smo == 'NEVER', na.rm = T) * 100,
                smo1 = mean(smo == 'CURRENT', na.rm = T) * 100,
                smo2 = mean(smo == 'EX_SMOKER', na.rm = T) * 100,
                alc0 = mean(alc == 0, na.rm = T) * 100,
                alc1 = mean(alc == 1 | alc == 2, na.rm = T) * 100,
                alc2 = mean(alc == 3 | alc == 4, na.rm = T) * 100,
                phy0 = mean(phy == 0, na.rm = T) * 100,
                phy1 = mean(phy == 1 | phy == 2, na.rm = T) * 100,
                phy2 = mean(phy == 3 | phy == 4, na.rm = T) * 100,
                ses = mean(ses, na.rm = T) * 100,
                sbp = mean(sbp, na.rm = T),
                dbp = mean(dbp, na.rm = T),
                pp = mean(pp, na.rm = T),
                hr = mean(hr, na.rm = T)) %>% 
      pivot_longer(cols = everything(), names_to = 'var', values_to = 'all'),
    df_3 %>% 
      filter(o3_24h < (60 - mean(df_1$o3_24h, na.rm = T)) / sd(df_1$o3_24h, na.rm = T)) %>% 
      summarise(n = n(),
                sex. = mean(sex == 'FEMALE', na.rm = T) * 100,
                age = mean(age, na.rm = T),
                diab = mean(diab == 'yes', na.rm = T) * 100,
                hpt = mean(hpt == 'yes', na.rm = T) * 100,
                whr = mean((sex == 'MALE' & whr > .9) |
                             (sex == 'FEMALE' & whr > .85), na.rm = T) * 100,
                smo0 = mean(smo == 'NEVER', na.rm = T) * 100,
                smo1 = mean(smo == 'CURRENT', na.rm = T) * 100,
                smo2 = mean(smo == 'EX_SMOKER', na.rm = T) * 100,
                alc0 = mean(alc == 0, na.rm = T) * 100,
                alc1 = mean(alc == 1 | alc == 2, na.rm = T) * 100,
                alc2 = mean(alc == 3 | alc == 4, na.rm = T) * 100,
                phy0 = mean(phy == 0, na.rm = T) * 100,
                phy1 = mean(phy == 1 | phy == 2, na.rm = T) * 100,
                phy2 = mean(phy == 3 | phy == 4, na.rm = T) * 100,
                ses = mean(ses, na.rm = T) * 100,
                sbp = mean(sbp, na.rm = T),
                dbp = mean(dbp, na.rm = T),
                pp = mean(pp, na.rm = T),
                hr = mean(hr, na.rm = T)) %>% 
      pivot_longer(cols = everything(), names_to = 'var', values_to = 'o3_low'),
    df_3 %>% 
      filter(o3_24h >= (60 - mean(df_1$o3_24h, na.rm = T)) / sd(df_1$o3_24h, na.rm = T)) %>% 
      summarise(n = n(),
                sex. = mean(sex == 'FEMALE', na.rm = T) * 100,
                age = mean(age, na.rm = T),
                diab = mean(diab == 'yes', na.rm = T) * 100,
                hpt = mean(hpt == 'yes', na.rm = T) * 100,
                whr = mean((sex == 'MALE' & whr > .9) |
                             (sex == 'FEMALE' & whr > .85), na.rm = T) * 100,
                smo0 = mean(smo == 'NEVER', na.rm = T) * 100,
                smo1 = mean(smo == 'CURRENT', na.rm = T) * 100,
                smo2 = mean(smo == 'EX_SMOKER', na.rm = T) * 100,
                alc0 = mean(alc == 0, na.rm = T) * 100,
                alc1 = mean(alc == 1 | alc == 2, na.rm = T) * 100,
                alc2 = mean(alc == 3 | alc == 4, na.rm = T) * 100,
                phy0 = mean(phy == 0, na.rm = T) * 100,
                phy1 = mean(phy == 1 | phy == 2, na.rm = T) * 100,
                phy2 = mean(phy == 3 | phy == 4, na.rm = T) * 100,
                ses = mean(ses, na.rm = T) * 100,
                sbp = mean(sbp, na.rm = T),
                dbp = mean(dbp, na.rm = T),
                pp = mean(pp, na.rm = T),
                hr = mean(hr, na.rm = T)) %>% 
      pivot_longer(cols = everything(), names_to = 'var', values_to = 'o3_high'),
    df_3 %>% 
      filter(temp_24h < -mean(df_1$temp_24h, na.rm = T) / sd(df_1$temp_24h, na.rm = T)) %>% 
      summarise(n = n(),
                sex. = mean(sex == 'FEMALE', na.rm = T) * 100,
                age = mean(age, na.rm = T),
                diab = mean(diab == 'yes', na.rm = T) * 100,
                hpt = mean(hpt == 'yes', na.rm = T) * 100,
                whr = mean((sex == 'MALE' & whr > .9) |
                             (sex == 'FEMALE' & whr > .85), na.rm = T) * 100,
                smo0 = mean(smo == 'NEVER', na.rm = T) * 100,
                smo1 = mean(smo == 'CURRENT', na.rm = T) * 100,
                smo2 = mean(smo == 'EX_SMOKER', na.rm = T) * 100,
                alc0 = mean(alc == 0, na.rm = T) * 100,
                alc1 = mean(alc == 1 | alc == 2, na.rm = T) * 100,
                alc2 = mean(alc == 3 | alc == 4, na.rm = T) * 100,
                phy0 = mean(phy == 0, na.rm = T) * 100,
                phy1 = mean(phy == 1 | phy == 2, na.rm = T) * 100,
                phy2 = mean(phy == 3 | phy == 4, na.rm = T) * 100,
                ses = mean(ses, na.rm = T) * 100,
                sbp = mean(sbp, na.rm = T),
                dbp = mean(dbp, na.rm = T),
                pp = mean(pp, na.rm = T),
                hr = mean(hr, na.rm = T)) %>% 
      pivot_longer(cols = everything(), names_to = 'var', values_to = 'temp_low'),
    df_3 %>% 
      filter(temp_24h >= -mean(df_1$temp_24h, na.rm = T) / sd(df_1$temp_24h, na.rm = T)) %>% 
      summarise(n = n(),
                sex. = mean(sex == 'FEMALE', na.rm = T) * 100,
                age = mean(age, na.rm = T),
                diab = mean(diab == 'yes', na.rm = T) * 100,
                hpt = mean(hpt == 'yes', na.rm = T) * 100,
                whr = mean((sex == 'MALE' & whr > .9) |
                             (sex == 'FEMALE' & whr > .85), na.rm = T) * 100,
                smo0 = mean(smo == 'NEVER', na.rm = T) * 100,
                smo1 = mean(smo == 'CURRENT', na.rm = T) * 100,
                smo2 = mean(smo == 'EX_SMOKER', na.rm = T) * 100,
                alc0 = mean(alc == 0, na.rm = T) * 100,
                alc1 = mean(alc == 1 | alc == 2, na.rm = T) * 100,
                alc2 = mean(alc == 3 | alc == 4, na.rm = T) * 100,
                phy0 = mean(phy == 0, na.rm = T) * 100,
                phy1 = mean(phy == 1 | phy == 2, na.rm = T) * 100,
                phy2 = mean(phy == 3 | phy == 4, na.rm = T) * 100,
                ses = mean(ses, na.rm = T) * 100,
                sbp = mean(sbp, na.rm = T),
                dbp = mean(dbp, na.rm = T),
                pp = mean(pp, na.rm = T),
                hr = mean(hr, na.rm = T)) %>% 
      pivot_longer(cols = everything(), names_to = 'var', values_to = 'temp_high'),
    df_3 %>% 
      filter(sbp < 140) %>% 
      summarise(n = n(),
                sex. = mean(sex == 'FEMALE', na.rm = T) * 100,
                age = mean(age, na.rm = T),
                diab = mean(diab == 'yes', na.rm = T) * 100,
                hpt = mean(hpt == 'yes', na.rm = T) * 100,
                whr = mean((sex == 'MALE' & whr > .9) |
                             (sex == 'FEMALE' & whr > .85), na.rm = T) * 100,
                smo0 = mean(smo == 'NEVER', na.rm = T) * 100,
                smo1 = mean(smo == 'CURRENT', na.rm = T) * 100,
                smo2 = mean(smo == 'EX_SMOKER', na.rm = T) * 100,
                alc0 = mean(alc == 0, na.rm = T) * 100,
                alc1 = mean(alc == 1 | alc == 2, na.rm = T) * 100,
                alc2 = mean(alc == 3 | alc == 4, na.rm = T) * 100,
                phy0 = mean(phy == 0, na.rm = T) * 100,
                phy1 = mean(phy == 1 | phy == 2, na.rm = T) * 100,
                phy2 = mean(phy == 3 | phy == 4, na.rm = T) * 100,
                ses = mean(ses, na.rm = T) * 100,
                sbp = mean(sbp, na.rm = T),
                dbp = mean(dbp, na.rm = T),
                pp = mean(pp, na.rm = T),
                hr = mean(hr, na.rm = T)) %>% 
      pivot_longer(cols = everything(), names_to = 'var', values_to = 'sbp_low'),
    df_3 %>% 
      filter(sbp >= 140) %>% 
      summarise(n = n(),
                sex. = mean(sex == 'FEMALE', na.rm = T) * 100,
                age = mean(age, na.rm = T),
                diab = mean(diab == 'yes', na.rm = T) * 100,
                hpt = mean(hpt == 'yes', na.rm = T) * 100,
                whr = mean((sex == 'MALE' & whr > .9) |
                             (sex == 'FEMALE' & whr > .85), na.rm = T) * 100,
                smo0 = mean(smo == 'NEVER', na.rm = T) * 100,
                smo1 = mean(smo == 'CURRENT', na.rm = T) * 100,
                smo2 = mean(smo == 'EX_SMOKER', na.rm = T) * 100,
                alc0 = mean(alc == 0, na.rm = T) * 100,
                alc1 = mean(alc == 1 | alc == 2, na.rm = T) * 100,
                alc2 = mean(alc == 3 | alc == 4, na.rm = T) * 100,
                phy0 = mean(phy == 0, na.rm = T) * 100,
                phy1 = mean(phy == 1 | phy == 2, na.rm = T) * 100,
                phy2 = mean(phy == 3 | phy == 4, na.rm = T) * 100,
                ses = mean(ses, na.rm = T) * 100,
                sbp = mean(sbp, na.rm = T),
                dbp = mean(dbp, na.rm = T),
                pp = mean(pp, na.rm = T),
                hr = mean(hr, na.rm = T)) %>% 
      pivot_longer(cols = everything(), names_to = 'var', values_to = 'sbp_high'),
    df_3 %>% 
      filter(dbp < 90) %>% 
      summarise(n = n(),
                sex. = mean(sex == 'FEMALE', na.rm = T) * 100,
                age = mean(age, na.rm = T),
                diab = mean(diab == 'yes', na.rm = T) * 100,
                hpt = mean(hpt == 'yes', na.rm = T) * 100,
                whr = mean((sex == 'MALE' & whr > .9) |
                             (sex == 'FEMALE' & whr > .85), na.rm = T) * 100,
                smo0 = mean(smo == 'NEVER', na.rm = T) * 100,
                smo1 = mean(smo == 'CURRENT', na.rm = T) * 100,
                smo2 = mean(smo == 'EX_SMOKER', na.rm = T) * 100,
                alc0 = mean(alc == 0, na.rm = T) * 100,
                alc1 = mean(alc == 1 | alc == 2, na.rm = T) * 100,
                alc2 = mean(alc == 3 | alc == 4, na.rm = T) * 100,
                phy0 = mean(phy == 0, na.rm = T) * 100,
                phy1 = mean(phy == 1 | phy == 2, na.rm = T) * 100,
                phy2 = mean(phy == 3 | phy == 4, na.rm = T) * 100,
                ses = mean(ses, na.rm = T) * 100,
                sbp = mean(sbp, na.rm = T),
                dbp = mean(dbp, na.rm = T),
                pp = mean(pp, na.rm = T),
                hr = mean(hr, na.rm = T)) %>% 
      pivot_longer(cols = everything(), names_to = 'var', values_to = 'dbp_low'),
    df_3 %>% 
      filter(dbp >= 90) %>% 
      summarise(n = n(),
                sex. = mean(sex == 'FEMALE', na.rm = T) * 100,
                age = mean(age, na.rm = T),
                diab = mean(diab == 'yes', na.rm = T) * 100,
                hpt = mean(hpt == 'yes', na.rm = T) * 100,
                whr = mean((sex == 'MALE' & whr > .9) |
                             (sex == 'FEMALE' & whr > .85), na.rm = T) * 100,
                smo0 = mean(smo == 'NEVER', na.rm = T) * 100,
                smo1 = mean(smo == 'CURRENT', na.rm = T) * 100,
                smo2 = mean(smo == 'EX_SMOKER', na.rm = T) * 100,
                alc0 = mean(alc == 0, na.rm = T) * 100,
                alc1 = mean(alc == 1 | alc == 2, na.rm = T) * 100,
                alc2 = mean(alc == 3 | alc == 4, na.rm = T) * 100,
                phy0 = mean(phy == 0, na.rm = T) * 100,
                phy1 = mean(phy == 1 | phy == 2, na.rm = T) * 100,
                phy2 = mean(phy == 3 | phy == 4, na.rm = T) * 100,
                ses = mean(ses, na.rm = T) * 100,
                sbp = mean(sbp, na.rm = T),
                dbp = mean(dbp, na.rm = T),
                pp = mean(pp, na.rm = T),
                hr = mean(hr, na.rm = T)) %>% 
      pivot_longer(cols = everything(), names_to = 'var', values_to = 'dbp_high'),
    df_3 %>% 
      filter(hpt == 'no' & hpt_med == 'no') %>% 
      summarise(n = n(),
                sex. = mean(sex == 'FEMALE', na.rm = T) * 100,
                age = mean(age, na.rm = T),
                diab = mean(diab == 'yes', na.rm = T) * 100,
                hpt = mean(hpt == 'yes', na.rm = T) * 100,
                whr = mean((sex == 'MALE' & whr > .9) |
                             (sex == 'FEMALE' & whr > .85), na.rm = T) * 100,
                smo0 = mean(smo == 'NEVER', na.rm = T) * 100,
                smo1 = mean(smo == 'CURRENT', na.rm = T) * 100,
                smo2 = mean(smo == 'EX_SMOKER', na.rm = T) * 100,
                alc0 = mean(alc == 0, na.rm = T) * 100,
                alc1 = mean(alc == 1 | alc == 2, na.rm = T) * 100,
                alc2 = mean(alc == 3 | alc == 4, na.rm = T) * 100,
                phy0 = mean(phy == 0, na.rm = T) * 100,
                phy1 = mean(phy == 1 | phy == 2, na.rm = T) * 100,
                phy2 = mean(phy == 3 | phy == 4, na.rm = T) * 100,
                ses = mean(ses, na.rm = T) * 100,
                sbp = mean(sbp, na.rm = T),
                dbp = mean(dbp, na.rm = T),
                pp = mean(pp, na.rm = T),
                hr = mean(hr, na.rm = T)) %>% 
      pivot_longer(cols = everything(), names_to = 'var', values_to = 'hpt_neg'),
    df_1 %>% 
      filter(hpt == 'yes' | hpt_med == 'yes') %>% 
      summarise(n = n(),
                sex. = mean(sex == 'FEMALE', na.rm = T) * 100,
                age = mean(age, na.rm = T),
                diab = mean(diab == 'yes', na.rm = T) * 100,
                hpt = mean(hpt == 'yes', na.rm = T) * 100,
                whr = mean((sex == 'MALE' & whr > .9) |
                             (sex == 'FEMALE' & whr > .85), na.rm = T) * 100,
                smo0 = mean(smo == 'NEVER', na.rm = T) * 100,
                smo1 = mean(smo == 'CURRENT', na.rm = T) * 100,
                smo2 = mean(smo == 'EX_SMOKER', na.rm = T) * 100,
                alc0 = mean(alc == 0, na.rm = T) * 100,
                alc1 = mean(alc == 1 | alc == 2, na.rm = T) * 100,
                alc2 = mean(alc == 3 | alc == 4, na.rm = T) * 100,
                phy0 = mean(phy == 0, na.rm = T) * 100,
                phy1 = mean(phy == 1 | phy == 2, na.rm = T) * 100,
                phy2 = mean(phy == 3 | phy == 4, na.rm = T) * 100,
                ses = mean(ses, na.rm = T) * 100,
                sbp = mean(sbp, na.rm = T),
                dbp = mean(dbp, na.rm = T),
                pp = mean(pp, na.rm = T),
                hr = mean(hr, na.rm = T)) %>% 
      pivot_longer(cols = everything(), names_to = 'var', values_to = 'hpt_pos'),
    df_1 %>% 
      summarise(sex. = sum(is.na(sex)),
                age = sum(is.na(age)),
                diab = sum(is.na(diab)),
                hpt = sum(is.na(hpt)),
                whr = sum(is.na(whr)),
                smo2 = sum(is.na(smo)),
                alc2 = sum(is.na(alc)),
                phy2 = sum(is.na(phy)),
                ses = sum(is.na(ses))) %>% 
      pivot_longer(cols = everything(), names_to = 'var', values_to = 'n_miss')) %>%
  reduce(left_join, by = 'var') %>% 
  mutate(across(-c('var', 'n_miss'), \(x) round(x, 1)))

writexl::write_xlsx(table1, here::here(paste0(Sys.Date(), '_table1.xlsx')))

df_1 %>% summarise(quantile(sbp,   c(.33, .5, .67), na.rm = T))
df_1 %>% summarise(quantile(dbp,   c(.33, .5, .67), na.rm = T))
df_1 %>% summarise(quantile(pp,    c(.33, .5, .67), na.rm = T))
df_1 %>% summarise(quantile(hr, c(.33, .5, .67), na.rm = T))

df_1 %>% summarise(quantile(temp_24h, c(.33, .5, .67), na.rm = T))
df_1 %>% summarise(quantile(o3_24h,   c(.33, .5, .67), na.rm = T))

df_1 %>% group_by(month = month(date, label = T) %>% factor(ordered = F)) %>% count()
df_1 %>% group_by(wday = wday(date, label = T) %>% factor(ordered = F)) %>% count()

lme4::lmer(o3_24h ~ 0 + site + (1|year) + (1|month) + (1|wday),
           df_1 %>% mutate(month = month(date, label = T) %>% factor(ordered = F),
                           year = factor(year, ordered = F),
                           wday = wday(date, label = T) %>% factor(ordered = F))) %>% tidy()

lme4::lmer(temp_24h ~ 0 + site + (1|year) + (1|month) + (1|wday),
           df_1 %>% mutate(month = month(date, label = T) %>% factor(ordered = F),
                           year = factor(year, ordered = F),
                           wday = wday(date, label = T) %>% factor(ordered = F))) %>% tidy()

lme4::lmer(pm10_24h ~ 0 + site + (1|year) + (1|month) + (1|wday),
           df_1 %>% mutate(month = month(date, label = T) %>% factor(ordered = F),
                           year = factor(year, ordered = F),
                           wday = wday(date, label = T) %>% factor(ordered = F))) %>% tidy()

df_1 %>% group_by(site) %>% summarise(quantile(o3_24h,   c(.5), na.rm = T))
df_1 %>% group_by(site) %>% summarise(quantile(temp_24h, c(.5), na.rm = T))
df_1 %>% group_by(site) %>% summarise(quantile(pm10_24h, c(.5), na.rm = T))

df_1 %>% group_by(month = month(date, label = T)) %>% summarise(mean(o3_24h,   na.rm = T))
df_1 %>% group_by(month = month(date, label = T)) %>% summarise(mean(temp_24h, na.rm = T))
df_1 %>% group_by(month = month(date, label = T)) %>% summarise(mean(pm10_24h, na.rm = T))

with(df_1, cor(temp_24h, temp_168h, use = 'pairwise'))
with(df_1, cor(temp_24h, temp, use = 'pairwise'))
with(df_1, cor(o3_24h, o3_168h, use = 'pairwise'))
with(df_1, cor(o3_24h, o3, use = 'pairwise'))

table2 <-
  bind_rows(
    left_join(
      summarise(df_1 %>% filter(id %in% df_3$id) %>% select(o3_24h, temp_24h, pm10_24h, no2_24h) %>% ungroup, 
                across(everything(),
                       \(x) paste0(sprintf('%.1f', median(x, na.rm = T)), ' (',
                                   sprintf('%.1f', quantile(x, .25, na.rm = T)), ', ',
                                   sprintf('%.1f', quantile(x, .75, na.rm = T)), ')')
                ) %>% pivot_longer(cols = everything(), names_to = 'exposure', values_to = 'all')
      ),
      summarise(df_1 %>% filter(id %in% df_3$id) %>% select(site, o3_24h, temp_24h, pm10_24h, no2_24h) %>% group_by(site), 
                across(everything(),
                       \(x) paste0(sprintf('%.1f', median(x, na.rm = T)), ' (',
                                   sprintf('%.1f', quantile(x, .25, na.rm = T)), ', ',
                                   sprintf('%.1f', quantile(x, .75, na.rm = T)), ')')
                )) %>% pivot_longer(cols = -'site', names_to = 'exposure') %>% 
        pivot_wider(id_cols = 'exposure', names_from = 'site')
    ) %>% 
      bind_cols(
        df_1 %>% 
          filter(id %in% df_3$id) %>% 
          select(o3_24h, temp_24h, pm10_24h, no2_24h) %>% 
          ungroup %>% 
          cor(use = 'pairwise') %>% 
          as.data.frame() %>% 
          mutate(across(everything(), \(x) sprintf('%.2f', x)))
      ),
    left_join(
      summarise(df_1 %>% 
          filter(id %in% df_3$id) %>% 
                  mutate(month = month(date, label = F)) %>% 
                  filter(month %in% c(1:3, 10:12)) %>% 
                  select(o3_24h, temp_24h, pm10_24h, no2_24h) %>% 
                  ungroup,
                across(everything(),
                       \(x) paste0(sprintf('%.1f', median(x, na.rm = T)), ' (',
                                   sprintf('%.1f', quantile(x, .25, na.rm = T)), ', ',
                                   sprintf('%.1f', quantile(x, .75, na.rm = T)), ')')
                ) %>% pivot_longer(cols = everything(), names_to = 'exposure', values_to = 'all')
      ),
      summarise(df_1 %>% 
          filter(id %in% df_3$id) %>% 
                  mutate(month = month(date, label = F)) %>% 
                  filter(month %in% c(1:3, 10:12)) %>% 
                  select(site, o3_24h, temp_24h, pm10_24h, no2_24h) %>% 
                  group_by(site), 
                across(everything(),
                       \(x) paste0(sprintf('%.1f', median(x, na.rm = T)), ' (',
                                   sprintf('%.1f', quantile(x, .25, na.rm = T)), ', ',
                                   sprintf('%.1f', quantile(x, .75, na.rm = T)), ')')
                )) %>% pivot_longer(cols = -'site', names_to = 'exposure') %>% 
        pivot_wider(id_cols = 'exposure', names_from = 'site')
    ) %>% 
      bind_cols(
        df_1 %>% 
          filter(id %in% df_3$id) %>% 
          mutate(month = month(date, label = F)) %>% 
          filter(month %in% c(1:3, 10:12)) %>% 
          select(o3_24h, temp_24h, pm10_24h, no2_24h) %>% 
          ungroup %>% 
          cor(use = 'pairwise') %>% 
          as.data.frame() %>% 
          mutate(across(everything(), \(x) sprintf('%.2f', x)))
      ),
    left_join(
      summarise(
        df_1 %>% 
          filter(id %in% df_3$id) %>% 
          mutate(month = month(date, label = F)) %>% 
          filter(month %in% 4:9) %>% 
          select(o3_24h, temp_24h, pm10_24h, no2_24h) %>% 
          ungroup, 
        across(everything(),
               \(x) paste0(sprintf('%.1f', median(x, na.rm = T)), ' (',
                           sprintf('%.1f', quantile(x, .25, na.rm = T)), ', ',
                           sprintf('%.1f', quantile(x, .75, na.rm = T)), ')')
        ) %>% pivot_longer(cols = everything(), names_to = 'exposure', values_to = 'all')
      ),
      summarise(
        df_1 %>% 
          filter(id %in% df_3$id) %>% 
          mutate(month = month(date, label = F)) %>% 
          filter(month %in% 4:9) %>% 
          select(site, o3_24h, temp_24h, pm10_24h, no2_24h) %>% 
          group_by(site), 
        across(everything(),
               \(x) paste0(sprintf('%.1f', median(x, na.rm = T)), ' (',
                           sprintf('%.1f', quantile(x, .25, na.rm = T)), ', ',
                           sprintf('%.1f', quantile(x, .75, na.rm = T)), ')')
        )) %>% pivot_longer(cols = -'site', names_to = 'exposure') %>% 
        pivot_wider(id_cols = 'exposure', names_from = 'site')
    ) %>% 
      bind_cols(
        df_1 %>% 
          filter(id %in% df_3$id) %>% 
          mutate(month = month(date, label = F)) %>% 
          filter(month %in% 4:9) %>% 
          select(o3_24h, temp_24h, pm10_24h, no2_24h) %>% 
          ungroup %>% 
          cor(use = 'pairwise') %>% 
          as.data.frame() %>% 
          mutate(across(everything(), \(x) sprintf('%.2f', x)))
      )
  )
  
writexl::write_xlsx(table2, here::here(paste0(Sys.Date(), '_table2.xlsx')))

cowplot::plot_grid(
  df_1 %>% filter(id %in% df_3$id) %>% 
    left_join(df_3 %>% select(id, season)) %>% 
    select(site, season, temp_24h) %>%
    mutate(site = case_match(site,
                             'sth' ~ 'Stockholm',
                             'mal' ~ 'Malm\u00f6',
                             'got' ~ 'Gothenburg'
    )) %>% 
    ggplot(aes(x = temp_24h, y = site, fill = season)) +
    geom_vline(aes(xintercept = 0), linetype = 'dotted') +
    geom_violin(trim = F, position = position_dodge(.8)) +
    geom_boxplot(colour = 'black', width = .1, outliers = F, position = position_dodge(.8)) +
    scale_fill_manual(values = c('lightblue', '#FF7F7F')) +
    scale_x_continuous(limits = c(-20, 30), expand = c(0, 0)) +
    labs(y = NULL, x = 'Temperature, 24h mean [\u00b0C]') +
    theme_classic() +
    theme(legend.position = 'none'),
  
  df_1 %>% filter(id %in% df_3$id) %>% 
    left_join(df_3 %>% select(id, season)) %>% 
    select(site, season, o3_24h) %>%
    mutate(site = case_match(site,
                             'sth' ~ 'Stockholm',
                             'mal' ~ 'Malm\u00f6',
                             'got' ~ 'Gothenburg'
    )) %>% 
    ggplot(aes(x = o3_24h, y = site, fill = season)) +
    geom_violin(trim = F, position = position_dodge(.8)) +
    geom_boxplot(colour = 'black', width = .1, outliers = F, position = position_dodge(.8)) +
    scale_fill_manual(values = c('lightblue', '#FF7F7F')) +
    scale_x_continuous(expand = c(0,0)) +
    coord_cartesian(xlim = c(0, 120)) +
    labs(y = NULL, x = 'O3, 24h mean [\u00b5g/m\u00b3]') +
    theme_classic() +
    theme(legend.position = 'none'),
  NULL,
  rel_widths = c(0.49, .49, .02),
  nrow = 1,
  labels = c('A', 'B', '')
)
ggsave(here::here('figures', paste0(Sys.Date(), '_expo_dist.png')), width = 8, height = 3)
       