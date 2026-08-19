library('mgcv')
library('gratia')
library('plotly')
source(here::here('00_00_functions.R'))

df_3 <- df_2 %>% 
  ungroup %>% 
  filter(!is.na(o3_24h) & !is.na(temp_24h) & !is.na(pm10_24h) & !is.na(no2_24h),
         !is.na(sbp) & !is.na(dbp) & !is.na(hr),
         !is.na(sex) & !is.na(age) & !is.na(diab) & !is.na(whr) & !is.na(smo) & !is.na(alc) & !is.na(phy) & !is.na(ses)) %>%
  select(-c(o3_24h, temp_24h)) %>% 
  left_join(df_1 %>% select(id, o3_24h, temp_24h), by = 'id')
  
gam_sbp <-
  gam(log(sbp) ~ te(o3_24h, temp_24h, k = 3) +
        s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
        sex + age + diab + whr + smo + alc + phy + ses + 
        year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
      data = df_3,
      method = 'REML')
#summary(gam_sbp)
#appraise(gam_sbp)

gam_dbp <-
  gam(log(dbp) ~ te(o3_24h, temp_24h, k = 3) +
        s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
        sex + age + diab + whr + smo + alc + phy + ses + 
        year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
      data = df_3,
      method = 'REML')

gam_pp <-
  gam(log(pp) ~ te(o3_24h, temp_24h, k = 3) +
        s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
        sex + age + diab + whr + smo + alc + phy + ses + 
        year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
      data = df_3,
      method = 'REML')

gam_hr <-
  gam(log(hr) ~ te(o3_24h, temp_24h, k = 3) +
        s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
        sex + age + diab + whr + smo + alc + phy + ses + 
        year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
      data = df_3,
      method = 'REML')

plot_gam_helper(gam_sbp, c(125.5, 131), 'SBP [mmHg]') %>% 
  config(
    toImageButtonOptions = list(
      format = 'png',
      filename = 'sbp_surface_900',
      width = 900,
      height = 900,
      scale = 1
    )
  )
plot_gam_helper(gam_sbp, c(125.5, 131), 'SBP [mmHg]') %>% 
  config(
    toImageButtonOptions = list(
      format = 'png',
      filename = 'sbp_surface_1200',
      width = 1200,
      height = 1200,
      scale = 1
    )
  )
plot_gam_helper(gam_dbp, c(75.5, 81), 'DBP [mmHg]') %>% 
  config(
    toImageButtonOptions = list(
      format = 'png',
      filename = 'dbp_surface_1200',
      width = 1200,
      height = 1200,
      scale = 1
    )
  )
plot_gam_helper(gam_dbp, c(75.5, 81), 'DBP [mmHg]') %>% 
  config(
    toImageButtonOptions = list(
      format = 'png',
      filename = 'dbp_surface_900',
      width = 900,
      height = 900,
      scale = 1
    )
  )
plot_gam_helper(gam_pp, c(46.5, 52), 'PP [mmHg]') %>% 
  config(
    toImageButtonOptions = list(
      format = 'png',
      filename = 'pp_surface_1200',
      width = 1200,
      height = 1200,
      scale = 1
    )
  )
plot_gam_helper(gam_pp, c(46.5, 52), 'PP [mmHg]') %>% 
  config(
    toImageButtonOptions = list(
      format = 'png',
      filename = 'pp_surface_900',
      width = 900,
      height = 900,
      scale = 1
    )
  )
plot_gam_helper(gam_hr, c(63.5, 69), 'HR [bpm]') %>% 
  config(
    toImageButtonOptions = list(
      format = 'png',
      filename = 'hr_surface_1200',
      width = 1200,
      height = 1200,
      scale = 1
    )
  )
plot_gam_helper(gam_hr, c(63.5, 69), 'HR [bpm]') %>% 
  config(
    toImageButtonOptions = list(
      format = 'png',
      filename = 'hr_surface_900',
      width = 900,
      height = 900,
      scale = 1
    )
  )
