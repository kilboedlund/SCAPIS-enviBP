library('mgcv')
library('gratia')
library('broom')
source(here::here('00_00_functions.R'))

# * 168 hours ------------------------------------------------------------------
df_3 <- df_2 %>% 
  ungroup %>% 
  filter(!is.na(o3_168h) & !is.na(temp_168h) & !is.na(pm10_168h) & !is.na(no2_168h),
         !is.na(sbp) & !is.na(dbp) & !is.na(hr),
         !is.na(sex) & !is.na(age) & !is.na(diab) & !is.na(whr) & !is.na(smo) & !is.na(alc) & !is.na(phy) & !is.na(ses)) %>%
  mutate(temp_168h_t = ntile(temp_168h, 3) %>% factor(),
         temp_ap_t = ntile(temp_ap, 3) %>% factor(),
         o3_168h_t = ntile(o3_168h, 3) %>% factor(),
         o3_t = ntile(o3, 3) %>% factor())

# 1 SBP ------------------------------------------------------------------------
fit_sbp_m1t_lt_168h <-
  gam(
    formula = log(sbp) ~ temp_168h + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_sbp_m1t_lt_168h)
#appraise(fit_sbp_m1t_lt_168h)
#draw(fit_sbp_m1t_lt_168h)

fit_sbp_m1t_st_168h <-
  gam(
    formula = log(sbp) ~ s(temp_168h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_sbp_m1t_st_168h)
#appraise(fit_sbp_m1t_st_168h)
#draw(fit_sbp_m1t_st_168h)

fit_sbp_m1o_lo_168h <-
  gam(
    formula = log(sbp) ~ o3_168h + 
      sex + age + diab + whr + smo + alc + phy + ses + edu +
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_sbp_m1o_lo_168h)
#appraise(fit_sbp_m1o_lo_168h)
#draw(fit_sbp_m1o_lo_168h)

fit_sbp_m1o_so_168h <-
  gam(
    formula = log(sbp) ~ s(o3_168h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_sbp_m1o_so_168h)
#appraise(fit_sbp_m1o_so_168h)
#draw(fit_sbp_m1o_so_168h)

fit_sbp_m2_sto_168h <-
  gam(
    formula = log(sbp) ~ s(temp_168h, bs = 'cs', k = 4, fx = T) + s(o3_168h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_sbp_m2_sto_168h)
#appraise(fit_sbp_m2_sto_168h)
#draw(fit_sbp_m2_sto_168h)

fit_sbp_m3_sto_168h <-
  gam(
    formula = log(sbp) ~ s(temp_168h, bs = 'cs', k = 4, fx = T) + s(o3_168h, bs = 'cs', k = 4, fx = T) + 
      s(pm10_168h, bs = 'cs', k = 4, fx = T) + s(no2_168h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_sto_168h)
#appraise(fit_sbp_m3_sto_168h)
#draw(fit_sbp_m3_sto_168h)

fit_sbp_m3_lt_so_168h <-
  gam(
    formula = log(sbp) ~ temp_168h + s(o3_168h, bs = 'cs', k = 4, fx = T) + 
      s(pm10_168h, bs = 'cs', k = 4, fx = T) + s(no2_168h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lt_so_168h)
#appraise(fit_sbp_m3_lt_so_168h)
#draw(fit_sbp_m3_lt_so_168h)

fit_sbp_m3_lo_st_168h <-
  gam(
    formula = log(sbp) ~ o3_168h + s(temp_168h, bs = 'cs', k = 4, fx = T) + 
      s(pm10_168h, bs = 'cs', k = 4, fx = T) + s(no2_168h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lo_st_168h)
#appraise(fit_sbp_m3_lo_st_168h)
#draw(fit_sbp_m3_lo_st_168h)

fit_sbp_m3_lto_168h <-
  gam(
    formula = log(sbp) ~ o3_168h + temp_168h + 
      s(pm10_168h, bs = 'cs', k = 4, fx = T) + s(no2_168h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lto_168h)
#appraise(fit_sbp_m3_lto_168h)
#draw(fit_sbp_m3_lto_168h)

fit_sbp_m3_lto_168h_s <-
  gam(
    formula = log(sbp) ~ o3_168h:season + temp_168h:season +
      s(pm10_168h, bs = 'cs', k = 4, fx = T) + s(no2_168h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lto_168h_s)
#appraise(fit_sbp_m3_lto_168h_s)
#draw(fit_sbp_m3_lto_168h_s)

fit_sbp_m3_lto_168h_si <-
  gam(
    formula = log(sbp) ~ o3_168h*season + temp_168h*season + 
      s(pm10_168h, bs = 'cs', k = 5) + s(no2_168h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lto_168h_si)
#appraise(fit_sbp_m3_lto_si)
#draw(fit_hr_sbp_lto_si)

fig1_sbp_temp_168h <- bind_rows(
  rel_smooth(fit_sbp_m1t_st_168h, 'temp_168h') %>% mutate(model = 1),
  rel_smooth(fit_sbp_m2_sto_168h, 'temp_168h') %>% mutate(model = 2),
  rel_smooth(fit_sbp_m3_sto_168h, 'temp_168h') %>% mutate(model = 3)
) %>% 
  mutate(model = factor(model)) %>% 
  ggplot(aes(x = value * sd(df_1$temp_168h, na.rm = T) + mean(df_1$temp_168h, na.rm = T), 
             group = model)) +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = model), alpha = .2) +
  geom_line(aes(y = est, colour = model)) +
  scale_y_continuous(labels = scales::percent, trans = 'log1p') +
  scale_colour_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_fill_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_x_continuous(expand = c(0,0), breaks = -1:3*5) +
  coord_cartesian(ylim = c(-.02, .02)) +
  labs(x = 'Temperature, 168h mean [\u00b0C]',
       y = 'sbp') +
  theme_classic() +
  theme(legend.position = 'none')

fig1_sbp_o3_168h <- bind_rows(
  rel_smooth(fit_sbp_m1o_so_168h, 'o3_168h') %>% mutate(model = 1),
  rel_smooth(fit_sbp_m2_sto_168h, 'o3_168h') %>% mutate(model = 2),
  rel_smooth(fit_sbp_m3_sto_168h, 'o3_168h') %>% mutate(model = 3)
) %>% 
  mutate(model = factor(model)) %>% 
  ggplot(aes(x = value * sd(df_1$o3_168h, na.rm = T) + mean(df_1$o3_168h, na.rm = T), 
             group = model)) +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = model), alpha = .2) +
  geom_line(aes(y = est, colour = model)) +
  scale_y_continuous(labels = scales::percent, trans = 'log1p') +
  scale_colour_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_fill_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_x_continuous(expand = c(0,0), breaks = 0:10*10) +
  coord_cartesian(ylim = c(-.02, .02)) +
  labs(x = 'O3, 168h mean [\u00b5g/m\u00b3]',
       y = 'SBP') +
  theme_classic() +
  theme(legend.position = 'none')

# 2 DBP ------------------------------------------------------------------------
fit_dbp_m1t_lt_168h <-
  gam(
    formula = log(dbp) ~ temp_168h + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_dbp_m1t_lt_168h)
#appraise(fit_dbp_m1t_lt_168h)
#draw(fit_dbp_m1t_lt_168h)

fit_dbp_m1t_st_168h <-
  gam(
    formula = log(dbp) ~ s(temp_168h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_dbp_m1t_st_168h)
#appraise(fit_dbp_m1t_st_168h)
#draw(fit_dbp_m1t_st_168h)

fit_dbp_m1o_lo_168h <-
  gam(
    formula = log(dbp) ~ o3_168h + 
      sex + age + diab + whr + smo + alc + phy + ses + edu +
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_dbp_m1o_lo_168h)
#appraise(fit_dbp_m1o_lo_168h)
#draw(fit_dbp_m1o_lo_168h)

fit_dbp_m1o_so_168h <-
  gam(
    formula = log(dbp) ~ s(o3_168h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_dbp_m1o_so_168h)
#appraise(fit_dbp_m1o_so_168h)
#draw(fit_dbp_m1o_so_168h)

fit_dbp_m2_sto_168h <-
  gam(
    formula = log(dbp) ~ s(temp_168h, bs = 'cs', k = 4, fx = T) + s(o3_168h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_dbp_m2_sto_168h)
#appraise(fit_dbp_m2_sto_168h)
#draw(fit_dbp_m2_sto_168h)

fit_dbp_m3_sto_168h <-
  gam(
    formula = log(dbp) ~ s(temp_168h, bs = 'cs', k = 4, fx = T) + s(o3_168h, bs = 'cs', k = 4, fx = T) + 
      s(pm10_168h, bs = 'cs', k = 4, fx = T) + s(no2_168h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_sto_168h)
#appraise(fit_dbp_m3_sto_168h)
#draw(fit_dbp_m3_sto_168h)

fit_dbp_m3_lt_so_168h <-
  gam(
    formula = log(dbp) ~ temp_168h + s(o3_168h, bs = 'cs', k = 4, fx = T) + 
      s(pm10_168h, bs = 'cs', k = 4, fx = T) + s(no2_168h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lt_so_168h)
#appraise(fit_dbp_m3_lt_so_168h)
#draw(fit_dbp_m3_lt_so_168h)

fit_dbp_m3_lo_st_168h <-
  gam(
    formula = log(dbp) ~ o3_168h + s(temp_168h, bs = 'cs', k = 4, fx = T) + 
      s(pm10_168h, bs = 'cs', k = 4, fx = T) + s(no2_168h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lo_st_168h)
#appraise(fit_dbp_m3_lo_st_168h)
#draw(fit_dbp_m3_lo_st_168h)

fit_dbp_m3_lto_168h <-
  gam(
    formula = log(dbp) ~ o3_168h + temp_168h + 
      s(pm10_168h, bs = 'cs', k = 4, fx = T) + s(no2_168h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lto_168h)
#appraise(fit_dbp_m3_lto_168h)
#draw(fit_dbp_m3_lto_168h)

fit_dbp_m3_lto_168h_s <-
  gam(
    formula = log(dbp) ~ o3_168h:season + temp_168h:season +
      s(pm10_168h, bs = 'cs', k = 4, fx = T) + s(no2_168h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lto_168h_s)
#appraise(fit_dbp_m3_lto_168h_s)
#draw(fit_dbp_m3_lto_168h_s)

fit_dbp_m3_lto_168h_si <-
  gam(
    formula = log(dbp) ~ o3_168h*season + temp_168h*season + 
      s(pm10_168h, bs = 'cs', k = 5) + s(no2_168h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lto_168h_si)
#appraise(fit_dbp_m3_lto_si)
#draw(fit_hr_dbp_lto_si)

fig1_dbp_temp_168h <- bind_rows(
  rel_smooth(fit_dbp_m1t_st_168h, 'temp_168h') %>% mutate(model = 1),
  rel_smooth(fit_dbp_m2_sto_168h, 'temp_168h') %>% mutate(model = 2),
  rel_smooth(fit_dbp_m3_sto_168h, 'temp_168h') %>% mutate(model = 3)
) %>% 
  mutate(model = factor(model)) %>% 
  ggplot(aes(x = value * sd(df_1$temp_168h, na.rm = T) + mean(df_1$temp_168h, na.rm = T), 
             group = model)) +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = model), alpha = .2) +
  geom_line(aes(y = est, colour = model)) +
  scale_y_continuous(labels = scales::percent, trans = 'log1p') +
  scale_colour_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_fill_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_x_continuous(expand = c(0,0), breaks = -1:3*5) +
  coord_cartesian(ylim = c(-.02, .02)) +
  labs(x = 'Temperature, 168h mean [\u00b0C]',
       y = 'DBP') +
  theme_classic() +
  theme(legend.position = 'none')

fig1_dbp_o3_168h <- bind_rows(
  rel_smooth(fit_dbp_m1o_so_168h, 'o3_168h') %>% mutate(model = 1),
  rel_smooth(fit_dbp_m2_sto_168h, 'o3_168h') %>% mutate(model = 2),
  rel_smooth(fit_dbp_m3_sto_168h, 'o3_168h') %>% mutate(model = 3)
) %>% 
  mutate(model = factor(model)) %>% 
  ggplot(aes(x = value * sd(df_1$o3_168h, na.rm = T) + mean(df_1$o3_168h, na.rm = T), 
             group = model)) +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = model), alpha = .2) +
  geom_line(aes(y = est, colour = model)) +
  scale_y_continuous(labels = scales::percent, trans = 'log1p') +
  scale_colour_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_fill_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_x_continuous(expand = c(0,0), breaks = 0:10*10) +
  coord_cartesian(ylim = c(-.02, .02)) +
  labs(x = 'O3, 168h mean [\u00b5g/m\u00b3]',
       y = 'DBP') +
  theme_classic() +
  theme(legend.position = 'none')

# 3 PP ------------------------------------------------------------------------
fit_pp_m1t_lt_168h <-
  gam(
    formula = log(pp) ~ temp_168h + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_pp_m1t_lt_168h)
#appraise(fit_pp_m1t_lt_168h)
#draw(fit_pp_m1t_lt_168h)

fit_pp_m1t_st_168h <-
  gam(
    formula = log(pp) ~ s(temp_168h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_pp_m1t_st_168h)
#appraise(fit_pp_m1t_st_168h)
#draw(fit_pp_m1t_st_168h)

fit_pp_m1o_lo_168h <-
  gam(
    formula = log(pp) ~ o3_168h + 
      sex + age + diab + whr + smo + alc + phy + ses + edu +
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_pp_m1o_lo_168h)
#appraise(fit_pp_m1o_lo_168h)
#draw(fit_pp_m1o_lo_168h)

fit_pp_m1o_so_168h <-
  gam(
    formula = log(pp) ~ s(o3_168h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_pp_m1o_so_168h)
#appraise(fit_pp_m1o_so_168h)
#draw(fit_pp_m1o_so_168h)

fit_pp_m2_sto_168h <-
  gam(
    formula = log(pp) ~ s(temp_168h, bs = 'cs', k = 4, fx = T) + s(o3_168h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_pp_m2_sto_168h)
#appraise(fit_pp_m2_sto_168h)
#draw(fit_pp_m2_sto_168h)

fit_pp_m3_sto_168h <-
  gam(
    formula = log(pp) ~ s(temp_168h, bs = 'cs', k = 4, fx = T) + s(o3_168h, bs = 'cs', k = 4, fx = T) + 
      s(pm10_168h, bs = 'cs', k = 4, fx = T) + s(no2_168h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_sto_168h)
#appraise(fit_pp_m3_sto_168h)
#draw(fit_pp_m3_sto_168h)

fit_pp_m3_lt_so_168h <-
  gam(
    formula = log(pp) ~ temp_168h + s(o3_168h, bs = 'cs', k = 4, fx = T) + 
      s(pm10_168h, bs = 'cs', k = 4, fx = T) + s(no2_168h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lt_so_168h)
#appraise(fit_pp_m3_lt_so_168h)
#draw(fit_pp_m3_lt_so_168h)

fit_pp_m3_lo_st_168h <-
  gam(
    formula = log(pp) ~ o3_168h + s(temp_168h, bs = 'cs', k = 4, fx = T) + 
      s(pm10_168h, bs = 'cs', k = 4, fx = T) + s(no2_168h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lo_st_168h)
#appraise(fit_pp_m3_lo_st_168h)
#draw(fit_pp_m3_lo_st_168h)

fit_pp_m3_lto_168h <-
  gam(
    formula = log(pp) ~ o3_168h + temp_168h + 
      s(pm10_168h, bs = 'cs', k = 4, fx = T) + s(no2_168h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lto_168h)
#appraise(fit_pp_m3_lto_168h)
#draw(fit_pp_m3_lto_168h)

fit_pp_m3_lto_168h_s <-
  gam(
    formula = log(pp) ~ o3_168h:season + temp_168h:season +
      s(pm10_168h, bs = 'cs', k = 4, fx = T) + s(no2_168h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lto_168h_s)
#appraise(fit_pp_m3_lto_168h_s)
#draw(fit_pp_m3_lto_168h_s)

fit_pp_m3_lto_168h_si <-
  gam(
    formula = log(pp) ~ o3_168h*season + temp_168h*season + 
      s(pm10_168h, bs = 'cs', k = 5) + s(no2_168h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lto_168h_si)
#appraise(fit_pp_m3_lto_si)
#draw(fit_pp_m3_lto_si)

fig1_pp_temp_168h <- bind_rows(
  rel_smooth(fit_pp_m1t_st_168h, 'temp_168h') %>% mutate(model = 1),
  rel_smooth(fit_pp_m2_sto_168h, 'temp_168h') %>% mutate(model = 2),
  rel_smooth(fit_pp_m3_sto_168h, 'temp_168h') %>% mutate(model = 3)
) %>% 
  mutate(model = factor(model)) %>% 
  ggplot(aes(x = value * sd(df_1$temp_168h, na.rm = T) + mean(df_1$temp_168h, na.rm = T), 
             group = model)) +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = model), alpha = .2) +
  geom_line(aes(y = est, colour = model)) +
  scale_y_continuous(labels = scales::percent, trans = 'log1p') +
  scale_colour_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_fill_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_x_continuous(expand = c(0,0), breaks = -1:3*5) +
  coord_cartesian(ylim = c(-.02, .02)) +
  labs(x = 'Temperature, 168h mean [\u00b0C]',
       y = 'PP') +
  theme_classic() +
  theme(legend.position = 'none')

fig1_pp_o3_168h <- bind_rows(
  rel_smooth(fit_pp_m1o_so_168h, 'o3_168h') %>% mutate(model = 1),
  rel_smooth(fit_pp_m2_sto_168h, 'o3_168h') %>% mutate(model = 2),
  rel_smooth(fit_pp_m3_sto_168h, 'o3_168h') %>% mutate(model = 3)
) %>% 
  mutate(model = factor(model)) %>% 
  ggplot(aes(x = value * sd(df_1$o3_168h, na.rm = T) + mean(df_1$o3_168h, na.rm = T), 
             group = model)) +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = model), alpha = .2) +
  geom_line(aes(y = est, colour = model)) +
  scale_y_continuous(labels = scales::percent, trans = 'log1p') +
  scale_colour_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_fill_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_x_continuous(expand = c(0,0), breaks = 0:10*10) +
  coord_cartesian(ylim = c(-.02, .02)) +
  labs(x = 'O3, 168h mean [\u00b5g/m\u00b3]',
       y = 'PP') +
  theme_classic() +
  theme(legend.position = 'none')

# 4 HR ------------------------------------------------------------------------
fit_hr_m1t_lt_168h <-
  gam(
    formula = log(hr) ~ temp_168h + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_hr_m1t_lt_168h)
#appraise(fit_hr_m1t_lt_168h)
#draw(fit_hr_m1t_lt_168h)

fit_hr_m1t_st_168h <-
  gam(
    formula = log(hr) ~ s(temp_168h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_hr_m1t_st_168h)
#appraise(fit_hr_m1t_st_168h)
#draw(fit_hr_m1t_st_168h)

fit_hr_m1o_lo_168h <-
  gam(
    formula = log(hr) ~ o3_168h + 
      sex + age + diab + whr + smo + alc + phy + ses + edu +
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_hr_m1o_lo_168h)
#appraise(fit_hr_m1o_lo_168h)
#draw(fit_hr_m1o_lo_168h)

fit_hr_m1o_so_168h <-
  gam(
    formula = log(hr) ~ s(o3_168h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_hr_m1o_so_168h)
#appraise(fit_hr_m1o_so_168h)
#draw(fit_hr_m1o_so_168h)

fit_hr_m2_sto_168h <-
  gam(
    formula = log(hr) ~ s(temp_168h, bs = 'cs', k = 4, fx = T) + s(o3_168h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_hr_m2_sto_168h)
#appraise(fit_hr_m2_sto_168h)
#draw(fit_hr_m2_sto_168h)

fit_hr_m3_sto_168h <-
  gam(
    formula = log(hr) ~ s(temp_168h, bs = 'cs', k = 4, fx = T) + s(o3_168h, bs = 'cs', k = 4, fx = T) + 
      s(pm10_168h, bs = 'cs', k = 4, fx = T) + s(no2_168h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_sto_168h)
#appraise(fit_hr_m3_sto_168h)
#draw(fit_hr_m3_sto_168h)

fit_hr_m3_lt_so_168h <-
  gam(
    formula = log(hr) ~ temp_168h + s(o3_168h, bs = 'cs', k = 4, fx = T) + 
      s(pm10_168h, bs = 'cs', k = 4, fx = T) + s(no2_168h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lt_so_168h)
#appraise(fit_hr_m3_lt_so_168h)
#draw(fit_hr_m3_lt_so_168h)

fit_hr_m3_lo_st_168h <-
  gam(
    formula = log(hr) ~ o3_168h + s(temp_168h, bs = 'cs', k = 4, fx = T) + 
      s(pm10_168h, bs = 'cs', k = 4, fx = T) + s(no2_168h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lo_st_168h)
#appraise(fit_hr_m3_lo_st_168h)
#draw(fit_hr_m3_lo_st_168h)

fit_hr_m3_lto_168h <-
  gam(
    formula = log(hr) ~ o3_168h + temp_168h + 
      s(pm10_168h, bs = 'cs', k = 4, fx = T) + s(no2_168h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lto_168h)
#appraise(fit_hr_m3_lto_168h)
#draw(fit_hr_m3_lto_168h)

fit_hr_m3_lto_168h_s <-
  gam(
    formula = log(hr) ~ o3_168h:season + temp_168h:season +
      s(pm10_168h, bs = 'cs', k = 4, fx = T) + s(no2_168h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lto_168h_s)
#appraise(fit_hr_m3_lto_168h_s)
#draw(fit_hr_m3_lto_168h_s)

fit_hr_m3_lto_168h_si <-
  gam(
    formula = log(hr) ~ o3_168h*season + temp_168h*season + 
      s(pm10_168h, bs = 'cs', k = 5) + s(no2_168h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lto_168h_si)
#appraise(fit_hr_m3_lto_si)
#draw(fit_hr_m3_lto_si)

fig1_hr_temp_168h <- bind_rows(
  rel_smooth(fit_hr_m1t_st_168h, 'temp_168h') %>% mutate(model = 1),
  rel_smooth(fit_hr_m2_sto_168h, 'temp_168h') %>% mutate(model = 2),
  rel_smooth(fit_hr_m3_sto_168h, 'temp_168h') %>% mutate(model = 3)
) %>% 
  mutate(model = factor(model)) %>% 
  ggplot(aes(x = value * sd(df_1$temp_168h, na.rm = T) + mean(df_1$temp_168h, na.rm = T), 
             group = model)) +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = model), alpha = .2) +
  geom_line(aes(y = est, colour = model)) +
  scale_y_continuous(labels = scales::percent, trans = 'log1p') +
  scale_colour_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_fill_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_x_continuous(expand = c(0,0), breaks = -1:3*5) +
  coord_cartesian(ylim = c(-.02, .02)) +
  labs(x = 'Temperature, 168h mean [\u00b0C]',
       y = 'HR') +
  theme_classic() +
  theme(legend.position = 'none')

fig1_hr_o3_168h <- bind_rows(
  rel_smooth(fit_hr_m1o_so_168h, 'o3_168h') %>% mutate(model = 1),
  rel_smooth(fit_hr_m2_sto_168h, 'o3_168h') %>% mutate(model = 2),
  rel_smooth(fit_hr_m3_sto_168h, 'o3_168h') %>% mutate(model = 3)
) %>% 
  mutate(model = factor(model)) %>% 
  ggplot(aes(x = value * sd(df_1$o3_168h, na.rm = T) + mean(df_1$o3_168h, na.rm = T), 
             group = model)) +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = model), alpha = .2) +
  geom_line(aes(y = est, colour = model)) +
  scale_y_continuous(labels = scales::percent, trans = 'log1p') +
  scale_colour_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_fill_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_x_continuous(expand = c(0,0), breaks = 0:10*10) +
  coord_cartesian(ylim = c(-.02, .02)) +
  labs(x = 'O3, 168h mean [\u00b5g/m\u00b3]',
       y = 'HR') +
  theme_classic() +
  theme(legend.position = 'none')

# * 8 am -----------------------------------------------------------------------
df_3 <- df_2 %>% 
  ungroup %>% 
  filter(!is.na(o3) & !is.na(temp_ap) & !is.na(pm10) & !is.na(no2),
         !is.na(sbp) & !is.na(dbp) & !is.na(hr),
         !is.na(sex) & !is.na(age) & !is.na(diab) & !is.na(whr) & !is.na(smo) & !is.na(alc) & !is.na(phy) & !is.na(ses)) %>%
  mutate(temp_ap_t = ntile(temp_ap, 3) %>% factor(),
         temp_ap_t = ntile(temp_ap, 3) %>% factor(),
         o3_t = ntile(o3, 3) %>% factor(),
         o3_t = ntile(o3, 3) %>% factor())

# 1 SBP ------------------------------------------------------------------------
fit_sbp_m1t_lt_8am <-
  gam(
    formula = log(sbp) ~ temp_ap + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_sbp_m1t_lt_8am)
#appraise(fit_sbp_m1t_lt_8am)
#draw(fit_sbp_m1t_lt_8am)

fit_sbp_m1t_st_8am <-
  gam(
    formula = log(sbp) ~ s(temp_ap, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_sbp_m1t_st_8am)
#appraise(fit_sbp_m1t_st_8am)
#draw(fit_sbp_m1t_st_8am)

fit_sbp_m1o_lo_8am <-
  gam(
    formula = log(sbp) ~ o3 + 
      sex + age + diab + whr + smo + alc + phy + ses + edu +
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_sbp_m1o_lo_8am)
#appraise(fit_sbp_m1o_lo_8am)
#draw(fit_sbp_m1o_lo_8am)

fit_sbp_m1o_so_8am <-
  gam(
    formula = log(sbp) ~ s(o3, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_sbp_m1o_so_8am)
#appraise(fit_sbp_m1o_so_8am)
#draw(fit_sbp_m1o_so_8am)

fit_sbp_m2_sto_8am <-
  gam(
    formula = log(sbp) ~ s(temp_ap, bs = 'cs', k = 4, fx = T) + s(o3, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_sbp_m2_sto_8am)
#appraise(fit_sbp_m2_sto_8am)
#draw(fit_sbp_m2_sto_8am)

fit_sbp_m3_sto_8am <-
  gam(
    formula = log(sbp) ~ s(temp_ap, bs = 'cs', k = 4, fx = T) + s(o3, bs = 'cs', k = 4, fx = T) + 
      s(pm10, bs = 'cs', k = 4, fx = T) + s(no2, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_sto_8am)
#appraise(fit_sbp_m3_sto_8am)
#draw(fit_sbp_m3_sto_8am)

fit_sbp_m3_lt_so_8am <-
  gam(
    formula = log(sbp) ~ temp_ap + s(o3, bs = 'cs', k = 4, fx = T) + 
      s(pm10, bs = 'cs', k = 4, fx = T) + s(no2, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lt_so_8am)
#appraise(fit_sbp_m3_lt_so_8am)
#draw(fit_sbp_m3_lt_so_8am)

fit_sbp_m3_lo_st_8am <-
  gam(
    formula = log(sbp) ~ o3 + s(temp_ap, bs = 'cs', k = 4, fx = T) + 
      s(pm10, bs = 'cs', k = 4, fx = T) + s(no2, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lo_st_8am)
#appraise(fit_sbp_m3_lo_st_8am)
#draw(fit_sbp_m3_lo_st_8am)

fit_sbp_m3_lto_8am <-
  gam(
    formula = log(sbp) ~ o3 + temp_ap + 
      s(pm10, bs = 'cs', k = 4, fx = T) + s(no2, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lto_8am)
#appraise(fit_sbp_m3_lto_8am)
#draw(fit_sbp_m3_lto_8am)

fit_sbp_m3_lto_8am_s <-
  gam(
    formula = log(sbp) ~ o3:season + temp_ap:season +
      s(pm10, bs = 'cs', k = 4, fx = T) + s(no2, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lto_8am_s)
#appraise(fit_sbp_m3_lto_8am_s)
#draw(fit_sbp_m3_lto_8am_s)

fit_sbp_m3_lto_8am_si <-
  gam(
    formula = log(sbp) ~ o3*season + temp_ap*season + 
      s(pm10, bs = 'cs', k = 5) + s(no2, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lto_8am_si)
#appraise(fit_sbp_m3_8am_lto_si)
#draw(fit_sbp_m3_lto_8am_si)

fig1_sbp_temp_8am <- bind_rows(
  rel_smooth(fit_sbp_m1t_st_8am, 'temp_ap') %>% mutate(model = 1),
  rel_smooth(fit_sbp_m2_sto_8am, 'temp_ap') %>% mutate(model = 2),
  rel_smooth(fit_sbp_m3_sto_8am, 'temp_ap') %>% mutate(model = 3)
) %>% 
  mutate(model = factor(model)) %>% 
  ggplot(aes(x = value * sd(df_1$temp_ap, na.rm = T) + mean(df_1$temp_ap, na.rm = T), 
             group = model)) +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = model), alpha = .2) +
  geom_line(aes(y = est, colour = model)) +
  scale_y_continuous(labels = scales::percent, trans = 'log1p') +
  scale_colour_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_fill_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_x_continuous(expand = c(0,0), breaks = -1:3*5) +
  coord_cartesian(ylim = c(-.02, .02)) +
  labs(x = 'Temperature, 8am [\u00b0C]',
       y = 'sbp') +
  theme_classic() +
  theme(legend.position = 'none')

fig1_sbp_o3_8am <- bind_rows(
  rel_smooth(fit_sbp_m1o_so_8am, 'o3') %>% mutate(model = 1),
  rel_smooth(fit_sbp_m2_sto_8am, 'o3') %>% mutate(model = 2),
  rel_smooth(fit_sbp_m3_sto_8am, 'o3') %>% mutate(model = 3)
) %>% 
  mutate(model = factor(model)) %>% 
  ggplot(aes(x = value * sd(df_1$o3, na.rm = T) + mean(df_1$o3, na.rm = T), 
             group = model)) +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = model), alpha = .2) +
  geom_line(aes(y = est, colour = model)) +
  scale_y_continuous(labels = scales::percent, trans = 'log1p') +
  scale_colour_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_fill_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_x_continuous(expand = c(0,0), breaks = 0:10*10) +
  coord_cartesian(ylim = c(-.02, .02)) +
  labs(x = 'O3, 8 am [\u00b5g/m\u00b3]',
       y = 'SBP') +
  theme_classic() +
  theme(legend.position = 'none')

# 2 DBP ------------------------------------------------------------------------
fit_dbp_m1t_lt_8am <-
  gam(
    formula = log(dbp) ~ temp_ap + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_dbp_m1t_lt_8am)
#appraise(fit_dbp_m1t_lt_8am)
#draw(fit_dbp_m1t_lt_8am)

fit_dbp_m1t_st_8am <-
  gam(
    formula = log(dbp) ~ s(temp_ap, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_dbp_m1t_st_8am)
#appraise(fit_dbp_m1t_st_8am)
#draw(fit_dbp_m1t_st_8am)

fit_dbp_m1o_lo_8am <-
  gam(
    formula = log(dbp) ~ o3 + 
      sex + age + diab + whr + smo + alc + phy + ses + edu +
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_dbp_m1o_lo_8am)
#appraise(fit_dbp_m1o_lo_8am)
#draw(fit_dbp_m1o_lo_8am)

fit_dbp_m1o_so_8am <-
  gam(
    formula = log(dbp) ~ s(o3, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_dbp_m1o_so_8am)
#appraise(fit_dbp_m1o_so_8am)
#draw(fit_dbp_m1o_so_8am)

fit_dbp_m2_sto_8am <-
  gam(
    formula = log(dbp) ~ s(temp_ap, bs = 'cs', k = 4, fx = T) + s(o3, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_dbp_m2_sto_8am)
#appraise(fit_dbp_m2_sto_8am)
#draw(fit_dbp_m2_sto_8am)

fit_dbp_m3_sto_8am <-
  gam(
    formula = log(dbp) ~ s(temp_ap, bs = 'cs', k = 4, fx = T) + s(o3, bs = 'cs', k = 4, fx = T) + 
      s(pm10, bs = 'cs', k = 4, fx = T) + s(no2, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_sto_8am)
#appraise(fit_dbp_m3_sto_8am)
#draw(fit_dbp_m3_sto_8am)

fit_dbp_m3_lt_so_8am <-
  gam(
    formula = log(dbp) ~ temp_ap + s(o3, bs = 'cs', k = 4, fx = T) + 
      s(pm10, bs = 'cs', k = 4, fx = T) + s(no2, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lt_so_8am)
#appraise(fit_dbp_m3_lt_so_8am)
#draw(fit_dbp_m3_lt_so_8am)

fit_dbp_m3_lo_st_8am <-
  gam(
    formula = log(dbp) ~ o3 + s(temp_ap, bs = 'cs', k = 4, fx = T) + 
      s(pm10, bs = 'cs', k = 4, fx = T) + s(no2, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lo_st_8am)
#appraise(fit_dbp_m3_lo_st_8am)
#draw(fit_dbp_m3_lo_st_8am)

fit_dbp_m3_lto_8am <-
  gam(
    formula = log(dbp) ~ o3 + temp_ap + 
      s(pm10, bs = 'cs', k = 4, fx = T) + s(no2, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lto_8am)
#appraise(fit_dbp_m3_lto_8am)
#draw(fit_dbp_m3_lto_8am)

fit_dbp_m3_lto_8am_s <-
  gam(
    formula = log(dbp) ~ o3:season + temp_ap:season +
      s(pm10, bs = 'cs', k = 4, fx = T) + s(no2, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lto_8am_s)
#appraise(fit_dbp_m3_lto_8am_s)
#draw(fit_dbp_m3_lto_8am_s)

fit_dbp_m3_lto_8am_si <-
  gam(
    formula = log(dbp) ~ o3*season + temp_ap*season + 
      s(pm10, bs = 'cs', k = 5) + s(no2, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lto_8am_si)
#appraise(fit_dbp_m3_8am_lto_si)
#draw(fit_dbp_m3_lto_8am_si)

fig1_dbp_temp_8am <- bind_rows(
  rel_smooth(fit_dbp_m1t_st_8am, 'temp_ap') %>% mutate(model = 1),
  rel_smooth(fit_dbp_m2_sto_8am, 'temp_ap') %>% mutate(model = 2),
  rel_smooth(fit_dbp_m3_sto_8am, 'temp_ap') %>% mutate(model = 3)
) %>% 
  mutate(model = factor(model)) %>% 
  ggplot(aes(x = value * sd(df_1$temp_ap, na.rm = T) + mean(df_1$temp_ap, na.rm = T), 
             group = model)) +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = model), alpha = .2) +
  geom_line(aes(y = est, colour = model)) +
  scale_y_continuous(labels = scales::percent, trans = 'log1p') +
  scale_colour_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_fill_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_x_continuous(expand = c(0,0), breaks = -1:3*5) +
  coord_cartesian(ylim = c(-.02, .02)) +
  labs(x = 'Temperature, 8 am [\u00b0C]',
       y = 'DBP') +
  theme_classic() +
  theme(legend.position = 'none')

fig1_dbp_o3_8am <- bind_rows(
  rel_smooth(fit_dbp_m1o_so_8am, 'o3') %>% mutate(model = 1),
  rel_smooth(fit_dbp_m2_sto_8am, 'o3') %>% mutate(model = 2),
  rel_smooth(fit_dbp_m3_sto_8am, 'o3') %>% mutate(model = 3)
) %>% 
  mutate(model = factor(model)) %>% 
  ggplot(aes(x = value * sd(df_1$o3, na.rm = T) + mean(df_1$o3, na.rm = T), 
             group = model)) +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = model), alpha = .2) +
  geom_line(aes(y = est, colour = model)) +
  scale_y_continuous(labels = scales::percent, trans = 'log1p') +
  scale_colour_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_fill_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_x_continuous(expand = c(0,0), breaks = 0:10*10) +
  coord_cartesian(ylim = c(-.02, .02)) +
  labs(x = 'O3, 8 am [\u00b5g/m\u00b3]',
       y = 'DBP') +
  theme_classic() +
  theme(legend.position = 'none')

# 3 PP ------------------------------------------------------------------------
fit_pp_m1t_lt_8am <-
  gam(
    formula = log(pp) ~ temp_ap + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_pp_m1t_lt_8am)
#appraise(fit_pp_m1t_lt_8am)
#draw(fit_pp_m1t_lt_8am)

fit_pp_m1t_st_8am <-
  gam(
    formula = log(pp) ~ s(temp_ap, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_pp_m1t_st_8am)
#appraise(fit_pp_m1t_st_8am)
#draw(fit_pp_m1t_st_8am)

fit_pp_m1o_lo_8am <-
  gam(
    formula = log(pp) ~ o3 + 
      sex + age + diab + whr + smo + alc + phy + ses + edu +
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_pp_m1o_lo_8am)
#appraise(fit_pp_m1o_lo_8am)
#draw(fit_pp_m1o_lo_8am)

fit_pp_m1o_so_8am <-
  gam(
    formula = log(pp) ~ s(o3, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_pp_m1o_so_8am)
#appraise(fit_pp_m1o_so_8am)
#draw(fit_pp_m1o_so_8am)

fit_pp_m2_sto_8am <-
  gam(
    formula = log(pp) ~ s(temp_ap, bs = 'cs', k = 4, fx = T) + s(o3, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_pp_m2_sto_8am)
#appraise(fit_pp_m2_sto_8am)
#draw(fit_pp_m2_sto_8am)

fit_pp_m3_sto_8am <-
  gam(
    formula = log(pp) ~ s(temp_ap, bs = 'cs', k = 4, fx = T) + s(o3, bs = 'cs', k = 4, fx = T) + 
      s(pm10, bs = 'cs', k = 4, fx = T) + s(no2, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_sto_8am)
#appraise(fit_pp_m3_sto_8am)
#draw(fit_pp_m3_sto_8am)

fit_pp_m3_lt_so_8am <-
  gam(
    formula = log(pp) ~ temp_ap + s(o3, bs = 'cs', k = 4, fx = T) + 
      s(pm10, bs = 'cs', k = 4, fx = T) + s(no2, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lt_so_8am)
#appraise(fit_pp_m3_lt_so_8am)
#draw(fit_pp_m3_lt_so_8am)

fit_pp_m3_lo_st_8am <-
  gam(
    formula = log(pp) ~ o3 + s(temp_ap, bs = 'cs', k = 4, fx = T) + 
      s(pm10, bs = 'cs', k = 4, fx = T) + s(no2, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lo_st_8am)
#appraise(fit_pp_m3_lo_st_8am)
#draw(fit_pp_m3_lo_st_8am)

fit_pp_m3_lto_8am <-
  gam(
    formula = log(pp) ~ o3 + temp_ap + 
      s(pm10, bs = 'cs', k = 4, fx = T) + s(no2, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lto_8am)
#appraise(fit_pp_m3_lto_8am)
#draw(fit_pp_m3_lto_8am)

fit_pp_m3_lto_8am_s <-
  gam(
    formula = log(pp) ~ o3:season + temp_ap:season +
      s(pm10, bs = 'cs', k = 4, fx = T) + s(no2, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lto_8am_s)
#appraise(fit_pp_m3_lto_8am_s)
#draw(fit_pp_m3_lto_8am_s)

fit_pp_m3_lto_8am_si <-
  gam(
    formula = log(pp) ~ o3*season + temp_ap*season + 
      s(pm10, bs = 'cs', k = 5) + s(no2, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lto_8am_si)
#appraise(fit_pp_m3_8am_lto_si)
#draw(fit_pp_m3_lto_8am_si)

fig1_pp_temp_8am <- bind_rows(
  rel_smooth(fit_pp_m1t_st_8am, 'temp_ap') %>% mutate(model = 1),
  rel_smooth(fit_pp_m2_sto_8am, 'temp_ap') %>% mutate(model = 2),
  rel_smooth(fit_pp_m3_sto_8am, 'temp_ap') %>% mutate(model = 3)
) %>% 
  mutate(model = factor(model)) %>% 
  ggplot(aes(x = value * sd(df_1$temp_ap, na.rm = T) + mean(df_1$temp_ap, na.rm = T), 
             group = model)) +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = model), alpha = .2) +
  geom_line(aes(y = est, colour = model)) +
  scale_y_continuous(labels = scales::percent, trans = 'log1p') +
  scale_colour_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_fill_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_x_continuous(expand = c(0,0), breaks = -1:3*5) +
  coord_cartesian(ylim = c(-.02, .02)) +
  labs(x = 'Temperature, 8 am [\u00b0C]',
       y = 'PP') +
  theme_classic() +
  theme(legend.position = 'none')

fig1_pp_o3_8am <- bind_rows(
  rel_smooth(fit_pp_m1o_so_8am, 'o3') %>% mutate(model = 1),
  rel_smooth(fit_pp_m2_sto_8am, 'o3') %>% mutate(model = 2),
  rel_smooth(fit_pp_m3_sto_8am, 'o3') %>% mutate(model = 3)
) %>% 
  mutate(model = factor(model)) %>% 
  ggplot(aes(x = value * sd(df_1$o3, na.rm = T) + mean(df_1$o3, na.rm = T), 
             group = model)) +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = model), alpha = .2) +
  geom_line(aes(y = est, colour = model)) +
  scale_y_continuous(labels = scales::percent, trans = 'log1p') +
  scale_colour_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_fill_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_x_continuous(expand = c(0,0), breaks = 0:10*10) +
  coord_cartesian(ylim = c(-.02, .02)) +
  labs(x = 'O3, 8 am [\u00b5g/m\u00b3]',
       y = 'PP') +
  theme_classic() +
  theme(legend.position = 'none')

# 4 HR ------------------------------------------------------------------------
fit_hr_m1t_lt_8am <-
  gam(
    formula = log(hr) ~ temp_ap + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_hr_m1t_lt_8am)
#appraise(fit_hr_m1t_lt_8am)
#draw(fit_hr_m1t_lt_8am)

fit_hr_m1t_st_8am <-
  gam(
    formula = log(hr) ~ s(temp_ap, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_hr_m1t_st_8am)
#appraise(fit_hr_m1t_st_8am)
#draw(fit_hr_m1t_st_8am)

fit_hr_m1o_lo_8am <-
  gam(
    formula = log(hr) ~ o3 + 
      sex + age + diab + whr + smo + alc + phy + ses + edu +
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_hr_m1o_lo_8am)
#appraise(fit_hr_m1o_lo_8am)
#draw(fit_hr_m1o_lo_8am)

fit_hr_m1o_so_8am <-
  gam(
    formula = log(hr) ~ s(o3, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_hr_m1o_so_8am)
#appraise(fit_hr_m1o_so_8am)
#draw(fit_hr_m1o_so_8am)

fit_hr_m2_sto_8am <-
  gam(
    formula = log(hr) ~ s(temp_ap, bs = 'cs', k = 4, fx = T) + s(o3, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_hr_m2_sto_8am)
#appraise(fit_hr_m2_sto_8am)
#draw(fit_hr_m2_sto_8am)

fit_hr_m3_sto_8am <-
  gam(
    formula = log(hr) ~ s(temp_ap, bs = 'cs', k = 4, fx = T) + s(o3, bs = 'cs', k = 4, fx = T) + 
      s(pm10, bs = 'cs', k = 4, fx = T) + s(no2, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_sto_8am)
#appraise(fit_hr_m3_sto_8am)
#draw(fit_hr_m3_sto_8am)

fit_hr_m3_lt_so_8am <-
  gam(
    formula = log(hr) ~ temp_ap + s(o3, bs = 'cs', k = 4, fx = T) + 
      s(pm10, bs = 'cs', k = 4, fx = T) + s(no2, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lt_so_8am)
#appraise(fit_hr_m3_lt_so_8am)
#draw(fit_hr_m3_lt_so_8am)

fit_hr_m3_lo_st_8am <-
  gam(
    formula = log(hr) ~ o3 + s(temp_ap, bs = 'cs', k = 4, fx = T) + 
      s(pm10, bs = 'cs', k = 4, fx = T) + s(no2, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lo_st_8am)
#appraise(fit_hr_m3_lo_st_8am)
#draw(fit_hr_m3_lo_st_8am)

fit_hr_m3_lto_8am <-
  gam(
    formula = log(hr) ~ o3 + temp_ap +
      s(pm10, bs = 'cs', k = 4, fx = T) + s(no2, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )

#summary(fit_hr_m3_lto_8am)
#appraise(fit_hr_m3_lto_8am)
#draw(fit_hr_m3_lto_8am)

fit_hr_m3_lto_8am_s <-
  gam(
    formula = log(hr) ~ o3:season + temp_ap:season +
      s(pm10, bs = 'cs', k = 4, fx = T) + s(no2, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lto_8am_s)
#appraise(fit_hr_m3_lto_8am_s)
#draw(fit_hr_m3_lto_8am_s)

fit_hr_m3_lto_8am_si <-
  gam(
    formula = log(hr) ~ o3*season + temp_ap*season + 
      s(pm10, bs = 'cs', k = 5) + s(no2, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lto_8am_si)
#appraise(fit_hr_m3_8am_lto_si)
#draw(fit_hr_m3_lto_8am_si)

fig1_hr_temp_8am <- bind_rows(
  rel_smooth(fit_hr_m1t_st_8am, 'temp_ap') %>% mutate(model = 1),
  rel_smooth(fit_hr_m2_sto_8am, 'temp_ap') %>% mutate(model = 2),
  rel_smooth(fit_hr_m3_sto_8am, 'temp_ap') %>% mutate(model = 3)
) %>% 
  mutate(model = factor(model)) %>% 
  ggplot(aes(x = value * sd(df_1$temp_ap, na.rm = T) + mean(df_1$temp_ap, na.rm = T), 
             group = model)) +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = model), alpha = .2) +
  geom_line(aes(y = est, colour = model)) +
  scale_y_continuous(labels = scales::percent, trans = 'log1p') +
  scale_colour_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_fill_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_x_continuous(expand = c(0,0), breaks = -1:3*5) +
  coord_cartesian(ylim = c(-.02, .02)) +
  labs(x = 'Temperature, 8 am [\u00b0C]',
       y = 'HR') +
  theme_classic() +
  theme(legend.position = 'none')

fig1_hr_o3_8am <- bind_rows(
  rel_smooth(fit_hr_m1o_so_8am, 'o3') %>% mutate(model = 1),
  rel_smooth(fit_hr_m2_sto_8am, 'o3') %>% mutate(model = 2),
  rel_smooth(fit_hr_m3_sto_8am, 'o3') %>% mutate(model = 3)
) %>% 
  mutate(model = factor(model)) %>% 
  ggplot(aes(x = value * sd(df_1$o3, na.rm = T) + mean(df_1$o3, na.rm = T), 
             group = model)) +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = model), alpha = .2) +
  geom_line(aes(y = est, colour = model)) +
  scale_y_continuous(labels = scales::percent, trans = 'log1p') +
  scale_colour_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_fill_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_x_continuous(expand = c(0,0), breaks = 0:10*10) +
  coord_cartesian(ylim = c(-.02, .02)) +
  labs(x = 'O3, 8 am [\u00b5g/m\u00b3]',
       y = 'HR') +
  theme_classic() +
  theme(legend.position = 'none')

# * Plot -----------------------------------------------------------------------
cowplot::plot_grid(
  fig1_sbp_temp_168h,
  fig1_dbp_temp_168h,
  fig1_pp_temp_168h,
  fig1_hr_temp_168h,
  fig1_sbp_o3_168h,
  fig1_dbp_o3_168h,
  fig1_pp_o3_168h,
  fig1_hr_o3_168h,
  fig1_sbp_temp_8am,
  fig1_dbp_temp_8am,
  fig1_pp_temp_8am,
  fig1_hr_temp_8am,
  fig1_sbp_o3_8am,
  fig1_dbp_o3_8am,
  fig1_pp_o3_8am,
  fig1_hr_o3_8am,
  nrow = 4,
  align = 'hv',
  labels = c('A', '', '', '', '', '', '', '', 'B', '', '', '')
)
ggsave(here::here('figures', paste0(Sys.Date(), '_figS1.svg')), width = 14, height = 12)
ggsave(here::here('figures', paste0(Sys.Date(), '_figS1.png')), width = 14, height = 12)

# * Table ----------------------------------------------------------------------
tableS1 <- 
  rbind(
    tableS1_helper(outc = 'sbp', expo = 'temp_168h', base_model = fit_sbp_m3_lto_168h) %>% mutate(season = 'All'),
    tableS1_helper(outc = 'dbp', expo = 'temp_168h', base_model = fit_dbp_m3_lto_168h) %>% mutate(season = 'All'),
    tableS1_helper(outc = 'pp', expo = 'temp_168h', base_model = fit_pp_m3_lto_168h) %>% mutate(season = 'All'),
    tableS1_helper(outc = 'hr', expo = 'temp_168h', base_model = fit_hr_m3_lto_168h) %>% mutate(season = 'All'),
    
    tableS1_helper(outc = 'sbp', expo = 'o3_168h', base_model = fit_sbp_m3_lto_168h) %>% mutate(season = 'All'),
    tableS1_helper(outc = 'dbp', expo = 'o3_168h', base_model = fit_dbp_m3_lto_168h) %>% mutate(season = 'All'),
    tableS1_helper(outc = 'pp', expo = 'o3_168h', base_model = fit_pp_m3_lto_168h) %>% mutate(season = 'All'),
    tableS1_helper(outc = 'hr', expo = 'o3_168h', base_model = fit_hr_m3_lto_168h) %>% mutate(season = 'All'),
    
    tableS1_helper(outc = 'sbp', expo = 'temp_ap', base_model = fit_sbp_m3_lto_8am) %>% mutate(season = 'All'),
    tableS1_helper(outc = 'dbp', expo = 'temp_ap', base_model = fit_dbp_m3_lto_8am) %>% mutate(season = 'All'),
    tableS1_helper(outc = 'pp', expo = 'temp_ap', base_model = fit_pp_m3_lto_8am) %>% mutate(season = 'All'),
    tableS1_helper(outc = 'hr', expo = 'temp_ap', base_model = fit_hr_m3_lto_8am) %>% mutate(season = 'All'),
    
    tableS1_helper(outc = 'sbp', expo = 'o3', base_model = fit_sbp_m3_lto_8am) %>% mutate(season = 'All'),
    tableS1_helper(outc = 'dbp', expo = 'o3', base_model = fit_dbp_m3_lto_8am) %>% mutate(season = 'All'),
    tableS1_helper(outc = 'pp', expo = 'o3', base_model = fit_pp_m3_lto_8am) %>% mutate(season = 'All'),
    tableS1_helper(outc = 'hr', expo = 'o3', base_model = fit_hr_m3_lto_8am) %>% mutate(season = 'All'),
    
    season_helper(outcome = 'sbp', fit = fit_sbp_m3_lto_168h_s),
    season_helper(outcome = 'dbp', fit = fit_dbp_m3_lto_168h_s),
    season_helper(outcome = 'pp', fit = fit_pp_m3_lto_168h_s),
    season_helper(outcome = 'hr', fit = fit_hr_m3_lto_168h_s),
    
    season_helper(outcome = 'sbp', fit = fit_sbp_m3_lto_8am_s),
    season_helper(outcome = 'dbp', fit = fit_dbp_m3_lto_8am_s),
    season_helper(outcome = 'pp', fit = fit_pp_m3_lto_8am_s),
    season_helper(outcome = 'hr', fit = fit_hr_m3_lto_8am_s)
) %>% 
  pivot_wider(id_cols = c('term', 'outcome'), names_from = 'season', values_from = 'f') %>% 
  left_join(
    bind_rows(
    tidy(fit_sbp_m3_lto_168h_si, parametric = T) %>% mutate(outcome = 'sbp'),
    tidy(fit_dbp_m3_lto_168h_si, parametric = T) %>% mutate(outcome = 'dbp'),
    tidy(fit_pp_m3_lto_168h_si, parametric = T) %>% mutate(outcome = 'pp'),
    tidy(fit_hr_m3_lto_168h_si, parametric = T) %>% mutate(outcome = 'hr'),
    tidy(fit_sbp_m3_lto_8am_si, parametric = T) %>% mutate(outcome = 'sbp'),
    tidy(fit_dbp_m3_lto_8am_si, parametric = T) %>% mutate(outcome = 'dbp'),
    tidy(fit_pp_m3_lto_8am_si, parametric = T) %>% mutate(outcome = 'pp'),
    tidy(fit_hr_m3_lto_8am_si, parametric = T) %>% mutate(outcome = 'hr')
    
  ) %>% 
    filter(str_detect(term, ':'))  %>% 
    mutate(term = str_remove_all(term, ':|season|W|C'))  %>% 
    select(outcome, term, p.value),
  by = c('outcome', 'term')
)

writexl::write_xlsx(tableS1 %>% as.data.frame(),
                    here::here(paste0(Sys.Date(), '_tableS1.xlsx')))

