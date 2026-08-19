library('mgcv')
library('gratia')
library('broom')
source(here::here('00_00_functions.R'))

df_3 <- df_2 %>% 
  ungroup %>% 
  filter(!is.na(o3_24h) & !is.na(temp_24h) & !is.na(pm10_24h) & !is.na(no2_24h),
         !is.na(sbp) & !is.na(dbp) & !is.na(hr),
         !is.na(sex) & !is.na(age) & !is.na(diab) & !is.na(whr) & !is.na(smo) & !is.na(alc) & !is.na(phy) & !is.na(ses)) %>%
  mutate(temp_24h_t = factor(ntile(temp_24h, 3)),
         o3_24h_t = factor(ntile(o3_24h, 3)),
         site = factor(site))

# 1 SBP ------------------------------------------------------------------------
fit_sbp_m1t_lt <-
  gam(
    formula = log(sbp) ~ temp_24h + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_sbp_m1t_lt)
#appraise(fit_sbp_m1t_lt)
#draw(fit_sbp_m1t_lt)

fit_sbp_m1t_st <-
  gam(
    formula = log(sbp) ~ s(temp_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_sbp_m1t_st)
#appraise(fit_sbp_m1t_st)
#draw(fit_sbp_m1t_st)

fit_sbp_m1o_lo <-
  gam(
    formula = log(sbp) ~ o3_24h + 
      sex + age + diab + whr + smo + alc + phy + ses + edu +
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_sbp_m1o_lo)
#appraise(fit_sbp_m1o_lo)
#draw(fit_sbp_m1o_lo)

fit_sbp_m1o_so <-
  gam(
    formula = log(sbp) ~ s(o3_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_sbp_m1o_so)
#appraise(fit_sbp_m1o_so)
#draw(fit_sbp_m1o_so)

fit_sbp_m2_sto <-
  gam(
    formula = log(sbp) ~ s(temp_24h, bs = 'cs', k = 4, fx = T) + s(o3_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_sbp_m2_sto)
#appraise(fit_sbp_m2_sto)
#draw(fit_sbp_m2_sto)

fit_sbp_m3_sto <-
  gam(
    formula = log(sbp) ~ s(temp_24h, bs = 'cs', k = 4, fx = T) + s(o3_24h, bs = 'cs', k = 4, fx = T) + 
      s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_sto)
#appraise(fit_sbp_m3_sto)
#draw(fit_sbp_m3_sto)

fit_sbp_m3_lt_so <-
  gam(
    formula = log(sbp) ~ temp_24h + s(o3_24h, bs = 'cs', k = 4, fx = T) + 
      s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lt_so)
#appraise(fit_sbp_m3_lt_so)
#draw(fit_sbp_m3_lt_so)

fit_sbp_m3_lo_st <-
  gam(
    formula = log(sbp) ~ o3_24h + s(temp_24h, bs = 'cs', k = 4, fx = T) + 
      s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lo_st)
#appraise(fit_sbp_m3_lo_st)
#draw(fit_sbp_m3_lo_st)

fit_sbp_m3_lto <-
  gam(
    formula = log(sbp) ~ o3_24h + temp_24h + 
      s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lto)
#appraise(fit_sbp_m3_lto)
#draw(fit_sbp_m3_lto)

fit_sbp_m3_lto_i <-
  gam(
    formula = log(sbp) ~ o3_24h * temp_24h + 
      s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lto_i)
#appraise(fit_sbp_m3_lto_i)
#draw(fit_sbp_m3_lto_i)

fit_sbp_m3_lto_tot <-
  gam(
    formula = log(sbp) ~ o3_24h : temp_24h_t + temp_24h +
      s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lto_i)
#appraise(fit_sbp_m3_lto_i)
#draw(fit_sbp_m3_lto_i)

fit_sbp_m3_lto_ttt <-
  gam(
    formula = log(sbp) ~ temp_24h : temp_24h_t + o3_24h +
      s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lto_ttt)
#appraise(fit_sbp_m3_lto_ttt)
#draw(fit_sbp_m3_lto_ttt)

fit_sbp_m3_lto_too <-
  gam(
    formula = log(sbp) ~ o3_24h : o3_24h_t + temp_24h +
      s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lto_i)
#appraise(fit_sbp_m3_lto_i)
#draw(fit_sbp_m3_lto_i)

fit_sbp_m3_lto_tto <-
  gam(
    formula = log(sbp) ~ temp_24h : o3_24h_t + o3_24h +
      s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lto_i)
#appraise(fit_sbp_m3_lto_i)
#draw(fit_sbp_m3_lto_i)

fig1_sbp_temp <- bind_rows(
  rel_smooth(fit_sbp_m1t_st, 'temp_24h') %>% mutate(model = 1),
  rel_smooth(fit_sbp_m2_sto, 'temp_24h') %>% mutate(model = 2),
  rel_smooth(fit_sbp_m3_sto, 'temp_24h') %>% mutate(model = 3)
) %>% 
  mutate(model = factor(model)) %>% 
  ggplot(aes(x = value * sd(df_1$temp_24h, na.rm = T) + mean(df_1$temp_24h, na.rm = T), 
             group = model)) +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = model), alpha = .2) +
  geom_line(aes(y = est, colour = model, linetype = model)) +
  scale_y_continuous(labels = scales::percent, trans = 'log1p') +
  scale_colour_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_fill_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_linetype_manual(values = c('dashed', 'dotted', 'solid')) +
  scale_x_continuous(expand = c(0,0), breaks = -1:3*5) +
  coord_cartesian(ylim = c(-.02, .02)) +
  labs(x = 'Temperature, 24h mean [\u00b0C]',
       y = 'SBP') +
  theme_classic() +
  theme(legend.position = 'none')

fig1_sbp_o3 <- bind_rows(
  rel_smooth(fit_sbp_m1o_so, 'o3_24h') %>% mutate(model = 1),
  rel_smooth(fit_sbp_m2_sto, 'o3_24h') %>% mutate(model = 2),
  rel_smooth(fit_sbp_m3_sto, 'o3_24h') %>% mutate(model = 3)
) %>% 
  mutate(model = factor(model)) %>% 
  ggplot(aes(x = value * sd(df_1$o3_24h, na.rm = T) + mean(df_1$o3_24h, na.rm = T), 
             group = model)) +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = model), alpha = .2) +
  geom_line(aes(y = est, colour = model, linetype = model)) +
  scale_y_continuous(labels = scales::percent, trans = 'log1p') +
  scale_colour_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_fill_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_linetype_manual(values = c('dashed', 'dotted', 'solid')) +
  scale_x_continuous(expand = c(0,0), breaks = 0:10*10) +
  coord_cartesian(ylim = c(-.02, .02)) +
  labs(x = 'O3, 24h mean [\u00b5g/m\u00b3]',
       y = 'SBP') +
  theme_classic() +
  theme(legend.position = 'none')

# 2 DBP ------------------------------------------------------------------------
fit_dbp_m1t_lt <-
  gam(
    formula = log(dbp) ~ temp_24h + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_dbp_m1t_lt)
#appraise(fit_dbp_m1t_lt)
#draw(fit_dbp_m1t_lt)

fit_dbp_m1t_st <-
  gam(
    formula = log(dbp) ~ s(temp_24h, bs = 'cs') + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_dbp_m1t_st)
#appraise(fit_dbp_m1t_st)
#draw(fit_dbp_m1t_st)

fit_dbp_m1o_lo <-
  gam(
    formula = log(dbp) ~ o3_24h + 
      sex + age + diab + whr + smo + alc + phy + ses + edu +
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_dbp_m1o_lo)
#appraise(fit_dbp_m1o_lo)
#draw(fit_dbp_m1o_lo)

fit_dbp_m1o_so <-
  gam(
    formula = log(dbp) ~ s(o3_24h, bs = 'cs') + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_dbp_m1o_so)
#appraise(fit_dbp_m1o_so)
#draw(fit_dbp_m1o_so)

fit_dbp_m2_sto <-
  gam(
    formula = log(dbp) ~ s(temp_24h, bs = 'cs', k = 4, fx = T) + s(o3_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_dbp_m2_sto)
#appraise(fit_dbp_m2_sto)
#draw(fit_dbp_m2_sto)

fit_dbp_m3_sto <-
  gam(
    formula = log(dbp) ~ s(temp_24h, bs = 'cs', k = 4, fx = T) + s(o3_24h, bs = 'cs', k = 4, fx = T) + 
      s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_sto)
#appraise(fit_dbp_m3_sto)
#draw(fit_dbp_m3_sto)

fit_dbp_m3_lt_so <-
  gam(
    formula = log(dbp) ~ temp_24h + s(o3_24h, bs = 'cs', k = 4, fx = T) + 
      s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lt_so)
#appraise(fit_dbp_m3_lt_so)
#draw(fit_dbp_m3_lt_so)

fit_dbp_m3_lo_st <-
  gam(
    formula = log(dbp) ~ o3_24h + s(temp_24h, bs = 'cs', k = 4, fx = T) + 
      s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lo_st)
#appraise(fit_dbp_m3_lo_st)
#draw(fit_dbp_m3_lo_st)

fit_dbp_m3_lto <-
  gam(
    formula = log(dbp) ~ o3_24h + temp_24h + 
      s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lto)
#appraise(fit_dbp_m3_lto)
#draw(fit_dbp_m3_lto)

fit_dbp_m3_lto_i <-
  gam(
    formula = log(dbp) ~ o3_24h * temp_24h + 
      s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lto_i)
#appraise(fit_dbp_m3_lto_i)
#draw(fit_dbp_m3_lto_i)

fit_dbp_m3_lto_tot <-
  gam(
    formula = log(dbp) ~ o3_24h : temp_24h_t + temp_24h +
      s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lto_i)
#appraise(fit_dbp_m3_lto_i)
#draw(fit_dbp_m3_lto_i)

fit_dbp_m3_lto_ttt <-
  gam(
    formula = log(dbp) ~ temp_24h : temp_24h_t + o3_24h +
      s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lto_ttt)
#appraise(fit_dbp_m3_lto_ttt)
#draw(fit_dbp_m3_lto_ttt)

fit_dbp_m3_lto_too <-
  gam(
    formula = log(dbp) ~ o3_24h : o3_24h_t + temp_24h +
      s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lto_i)
#appraise(fit_dbp_m3_lto_i)
#draw(fit_dbp_m3_lto_i)

fit_dbp_m3_lto_tto <-
  gam(
    formula = log(dbp) ~ temp_24h : o3_24h_t + o3_24h +
      s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lto_i)
#appraise(fit_dbp_m3_lto_i)
#draw(fit_dbp_m3_lto_i)

fig1_dbp_temp <- bind_rows(
  rel_smooth(fit_dbp_m1t_st, 'temp_24h') %>% mutate(model = 1),
  rel_smooth(fit_dbp_m2_sto, 'temp_24h') %>% mutate(model = 2),
  rel_smooth(fit_dbp_m3_sto, 'temp_24h') %>% mutate(model = 3)
) %>% 
  mutate(model = factor(model)) %>% 
  ggplot(aes(x = value * sd(df_1$temp_24h, na.rm = T) + mean(df_1$temp_24h, na.rm = T), 
             group = model)) +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = model), alpha = .2) +
  geom_line(aes(y = est, colour = model, linetype = model)) +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_fill_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_linetype_manual(values = c('dashed', 'dotted', 'solid')) +
  scale_x_continuous(expand = c(0,0), breaks = -1:3*5) +
  coord_cartesian(ylim = c(-.02, .02)) +
  labs(x = 'Temperature, 24h mean [\u00b0C]',
       y = 'DBP') +
  theme_classic() +
  theme(legend.position = 'none')

fig1_dbp_o3 <- bind_rows(
  rel_smooth(fit_dbp_m1o_so, 'o3_24h') %>% mutate(model = 1),
  rel_smooth(fit_dbp_m2_sto, 'o3_24h') %>% mutate(model = 2),
  rel_smooth(fit_dbp_m3_sto, 'o3_24h') %>% mutate(model = 3)
) %>% 
  mutate(model = factor(model)) %>% 
  ggplot(aes(x = value * sd(df_1$o3_24h, na.rm = T) + mean(df_1$o3_24h, na.rm = T), 
             group = model)) +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = model), alpha = .2) +
  geom_line(aes(y = est, colour = model, linetype = model)) +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_fill_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_linetype_manual(values = c('dashed', 'dotted', 'solid')) +
  scale_x_continuous(expand = c(0,0), breaks = 0:10*10) +
  coord_cartesian(ylim = c(-.02, .02)) +
  labs(x = 'O3, 24h mean [\u00b5g/m\u00b3]',
       y = 'DBP') +
  theme_classic() +
  theme(legend.position = 'none')

# 3 PP -------------------------------------------------------------------------
fit_pp_m1t_lt <-
  gam(
    formula = log(pp) ~ temp_24h + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_pp_m1t_lt)
#appraise(fit_pp_m1t_lt)
#draw(fit_pp_m1t_lt)

fit_pp_m1t_st <-
  gam(
    formula = log(pp) ~ s(temp_24h, bs = 'cs') + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_pp_m1t_st)
#appraise(fit_pp_m1t_st)
#draw(fit_pp_m1t_st)

fit_pp_m1o_lo <-
  gam(
    formula = log(pp) ~ o3_24h + 
      sex + age + diab + whr + smo + alc + phy + ses + edu +
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_pp_m1o_lo)
#appraise(fit_pp_m1o_lo)
#draw(fit_pp_m1o_lo)

fit_pp_m1o_so <-
  gam(
    formula = log(pp) ~ s(o3_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_pp_m1o_so)
#appraise(fit_pp_m1o_so)
#draw(fit_pp_m1o_so)

fit_pp_m2_sto <-
  gam(
    formula = log(pp) ~ s(temp_24h, bs = 'cs', k = 4, fx = T) + s(o3_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_pp_m2_sto)
#appraise(fit_pp_m2_sto)
#draw(fit_pp_m2_sto)

fit_pp_m3_sto <-
  gam(
    formula = log(pp) ~ s(temp_24h, bs = 'cs', k = 4, fx = T) + s(o3_24h, bs = 'cs', k = 4, fx = T) + 
      s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_sto)
#appraise(fit_pp_m3_sto)
#draw(fit_pp_m3_sto)

fit_pp_m3_lt_so <-
  gam(
    formula = log(pp) ~ temp_24h + s(o3_24h, bs = 'cs', k = 4, fx = T) + 
      s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lt_so)
#appraise(fit_pp_m3_lt_so)
#draw(fit_pp_m3_lt_so)

fit_pp_m3_lo_st <-
  gam(
    formula = log(pp) ~ o3_24h + s(temp_24h, bs = 'cs', k = 4, fx = T) + 
      s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lo_st)
#appraise(fit_pp_m3_lo_st)
#draw(fit_pp_m3_lo_st)

fit_pp_m3_lto <-
  gam(
    formula = log(pp) ~ o3_24h + temp_24h + 
      s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lto)
#appraise(fit_pp_m3_lto)
#draw(fit_pp_m3_lto)

fit_pp_m3_lto_i <-
  gam(
    formula = log(pp) ~ o3_24h * temp_24h + 
      s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lto_i)
#appraise(fit_pp_m3_lto_i)
#draw(fit_pp_m3_lto_i)

fit_pp_m3_lto_tot <-
  gam(
    formula = log(pp) ~ o3_24h : temp_24h_t + temp_24h +
      s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lto_i)
#appraise(fit_pp_m3_lto_i)
#draw(fit_pp_m3_lto_i)

fit_pp_m3_lto_ttt <-
  gam(
    formula = log(pp) ~ temp_24h : temp_24h_t + o3_24h +
      s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lto_ttt)
#appraise(fit_pp_m3_lto_ttt)
#draw(fit_pp_m3_lto_ttt)

fit_pp_m3_lto_too <-
  gam(
    formula = log(pp) ~ o3_24h : o3_24h_t + temp_24h +
      s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lto_i)
#appraise(fit_pp_m3_lto_i)
#draw(fit_pp_m3_lto_i)

fit_pp_m3_lto_tto <-
  gam(
    formula = log(pp) ~ temp_24h : o3_24h_t + o3_24h +
      s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lto_i)
#appraise(fit_pp_m3_lto_i)
#draw(fit_pp_m3_lto_i)

fig1_pp_temp <- bind_rows(
  rel_smooth(fit_pp_m1t_st, 'temp_24h') %>% mutate(model = 1),
  rel_smooth(fit_pp_m2_sto, 'temp_24h') %>% mutate(model = 2),
  rel_smooth(fit_pp_m3_sto, 'temp_24h') %>% mutate(model = 3)
) %>% 
  mutate(model = factor(model)) %>% 
  ggplot(aes(x = value * sd(df_1$temp_24h, na.rm = T) + mean(df_1$temp_24h, na.rm = T), 
             group = model)) +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = model), alpha = .2) +
  geom_line(aes(y = est, colour = model, linetype = model)) +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_fill_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_linetype_manual(values = c('dashed', 'dotted', 'solid')) +
  scale_x_continuous(expand = c(0,0), breaks = -1:3*5) +
  coord_cartesian(ylim = c(-.02, .02)) +
  labs(x = 'Temperature, 24h mean [\u00b0C]',
       y = 'PP') +
  theme_classic() +
  theme(legend.position = 'none')

fig1_pp_o3 <- bind_rows(
  rel_smooth(fit_pp_m1o_so, 'o3_24h') %>% mutate(model = 1),
  rel_smooth(fit_pp_m2_sto, 'o3_24h') %>% mutate(model = 2),
  rel_smooth(fit_pp_m3_sto, 'o3_24h') %>% mutate(model = 3)
) %>% 
  mutate(model = factor(model)) %>% 
  ggplot(aes(x = value * sd(df_1$o3_24h, na.rm = T) + mean(df_1$o3_24h, na.rm = T), 
             group = model)) +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = model), alpha = .2) +
  geom_line(aes(y = est, colour = model, linetype = model)) +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_fill_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_linetype_manual(values = c('dashed', 'dotted', 'solid')) +
  scale_x_continuous(expand = c(0,0), breaks = 0:10*10) +
  coord_cartesian(ylim = c(-.02, .02)) +
  labs(x = 'O3, 24h mean [\u00b5g/m\u00b3]',
       y = 'PP') +
  theme_classic() +
  theme(legend.position = 'none')

# 4 HR -------------------------------------------------------------------------
fit_hr_m1t_lt <-
  gam(
    formula = log(hr) ~ temp_24h + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_hr_m1t_lt)
#appraise(fit_hr_m1t_lt)
#draw(fit_hr_m1t_lt)

fit_hr_m1t_st <-
  gam(
    formula = log(hr) ~ s(temp_24h, bs = 'cs') + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_hr_m1t_st)
#appraise(fit_hr_m1t_st)
#draw(fit_hr_m1t_st)

fit_hr_m1o_lo <-
  gam(
    formula = log(hr) ~ o3_24h + 
      sex + age + diab + whr + smo + alc + phy + ses + edu +
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_hr_m1o_lo)
#appraise(fit_hr_m1o_lo)
#draw(fit_hr_m1o_lo)

fit_hr_m1o_so <-
  gam(
    formula = log(hr) ~ s(o3_24h, bs = 'cs') + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_hr_m1o_so)
#appraise(fit_hr_m1o_so)
#draw(fit_hr_m1o_so)

fit_hr_m2_sto <-
  gam(
    formula = log(hr) ~ s(temp_24h, bs = 'cs', k = 4, fx = T) + s(o3_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc') + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    method = 'REML'
  )
#summary(fit_hr_m2_sto)
#appraise(fit_hr_m2_sto)
#draw(fit_hr_m2_sto)

fit_hr_m3_sto <-
  gam(
    formula = log(hr) ~ s(temp_24h, bs = 'cs', k = 4, fx = T) + s(o3_24h, bs = 'cs', k = 4, fx = T) + 
      s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_sto)
#appraise(fit_hr_m3_sto)
#draw(fit_hr_m3_sto)

fit_hr_m3_lt_so <-
  gam(
    formula = log(hr) ~ temp_24h + s(o3_24h, bs = 'cs', k = 4, fx = T) + 
      s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lt_so)
#appraise(fit_hr_m3_lt_so)
#draw(fit_hr_m3_lt_so)

fit_hr_m3_lo_st <-
  gam(
    formula = log(hr) ~ o3_24h + s(temp_24h, bs = 'cs', k = 4, fx = T) + 
      s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lo_st)
#appraise(fit_hr_m3_lo_st)
#draw(fit_hr_m3_lo_st)

fit_hr_m3_lto <-
  gam(
    formula = log(hr) ~ o3_24h + temp_24h + 
      s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lto)
#appraise(fit_hr_m3_lto)
#draw(fit_hr_m3_lto)

fit_hr_m3_lto_i <-
  gam(
    formula = log(hr) ~ o3_24h * temp_24h + 
      s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lto_i)
#appraise(fit_hr_m3_lto_i)
#draw(fit_hr_m3_lto_i)

fit_hr_m3_lto_tot <-
  gam(
    formula = log(hr) ~ o3_24h : temp_24h_t + temp_24h +
      s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lto_i)
#appraise(fit_hr_m3_lto_i)
#draw(fit_hr_m3_lto_i)

fit_hr_m3_lto_ttt <-
  gam(
    formula = log(hr) ~ temp_24h : temp_24h_t + o3_24h +
      s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lto_ttt)
#appraise(fit_hr_m3_lto_ttt)
#draw(fit_hr_m3_lto_ttt)

fit_hr_m3_lto_too <-
  gam(
    formula = log(hr) ~ o3_24h : o3_24h_t + temp_24h +
      s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lto_i)
#appraise(fit_hr_m3_lto_i)
#draw(fit_hr_m3_lto_i)

fit_hr_m3_lto_tto <-
  gam(
    formula = log(hr) ~ temp_24h : o3_24h_t + o3_24h +
      s(pm10_24h, bs = 'cs', k = 4, fx = T) + s(no2_24h, bs = 'cs', k = 4, fx = T) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lto_i)
#appraise(fit_hr_m3_lto_i)
#draw(fit_hr_m3_lto_i)

fig1_hr_temp <- bind_rows(
  rel_smooth(fit_sbp_m1t_st, 'temp_24h') %>% mutate(model = 1),
  rel_smooth(fit_sbp_m2_sto, 'temp_24h') %>% mutate(model = 2),
  rel_smooth(fit_sbp_m3_sto, 'temp_24h') %>% mutate(model = 3)
) %>% 
  mutate(model = factor(model)) %>% 
  ggplot(aes(x = value * sd(df_1$temp_24h, na.rm = T) + mean(df_1$temp_24h, na.rm = T), 
             group = model)) +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = model), alpha = .2) +
  geom_line(aes(y = est, colour = model, linetype = model)) +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_fill_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_linetype_manual(values = c('dashed', 'dotted', 'solid')) +
  scale_x_continuous(expand = c(0,0), breaks = -1:3*5) +
  coord_cartesian(ylim = c(-.02, .02)) +
  labs(x = 'Temperature, 24h mean [\u00b0C]',
       y = 'HR') +
  theme_classic() +
  theme(legend.position = 'none')

fig1_hr_o3 <- bind_rows(
  rel_smooth(fit_sbp_m1o_so, 'o3_24h') %>% mutate(model = 1),
  rel_smooth(fit_sbp_m2_sto, 'o3_24h') %>% mutate(model = 2),
  rel_smooth(fit_sbp_m3_sto, 'o3_24h') %>% mutate(model = 3)
) %>% 
  mutate(model = factor(model)) %>% 
  ggplot(aes(x = value * sd(df_1$o3_24h, na.rm = T) + mean(df_1$o3_24h, na.rm = T), 
             group = model)) +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = model), alpha = .2) +
  geom_line(aes(y = est, colour = model, linetype = model)) +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_fill_manual(values = c('darkorange', 'darkgreen', 'darkblue')) +
  scale_linetype_manual(values = c('dashed', 'dotted', 'solid')) +
  scale_x_continuous(expand = c(0,0), breaks = 0:10*10) +
  coord_cartesian(ylim = c(-.02, .02)) +
  labs(x = 'O3, 24h mean [\u00b5g/m\u00b3]',
       y = 'HR') +
  theme_classic() +
  theme(legend.position = 'none')

# Plot ------------------------------------------------------------------------
cowplot::plot_grid(
  fig1_sbp_o3,
  fig1_dbp_o3,
  fig1_pp_o3,
  fig1_hr_o3,
  fig1_sbp_temp,
  fig1_dbp_temp,
  fig1_pp_temp,
  fig1_hr_temp,
  nrow = 2,
  align = 'hv',
  labels = c('A', '', '', '', 'B', '', '', '')
)
ggsave(here::here('figures', paste0(Sys.Date(), '_fig1.svg')), width = 14, height = 6)
ggsave(here::here('figures', paste0(Sys.Date(), '_fig1.png')), width = 14, height = 6)

# Table ------------------------------------------------------------------------
table3 <- 
  rbind(
    table3_helper(outc = 'sbp', 
                  expo = 'temp_24h', 
                  base_model = fit_sbp_m3_lt_so, 
                  spline_model = fit_sbp_m3_sto,
                  inter_model = fit_sbp_m3_lto_i,
                  tertile_model_same = fit_sbp_m3_lto_ttt,
                  tertile_model_other = fit_sbp_m3_lto_tto) %>% table3_format(),
    table3_helper(outc = 'dbp', 
                  expo = 'temp_24h', 
                  base_model = fit_dbp_m3_lt_so, 
                  spline_model = fit_dbp_m3_sto,
                  inter_model = fit_dbp_m3_lto_i,
                  tertile_model_same = fit_dbp_m3_lto_ttt,
                  tertile_model_other = fit_dbp_m3_lto_tto) %>% table3_format(),
    table3_helper(outc = 'pp', 
                  expo = 'temp_24h', 
                  base_model = fit_pp_m3_lt_so, 
                  spline_model = fit_pp_m3_sto,
                  inter_model = fit_pp_m3_lto_i,
                  tertile_model_same = fit_pp_m3_lto_ttt,
                  tertile_model_other = fit_pp_m3_lto_tto) %>% table3_format(),
    table3_helper(outc = 'hr', 
                  expo = 'temp_24h', 
                  base_model = fit_hr_m3_lt_so, 
                  spline_model = fit_hr_m3_sto,
                  inter_model = fit_hr_m3_lto_i,
                  tertile_model_same = fit_hr_m3_lto_ttt,
                  tertile_model_other = fit_hr_m3_lto_tto) %>% table3_format(),
    
    table3_helper(outc = 'sbp', 
                  expo = 'o3_24h', 
                  base_model = fit_sbp_m3_lo_st, 
                  spline_model = fit_sbp_m3_sto,
                  inter_model = fit_sbp_m3_lto_i,
                  tertile_model_same = fit_sbp_m3_lto_too,
                  tertile_model_other = fit_sbp_m3_lto_tot) %>% table3_format(),
    table3_helper(outc = 'dbp', 
                  expo = 'o3_24h', 
                  base_model = fit_dbp_m3_lo_st, 
                  spline_model = fit_dbp_m3_sto,
                  inter_model = fit_dbp_m3_lto_i,
                  tertile_model_same = fit_dbp_m3_lto_too,
                  tertile_model_other = fit_dbp_m3_lto_tot) %>% table3_format(),
    table3_helper(outc = 'pp', 
                  expo = 'o3_24h', 
                  base_model = fit_pp_m3_lo_st, 
                  spline_model = fit_pp_m3_sto,
                  inter_model = fit_pp_m3_lto_i,
                  tertile_model_same = fit_pp_m3_lto_too,
                  tertile_model_other = fit_pp_m3_lto_tot) %>% table3_format(),
    table3_helper(outc = 'hr', 
                  expo = 'o3_24h', 
                  base_model = fit_hr_m3_lo_st, 
                  spline_model = fit_hr_m3_sto,
                  inter_model = fit_hr_m3_lto_i,
                  tertile_model_same = fit_hr_m3_lto_too,
                  tertile_model_other = fit_hr_m3_lto_tot) %>% table3_format()
  )

writexl::write_xlsx(table3 %>% as.data.frame(),
                    here::here(paste0(Sys.Date(), '_table3.xlsx')))
