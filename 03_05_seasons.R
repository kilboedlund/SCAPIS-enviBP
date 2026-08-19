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
fit_sbp_m3_lto_s <-
  gam(
    formula = log(sbp) ~ o3_24h:season + temp_24h:season + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lto_s)
#appraise(fit_sbp_m3_lto_s)
#draw(fit_sbp_m3_lto_s)

fit_sbp_m3_lto_si <-
  gam(
    formula = log(sbp) ~ o3_24h*season + temp_24h*season + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lto_si)
#appraise(fit_sbp_m3_lto_si)
#draw(fit_sbp_m3_lto_si)

# 2 DBP ------------------------------------------------------------------------
fit_dbp_m3_lto_s <-
  gam(
    formula = log(dbp) ~ o3_24h:season + temp_24h:season + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lto_s)
#appraise(fit_dbp_m3_lto_s)
#draw(fit_dbp_m3_lto_s)

fit_dbp_m3_lto_si <-
  gam(
    formula = log(dbp) ~ o3_24h*season + temp_24h*season + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lto_si)
#appraise(fit_dbp_m3_lto_si)
#draw(fit_dbp_m3_lto_si)

# 3 PP -------------------------------------------------------------------------
fit_pp_m3_lto_s <-
  gam(
    formula = log(pp) ~ o3_24h:season + temp_24h:season + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lto_s)
#appraise(fit_pp_m3_lto_s)
#draw(fit_pp_m3_lto_s)

fit_pp_m3_lto_si <-
  gam(
    formula = log(pp) ~ o3_24h*season + temp_24h*season + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lto_si)
#appraise(fit_pp_m3_lto_si)
#draw(fit_pp_m3_lto_si)

# 4 HR -------------------------------------------------------------------------
fit_hr_m3_lto_s <-
  gam(
    formula = log(hr) ~ o3_24h:season + temp_24h:season + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lto_s)
#appraise(fit_hr_m3_lto_s)
#draw(fit_hr_m3_lto_s)

fit_hr_m3_lto_si <-
  gam(
    formula = log(hr) ~ o3_24h*season + temp_24h*season + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lto_si)
#appraise(fit_hr_m3_lto_si)
#draw(fit_hr_m3_lto_si)

table4 <- 
  bind_rows(
    season_helper(fit_sbp_m3_lto_s, 'sbp'),
    season_helper(fit_dbp_m3_lto_s, 'dbp'),
    season_helper(fit_pp_m3_lto_s, 'pp'),
    season_helper(fit_hr_m3_lto_s, 'hr')
  ) %>% 
  pivot_wider(id_cols = c('outcome', 'term'), names_from = 'season', values_from = 'f') %>% 
  left_join(
    bind_rows(
      tidy(fit_sbp_m3_lto_si, parametric = T) %>% mutate(outcome = 'sbp'),
      tidy(fit_dbp_m3_lto_si, parametric = T) %>% mutate(outcome = 'dbp'),
      tidy(fit_pp_m3_lto_si, parametric = T) %>% mutate(outcome = 'pp'),
      tidy(fit_hr_m3_lto_si, parametric = T) %>% mutate(outcome = 'hr')
    ) %>% 
      filter(str_detect(term, ':'))  %>% 
      mutate(term = str_remove_all(term, ':|season|W|C'))  %>% 
      select(outcome, term, p.value),
    by = c('outcome', 'term')
  )

writexl::write_xlsx(table4 %>% as.data.frame(),
                    here::here(paste0(Sys.Date(), '_table4.xlsx')))

