library('splines')
library('broom.mixed')
source(here::here('00_00_functions.R'))

# * Effect modification by NDVI ------------------------------------------------
df_3 <- df_2 %>% 
  ungroup %>% 
  filter(!is.na(o3_24h) & !is.na(temp_24h) & !is.na(pm10_24h) & !is.na(no2_24h),
         !is.na(sbp) & !is.na(dbp) & !is.na(hr),
         !is.na(sex) & !is.na(age) & !is.na(diab) & !is.na(whr) & !is.na(smo) & !is.na(alc) & !is.na(phy) & !is.na(ses)) %>%
  mutate(temp_24h_t = factor(ntile(temp_24h, 3)),
         o3_24h_t = factor(ntile(o3_24h, 3)),
         site = factor(site),
         ndvi_trend = ntile(ndvi, 3) - 2,
         ndvi_t = factor(ntile(ndvi, 3)))

## 1 SBP -----------------------------------------------------------------------
fit_sbp_m3_lt_so_ndvi <- 
  gam(
    formula = log(sbp) ~ temp_24h:ndvi_t + ndvi_t + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lt_so_ndvi)
#appraise(fit_sbp_m3_lt_so_ndvi)
#draw(fit_sbp_m3_lt_so_ndvi)

fit_sbp_m3_lt_so_ndvi_trend <- 
  gam(
    formula = log(sbp) ~ temp_24h + temp_24h:ndvi_trend + ndvi_t + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lt_so_ndvi_trend)
#appraise(fit_sbp_m3_lt_so_ndvi_trend)
#draw(fit_sbp_m3_lt_so_ndvi_trend)

fit_sbp_m3_lo_st_ndvi <- 
  gam(
    formula = log(sbp) ~ o3_24h:ndvi_t + ndvi_t + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lo_st_ndvi)
#appraise(fit_sbp_m3_lo_st_ndvi)
#draw(fit_sbp_m3_lo_st_ndvi)

fit_sbp_m3_lo_st_ndvi_trend <- 
  gam(
    formula = log(sbp) ~ o3_24h + o3_24h:ndvi_trend + ndvi_t + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lo_st_ndvi_trend)
#appraise(fit_sbp_m3_lo_st_ndvi_trend)
#draw(fit_sbp_m3_lo_st_ndvi_trend)

## 2 DBP -----------------------------------------------------------------------
fit_dbp_m3_lt_so_ndvi <- 
  gam(
    formula = log(dbp) ~ temp_24h:ndvi_t + ndvi_t + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lt_so_ndvi)
#appraise(fit_dbp_m3_lt_so_ndvi)
#draw(fit_dbp_m3_lt_so_ndvi)

fit_dbp_m3_lt_so_ndvi_trend <- 
  gam(
    formula = log(dbp) ~ temp_24h + temp_24h:ndvi_trend + ndvi_t + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lt_so_ndvi_trend)
#appraise(fit_dbp_m3_lt_so_ndvi_trend)
#draw(fit_dbp_m3_lt_so_ndvi_trend)

fit_dbp_m3_lo_st_ndvi <- 
  gam(
    formula = log(dbp) ~ o3_24h:ndvi_t + ndvi_t + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lo_st_ndvi)
#appraise(fit_dbp_m3_lo_st_ndvi)
#draw(fit_dbp_m3_lo_st_ndvi)

fit_dbp_m3_lo_st_ndvi_trend <- 
  gam(
    formula = log(dbp) ~ o3_24h + o3_24h:ndvi_trend + ndvi_t + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lo_st_ndvi_trend)
#appraise(fit_dbp_m3_lo_st_ndvi_trend)
#draw(fit_dbp_m3_lo_st_ndvi_trend)

## 3 PP ------------------------------------------------------------------------
fit_pp_m3_lt_so_ndvi <- 
  gam(
    formula = log(pp) ~ temp_24h:ndvi_t + ndvi_t + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lt_so_ndvi)
#appraise(fit_pp_m3_lt_so_ndvi)
#draw(fit_pp_m3_lt_so_ndvi)

fit_pp_m3_lt_so_ndvi_trend <- 
  gam(
    formula = log(pp) ~ temp_24h + temp_24h:ndvi_trend + ndvi_t + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lt_so_ndvi_trend)
#appraise(fit_pp_m3_lt_so_ndvi_trend)
#draw(fit_pp_m3_lt_so_ndvi_trend)

fit_pp_m3_lo_st_ndvi <- 
  gam(
    formula = log(pp) ~ o3_24h:ndvi_t + ndvi_t + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lo_st_ndvi)
#appraise(fit_pp_m3_lo_st_ndvi)
#draw(fit_pp_m3_lo_st_ndvi)

fit_pp_m3_lo_st_ndvi_trend <- 
  gam(
    formula = log(pp) ~ o3_24h + o3_24h:ndvi_trend + ndvi_t + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lo_st_ndvi_trend)
#appraise(fit_pp_m3_lo_st_ndvi_trend)
#draw(fit_pp_m3_lo_st_ndvi_trend)

## 4 HR ------------------------------------------------------------------------
fit_hr_m3_lt_so_ndvi <- 
  gam(
    formula = log(hr) ~ temp_24h:ndvi_t + ndvi_t + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lt_so_ndvi)
#appraise(fit_hr_m3_lt_so_ndvi)
#draw(fit_hr_m3_lt_so_ndvi)

fit_hr_m3_lt_so_ndvi_trend <- 
  gam(
    formula = log(hr) ~ temp_24h + temp_24h:ndvi_trend + ndvi_t + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lt_so_ndvi_trend)
#appraise(fit_hr_m3_lt_so_ndvi_trend)
#draw(fit_hr_m3_lt_so_ndvi_trend)

fit_hr_m3_lo_st_ndvi <- 
  gam(
    formula = log(hr) ~ o3_24h:ndvi_t + ndvi_t + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lo_st_ndvi)
#appraise(fit_hr_m3_lo_st_ndvi)
#draw(fit_hr_m3_lo_st_ndvi)

fit_hr_m3_lo_st_ndvi_trend <- 
  gam(
    formula = log(hr) ~ o3_24h + o3_24h:ndvi_trend + ndvi_t + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lo_st_ndvi_trend)
#appraise(fit_hr_m3_lo_st_ndvi_trend)
#draw(fit_hr_m3_lo_st_ndvi_trend)

## 5 Table ---------------------------------------------------------------------
table5_ndvi <- bind_rows(
  effect_modific_helper(fit_sbp_m3_lt_so_ndvi, fit_sbp_m3_lt_so_ndvi_trend, 'sbp'),
  effect_modific_helper(fit_dbp_m3_lt_so_ndvi, fit_dbp_m3_lt_so_ndvi_trend, 'dbp'),
  effect_modific_helper(fit_pp_m3_lt_so_ndvi, fit_pp_m3_lt_so_ndvi_trend, 'pp'),
  effect_modific_helper(fit_hr_m3_lt_so_ndvi, fit_hr_m3_lt_so_ndvi_trend, 'hr'),
  effect_modific_helper(fit_sbp_m3_lo_st_ndvi, fit_sbp_m3_lo_st_ndvi_trend, 'sbp'),
  effect_modific_helper(fit_dbp_m3_lo_st_ndvi, fit_dbp_m3_lo_st_ndvi_trend, 'dbp'),
  effect_modific_helper(fit_pp_m3_lo_st_ndvi, fit_pp_m3_lo_st_ndvi_trend, 'pp'),
  effect_modific_helper(fit_hr_m3_lo_st_ndvi, fit_hr_m3_lo_st_ndvi_trend, 'hr')
)

# * Effect modification by CAC score -------------------------------------------
df_3 <- df_2 %>% 
  ungroup %>% 
  filter(!is.na(o3_24h) & !is.na(temp_24h) & !is.na(pm10_24h) & !is.na(no2_24h),
         !is.na(sbp) & !is.na(dbp) & !is.na(hr),
         !is.na(sex) & !is.na(age) & !is.na(diab) & !is.na(whr) & !is.na(smo) & !is.na(alc) & !is.na(phy) & !is.na(ses)) %>%
  mutate(cacs_trend = case_when(cacs == 0 ~ -1,
                                           cacs <= 100 ~ 0,
                                           cacs > 100 ~ 1),
                        cacs_t = factor(cacs_trend + 2))

## 1 SBP -----------------------------------------------------------------------
fit_sbp_m3_lt_so_cacs <- 
  gam(
    formula = log(sbp) ~ temp_24h:cacs_t + cacs_t + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lt_so_cacs)
#appraise(fit_sbp_m3_lt_so_cacs)
#draw(fit_sbp_m3_lt_so_cacs)

fit_sbp_m3_lt_so_cacs_trend <- 
  gam(
    formula = log(sbp) ~ temp_24h + temp_24h:cacs_trend + cacs_t + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lt_so_cacs)
#appraise(fit_sbp_m3_lt_so_cacs)
#draw(fit_sbp_m3_lt_so_cacs)

fit_sbp_m3_lo_st_cacs <- 
  gam(
    formula = log(sbp) ~ o3_24h:cacs_t + cacs + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lo_st_cacs)
#appraise(fit_sbp_m3_lo_st_cacs)
#draw(fit_sbp_m3_lo_st_cacs)

fit_sbp_m3_lo_st_cacs_trend <- 
  gam(
    formula = log(sbp) ~ o3_24h + o3_24h:cacs_trend + cacs_t + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lo_st_cacs_trend)
#appraise(fit_sbp_m3_lo_st_cacs_trend)
#draw(fit_sbp_m3_lo_st_cacs_trend)

## 2 DBP -----------------------------------------------------------------------
fit_dbp_m3_lt_so_cacs <- 
  gam(
    formula = log(dbp) ~ temp_24h:cacs_t + cacs_t + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lt_so_cacs)
#appraise(fit_dbp_m3_lt_so_cacs)
#draw(fit_dbp_m3_lt_so_cacs)

fit_dbp_m3_lt_so_cacs_trend <- 
  gam(
    formula = log(dbp) ~ temp_24h + temp_24h:cacs_trend + cacs_t + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lt_so_cacs_trend)
#appraise(fit_dbp_m3_lt_so_cacs_trend)
#draw(fit_dbp_m3_lt_so_cacs_trend)

fit_dbp_m3_lo_st_cacs <- 
  gam(
    formula = log(dbp) ~ o3_24h:cacs_t + cacs_t + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lo_st_cacs)
#appraise(fit_dbp_m3_lo_st_cacs)
#draw(fit_dbp_m3_lo_st_cacs)

fit_dbp_m3_lo_st_cacs_trend <- 
  gam(
    formula = log(dbp) ~o3_24h + o3_24h:cacs_trend + cacs_t + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lo_st_cacs_trend)
#appraise(fit_dbp_m3_lo_st_cacs_trend)
#draw(fit_dbp_m3_lo_st_cacs_trend)

## 3 PP ------------------------------------------------------------------------
fit_pp_m3_lt_so_cacs <- 
  gam(
    formula = log(pp) ~ temp_24h:cacs_t + cacs_t + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lt_so_cacs)
#appraise(fit_pp_m3_lt_so_cacs)
#draw(fit_pp_m3_lt_so_cacs)

fit_pp_m3_lt_so_cacs_trend <- 
  gam(
    formula = log(pp) ~ temp_24h + temp_24h:cacs_trend + cacs_t + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lt_so_cacs_trend)
#appraise(fit_pp_m3_lt_so_cacs_trend)
#draw(fit_pp_m3_lt_so_cacs_trend)

fit_pp_m3_lo_st_cacs <- 
  gam(
    formula = log(pp) ~ o3_24h:cacs_t + cacs_t + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lo_st_cacs)
#appraise(fit_pp_m3_lo_st_cacs)
#draw(fit_pp_m3_lo_st_cacs)

fit_pp_m3_lo_st_cacs_trend <- 
  gam(
    formula = log(pp) ~ o3_24h + o3_24h:cacs_trend + cacs_t + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lo_st_cacs_trend)
#appraise(fit_pp_m3_lo_st_cacs_trend)
#draw(fit_pp_m3_lo_st_cacs_trend)

## 4 HR ------------------------------------------------------------------------
fit_hr_m3_lt_so_cacs <- 
  gam(
    formula = log(hr) ~ temp_24h:cacs_t + cacs_t + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lt_so_cacs)
#appraise(fit_hr_m3_lt_so_cacs)
#draw(fit_hr_m3_lt_so_cacs)

fit_hr_m3_lt_so_cacs_trend <- 
  gam(
    formula = log(hr) ~ temp_24h + temp_24h:cacs_trend + cacs_t + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lt_so_cacs_trend)
#appraise(fit_hr_m3_lt_so_cacs_trend)
#draw(fit_hr_m3_lt_so_cacs_trend)

fit_hr_m3_lo_st_cacs <- 
  gam(
    formula = log(hr) ~ o3_24h:cacs_t + cacs_t + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lo_st_cacs)
#appraise(fit_hr_m3_lo_st_cacs)
#draw(fit_hr_m3_lo_st_cacs)

fit_hr_m3_lo_st_cacs_trend <- 
  gam(
    formula = log(hr) ~ o3_24h + o3_24h:cacs_trend + cacs_t + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lo_st_cacs_trend)
#appraise(fit_hr_m3_lo_st_cacs_trend)
#draw(fit_hr_m3_lo_st_cacs_trend)

## 5 Table ---------------------------------------------------------------------
table5_cacs <- bind_rows(
  effect_modific_helper(fit_sbp_m3_lt_so_cacs, fit_sbp_m3_lt_so_cacs_trend, 'sbp'),
  effect_modific_helper(fit_dbp_m3_lt_so_cacs, fit_dbp_m3_lt_so_cacs_trend, 'dbp'),
  effect_modific_helper(fit_pp_m3_lt_so_cacs, fit_pp_m3_lt_so_cacs_trend, 'pp'),
  effect_modific_helper(fit_hr_m3_lt_so_cacs, fit_hr_m3_lt_so_cacs_trend, 'hr'),
  effect_modific_helper(fit_sbp_m3_lo_st_cacs, fit_sbp_m3_lo_st_cacs_trend, 'sbp'),
  effect_modific_helper(fit_dbp_m3_lo_st_cacs, fit_dbp_m3_lo_st_cacs_trend, 'dbp'),
  effect_modific_helper(fit_pp_m3_lo_st_cacs, fit_pp_m3_lo_st_cacs_trend, 'pp'),
  effect_modific_helper(fit_hr_m3_lo_st_cacs, fit_hr_m3_lo_st_cacs_trend, 'hr')
)

# * Effect modification by age group -------------------------------------------
df_3 <- df_2 %>% 
  ungroup %>% 
  filter(!is.na(o3_24h) & !is.na(temp_24h) & !is.na(pm10_24h) & !is.na(no2_24h),
         !is.na(sbp) & !is.na(dbp) & !is.na(hr),
         !is.na(sex) & !is.na(age) & !is.na(diab) & !is.na(whr) & !is.na(smo) & !is.na(alc) & !is.na(phy) & !is.na(ses)) %>%
  mutate(agegr_trend = case_when(age < 55 ~ -1,
                                            age >= 55 & age < 60 ~ 0,
                                            age >= 60 ~ 1),
                        agegr_t = factor(agegr_trend + 2))

## 1 SBP -----------------------------------------------------------------------
fit_sbp_m3_lt_so_agegr <- 
  gam(
    formula = log(sbp) ~ temp_24h:agegr_t + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + agegr_t + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lt_so_agegr)
#appraise(fit_sbp_m3_lt_so_agegr)
#draw(fit_sbp_m3_lt_so_agegr)

fit_sbp_m3_lt_so_agegr_trend <- 
  gam(
    formula = log(sbp) ~ temp_24h + temp_24h:agegr_trend + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + agegr_t + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lt_so_agegr)
#appraise(fit_sbp_m3_lt_so_agegr)
#draw(fit_sbp_m3_lt_so_agegr)

fit_sbp_m3_lo_st_agegr <- 
  gam(
    formula = log(sbp) ~ o3_24h:agegr_t + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + agegr_t + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lo_st_agegr)
#appraise(fit_sbp_m3_lo_st_agegr)
#draw(fit_sbp_m3_lo_st_agegr)

fit_sbp_m3_lo_st_agegr_trend <- 
  gam(
    formula = log(sbp) ~ o3_24h + o3_24h:agegr_trend + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + agegr_t + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lo_st_agegr_trend)
#appraise(fit_sbp_m3_lo_st_agegr_trend)
#draw(fit_sbp_m3_lo_st_agegr_trend)

## 2 DBP -----------------------------------------------------------------------
fit_dbp_m3_lt_so_agegr <- 
  gam(
    formula = log(dbp) ~ temp_24h:agegr_t + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + agegr_t + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lt_so_agegr)
#appraise(fit_dbp_m3_lt_so_agegr)
#draw(fit_dbp_m3_lt_so_agegr)

fit_dbp_m3_lt_so_agegr_trend <- 
  gam(
    formula = log(dbp) ~ temp_24h + temp_24h:agegr_trend + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + agegr_t + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lt_so_agegr_trend)
#appraise(fit_dbp_m3_lt_so_agegr_trend)
#draw(fit_dbp_m3_lt_so_agegr_trend)

fit_dbp_m3_lo_st_agegr <- 
  gam(
    formula = log(dbp) ~ o3_24h:agegr_t + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + agegr_t + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lo_st_agegr)
#appraise(fit_dbp_m3_lo_st_agegr)
#draw(fit_dbp_m3_lo_st_agegr)

fit_dbp_m3_lo_st_agegr_trend <- 
  gam(
    formula = log(dbp) ~ o3_24h + o3_24h:agegr_trend + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + agegr_t + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lo_st_agegr_trend)
#appraise(fit_dbp_m3_lo_st_agegr_trend)
#draw(fit_dbp_m3_lo_st_agegr_trend)

## 3 PP ------------------------------------------------------------------------
fit_pp_m3_lt_so_agegr <- 
  gam(
    formula = log(pp) ~ temp_24h:agegr_t + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + agegr_t + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lt_so_agegr)
#appraise(fit_pp_m3_lt_so_agegr)
#draw(fit_pp_m3_lt_so_agegr)

fit_pp_m3_lt_so_agegr_trend <- 
  gam(
    formula = log(pp) ~ temp_24h + temp_24h:agegr_trend + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + agegr_t + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lt_so_agegr_trend)
#appraise(fit_pp_m3_lt_so_agegr_trend)
#draw(fit_pp_m3_lt_so_agegr_trend)

fit_pp_m3_lo_st_agegr <- 
  gam(
    formula = log(pp) ~ o3_24h:agegr_t + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + agegr_t + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lo_st_agegr)
#appraise(fit_pp_m3_lo_st_agegr)
#draw(fit_pp_m3_lo_st_agegr)

fit_pp_m3_lo_st_agegr_trend <- 
  gam(
    formula = log(pp) ~ o3_24h + o3_24h:agegr_trend + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + agegr_t + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lo_st_agegr_trend)
#appraise(fit_pp_m3_lo_st_agegr_trend)
#draw(fit_pp_m3_lo_st_agegr_trend)

## 4 HR --------------------------------------------------------------------------
fit_hr_m3_lt_so_agegr <- 
  gam(
    formula = log(hr) ~ temp_24h:agegr_t + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + agegr_t + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lt_so_agegr)
#appraise(fit_hr_m3_lt_so_agegr)
#draw(fit_hr_m3_lt_so_agegr)

fit_hr_m3_lt_so_agegr_trend <- 
  gam(
    formula = log(hr) ~ temp_24h + temp_24h:agegr_trend + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + agegr_t + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lt_so_agegr_trend)
#appraise(fit_hr_m3_lt_so_agegr_trend)
#draw(fit_hr_m3_lt_so_agegr_trend)

fit_hr_m3_lo_st_agegr <- 
  gam(
    formula = log(hr) ~ o3_24h:agegr_t + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + agegr_t + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lo_st_agegr)
#appraise(fit_hr_m3_lo_st_agegr)
#draw(fit_hr_m3_lo_st_agegr)

fit_hr_m3_lo_st_agegr_trend <- 
  gam(
    formula = log(hr) ~ o3_24h + o3_24h:agegr_trend + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + agegr_t + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lo_st_agegr_trend)
#appraise(fit_hr_m3_lo_st_agegr_trend)
#draw(fit_hr_m3_lo_st_agegr_trend)

## 5 Table ---------------------------------------------------------------------
table5_age <- bind_rows(
  effect_modific_helper(fit_sbp_m3_lt_so_agegr, fit_sbp_m3_lt_so_agegr_trend, 'sbp'),
  effect_modific_helper(fit_dbp_m3_lt_so_agegr, fit_dbp_m3_lt_so_agegr_trend, 'dbp'),
  effect_modific_helper(fit_pp_m3_lt_so_agegr, fit_pp_m3_lt_so_agegr_trend, 'pp'),
  effect_modific_helper(fit_hr_m3_lt_so_agegr, fit_hr_m3_lt_so_agegr_trend, 'hr'),
  effect_modific_helper(fit_sbp_m3_lo_st_agegr, fit_sbp_m3_lo_st_agegr_trend, 'sbp'),
  effect_modific_helper(fit_dbp_m3_lo_st_agegr, fit_dbp_m3_lo_st_agegr_trend, 'dbp'),
  effect_modific_helper(fit_pp_m3_lo_st_agegr, fit_pp_m3_lo_st_agegr_trend, 'pp'),
  effect_modific_helper(fit_hr_m3_lo_st_agegr, fit_hr_m3_lo_st_agegr_trend, 'hr')
)

# * Effect modification by sex -------------------------------------------------
df_3 <- df_2 %>% 
  ungroup %>% 
  filter(!is.na(o3_24h) & !is.na(temp_24h) & !is.na(pm10_24h) & !is.na(no2_24h),
         !is.na(sbp) & !is.na(dbp) & !is.na(hr),
         !is.na(sex) & !is.na(age) & !is.na(diab) & !is.na(whr) & !is.na(smo) & !is.na(alc) & !is.na(phy) & !is.na(ses)) %>%
  rename('sex_' = 'sex')

## 1 SBP -----------------------------------------------------------------------
fit_sbp_m3_lt_so_sex <- 
  gam(
    formula = log(sbp) ~ temp_24h:sex_ + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex_ + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lt_so_sex)
#appraise(fit_sbp_m3_lt_so_sex)
#draw(fit_sbp_m3_lt_so_sex)

fit_sbp_m3_lt_so_sex_int <- 
  gam(
    formula = log(sbp) ~ temp_24h + temp_24h:sex_ + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex_ + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lt_so_sex_int)
#appraise(fit_sbp_m3_lt_so_sex_int)
#draw(fit_sbp_m3_lt_so_sex_int)

fit_sbp_m3_lo_st_sex <- 
  gam(
    formula = log(sbp) ~ o3_24h:sex_ + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex_ + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lo_st_sex)
#appraise(fit_sbp_m3_lo_st_sex)
#draw(fit_sbp_m3_lo_st_sex)

fit_sbp_m3_lo_st_sex_int <- 
  gam(
    formula = log(sbp) ~ o3_24h + o3_24h:sex_ + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex_ + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lo_st_sex_int)
#appraise(fit_sbp_m3_lo_st_sex_int)
#draw(fit_sbp_m3_lo_st_sex_int)

## 2 DBP -----------------------------------------------------------------------
fit_dbp_m3_lt_so_sex <- 
  gam(
    formula = log(dbp) ~ temp_24h:sex_ + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex_ + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lt_so_sex)
#appraise(fit_dbp_m3_lt_so_sex)
#draw(fit_dbp_m3_lt_so_sex)

fit_dbp_m3_lt_so_sex_int <- 
  gam(
    formula = log(dbp) ~ temp_24h + temp_24h:sex_ + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex_ + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lt_so_sex_int)
#appraise(fit_dbp_m3_lt_so_sex_int)
#draw(fit_dbp_m3_lt_so_sex_int)

fit_dbp_m3_lo_st_sex <- 
  gam(
    formula = log(dbp) ~ o3_24h:sex_ + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex_ + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lo_st_sex)
#appraise(fit_dbp_m3_lo_st_sex)
#draw(fit_dbp_m3_lo_st_sex)

fit_dbp_m3_lo_st_sex_int <- 
  gam(
    formula = log(dbp) ~ o3_24h + o3_24h:sex_ + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex_ + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lo_st_sex_int)
#appraise(fit_dbp_m3_lo_st_sex_int)
#draw(fit_dbp_m3_lo_st_sex_int)

## 3 PP ------------------------------------------------------------------------
fit_pp_m3_lt_so_sex <- 
  gam(
    formula = log(pp) ~ temp_24h:sex_ + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex_ + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lt_so_sex)
#appraise(fit_pp_m3_lt_so_sex)
#draw(fit_pp_m3_lt_so_sex)

fit_pp_m3_lt_so_sex_int <- 
  gam(
    formula = log(pp) ~ temp_24h + temp_24h:sex_ + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex_ + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lt_so_sex_int)
#appraise(fit_pp_m3_lt_so_sex_int)
#draw(fit_pp_m3_lt_so_sex_int)

fit_pp_m3_lo_st_sex <- 
  gam(
    formula = log(pp) ~ o3_24h:sex_ + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex_ + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lo_st_sex)
#appraise(fit_pp_m3_lo_st_sex)
#draw(fit_pp_m3_lo_st_sex)

fit_pp_m3_lo_st_sex_int <- 
  gam(
    formula = log(pp) ~ o3_24h + o3_24h:sex_ + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex_ + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lo_st_sex_int)
#appraise(fit_pp_m3_lo_st_sex_int)
#draw(fit_pp_m3_lo_st_sex_int)

## 4 HR --------------------------------------------------------------------------
fit_hr_m3_lt_so_sex <- 
  gam(
    formula = log(hr) ~ temp_24h:sex_ + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex_ + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lt_so_sex)
#appraise(fit_hr_m3_lt_so_sex)
#draw(fit_hr_m3_lt_so_sex)

fit_hr_m3_lt_so_sex_int <- 
  gam(
    formula = log(hr) ~ temp_24h + temp_24h:sex_ + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex_ + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lt_so_sex_int)
#appraise(fit_hr_m3_lt_so_sex_int)
#draw(fit_hr_m3_lt_so_sex_int)

fit_hr_m3_lo_st_sex <- 
  gam(
    formula = log(hr) ~ o3_24h:sex_ + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex_ + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lo_st_sex)
#appraise(fit_hr_m3_lo_st_sex)
#draw(fit_hr_m3_lo_st_sex)

fit_hr_m3_lo_st_sex_int <- 
  gam(
    formula = log(hr) ~ o3_24h + o3_24h:sex_ + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex_ + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lo_st_sex_int)
#appraise(fit_hr_m3_lo_st_sex_int)
#draw(fit_hr_m3_lo_st_sex_int)

## 5 Table ---------------------------------------------------------------------
tables2_sex <- bind_rows(
  effect_modific_helper(fit_sbp_m3_lt_so_sex, fit_sbp_m3_lt_so_sex_int, 'sbp'),
  effect_modific_helper(fit_dbp_m3_lt_so_sex, fit_dbp_m3_lt_so_sex_int, 'dbp'),
  effect_modific_helper(fit_pp_m3_lt_so_sex, fit_pp_m3_lt_so_sex_int, 'pp'),
  effect_modific_helper(fit_hr_m3_lt_so_sex, fit_hr_m3_lt_so_sex_int, 'hr'),
  effect_modific_helper(fit_sbp_m3_lo_st_sex, fit_sbp_m3_lo_st_sex_int, 'sbp'),
  effect_modific_helper(fit_dbp_m3_lo_st_sex, fit_dbp_m3_lo_st_sex_int, 'dbp'),
  effect_modific_helper(fit_pp_m3_lo_st_sex, fit_pp_m3_lo_st_sex_int, 'pp'),
  effect_modific_helper(fit_hr_m3_lo_st_sex, fit_hr_m3_lo_st_sex_int, 'hr')
)

# * Effect modification by HPT -------------------------------------------
df_3 <- df_2 %>% 
  ungroup %>% 
  filter(!is.na(o3_24h) & !is.na(temp_24h) & !is.na(pm10_24h) & !is.na(no2_24h),
         !is.na(sbp) & !is.na(dbp) & !is.na(hr),
         !is.na(sex) & !is.na(age) & !is.na(diab) & !is.na(whr) & !is.na(smo) & !is.na(alc) & !is.na(phy) & !is.na(ses)) %>%
  mutate(hpt_ = hpt == 'no' & hpt_med == 'no')

## 1 SBP -----------------------------------------------------------------------
fit_sbp_m3_lt_so_hpt <- 
  gam(
    formula = log(sbp) ~ temp_24h:hpt_ + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      hpt_ + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lt_so_hpt)
#appraise(fit_sbp_m3_lt_so_hpt)
#draw(fit_sbp_m3_lt_so_hpt)

fit_sbp_m3_lt_so_hpt_int <- 
  gam(
    formula = log(sbp) ~ temp_24h + temp_24h:hpt_ + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      hpt_ + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lt_so_hpt_int)
#appraise(fit_sbp_m3_lt_so_hpt_int)
#draw(fit_sbp_m3_lt_so_hpt_int)

fit_sbp_m3_lo_st_hpt <- 
  gam(
    formula = log(sbp) ~ o3_24h:hpt_ + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      hpt_ + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lo_st_hpt)
#appraise(fit_sbp_m3_lo_st_hpt)
#draw(fit_sbp_m3_lo_st_hpt)

fit_sbp_m3_lo_st_hpt_int <- 
  gam(
    formula = log(sbp) ~ o3_24h + o3_24h:hpt_ + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      hpt_ + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lo_st_hpt_int)
#appraise(fit_sbp_m3_lo_st_hpt_int)
#draw(fit_sbp_m3_lo_st_hpt_int)

## 2 DBP -----------------------------------------------------------------------
fit_dbp_m3_lt_so_hpt <- 
  gam(
    formula = log(dbp) ~ temp_24h:hpt_ + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      hpt_ + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lt_so_hpt)
#appraise(fit_dbp_m3_lt_so_hpt)
#draw(fit_dbp_m3_lt_so_hpt)

fit_dbp_m3_lt_so_hpt_int <- 
  gam(
    formula = log(dbp) ~ temp_24h + temp_24h:hpt_ + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      hpt_ + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lt_so_hpt_int)
#appraise(fit_dbp_m3_lt_so_hpt_int)
#draw(fit_dbp_m3_lt_so_hpt_int)

fit_dbp_m3_lo_st_hpt <- 
  gam(
    formula = log(dbp) ~ o3_24h:hpt_ + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      hpt_ + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lo_st_hpt)
#appraise(fit_dbp_m3_lo_st_hpt)
#draw(fit_dbp_m3_lo_st_hpt)

fit_dbp_m3_lo_st_hpt_int <- 
  gam(
    formula = log(dbp) ~ o3_24h + o3_24h:hpt_ + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      hpt_ + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lo_st_hpt_int)
#appraise(fit_dbp_m3_lo_st_hpt_int)
#draw(fit_dbp_m3_lo_st_hpt_int)

## 3 PP ------------------------------------------------------------------------
fit_pp_m3_lt_so_hpt <- 
  gam(
    formula = log(pp) ~ temp_24h:hpt_ + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      hpt_ + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lt_so_hpt)
#appraise(fit_pp_m3_lt_so_hpt)
#draw(fit_pp_m3_lt_so_hpt)

fit_pp_m3_lt_so_hpt_int  <- 
  gam(
    formula = log(pp) ~ temp_24h + temp_24h:hpt_ + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      hpt_ + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lt_so_hpt_int)
#appraise(fit_pp_m3_lt_so_hpt_int)
#draw(fit_pp_m3_lt_so_hpt_int)

fit_pp_m3_lo_st_hpt <- 
  gam(
    formula = log(pp) ~ o3_24h:hpt_ + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      hpt_ + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lo_st_hpt)
#appraise(fit_pp_m3_lo_st_hpt)
#draw(fit_pp_m3_lo_st_hpt)

fit_pp_m3_lo_st_hpt_int <- 
  gam(
    formula = log(pp) ~ o3_24h + o3_24h:hpt_ + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      hpt_ + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lo_st_hpt_int)
#appraise(fit_pp_m3_lo_st_hpt_int)
#draw(fit_pp_m3_lo_st_hpt_int)

## 4 HR --------------------------------------------------------------------------
fit_hr_m3_lt_so_hpt <- 
  gam(
    formula = log(hr) ~ temp_24h:hpt_ + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      hpt_ + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lt_so_hpt)
#appraise(fit_hr_m3_lt_so_hpt)
#draw(fit_hr_m3_lt_so_hpt)

fit_hr_m3_lt_so_hpt_int <- 
  gam(
    formula = log(hr) ~ temp_24h + temp_24h:hpt_ + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      hpt_ + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lt_so_hpt_int)
#appraise(fit_hr_m3_lt_so_hpt_int)
#draw(fit_hr_m3_lt_so_hpt_int)

fit_hr_m3_lo_st_hpt <- 
  gam(
    formula = log(hr) ~ o3_24h:hpt_ + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      hpt_ + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lo_st_hpt)
#appraise(fit_hr_m3_lo_st_hpt)
#draw(fit_hr_m3_lo_st_hpt)

fit_hr_m3_lo_st_hpt_int <- 
  gam(
    formula = log(hr) ~ o3_24h + o3_24h:hpt_ + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      hpt_ + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lo_st_hpt_int)
#appraise(fit_hr_m3_lo_st_hpt_int)
#draw(fit_hr_m3_lo_st_hpt_int)

## 5 Table ---------------------------------------------------------------------
tables2_hpt <- bind_rows(
  effect_modific_helper(fit_sbp_m3_lt_so_hpt, fit_sbp_m3_lt_so_hpt_int, 'sbp'),
  effect_modific_helper(fit_dbp_m3_lt_so_hpt, fit_dbp_m3_lt_so_hpt_int, 'dbp'),
  effect_modific_helper(fit_pp_m3_lt_so_hpt, fit_pp_m3_lt_so_hpt_int, 'pp'),
  effect_modific_helper(fit_hr_m3_lt_so_hpt, fit_hr_m3_lt_so_hpt_int, 'hr'),
  effect_modific_helper(fit_sbp_m3_lo_st_hpt, fit_sbp_m3_lo_st_hpt_int, 'sbp'),
  effect_modific_helper(fit_dbp_m3_lo_st_hpt, fit_dbp_m3_lo_st_hpt_int, 'dbp'),
  effect_modific_helper(fit_pp_m3_lo_st_hpt, fit_pp_m3_lo_st_hpt_int, 'pp'),
  effect_modific_helper(fit_hr_m3_lo_st_hpt, fit_hr_m3_lo_st_hpt_int, 'hr')
)

# * Effect modification by site ------------------------------------------
df_3 <- df_2 %>% 
  ungroup %>% 
  filter(!is.na(o3_24h) & !is.na(temp_24h) & !is.na(pm10_24h) & !is.na(no2_24h),
         !is.na(sbp) & !is.na(dbp) & !is.na(hr),
         !is.na(sex) & !is.na(age) & !is.na(diab) & !is.na(whr) & !is.na(smo) & !is.na(alc) & !is.na(phy) & !is.na(ses))

## 1 SBP -----------------------------------------------------------------------
fit_sbp_m3_lt_so_site <- 
  gam(
    formula = log(sbp) ~ temp_24h:site + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      site + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lt_so_site)
#appraise(fit_sbp_m3_lt_so_site)
#draw(fit_sbp_m3_lt_so_site)

fit_sbp_m3_lt_so_site_int <- 
  gam(
    formula = log(sbp) ~ temp_24h + temp_24h:site + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      site + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lt_so_site_int)
#appraise(fit_sbp_m3_lt_so_site_int)
#draw(fit_sbp_m3_lt_so_site_int)

fit_sbp_m3_lo_st_site <- 
  gam(
    formula = log(sbp) ~ o3_24h:site + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      site + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lo_st_site)
#appraise(fit_sbp_m3_lo_st_site)
#draw(fit_sbp_m3_lo_st_site)

fit_sbp_m3_lo_st_site_int <- 
  gam(
    formula = log(sbp) ~ o3_24h + o3_24h:site + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      site + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_sbp_m3_lo_st_site_int)
#appraise(fit_sbp_m3_lo_st_site_int)
#draw(fit_sbp_m3_lo_st_site_int)

## 2 DBP -----------------------------------------------------------------------
fit_dbp_m3_lt_so_site <- 
  gam(
    formula = log(dbp) ~ temp_24h:site + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      site + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lt_so_site)
#appraise(fit_dbp_m3_lt_so_site)
#draw(fit_dbp_m3_lt_so_site)

fit_dbp_m3_lt_so_site_int <- 
  gam(
    formula = log(dbp) ~ temp_24h + temp_24h:site + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      site + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lt_so_site_int)
#appraise(fit_dbp_m3_lt_so_site_int)
#draw(fit_dbp_m3_lt_so_site_int)

fit_dbp_m3_lo_st_site <- 
  gam(
    formula = log(dbp) ~ o3_24h:site + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      site + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lo_st_site)
#appraise(fit_dbp_m3_lo_st_site)
#draw(fit_dbp_m3_lo_st_site)

fit_dbp_m3_lo_st_site_int <- 
  gam(
    formula = log(dbp) ~ o3_24h + o3_24h:site + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      site + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_dbp_m3_lo_st_site_int)
#appraise(fit_dbp_m3_lo_st_site_int)
#draw(fit_dbp_m3_lo_st_site_int)

## 3 PP ------------------------------------------------------------------------
fit_pp_m3_lt_so_site <- 
  gam(
    formula = log(pp) ~ temp_24h:site + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      site + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lt_so_site)
#appraise(fit_pp_m3_lt_so_site)
#draw(fit_pp_m3_lt_so_site)

fit_pp_m3_lt_so_site_int <- 
  gam(
    formula = log(pp) ~ temp_24h + temp_24h:site + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      site + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lt_so_site_int)
#appraise(fit_pp_m3_lt_so_site_int)
#draw(fit_pp_m3_lt_so_site_int)

fit_pp_m3_lo_st_site <- 
  gam(
    formula = log(pp) ~ o3_24h:site + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      site + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lo_st_site)
#appraise(fit_pp_m3_lo_st_site)
#draw(fit_pp_m3_lo_st_site)

fit_pp_m3_lo_st_site_int <- 
  gam(
    formula = log(pp) ~ o3_24h + o3_24h:site + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      site + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_pp_m3_lo_st_site_int)
#appraise(fit_pp_m3_lo_st_site_int)
#draw(fit_pp_m3_lo_st_site_int)

## 4 HR --------------------------------------------------------------------------
fit_hr_m3_lt_so_site <- 
  gam(
    formula = log(hr) ~ temp_24h:site + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      site + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lt_so_site)
#appraise(fit_hr_m3_lt_so_site)
#draw(fit_hr_m3_lt_so_site)

fit_hr_m3_lt_so_site_int <- 
  gam(
    formula = log(hr) ~ temp_24h + temp_24h:site + s(o3_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      site + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lt_so_site)
#appraise(fit_hr_m3_lt_so_site)
#draw(fit_hr_m3_lt_so_site)

fit_hr_m3_lo_st_site <- 
  gam(
    formula = log(hr) ~ o3_24h:site + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      site + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lo_st_site)
#appraise(fit_hr_m3_lo_st_site)
#draw(fit_hr_m3_lo_st_site)

fit_hr_m3_lo_st_site_int <- 
  gam(
    formula = log(hr) ~ o3_24h + o3_24h:site + s(temp_24h, bs = 'cs', k = 5) + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      site + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3
  )
#summary(fit_hr_m3_lo_st_site_int)
#appraise(fit_hr_m3_lo_st_site_int)
#draw(fit_hr_m3_lo_st_site_int)

## 5 Table ---------------------------------------------------------------------
tables2_site <- bind_rows(
  effect_modific_helper(fit_sbp_m3_lt_so_site, fit_sbp_m3_lt_so_site_int, 'sbp'),
  effect_modific_helper(fit_dbp_m3_lt_so_site, fit_dbp_m3_lt_so_site_int, 'dbp'),
  effect_modific_helper(fit_pp_m3_lt_so_site, fit_pp_m3_lt_so_site_int, 'pp'),
  effect_modific_helper(fit_hr_m3_lt_so_site, fit_hr_m3_lt_so_site_int , 'hr'),
  effect_modific_helper(fit_sbp_m3_lo_st_site, fit_sbp_m3_lo_st_site_int, 'sbp'),
  effect_modific_helper(fit_dbp_m3_lo_st_site, fit_dbp_m3_lo_st_site_int, 'dbp'),
  effect_modific_helper(fit_pp_m3_lo_st_site, fit_pp_m3_lo_st_site_int, 'pp'),
  effect_modific_helper(fit_hr_m3_lo_st_site, fit_hr_m3_lo_st_site_int, 'hr')
)


# Table ------------------------------------------------------------------------
writexl::write_xlsx(
  list(table5_ndvi,
       table5_cacs,
       table5_age) %>% 
    map(\(x) rename(x, 'p_trend' = 'p_int')),
  here::here(paste0(Sys.Date(), '_table5.xlsx'))
)

writexl::write_xlsx(
  list(tables2_sex,
       tables2_hpt,
       tables2_site),
  here::here(paste0(Sys.Date(), '_tables2.xlsx'))
)

