library('splines')
library('broom.mixed')
source(here::here('00_00_functions.R'))

# * Effect modification by NDVI ------------------------------------------------
# 1 Single exposure temperature ------------------------------------------------
df_3 <- df_2 %>% mutate(ndvi = ntile(ndvi, 3) %>% factor)

## SBP -------------------------------------------------------------------------
fit_sbp_temp_o3_linear_ndvi <-
  lme4::lmer(
    formula = sbp ~ temp_24h:ndvi + ndvi + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_sbp_temp_o3_linear <-
  lme4::lmer(
    formula = sbp ~ temp_24h + ndvi + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## DBP -------------------------------------------------------------------------
fit_dbp_temp_o3_linear_ndvi <-
  lme4::lmer(
    formula = dbp ~ temp_24h:ndvi + ndvi + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_dbp_temp_o3_linear <-
  lme4::lmer(
    formula = dbp ~ temp_24h + ndvi + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## PP --------------------------------------------------------------------------
fit_pp_temp_o3_linear_ndvi <-
  lme4::lmer(
    formula = pp ~ temp_24h : ndvi + ndvi + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_pp_temp_o3_linear <-
  lme4::lmer(
    formula = pp ~ temp_24h + ndvi + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## HR --------------------------------------------------------------------------
fit_hr_temp_o3_linear_ndvi <-
  lme4::lmer(
    formula = pulse ~ temp_24h : ndvi + ndvi + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_hr_temp_o3_linear <-
  lme4::lmer(
    formula = pulse ~ temp_24h + ndvi + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

# 2 Single exposure O3 ---------------------------------------------------------
df_3 <- df_2 %>% mutate(ndvi = ntile(ndvi, 3) %>% factor)

## SBP -------------------------------------------------------------------------
fit_sbp_o3_temp_linear_ndvi <-
  lme4::lmer(
    formula = sbp ~ o3_24h : ndvi + ndvi + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_sbp_o3_temp_linear <-
  lme4::lmer(
    formula = sbp ~ o3_24h + ndvi + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## DBP -------------------------------------------------------------------------
fit_dbp_o3_temp_linear_ndvi <-
  lme4::lmer(
    formula = dbp ~ o3_24h : ndvi + ndvi + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_dbp_o3_temp_linear <-
  lme4::lmer(
    formula = dbp ~ o3_24h + ndvi + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## PP -------------------------------------------------------------------------
fit_pp_o3_temp_linear_ndvi <-
  lme4::lmer(
    formula = pp ~ o3_24h : ndvi + ndvi + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_pp_o3_temp_linear <-
  lme4::lmer(
    formula = pp ~ o3_24h + ndvi + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## HR --------------------------------------------------------------------------
fit_hr_o3_temp_linear_ndvi <-
  lme4::lmer(
    formula = pulse ~ o3_24h : ndvi + ndvi + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )


fit_hr_o3_temp_linear <-
  lme4::lmer(
    formula = pulse ~ o3_24h + ndvi + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## Plot ------------------------------------------------------------------------
table4a <-
  bind_rows(
    fit_sbp_temp_o3_linear_ndvi %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'temp_24h:ndvi')) %>% 
      mutate(outcome = 'sbp',
             p_int = anova(fit_sbp_temp_o3_linear, fit_sbp_temp_o3_linear_ndvi)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_24h, na.rm = T))),
    fit_dbp_temp_o3_linear_ndvi %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'temp_24h:ndvi')) %>% 
      mutate(outcome = 'dbp',
             p_int = anova(fit_dbp_temp_o3_linear, fit_dbp_temp_o3_linear_ndvi)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_24h, na.rm = T))),
    fit_pp_temp_o3_linear_ndvi %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'temp_24h:ndvi')) %>% 
      mutate(outcome = 'pp',
             p_int = anova(fit_pp_temp_o3_linear, fit_pp_temp_o3_linear_ndvi)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_24h, na.rm = T))),
    fit_hr_temp_o3_linear_ndvi %>% 
      tidy(conf.int = T) %>%
      filter(str_detect(term, 'temp_24h:ndvi')) %>% 
      mutate(outcome = 'hr',
             p_int = anova(fit_hr_temp_o3_linear, fit_hr_temp_o3_linear_ndvi)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_24h, na.rm = T))),
    fit_sbp_o3_temp_linear_ndvi %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'o3_24h:ndvi')) %>% 
      mutate(outcome = 'sbp',
             p_int = anova(fit_sbp_o3_temp_linear, fit_sbp_o3_temp_linear_ndvi)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3_24h, na.rm = T))),
    fit_dbp_o3_temp_linear_ndvi %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'o3_24h:ndvi')) %>% 
      mutate(outcome = 'dbp',
             p_int = anova(fit_dbp_o3_temp_linear, fit_dbp_o3_temp_linear_ndvi)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3_24h, na.rm = T))),
    fit_pp_o3_temp_linear_ndvi %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'o3_24h:ndvi')) %>% 
      mutate(outcome = 'pp',
             p_int = anova(fit_pp_o3_temp_linear, fit_pp_o3_temp_linear_ndvi)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3_24h, na.rm = T))),
    fit_hr_o3_temp_linear_ndvi %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'o3_24h:ndvi')) %>% 
      mutate(outcome = 'hr',
             p_int = anova(fit_hr_o3_temp_linear, fit_hr_o3_temp_linear_ndvi)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3_24h, na.rm = T)))
  ) %>% 
  separate(term, into = c('main', 'inter'), sep = ':') %>% 
  mutate(estimate = paste0(sprintf('%.2f', estimate), ' (',
                           sprintf('%.2f', conf.low), ', ',
                           sprintf('%.2f', conf.high), ')')) %>% 
  select(main, inter, outcome, estimate, p_int) %>%
  pivot_wider(id_cols = c('main', 'outcome'),
              names_from = 'inter',
              values_from = c('estimate', 'p_int')) 


# * Effect modification by CAC score -------------------------------------------
# 1 Single exposure temperature ------------------------------------------------
df_3 <- df_2 %>% mutate(cacs = case_when(cacs == 0 ~ 0,
                                           cacs <= 100 ~ 1,
                                           cacs > 100 ~ 2) %>% factor)

## SBP -------------------------------------------------------------------------
fit_sbp_temp_o3_linear_cacs <-
  lme4::lmer(
    formula = sbp ~ temp_24h:cacs + cacs + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_sbp_temp_o3_linear <-
  lme4::lmer(
    formula = sbp ~ temp_24h + cacs + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## DBP -------------------------------------------------------------------------
fit_dbp_temp_o3_linear_cacs <-
  lme4::lmer(
    formula = dbp ~ temp_24h:cacs + cacs + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_dbp_temp_o3_linear <-
  lme4::lmer(
    formula = dbp ~ temp_24h + cacs + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## PP --------------------------------------------------------------------------
fit_pp_temp_o3_linear_cacs <-
  lme4::lmer(
    formula = pp ~ temp_24h : cacs + cacs + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_pp_temp_o3_linear <-
  lme4::lmer(
    formula = pp ~ temp_24h + cacs + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## HR --------------------------------------------------------------------------
fit_hr_temp_o3_linear_cacs <-
  lme4::lmer(
    formula = pulse ~ temp_24h : cacs + cacs + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_hr_temp_o3_linear <-
  lme4::lmer(
    formula = pulse ~ temp_24h + cacs + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

# 2 Single exposure O3 ---------------------------------------------------------
## SBP -------------------------------------------------------------------------
fit_sbp_o3_temp_linear_cacs <-
  lme4::lmer(
    formula = sbp ~ o3_24h : cacs + cacs + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_sbp_o3_temp_linear <-
  lme4::lmer(
    formula = sbp ~ o3_24h + cacs + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## DBP -------------------------------------------------------------------------
fit_dbp_o3_temp_linear_cacs <-
  lme4::lmer(
    formula = dbp ~ o3_24h : cacs + cacs + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_dbp_o3_temp_linear <-
  lme4::lmer(
    formula = dbp ~ o3_24h + cacs + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## PP -------------------------------------------------------------------------
fit_pp_o3_temp_linear_cacs <-
  lme4::lmer(
    formula = pp ~ o3_24h : cacs + cacs + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_pp_o3_temp_linear <-
  lme4::lmer(
    formula = pp ~ o3_24h + cacs + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## HR --------------------------------------------------------------------------
fit_hr_o3_temp_linear_cacs <-
  lme4::lmer(
    formula = pulse ~ o3_24h : cacs + cacs + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )


fit_hr_o3_temp_linear <-
  lme4::lmer(
    formula = pulse ~ o3_24h + cacs + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## Plot ------------------------------------------------------------------------
table4bx <-
  bind_rows(
    fit_sbp_temp_o3_linear_cacs %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'temp_24h:cacs')) %>% 
      mutate(outcome = 'sbp',
             p_int = anova(fit_sbp_temp_o3_linear, fit_sbp_temp_o3_linear_cacs)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_24h, na.rm = T))),
    fit_dbp_temp_o3_linear_cacs %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'temp_24h:cacs')) %>% 
      mutate(outcome = 'dbp',
             p_int = anova(fit_dbp_temp_o3_linear, fit_dbp_temp_o3_linear_cacs)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_24h, na.rm = T))),
    fit_pp_temp_o3_linear_cacs %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'temp_24h:cacs')) %>% 
      mutate(outcome = 'pp',
             p_int = anova(fit_pp_temp_o3_linear, fit_pp_temp_o3_linear_cacs)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_24h, na.rm = T))),
    fit_hr_temp_o3_linear_cacs %>% 
      tidy(conf.int = T) %>%
      filter(str_detect(term, 'temp_24h:cacs')) %>% 
      mutate(outcome = 'hr',
             p_int = anova(fit_hr_temp_o3_linear, fit_hr_temp_o3_linear_cacs)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_24h, na.rm = T))),
    fit_sbp_o3_temp_linear_cacs %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'o3_24h:cacs')) %>% 
      mutate(outcome = 'sbp',
             p_int = anova(fit_sbp_o3_temp_linear, fit_sbp_o3_temp_linear_cacs)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3_24h, na.rm = T))),
    fit_dbp_o3_temp_linear_cacs %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'o3_24h:cacs')) %>% 
      mutate(outcome = 'dbp',
             p_int = anova(fit_dbp_o3_temp_linear, fit_dbp_o3_temp_linear_cacs)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3_24h, na.rm = T))),
    fit_pp_o3_temp_linear_cacs %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'o3_24h:cacs')) %>% 
      mutate(outcome = 'pp',
             p_int = anova(fit_pp_o3_temp_linear, fit_pp_o3_temp_linear_cacs)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3_24h, na.rm = T))),
    fit_hr_o3_temp_linear_cacs %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'o3_24h:cacs')) %>% 
      mutate(outcome = 'hr',
             p_int = anova(fit_hr_o3_temp_linear, fit_hr_o3_temp_linear_cacs)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3_24h, na.rm = T)))
  ) %>% 
  separate(term, into = c('main', 'inter'), sep = ':') %>% 
  mutate(estimate = paste0(sprintf('%.2f', estimate), ' (',
                           sprintf('%.2f', conf.low), ', ',
                           sprintf('%.2f', conf.high), ')')) %>% 
  select(main, inter, outcome, estimate, p_int) %>%
  pivot_wider(id_cols = c('main', 'outcome'),
              names_from = 'inter',
              values_from = c('estimate', 'p_int')) 

# * Effect modification by age group -------------------------------------------
df_3 <- df_2 %>% mutate(age_gr = case_when(age < 55 ~ 1,
                                           age >= 55 & age < 60 ~ 2,
                                           age >= 60 ~ 3) %>% factor())
# 1 Single exposure temperature ------------------------------------------------
## SBP -------------------------------------------------------------------------
fit_sbp_temp_o3_linear_age_gr <-
  lme4::lmer(
    formula = sbp ~ temp_24h:age_gr + age_gr + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_sbp_temp_o3_linear <-
  lme4::lmer(
    formula = sbp ~ temp_24h + age_gr + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## DBP -------------------------------------------------------------------------
fit_dbp_temp_o3_linear_age_gr <-
  lme4::lmer(
    formula = dbp ~ temp_24h:age_gr + age_gr + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_dbp_temp_o3_linear <-
  lme4::lmer(
    formula = dbp ~ temp_24h + age_gr + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## PP --------------------------------------------------------------------------
fit_pp_temp_o3_linear_age_gr <-
  lme4::lmer(
    formula = pp ~ temp_24h : age_gr + age_gr + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_pp_temp_o3_linear <-
  lme4::lmer(
    formula = pp ~ temp_24h + age_gr + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## HR --------------------------------------------------------------------------
fit_hr_temp_o3_linear_age_gr <-
  lme4::lmer(
    formula = pulse ~ temp_24h : age_gr + age_gr + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_hr_temp_o3_linear <-
  lme4::lmer(
    formula = pulse ~ temp_24h + age_gr + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

# 2 Single exposure O3 ---------------------------------------------------------
## SBP -------------------------------------------------------------------------
fit_sbp_o3_temp_linear_age_gr <-
  lme4::lmer(
    formula = sbp ~ o3_24h : age_gr + age_gr + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_sbp_o3_temp_linear <-
  lme4::lmer(
    formula = sbp ~ o3_24h + age_gr + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## DBP -------------------------------------------------------------------------
fit_dbp_o3_temp_linear_age_gr <-
  lme4::lmer(
    formula = dbp ~ o3_24h : age_gr + age_gr + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_dbp_o3_temp_linear <-
  lme4::lmer(
    formula = dbp ~ o3_24h + age_gr + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## PP -------------------------------------------------------------------------
fit_pp_o3_temp_linear_age_gr <-
  lme4::lmer(
    formula = pp ~ o3_24h : age_gr + age_gr + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_pp_o3_temp_linear <-
  lme4::lmer(
    formula = pp ~ o3_24h + age_gr + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## HR --------------------------------------------------------------------------
fit_hr_o3_temp_linear_age_gr <-
  lme4::lmer(
    formula = pulse ~ o3_24h : age_gr + age_gr + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )


fit_hr_o3_temp_linear <-
  lme4::lmer(
    formula = pulse ~ o3_24h + age_gr + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## Plot ------------------------------------------------------------------------
table4c <-
  bind_rows(
    fit_sbp_temp_o3_linear_age_gr %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'temp_24h:age_gr')) %>% 
      mutate(outcome = 'sbp',
             p_int = anova(fit_sbp_temp_o3_linear, fit_sbp_temp_o3_linear_age_gr)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_24h, na.rm = T))),
    fit_dbp_temp_o3_linear_age_gr %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'temp_24h:age_gr')) %>% 
      mutate(outcome = 'dbp',
             p_int = anova(fit_dbp_temp_o3_linear, fit_dbp_temp_o3_linear_age_gr)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_24h, na.rm = T))),
    fit_pp_temp_o3_linear_age_gr %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'temp_24h:age_gr')) %>% 
      mutate(outcome = 'pp',
             p_int = anova(fit_pp_temp_o3_linear, fit_pp_temp_o3_linear_age_gr)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_24h, na.rm = T))),
    fit_hr_temp_o3_linear_age_gr %>% 
      tidy(conf.int = T) %>%
      filter(str_detect(term, 'temp_24h:age_gr')) %>% 
      mutate(outcome = 'hr',
             p_int = anova(fit_hr_temp_o3_linear, fit_hr_temp_o3_linear_age_gr)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_24h, na.rm = T))),
    fit_sbp_o3_temp_linear_age_gr %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'o3_24h:age_gr')) %>% 
      mutate(outcome = 'sbp',
             p_int = anova(fit_sbp_o3_temp_linear, fit_sbp_o3_temp_linear_age_gr)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3_24h, na.rm = T))),
    fit_dbp_o3_temp_linear_age_gr %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'o3_24h:age_gr')) %>% 
      mutate(outcome = 'dbp',
             p_int = anova(fit_dbp_o3_temp_linear, fit_dbp_o3_temp_linear_age_gr)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3_24h, na.rm = T))),
    fit_pp_o3_temp_linear_age_gr %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'o3_24h:age_gr')) %>% 
      mutate(outcome = 'pp',
             p_int = anova(fit_pp_o3_temp_linear, fit_pp_o3_temp_linear_age_gr)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3_24h, na.rm = T))),
    fit_hr_o3_temp_linear_age_gr %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'o3_24h:age_gr')) %>% 
      mutate(outcome = 'hr',
             p_int = anova(fit_hr_o3_temp_linear, fit_hr_o3_temp_linear_age_gr)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3_24h, na.rm = T)))
  ) %>% 
  separate(term, into = c('main', 'inter'), sep = ':') %>% 
  mutate(estimate = paste0(sprintf('%.2f', estimate), ' (',
                           sprintf('%.2f', conf.low), ', ',
                           sprintf('%.2f', conf.high), ')')) %>% 
  select(main, inter, outcome, estimate, p_int) %>%
  pivot_wider(id_cols = c('main', 'outcome'),
              names_from = 'inter',
              values_from = c('estimate', 'p_int')) 

# * Effect modification by sex -------------------------------------------
df_3 <- df_2

# 1 Single exposure temperature ------------------------------------------------
## SBP -------------------------------------------------------------------------
fit_sbp_temp_o3_linear_sex <-
  lme4::lmer(
    formula = sbp ~ temp_24h:sex + sex + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_sbp_temp_o3_linear <-
  lme4::lmer(
    formula = sbp ~ temp_24h + sex + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## DBP -------------------------------------------------------------------------
fit_dbp_temp_o3_linear_sex <-
  lme4::lmer(
    formula = dbp ~ temp_24h:sex + sex + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_dbp_temp_o3_linear <-
  lme4::lmer(
    formula = dbp ~ temp_24h + sex + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## PP --------------------------------------------------------------------------
fit_pp_temp_o3_linear_sex <-
  lme4::lmer(
    formula = pp ~ temp_24h : sex + sex + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_pp_temp_o3_linear <-
  lme4::lmer(
    formula = pp ~ temp_24h + sex + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## HR --------------------------------------------------------------------------
fit_hr_temp_o3_linear_sex <-
  lme4::lmer(
    formula = pulse ~ temp_24h : sex + sex + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_hr_temp_o3_linear <-
  lme4::lmer(
    formula = pulse ~ temp_24h + sex + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

# 2 Single exposure O3 ---------------------------------------------------------
## SBP -------------------------------------------------------------------------
fit_sbp_o3_temp_linear_sex <-
  lme4::lmer(
    formula = sbp ~ o3_24h : sex + sex + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_sbp_o3_temp_linear <-
  lme4::lmer(
    formula = sbp ~ o3_24h + sex + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## DBP -------------------------------------------------------------------------
fit_dbp_o3_temp_linear_sex <-
  lme4::lmer(
    formula = dbp ~ o3_24h : sex + sex + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_dbp_o3_temp_linear <-
  lme4::lmer(
    formula = dbp ~ o3_24h + sex + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## PP -------------------------------------------------------------------------
fit_pp_o3_temp_linear_sex <-
  lme4::lmer(
    formula = pp ~ o3_24h : sex + sex + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_pp_o3_temp_linear <-
  lme4::lmer(
    formula = pp ~ o3_24h + sex + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## HR --------------------------------------------------------------------------
fit_hr_o3_temp_linear_sex <-
  lme4::lmer(
    formula = pulse ~ o3_24h : sex + sex + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )


fit_hr_o3_temp_linear <-
  lme4::lmer(
    formula = pulse ~ o3_24h + sex + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## Plot ------------------------------------------------------------------------
table4d <-
  bind_rows(
    fit_sbp_temp_o3_linear_sex %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'temp_24h:sex')) %>% 
      mutate(outcome = 'sbp',
             p_int = anova(fit_sbp_temp_o3_linear, fit_sbp_temp_o3_linear_sex)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_24h, na.rm = T))),
    fit_dbp_temp_o3_linear_sex %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'temp_24h:sex')) %>% 
      mutate(outcome = 'dbp',
             p_int = anova(fit_dbp_temp_o3_linear, fit_dbp_temp_o3_linear_sex)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_24h, na.rm = T))),
    fit_pp_temp_o3_linear_sex %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'temp_24h:sex')) %>% 
      mutate(outcome = 'pp',
             p_int = anova(fit_pp_temp_o3_linear, fit_pp_temp_o3_linear_sex)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_24h, na.rm = T))),
    fit_hr_temp_o3_linear_sex %>% 
      tidy(conf.int = T) %>%
      filter(str_detect(term, 'temp_24h:sex')) %>% 
      mutate(outcome = 'hr',
             p_int = anova(fit_hr_temp_o3_linear, fit_hr_temp_o3_linear_sex)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_24h, na.rm = T))),
    fit_sbp_o3_temp_linear_sex %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'o3_24h:sex')) %>% 
      mutate(outcome = 'sbp',
             p_int = anova(fit_sbp_o3_temp_linear, fit_sbp_o3_temp_linear_sex)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3_24h, na.rm = T))),
    fit_dbp_o3_temp_linear_sex %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'o3_24h:sex')) %>% 
      mutate(outcome = 'dbp',
             p_int = anova(fit_dbp_o3_temp_linear, fit_dbp_o3_temp_linear_sex)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3_24h, na.rm = T))),
    fit_pp_o3_temp_linear_sex %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'o3_24h:sex')) %>% 
      mutate(outcome = 'pp',
             p_int = anova(fit_pp_o3_temp_linear, fit_pp_o3_temp_linear_sex)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3_24h, na.rm = T))),
    fit_hr_o3_temp_linear_sex %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'o3_24h:sex')) %>% 
      mutate(outcome = 'hr',
             p_int = anova(fit_hr_o3_temp_linear, fit_hr_o3_temp_linear_sex)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3_24h, na.rm = T)))
  ) %>% 
  separate(term, into = c('main', 'inter'), sep = ':') %>% 
  mutate(estimate = paste0(sprintf('%.2f', estimate), ' (',
                           sprintf('%.2f', conf.low), ', ',
                           sprintf('%.2f', conf.high), ')')) %>% 
  select(main, inter, outcome, estimate, p_int) %>%
  pivot_wider(id_cols = c('main', 'outcome'),
              names_from = 'inter',
              values_from = c('estimate', 'p_int')) 

# * Effect modification by HPT -------------------------------------------
df_3 <- df_2 %>% mutate(hpt = hpt == 'no' & hpt_med == 'no')

# 1 Single exposure temperature ------------------------------------------------
## SBP -------------------------------------------------------------------------
fit_sbp_temp_o3_linear_hpt <-
  lme4::lmer(
    formula = sbp ~ temp_24h:hpt + hpt + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_sbp_temp_o3_linear <-
  lme4::lmer(
    formula = sbp ~ temp_24h + hpt + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## DBP -------------------------------------------------------------------------
fit_dbp_temp_o3_linear_hpt <-
  lme4::lmer(
    formula = dbp ~ temp_24h:hpt + hpt + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_dbp_temp_o3_linear <-
  lme4::lmer(
    formula = dbp ~ temp_24h + hpt + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## PP --------------------------------------------------------------------------
fit_pp_temp_o3_linear_hpt <-
  lme4::lmer(
    formula = pp ~ temp_24h:site + hpt + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_pp_temp_o3_linear <-
  lme4::lmer(
    formula = pp ~ temp_24h + hpt + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## HR --------------------------------------------------------------------------
fit_hr_temp_o3_linear_hpt <-
  lme4::lmer(
    formula = pulse ~ temp_24h:site + hpt + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_hr_temp_o3_linear <-
  lme4::lmer(
    formula = pulse ~ temp_24h + hpt + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

# 2 Single exposure O3 ---------------------------------------------------------
## SBP -------------------------------------------------------------------------
fit_sbp_o3_temp_linear_hpt <-
  lme4::lmer(
    formula = sbp ~ o3_24h:site + hpt + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_sbp_o3_temp_linear <-
  lme4::lmer(
    formula = sbp ~ o3_24h + hpt + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## DBP -------------------------------------------------------------------------
fit_dbp_o3_temp_linear_hpt <-
  lme4::lmer(
    formula = dbp ~ o3_24h:site + hpt + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_dbp_o3_temp_linear <-
  lme4::lmer(
    formula = dbp ~ o3_24h + hpt + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## PP -------------------------------------------------------------------------
fit_pp_o3_temp_linear_hpt <-
  lme4::lmer(
    formula = pp ~ o3_24h:site + hpt + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_pp_o3_temp_linear <-
  lme4::lmer(
    formula = pp ~ o3_24h + hpt + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## HR --------------------------------------------------------------------------
fit_hr_o3_temp_linear_hpt <-
  lme4::lmer(
    formula = pulse ~ o3_24h:site + hpt + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )


fit_hr_o3_temp_linear <-
  lme4::lmer(
    formula = pulse ~ o3_24h + hpt + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## Plot ------------------------------------------------------------------------
table4e <-
  bind_rows(
    fit_sbp_temp_o3_linear_hpt %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'temp_24h:hpt')) %>% 
      mutate(outcome = 'sbp',
             p_int = anova(fit_sbp_temp_o3_linear, fit_sbp_temp_o3_linear_hpt)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_24h, na.rm = T))),
    fit_dbp_temp_o3_linear_hpt %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'temp_24h:hpt')) %>% 
      mutate(outcome = 'dbp',
             p_int = anova(fit_dbp_temp_o3_linear, fit_dbp_temp_o3_linear_hpt)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_24h, na.rm = T))),
    fit_pp_temp_o3_linear_hpt %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'temp_24h:hpt')) %>% 
      mutate(outcome = 'pp',
             p_int = anova(fit_pp_temp_o3_linear, fit_pp_temp_o3_linear_hpt)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_24h, na.rm = T))),
    fit_hr_temp_o3_linear_hpt %>% 
      tidy(conf.int = T) %>%
      filter(str_detect(term, 'temp_24h:hpt')) %>% 
      mutate(outcome = 'hr',
             p_int = anova(fit_hr_temp_o3_linear, fit_hr_temp_o3_linear_hpt)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_24h, na.rm = T))),
    fit_sbp_o3_temp_linear_hpt %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'o3_24h:hpt')) %>% 
      mutate(outcome = 'sbp',
             p_int = anova(fit_sbp_o3_temp_linear, fit_sbp_o3_temp_linear_hpt)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3_24h, na.rm = T))),
    fit_dbp_o3_temp_linear_hpt %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'o3_24h:hpt')) %>% 
      mutate(outcome = 'dbp',
             p_int = anova(fit_dbp_o3_temp_linear, fit_dbp_o3_temp_linear_hpt)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3_24h, na.rm = T))),
    fit_pp_o3_temp_linear_hpt %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'o3_24h:hpt')) %>% 
      mutate(outcome = 'pp',
             p_int = anova(fit_pp_o3_temp_linear, fit_pp_o3_temp_linear_hpt)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3_24h, na.rm = T))),
    fit_hr_o3_temp_linear_hpt %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'o3_24h:hpt')) %>% 
      mutate(outcome = 'hr',
             p_int = anova(fit_hr_o3_temp_linear, fit_hr_o3_temp_linear_hpt)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3_24h, na.rm = T)))
  ) %>% 
  separate(term, into = c('main', 'inter'), sep = ':') %>% 
  mutate(estimate = paste0(sprintf('%.2f', estimate), ' (',
                           sprintf('%.2f', conf.low), ', ',
                           sprintf('%.2f', conf.high), ')')) %>% 
  select(main, inter, outcome, estimate, p_int) %>%
  pivot_wider(id_cols = c('main', 'outcome'),
              names_from = 'inter',
              values_from = c('estimate', 'p_int')) 

# * Effect modification by site ------------------------------------------
df_3 <- df_2

# 1 Single exposure temperature ------------------------------------------------
## SBP -------------------------------------------------------------------------
fit_sbp_temp_o3_linear_site <-
  lme4::lmer(
    formula = sbp ~ temp_24h:site + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_sbp_temp_o3_linear <-
  lme4::lmer(
    formula = sbp ~ temp_24h + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## DBP -------------------------------------------------------------------------
fit_dbp_temp_o3_linear_site <-
  lme4::lmer(
    formula = dbp ~ temp_24h:site + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_dbp_temp_o3_linear <-
  lme4::lmer(
    formula = dbp ~ temp_24h + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## PP --------------------------------------------------------------------------
fit_pp_temp_o3_linear_site <-
  lme4::lmer(
    formula = pp ~ temp_24h:site + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_pp_temp_o3_linear <-
  lme4::lmer(
    formula = pp ~ temp_24h + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## HR --------------------------------------------------------------------------
fit_hr_temp_o3_linear_site <-
  lme4::lmer(
    formula = pulse ~ temp_24h:site + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_hr_temp_o3_linear <-
  lme4::lmer(
    formula = pulse ~ temp_24h + ns(o3_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

# 2 Single exposure O3 ---------------------------------------------------------
## SBP -------------------------------------------------------------------------
fit_sbp_o3_temp_linear_site <-
  lme4::lmer(
    formula = sbp ~ o3_24h:site + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_sbp_o3_temp_linear <-
  lme4::lmer(
    formula = sbp ~ o3_24h + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## DBP -------------------------------------------------------------------------
fit_dbp_o3_temp_linear_site <-
  lme4::lmer(
    formula = dbp ~ o3_24h:site + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_dbp_o3_temp_linear <-
  lme4::lmer(
    formula = dbp ~ o3_24h + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## PP -------------------------------------------------------------------------
fit_pp_o3_temp_linear_site <-
  lme4::lmer(
    formula = pp ~ o3_24h:site + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

fit_pp_o3_temp_linear <-
  lme4::lmer(
    formula = pp ~ o3_24h + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## HR --------------------------------------------------------------------------
fit_hr_o3_temp_linear_site <-
  lme4::lmer(
    formula = pulse ~ o3_24h:site + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )


fit_hr_o3_temp_linear <-
  lme4::lmer(
    formula = pulse ~ o3_24h + ns(temp_24h, 3) + ns(pm10_24h, 3) + ns(no2_24h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

## Plot ------------------------------------------------------------------------
table4f <-
  bind_rows(
    fit_sbp_temp_o3_linear_site %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'temp_24h:site')) %>% 
      mutate(outcome = 'sbp',
             p_int = anova(fit_sbp_temp_o3_linear, fit_sbp_temp_o3_linear_site)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_24h, na.rm = T))),
    fit_dbp_temp_o3_linear_site %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'temp_24h:site')) %>% 
      mutate(outcome = 'dbp',
             p_int = anova(fit_dbp_temp_o3_linear, fit_dbp_temp_o3_linear_site)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_24h, na.rm = T))),
    fit_pp_temp_o3_linear_site %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'temp_24h:site')) %>% 
      mutate(outcome = 'pp',
             p_int = anova(fit_pp_temp_o3_linear, fit_pp_temp_o3_linear_site)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_24h, na.rm = T))),
    fit_hr_temp_o3_linear_site %>% 
      tidy(conf.int = T) %>%
      filter(str_detect(term, 'temp_24h:site')) %>% 
      mutate(outcome = 'hr',
             p_int = anova(fit_hr_temp_o3_linear, fit_hr_temp_o3_linear_site)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_24h, na.rm = T))),
    fit_sbp_o3_temp_linear_site %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'o3_24h:site')) %>% 
      mutate(outcome = 'sbp',
             p_int = anova(fit_sbp_o3_temp_linear, fit_sbp_o3_temp_linear_site)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3_24h, na.rm = T))),
    fit_dbp_o3_temp_linear_site %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'o3_24h:site')) %>% 
      mutate(outcome = 'dbp',
             p_int = anova(fit_dbp_o3_temp_linear, fit_dbp_o3_temp_linear_site)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3_24h, na.rm = T))),
    fit_pp_o3_temp_linear_site %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'o3_24h:site')) %>% 
      mutate(outcome = 'pp',
             p_int = anova(fit_pp_o3_temp_linear, fit_pp_o3_temp_linear_site)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3_24h, na.rm = T))),
    fit_hr_o3_temp_linear_site %>% 
      tidy(conf.int = T) %>% 
      filter(str_detect(term, 'o3_24h:site')) %>% 
      mutate(outcome = 'hr',
             p_int = anova(fit_hr_o3_temp_linear, fit_hr_o3_temp_linear_site)$`Pr(>Chisq)`[2],
             across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3_24h, na.rm = T)))
  ) %>% 
  separate(term, into = c('main', 'inter'), sep = ':') %>% 
  mutate(estimate = paste0(sprintf('%.2f', estimate), ' (',
                           sprintf('%.2f', conf.low), ', ',
                           sprintf('%.2f', conf.high), ')')) %>% 
  select(main, inter, outcome, estimate, p_int) %>%
  pivot_wider(id_cols = c('main', 'outcome'),
              names_from = 'inter',
              values_from = c('estimate', 'p_int')) 



writexl::write_xlsx(
  list(table4a,
       table4b,
       table4c,
       table4d,
       table4e,
       table4f),
  here::here(paste0(Sys.Date(), '_table4.xlsx'))
)
