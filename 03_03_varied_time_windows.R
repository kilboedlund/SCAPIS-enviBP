library('splines')
library('broom.mixed')
source(here::here('00_00_functions.R'))

df_4 <- df_2 %>% 
  mutate(temp_168h_t = ntile(temp_168h, 3) %>% factor(),
         temp_ap_t = ntile(temp_ap, 3) %>% factor(),
         o3_168h_t = ntile(o3_168h, 3) %>% factor(),
         o3_t = ntile(o3, 3) %>% factor())

# * 168 hours ------------------------------------------------------------------
# 1 Single exposure temperature ------------------------------------------------
## SBP -------------------------------------------------------------------------
fit_sbp_temp_o3_pm10_linear_168h <-
  lme4::lmer(
    formula = sbp ~ temp_168h + ns(o3_168h, 3) + ns(pm10_168h, 3) + ns(no2_168h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_4
  )

fit_sbp_temp_o3_pm10_linear_168h_inter <-
  lme4::lmer(
    formula = sbp ~ temp_168h:temp_168h_t + ns(o3_168h, 3) + ns(pm10_168h, 3) + ns(no2_168h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_4
  )

## DBP -------------------------------------------------------------------------
fit_dbp_temp_o3_pm10_linear_168h <-
  lme4::lmer(
    formula = dbp ~ temp_168h + ns(o3_168h, 3) + ns(pm10_168h, 3) + ns(no2_168h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_4
  )

fit_dbp_temp_o3_pm10_linear_168h_inter <-
  lme4::lmer(
    formula = dbp ~ temp_168h:temp_168h_t + ns(o3_168h, 3) + ns(pm10_168h, 3) + ns(no2_168h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_4
  )

## PP --------------------------------------------------------------------------
fit_pp_temp_o3_pm10_linear_168h <-
  lme4::lmer(
    formula = pp ~ temp_168h + ns(o3_168h, 3) + ns(pm10_168h, 3) + ns(no2_168h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_4
  )

fit_pp_temp_o3_pm10_linear_168h_inter <-
  lme4::lmer(
    formula = pp ~ temp_168h:temp_168h_t + ns(o3_168h, 3) + ns(pm10_168h, 3) + ns(no2_168h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_4
  )

## HR --------------------------------------------------------------------------
fit_hr_temp_o3_pm10_linear_168h <-
  lme4::lmer(
    formula = pulse ~ temp_168h + ns(o3_168h, 3) + ns(pm10_168h, 3) + ns(no2_168h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_4
  )

fit_hr_temp_o3_pm10_linear_168h_inter <-
  lme4::lmer(
    formula = pulse ~ temp_168h:temp_168h_t + ns(o3_168h, 3) + ns(pm10_168h, 3) + ns(no2_168h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_4
  )

# 2 Single exposure O3 ---------------------------------------------------------
## SBP -------------------------------------------------------------------------
fit_sbp_o3_temp_pm10_linear_168h <-
  lme4::lmer(
    formula = sbp ~ o3_168h + ns(temp_168h, 3) + ns(pm10_168h, 3) + ns(no2_168h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_4
  )

fit_sbp_o3_temp_pm10_linear_168h_inter <-
  lme4::lmer(
    formula = sbp ~ o3_168h:o3_168h_t + ns(temp_168h, 3) + ns(pm10_168h, 3) + ns(no2_168h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_4
  )

## DBP -------------------------------------------------------------------------
fit_dbp_o3_temp_pm10_linear_168h <-
  lme4::lmer(
    formula = dbp ~ o3_168h + ns(temp_168h, 3) + ns(pm10_168h, 3) + ns(no2_168h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_4
  )

fit_dbp_o3_temp_pm10_linear_168h_inter <-
  lme4::lmer(
    formula = dbp ~ o3_168h:o3_168h_t + ns(temp_168h, 3) + ns(pm10_168h, 3) + ns(no2_168h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_4
  )

## PP -------------------------------------------------------------------------
fit_pp_o3_temp_pm10_linear_168h <-
  lme4::lmer(
    formula = pp ~ o3_168h + ns(temp_168h, 3) + ns(pm10_168h, 3) + ns(no2_168h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_4
  )

fit_pp_o3_temp_pm10_linear_168h_inter <-
  lme4::lmer(
    formula = pp ~ o3_168h:o3_168h_t + ns(temp_168h, 3) + ns(pm10_168h, 3) + ns(no2_168h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_4
  )

## HR --------------------------------------------------------------------------
fit_hr_o3_temp_pm10_linear_168h <-
  lme4::lmer(
    formula = pulse ~ o3_168h + ns(temp_168h, 3) + ns(pm10_168h, 3) + ns(no2_168h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_4
  )

fit_hr_o3_temp_pm10_linear_168h_inter <-
  lme4::lmer(
    formula = pulse ~ o3_168h:o3_168h_t + ns(temp_168h, 3) + ns(pm10_168h, 3) + ns(no2_168h, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_4
  )

# * 8 am -----------------------------------------------------------------------
# 1 Single exposure temperature ------------------------------------------------
## SBP -------------------------------------------------------------------------
fit_sbp_temp_o3_pm10_linear_8am <-
  lme4::lmer(
    formula = sbp ~ temp_ap + ns(o3, 3) + ns(pm10, 3) + ns(no2, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_4
  )

fit_sbp_temp_o3_pm10_linear_8am_inter <-
  lme4::lmer(
    formula = sbp ~ temp_ap:temp_ap_t + ns(o3, 3) + ns(pm10, 3) + ns(no2, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_4
  )

## DBP -------------------------------------------------------------------------
fit_dbp_temp_o3_pm10_linear_8am <-
  lme4::lmer(
    formula = dbp ~ temp_ap + ns(o3, 3) + ns(pm10, 3) + ns(no2, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_4
  )

fit_dbp_temp_o3_pm10_linear_8am_inter <-
  lme4::lmer(
    formula = dbp ~ temp_ap:temp_ap_t + ns(o3, 3) + ns(pm10, 3) + ns(no2, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_4
  )

## PP --------------------------------------------------------------------------
fit_pp_temp_o3_pm10_linear_8am <-
  lme4::lmer(
    formula = pp ~ temp_ap + ns(o3, 3) + ns(pm10, 3) + ns(no2, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_4
  )

fit_pp_temp_o3_pm10_linear_8am_inter <-
  lme4::lmer(
    formula = pp ~ temp_ap:temp_ap_t + ns(o3, 3) + ns(pm10, 3) + ns(no2, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_4
  )

## HR --------------------------------------------------------------------------
fit_hr_temp_o3_pm10_linear_8am <-
  lme4::lmer(
    formula = pulse ~ temp_ap + ns(o3, 3) + ns(pm10, 3) + ns(no2, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_4
  )

fit_hr_temp_o3_pm10_linear_8am_inter <-
  lme4::lmer(
    formula = pulse ~ temp_ap:temp_ap_t + ns(o3, 3) + ns(pm10, 3) + ns(no2, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_4
  )

# 2 Single exposure O3 ---------------------------------------------------------
## SBP -------------------------------------------------------------------------
fit_sbp_o3_temp_pm10_linear_8am <-
  lme4::lmer(
    formula = sbp ~ o3 + ns(temp_ap, 3) + ns(pm10, 3) + ns(no2, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_4
  )

fit_sbp_o3_temp_pm10_linear_8am_inter <-
  lme4::lmer(
    formula = sbp ~ o3:o3_t + ns(temp_ap, 3) + ns(pm10, 3) + ns(no2, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_4
  )

## DBP -------------------------------------------------------------------------
fit_dbp_o3_temp_pm10_linear_8am <-
  lme4::lmer(
    formula = dbp ~ o3 + ns(temp_ap, 3) + ns(pm10, 3) + ns(no2, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_4
  )

fit_dbp_o3_temp_pm10_linear_8am_inter <-
  lme4::lmer(
    formula = dbp ~ o3:o3_t + ns(temp_ap, 3) + ns(pm10, 3) + ns(no2, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_4
  )

## PP -------------------------------------------------------------------------
fit_pp_o3_temp_pm10_linear_8am <-
  lme4::lmer(
    formula = pp ~ o3 + ns(temp_ap, 3) + ns(pm10, 3) + ns(no2, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_4
  )

fit_pp_o3_temp_pm10_linear_8am_inter <-
  lme4::lmer(
    formula = pp ~ o3:o3_t + ns(temp_ap, 3) + ns(pm10, 3) + ns(no2, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_4
  )

## HR --------------------------------------------------------------------------
fit_hr_o3_temp_pm10_linear_8am <-
  lme4::lmer(
    formula = pulse ~ o3 + ns(temp_ap, 3) + ns(pm10, 3) + ns(no2, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_4
  )

fit_hr_o3_temp_pm10_linear_8am_inter <-
  lme4::lmer(
    formula = pulse ~ o3:o3_t + ns(temp_ap, 3) + ns(pm10, 3) + ns(no2, 3) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_4
  )

# * Table ----------------------------------------------------------------------
tableS1 <- bind_rows(
  fit_sbp_temp_o3_pm10_linear_168h %>% 
    tidy(conf.int = T) %>% 
    filter(str_detect(term, 'temp_168h')) %>% 
    mutate(outcome = 'sbp',
           time = '168h',
           p_lin = anova(fit_sbp_temp_o3_pm10_linear_168h, fit_sbp_temp_o3_pm10_linear_168h_inter)$`Pr(>Chisq)`[2],
           across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_168h, na.rm = T))),
  fit_sbp_temp_o3_pm10_linear_168h_inter %>% 
    tidy(conf.int = T) %>% 
    filter(str_detect(term, 'temp_168h')) %>% 
    mutate(outcome = 'sbp',
           time = '168h',
           across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_168h, na.rm = T))),
  fit_dbp_temp_o3_pm10_linear_168h %>% 
    tidy(conf.int = T) %>% 
    filter(str_detect(term, 'temp_168h')) %>% 
    mutate(outcome = 'dbp',
           time = '168h',
           p_lin = anova(fit_dbp_temp_o3_pm10_linear_168h, fit_dbp_temp_o3_pm10_linear_168h_inter)$`Pr(>Chisq)`[2],
           across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_168h, na.rm = T))),
  fit_dbp_temp_o3_pm10_linear_168h_inter %>% 
    tidy(conf.int = T) %>% 
    filter(str_detect(term, 'temp_168h')) %>% 
    mutate(outcome = 'dbp',
           time = '168h',
           across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_168h, na.rm = T))),
  fit_pp_temp_o3_pm10_linear_168h %>% 
    tidy(conf.int = T) %>% 
    filter(str_detect(term, 'temp_168h')) %>% 
    mutate(outcome = 'pp',
           time = '168h',
           p_lin = anova(fit_pp_temp_o3_pm10_linear_168h, fit_pp_temp_o3_pm10_linear_168h_inter)$`Pr(>Chisq)`[2],
           across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_168h, na.rm = T))),
  fit_pp_temp_o3_pm10_linear_168h_inter %>% 
    tidy(conf.int = T) %>% 
    filter(str_detect(term, 'temp_168h')) %>% 
    mutate(outcome = 'pp',
           time = '168h',
           across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_168h, na.rm = T))),
  fit_hr_temp_o3_pm10_linear_168h %>% 
    tidy(conf.int = T) %>% 
    filter(str_detect(term, 'temp_168h')) %>% 
    mutate(outcome = 'hr',
           time = '168h',
           p_lin = anova(fit_hr_temp_o3_pm10_linear_168h, fit_hr_temp_o3_pm10_linear_168h_inter)$`Pr(>Chisq)`[2],
           across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_168h, na.rm = T))),
  fit_hr_temp_o3_pm10_linear_168h_inter %>% 
    tidy(conf.int = T) %>% 
    filter(str_detect(term, 'temp_168h')) %>% 
    mutate(outcome = 'hr',
           time = '168h',
           across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_168h, na.rm = T))),
  
  fit_sbp_o3_temp_pm10_linear_168h %>% 
    tidy(conf.int = T) %>% 
    filter(str_detect(term, 'o3_168h')) %>% 
    mutate(outcome = 'sbp',
           time = '168h',
           p_lin = anova(fit_sbp_o3_temp_pm10_linear_168h, fit_sbp_o3_temp_pm10_linear_168h_inter)$`Pr(>Chisq)`[2],
           across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3_168h, na.rm = T))),
  fit_sbp_o3_temp_pm10_linear_168h_inter %>% 
    tidy(conf.int = T) %>% 
    filter(str_detect(term, 'o3_168h')) %>% 
    mutate(outcome = 'sbp',
           time = '168h',
           across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3_168h, na.rm = T))),
  fit_dbp_o3_temp_pm10_linear_168h %>% 
    tidy(conf.int = T) %>% 
    filter(str_detect(term, 'o3_168h')) %>% 
    mutate(outcome = 'dbp',
           time = '168h',
           p_lin = anova(fit_dbp_o3_temp_pm10_linear_168h, fit_dbp_o3_temp_pm10_linear_168h_inter)$`Pr(>Chisq)`[2],
           across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3_168h, na.rm = T))),
  fit_dbp_o3_temp_pm10_linear_168h_inter %>% 
    tidy(conf.int = T) %>% 
    filter(str_detect(term, 'o3_168h')) %>% 
    mutate(outcome = 'dbp',
           time = '168h',
           across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3_168h, na.rm = T))),
  fit_pp_o3_temp_pm10_linear_168h %>% 
    tidy(conf.int = T) %>% 
    filter(str_detect(term, 'o3_168h')) %>% 
    mutate(outcome = 'pp',
           time = '168h',
           p_lin = anova(fit_pp_o3_temp_pm10_linear_168h, fit_pp_o3_temp_pm10_linear_168h_inter)$`Pr(>Chisq)`[2],
           across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3_168h, na.rm = T))),
  fit_pp_o3_temp_pm10_linear_168h_inter %>% 
    tidy(conf.int = T) %>% 
    filter(str_detect(term, 'o3_168h')) %>% 
    mutate(outcome = 'pp',
           time = '168h',
           across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3_168h, na.rm = T))),
  fit_hr_o3_temp_pm10_linear_168h %>% 
    tidy(conf.int = T) %>% 
    filter(str_detect(term, 'o3_168h')) %>% 
    mutate(outcome = 'hr',
           time = '168h',
           p_lin = anova(fit_hr_o3_temp_pm10_linear_168h, fit_hr_o3_temp_pm10_linear_168h_inter)$`Pr(>Chisq)`[2],
           across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3_168h, na.rm = T))),
  fit_hr_o3_temp_pm10_linear_168h_inter %>% 
    tidy(conf.int = T) %>% 
    filter(str_detect(term, 'o3_168h')) %>% 
    mutate(outcome = 'hr',
           time = '168h',
           across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3_168h, na.rm = T))),
  
  
  fit_sbp_temp_o3_pm10_linear_8am %>% 
    tidy(conf.int = T) %>% 
    filter(str_detect(term, 'temp_ap')) %>% 
    mutate(outcome = 'sbp',
           time = '8am',
           p_lin = anova(fit_sbp_temp_o3_pm10_linear_8am, fit_sbp_temp_o3_pm10_linear_8am_inter)$`Pr(>Chisq)`[2],
           across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_ap, na.rm = T))),  
  fit_sbp_temp_o3_pm10_linear_8am_inter %>% 
    tidy(conf.int = T) %>% 
    filter(str_detect(term, 'temp_ap')) %>% 
    mutate(outcome = 'sbp',
           time = '8am',
           across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_ap, na.rm = T))),
  fit_dbp_temp_o3_pm10_linear_8am %>% 
    tidy(conf.int = T) %>% 
    filter(str_detect(term, 'temp_ap')) %>% 
    mutate(outcome = 'dbp',
           time = '8am',
           p_lin = anova(fit_dbp_temp_o3_pm10_linear_8am, fit_dbp_temp_o3_pm10_linear_8am_inter)$`Pr(>Chisq)`[2],
           across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_ap, na.rm = T))),  
  fit_dbp_temp_o3_pm10_linear_8am_inter %>% 
    tidy(conf.int = T) %>% 
    filter(str_detect(term, 'temp_ap')) %>% 
    mutate(outcome = 'dbp',
           time = '8am',
           across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_ap, na.rm = T))),
  fit_pp_temp_o3_pm10_linear_8am %>% 
    tidy(conf.int = T) %>% 
    filter(str_detect(term, 'temp_ap')) %>% 
    mutate(outcome = 'pp',
           time = '8am',
           p_lin = anova(fit_pp_temp_o3_pm10_linear_8am, fit_pp_temp_o3_pm10_linear_8am_inter)$`Pr(>Chisq)`[2],
           across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_ap, na.rm = T))),  
  fit_pp_temp_o3_pm10_linear_8am_inter %>% 
    tidy(conf.int = T) %>% 
    filter(str_detect(term, 'temp_ap')) %>% 
    mutate(outcome = 'pp',
           time = '8am',
           across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_ap, na.rm = T))),
  fit_hr_temp_o3_pm10_linear_8am %>% 
    tidy(conf.int = T) %>% 
    filter(str_detect(term, 'temp_ap')) %>% 
    mutate(outcome = 'hr',
           time = '8am',
           p_lin = anova(fit_hr_temp_o3_pm10_linear_8am, fit_hr_temp_o3_pm10_linear_8am_inter)$`Pr(>Chisq)`[2],
           across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_ap, na.rm = T))),  
  fit_hr_temp_o3_pm10_linear_8am_inter %>% 
    tidy(conf.int = T) %>% 
    filter(str_detect(term, 'temp_ap')) %>% 
    mutate(outcome = 'hr',
           time = '8am',
           across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$temp_ap, na.rm = T))),
  
  fit_sbp_o3_temp_pm10_linear_8am %>% 
    tidy(conf.int = T) %>% 
    filter(str_detect(term, 'o3')) %>% 
    mutate(outcome = 'sbp',
           time = '8am',
           p_lin = anova(fit_sbp_o3_temp_pm10_linear_8am, fit_sbp_o3_temp_pm10_linear_8am_inter)$`Pr(>Chisq)`[2],
           across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3, na.rm = T))),
  fit_sbp_o3_temp_pm10_linear_8am_inter %>% 
    tidy(conf.int = T) %>% 
    filter(str_detect(term, 'o3')) %>% 
    mutate(outcome = 'sbp',
           time = '8am',
           across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3, na.rm = T))),
  fit_dbp_o3_temp_pm10_linear_8am %>% 
    tidy(conf.int = T) %>% 
    filter(str_detect(term, 'o3')) %>% 
    mutate(outcome = 'dbp',
           time = '8am',
           p_lin = anova(fit_dbp_o3_temp_pm10_linear_8am, fit_dbp_o3_temp_pm10_linear_8am_inter)$`Pr(>Chisq)`[2],
           across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3, na.rm = T))),
  fit_dbp_o3_temp_pm10_linear_8am_inter %>% 
    tidy(conf.int = T) %>% 
    filter(str_detect(term, 'o3')) %>% 
    mutate(outcome = 'dbp',
           time = '8am',
           across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3, na.rm = T))),
  fit_pp_o3_temp_pm10_linear_8am %>% 
    tidy(conf.int = T) %>% 
    filter(str_detect(term, 'o3')) %>% 
    mutate(outcome = 'pp',
           time = '8am',
           p_lin = anova(fit_pp_o3_temp_pm10_linear_8am, fit_pp_o3_temp_pm10_linear_8am_inter)$`Pr(>Chisq)`[2],
           across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3, na.rm = T))),
  fit_pp_o3_temp_pm10_linear_8am_inter %>% 
    tidy(conf.int = T) %>% 
    filter(str_detect(term, 'o3')) %>% 
    mutate(outcome = 'pp',
           time = '8am',
           across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3, na.rm = T))),
  fit_hr_o3_temp_pm10_linear_8am %>% 
    tidy(conf.int = T) %>% 
    filter(str_detect(term, 'o3')) %>% 
    mutate(outcome = 'hr',
           time = '8am',
           p_lin = anova(fit_hr_o3_temp_pm10_linear_8am, fit_hr_o3_temp_pm10_linear_8am_inter)$`Pr(>Chisq)`[2],
           across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3, na.rm = T))),
  fit_hr_o3_temp_pm10_linear_8am_inter %>% 
    tidy(conf.int = T) %>% 
    filter(str_detect(term, 'o3')) %>% 
    mutate(outcome = 'hr',
           time = '8am',
           across(c(estimate, conf.low, conf.high), \(x) 10 * x / sd(df_1$o3, na.rm = T)))
) %>% 
  mutate(estimate = paste0(sprintf('%.2f', estimate), ' (',
                           sprintf('%.2f', conf.low), ', ',
                           sprintf('%.2f', conf.high), ')')) %>% 
  separate(term, into = c('main', 'inter'), sep = ':') %>%
  mutate(inter = str_sub(inter, -2, -1)) %>% 
  pivot_wider(id_cols = c('main', 'outcome'),
              names_from = 'inter',
              values_from = c('estimate', 'p_lin'))

writexl::write_xlsx(
  tableS1 %>% 
    select('main', 'outcome', contains('estimate'), p_lin = p_lin_NA),
  here::here(paste0(Sys.Date(), '_tableS1.xlsx')))

  