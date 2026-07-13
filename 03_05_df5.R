library('splines')
library('broom.mixed')
source(here::here('00_00_functions.R'))

# 1 Single exposure temperature ------------------------------------------------
newdat <- expand.grid(temp_24h = seq(quantile(df_2$temp_24h, .05, na.rm = T),
                                     quantile(df_2$temp_24h, .95, na.rm = T),
                                     length.out = 100)) %>% 
  mutate(o3_24h = 0,
         pm10_24h = 0,
         sex = 'FEMALE',
         age = 55,
         diab = 'no',
         whr = .9,
         smo = 'NEVER',
         alc = factor('2', levels = levels(df_2$alc)),
         phy = factor('0', levels = levels(df_2$alc)),
         ses = .28,
         year = factor('3', levels = levels(df_2$year)),
         month = factor('Jan', levels = levels(df_2$month)),
         wday = factor('Mon', levels = levels(df_2$wday)),
         site = 'got')

refdat <- newdat[1, ]
refdat$temp_24h <- 0

## SBP -------------------------------------------------------------------------
fit_sbp_temp_spline <-
  lme4::lmer(
    formula = sbp ~ ns(temp_24h, 5) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_2
  )

fit_sbp_temp_o3_spline <-
  lme4::lmer(
    formula = sbp ~ ns(temp_24h, 5) + ns(o3_24h, 5) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_2
  )

fit_sbp_temp_o3_pm10_spline <-
  lme4::lmer(
    formula = sbp ~ ns(temp_24h, 5) + ns(o3_24h, 5) + ns(pm10_24h, 5) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_2
  )

boot_temp_sbp <- lme4::bootMer(
  fit_sbp_temp_spline,
  FUN = boot_fun,
  nsim = 100,
  type = 'parametric',
  use.u = T
)

boot_temp_o3_sbp <- lme4::bootMer(
  fit_sbp_temp_o3_spline,
  FUN = boot_fun,
  nsim = 100,
  type = 'parametric',
  use.u = T
)

boot_temp_o3_pm10_sbp <- lme4::bootMer(
  fit_sbp_temp_o3_pm10_spline,
  FUN = boot_fun,
  nsim = 100,
  type = 'parametric',
  use.u = T
)

newdat_sbp <- newdat

newdat_sbp$fit <- predict(
  fit_sbp_temp_spline,
  newdata = newdat,
  re.form = NA
) - as.numeric(predict(
  fit_sbp_temp_spline,
  newdata = refdat,
  re.form = NA
))

newdat_sbp$fit_o3 <- predict(
  fit_sbp_temp_o3_spline,
  newdata = newdat,
  re.form = NA
) - as.numeric(predict(
  fit_sbp_temp_o3_spline,
  newdata = refdat,
  re.form = NA
))

newdat_sbp$fit_o3_pm10 <- predict(
  fit_sbp_temp_o3_pm10_spline,
  newdata = newdat,
  re.form = NA
) - as.numeric(predict(
  fit_sbp_temp_o3_pm10_spline,
  newdata = refdat,
  re.form = NA
))

newdat_sbp$lcl <- apply(boot_temp_sbp$t, 2, quantile, 0.025, na.rm = T)
newdat_sbp$ucl <- apply(boot_temp_sbp$t, 2, quantile, 0.975, na.rm = T)
newdat_sbp$lcl_o3 <- apply(boot_temp_o3_sbp$t, 2, quantile, 0.025, na.rm = T)
newdat_sbp$ucl_o3 <- apply(boot_temp_o3_sbp$t, 2, quantile, 0.975, na.rm = T)
newdat_sbp$lcl_o3_pm10 <- apply(boot_temp_o3_pm10_sbp$t, 2, quantile, 0.025, na.rm = T)
newdat_sbp$ucl_o3_pm10 <- apply(boot_temp_o3_pm10_sbp$t, 2, quantile, 0.975, na.rm = T)

p1sbp <- 
  ggplot(newdat_sbp, 
         aes(x = temp_24h*sd(df_1$temp_24h, na.rm = T) + mean(df_1$temp_24h, na.rm = T))) +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = .25, fill = 'darkorange') + 
  geom_ribbon(aes(ymin = lcl_o3, ymax = ucl_o3), alpha = .25, fill = 'darkgreen') + 
  geom_ribbon(aes(ymin = lcl_o3_pm10, ymax = ucl_o3_pm10), alpha = .25, fill = 'darkblue') + 
  geom_line(aes(y = fit), colour = 'darkorange', linetype = 'dashed') +
  geom_line(aes(y = fit_o3), colour = 'darkgreen') +
  geom_line(aes(y = fit_o3_pm10), colour = 'darkblue', linetype = 'dotted') +
  scale_x_continuous(expand = c(0,0), breaks = -1:2*5) +
  coord_cartesian(ylim = c(-1, 2)) +
  labs(x = 'Temperature, 24h mean [\u00b0C]',
       y = 'SBP difference [mmHg]') +
  theme_classic()

## DBP -------------------------------------------------------------------------
fit_dbp_temp_spline <-
  lme4::lmer(
    formula = dbp ~ ns(temp_24h, 5) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_2
  )

fit_dbp_temp_o3_spline <-
  lme4::lmer(
    formula = dbp ~ ns(temp_24h, 5) + ns(o3_24h, 5) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_2
  )

fit_dbp_temp_o3_pm10_spline <-
  lme4::lmer(
    formula = dbp ~ ns(temp_24h, 5) + ns(o3_24h, 5) + ns(pm10_24h, 5) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_2
  )

boot_temp_dbp <- lme4::bootMer(
  fit_dbp_temp_spline,
  FUN = boot_fun,
  nsim = 100,
  type = 'parametric',
  use.u = T
)

boot_temp_o3_dbp <- lme4::bootMer(
  fit_dbp_temp_o3_spline,
  FUN = boot_fun,
  nsim = 100,
  type = 'parametric',
  use.u = T
)

boot_temp_o3_pm10_dbp <- lme4::bootMer(
  fit_dbp_temp_o3_pm10_spline,
  FUN = boot_fun,
  nsim = 100,
  type = 'parametric',
  use.u = T
)

newdat_dbp <- newdat
newdat_dbp$fit <- predict(
  fit_dbp_temp_spline,
  newdata = newdat,
  re.form = NA
) - as.numeric(predict(
  fit_dbp_temp_spline,
  newdata = refdat,
  re.form = NA
))

newdat_dbp$fit_o3 <- predict(
  fit_dbp_temp_o3_spline,
  newdata = newdat,
  re.form = NA
) - as.numeric(predict(
  fit_dbp_temp_o3_spline,
  newdata = refdat,
  re.form = NA
))

newdat_dbp$fit_o3_pm10 <- predict(
  fit_dbp_temp_o3_pm10_spline,
  newdata = newdat,
  re.form = NA
) - as.numeric(predict(
  fit_dbp_temp_o3_pm10_spline,
  newdata = refdat,
  re.form = NA
))

newdat_dbp$lcl <- apply(boot_temp_dbp$t, 2, quantile, 0.025, na.rm = T)
newdat_dbp$ucl <- apply(boot_temp_dbp$t, 2, quantile, 0.975, na.rm = T)
newdat_dbp$lcl_o3 <- apply(boot_temp_o3_dbp$t, 2, quantile, 0.025, na.rm = T)
newdat_dbp$ucl_o3 <- apply(boot_temp_o3_dbp$t, 2, quantile, 0.975, na.rm = T)
newdat_dbp$lcl_o3_pm10 <- apply(boot_temp_o3_pm10_dbp$t, 2, quantile, 0.025, na.rm = T)
newdat_dbp$ucl_o3_pm10 <- apply(boot_temp_o3_pm10_dbp$t, 2, quantile, 0.975, na.rm = T)

p1dbp <- 
  ggplot(newdat_dbp, 
         aes(x = temp_24h*sd(df_1$temp_24h, na.rm = T) + mean(df_1$temp_24h, na.rm = T))) +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = .25, fill = 'darkorange') + 
  geom_ribbon(aes(ymin = lcl_o3, ymax = ucl_o3), alpha = .25, fill = 'darkgreen') + 
  geom_ribbon(aes(ymin = lcl_o3_pm10, ymax = ucl_o3_pm10), alpha = .25, fill = 'darkblue') + 
  geom_line(aes(y = fit), colour = 'darkorange', linetype = 'dashed') +
  geom_line(aes(y = fit_o3), colour = 'darkgreen') +
  geom_line(aes(y = fit_o3_pm10), colour = 'darkblue', linetype = 'dotted') +
  scale_x_continuous(expand = c(0,0), breaks = -1:2*5) +
  coord_cartesian(ylim = c(-1, 2)) +
  labs(x = 'Temperature, 24h mean [\u00b0C]',
       y = 'DBP difference [mmHg]') +
  theme_classic()

## PP --------------------------------------------------------------------------
fit_pp_temp_spline <-
  lme4::lmer(
    formula = pp ~ ns(temp_24h, 5) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_2
  )

fit_pp_temp_o3_spline <-
  lme4::lmer(
    formula = pp ~ ns(temp_24h, 5) + ns(o3_24h, 5) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_2
  )

fit_pp_temp_o3_pm10_spline <-
  lme4::lmer(
    formula = pp ~ ns(temp_24h, 5) + ns(o3_24h, 5) + ns(pm10_24h, 5) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_2
  )

boot_temp_pp <- lme4::bootMer(
  fit_pp_temp_spline,
  FUN = boot_fun,
  nsim = 100,
  type = 'parametric',
  use.u = T
)

boot_temp_o3_pp <- lme4::bootMer(
  fit_pp_temp_o3_spline,
  FUN = boot_fun,
  nsim = 100,
  type = 'parametric',
  use.u = T
)

boot_temp_o3_pm10_pp <- lme4::bootMer(
  fit_pp_temp_o3_pm10_spline,
  FUN = boot_fun,
  nsim = 100,
  type = 'parametric',
  use.u = T
)

newdat_pp <- newdat
newdat_pp$fit <- predict(
  fit_pp_temp_spline,
  newdata = newdat,
  re.form = NA
) - as.numeric(predict(
  fit_pp_temp_spline,
  newdata = refdat,
  re.form = NA
))

newdat_pp$fit_o3 <- predict(
  fit_pp_temp_o3_spline,
  newdata = newdat,
  re.form = NA
) - as.numeric(predict(
  fit_pp_temp_o3_spline,
  newdata = refdat,
  re.form = NA
))


newdat_pp$fit_o3_pm10 <- predict(
  fit_pp_temp_o3_pm10_spline,
  newdata = newdat,
  re.form = NA
) - as.numeric(predict(
  fit_pp_temp_o3_pm10_spline,
  newdata = refdat,
  re.form = NA
))

newdat_pp$lcl <- apply(boot_temp_pp$t, 2, quantile, 0.025, na.rm = T)
newdat_pp$ucl <- apply(boot_temp_pp$t, 2, quantile, 0.975, na.rm = T)
newdat_pp$lcl_o3 <- apply(boot_temp_o3_pp$t, 2, quantile, 0.025, na.rm = T)
newdat_pp$ucl_o3 <- apply(boot_temp_o3_pp$t, 2, quantile, 0.975, na.rm = T)
newdat_pp$lcl_o3_pm10 <- apply(boot_temp_o3_pm10_pp$t, 2, quantile, 0.025, na.rm = T)
newdat_pp$ucl_o3_pm10 <- apply(boot_temp_o3_pm10_pp$t, 2, quantile, 0.975, na.rm = T)

p1pp <- 
  ggplot(newdat_pp, 
         aes(x = temp_24h*sd(df_1$temp_24h, na.rm = T) + mean(df_1$temp_24h, na.rm = T))) +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = .25, fill = 'darkorange') + 
  geom_ribbon(aes(ymin = lcl_o3, ymax = ucl_o3), alpha = .25, fill = 'darkgreen') + 
  geom_ribbon(aes(ymin = lcl_o3_pm10, ymax = ucl_o3_pm10), alpha = .25, fill = 'darkblue') + 
  geom_line(aes(y = fit), colour = 'darkorange', linetype = 'dashed') +
  geom_line(aes(y = fit_o3), colour = 'darkgreen') +
  geom_line(aes(y = fit_o3_pm10), colour = 'darkblue', linetype = 'dotted') +
  scale_x_continuous(expand = c(0,0), breaks = -1:2*5) +
  coord_cartesian(ylim = c(-1, 2)) +
  labs(x = 'Temperature, 24h mean [\u00b0C]',
       y = 'PP difference [mmHg]') +
  theme_classic()

## HR --------------------------------------------------------------------------
fit_hr_temp_spline <-
  lme4::lmer(
    formula = pulse ~ ns(temp_24h, 5) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_2
  )

fit_hr_temp_o3_spline <-
  lme4::lmer(
    formula = pulse ~ ns(temp_24h, 5) + ns(o3_24h, 5) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_2
  )

fit_hr_temp_o3_pm10_spline <-
  lme4::lmer(
    formula = pulse ~ ns(temp_24h, 5) + ns(o3_24h, 5) + ns(pm10_24h) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_2
  )

boot_temp_hr <- lme4::bootMer(
  fit_hr_temp_spline,
  FUN = boot_fun,
  nsim = 100,
  type = 'parametric',
  use.u = T
)

boot_temp_o3_hr <- lme4::bootMer(
  fit_hr_temp_o3_spline,
  FUN = boot_fun,
  nsim = 100,
  type = 'parametric',
  use.u = T
)

boot_temp_o3_pm10_hr <- lme4::bootMer(
  fit_hr_temp_o3_pm10_spline,
  FUN = boot_fun,
  nsim = 100,
  type = 'parametric',
  use.u = T
)

newdat_hr <- newdat
newdat_hr$fit <- predict(
  fit_hr_temp_spline,
  newdata = newdat,
  re.form = NA
) - as.numeric(predict(
  fit_hr_temp_spline,
  newdata = refdat,
  re.form = NA
))

newdat_hr$fit_o3 <- predict(
  fit_hr_temp_o3_spline,
  newdata = newdat,
  re.form = NA
) - as.numeric(predict(
  fit_hr_temp_o3_spline,
  newdata = refdat,
  re.form = NA
))

newdat_hr$fit_o3_pm10 <- predict(
  fit_hr_temp_o3_pm10_spline,
  newdata = newdat,
  re.form = NA
) - as.numeric(predict(
  fit_hr_temp_o3_pm10_spline,
  newdata = refdat,
  re.form = NA
))

newdat_hr$lcl <- apply(boot_temp_hr$t, 2, quantile, 0.025, na.rm = T)
newdat_hr$ucl <- apply(boot_temp_hr$t, 2, quantile, 0.975, na.rm = T)
newdat_hr$lcl_o3 <- apply(boot_temp_o3_hr$t, 2, quantile, 0.025, na.rm = T)
newdat_hr$ucl_o3 <- apply(boot_temp_o3_hr$t, 2, quantile, 0.975, na.rm = T)
newdat_hr$lcl_o3_pm10 <- apply(boot_temp_o3_pm10_hr$t, 2, quantile, 0.025, na.rm = T)
newdat_hr$ucl_o3_pm10 <- apply(boot_temp_o3_pm10_hr$t, 2, quantile, 0.975, na.rm = T)

p1hr <- 
  ggplot(newdat_hr, 
         aes(x = temp_24h*sd(df_1$temp_24h, na.rm = T) + mean(df_1$temp_24h, na.rm = T))) +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = .25, fill = 'darkorange') + 
  geom_ribbon(aes(ymin = lcl_o3, ymax = ucl_o3), alpha = .25, fill = 'darkgreen') + 
  geom_ribbon(aes(ymin = lcl_o3_pm10, ymax = ucl_o3_pm10), alpha = .25, fill = 'darkblue') +
  geom_line(aes(y = fit), colour = 'darkorange', linetype = 'dashed') +
  geom_line(aes(y = fit_o3), colour = 'darkgreen') +
  geom_line(aes(y = fit_o3_pm10), colour = 'darkblue', linetype = 'dotted') +
  scale_x_continuous(expand = c(0,0), breaks = -1:2*5) +
  coord_cartesian(ylim = c(-1, 2)) +
  labs(x = 'Temperature, 24h mean [\u00b0C]',
       y = 'HR difference [bpm]') +
  theme_classic()

# 2 Single exposure O3 ---------------------------------------------------------
newdat <- expand.grid(o3_24h = seq(quantile(df_2$o3_24h, .05, na.rm = T),
                                   quantile(df_2$o3_24h, .95, na.rm = T),
                                   length.out = 100)) %>% 
  mutate(temp_24h = 0,
         pm10_24h = 0,
         sex = 'FEMALE',
         age = 55,
         diab = 'no',
         whr = .9,
         smo = 'NEVER',
         alc = factor('2', levels = levels(df_2$alc)),
         phy = factor('0', levels = levels(df_2$alc)),
         ses = .28,
         year = factor('3', levels = levels(df_2$year)),
         month = factor('Jan', levels = levels(df_2$month)),
         wday = factor('Mon', levels = levels(df_2$wday)),
         site = 'got')

refdat <- newdat[1, ]
refdat$o3_24h <- 0

## SBP -------------------------------------------------------------------------
fit_sbp_o3_spline <-
  lme4::lmer(
    formula = sbp ~ ns(o3_24h, 5) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_2
  )

fit_sbp_o3_temp_spline <-
  lme4::lmer(
    formula = sbp ~ ns(o3_24h, 5) + ns(temp_24h, 5) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_2
  )

fit_sbp_o3_temp_pm10_spline <-
  lme4::lmer(
    formula = sbp ~ ns(o3_24h, 5) + ns(temp_24h, 5) + ns(pm10_24h, 5) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_2
  )

boot_o3_sbp <- lme4::bootMer(
  fit_sbp_o3_spline,
  FUN = boot_fun,
  nsim = 100,
  type = 'parametric',
  use.u = T
)

boot_o3_temp_sbp <- lme4::bootMer(
  fit_sbp_o3_temp_spline,
  FUN = boot_fun,
  nsim = 100,
  type = 'parametric',
  use.u = T
)

boot_o3_temp_pm10_sbp <- lme4::bootMer(
  fit_sbp_o3_temp_pm10_spline,
  FUN = boot_fun,
  nsim = 100,
  type = 'parametric',
  use.u = T
)

newdat_sbp <- newdat

newdat_sbp$fit <- predict(
  fit_sbp_o3_spline,
  newdata = newdat,
  re.form = NA
) - as.numeric(predict(
  fit_sbp_o3_spline,
  newdata = refdat,
  re.form = NA
))

newdat_sbp$fit_temp <- predict(
  fit_sbp_o3_temp_spline,
  newdata = newdat,
  re.form = NA
) - as.numeric(predict(
  fit_sbp_o3_temp_spline,
  newdata = refdat,
  re.form = NA
))

newdat_sbp$fit_temp_pm10 <- predict(
  fit_sbp_o3_temp_pm10_spline,
  newdata = newdat,
  re.form = NA
) - as.numeric(predict(
  fit_sbp_o3_temp_pm10_spline,
  newdata = refdat,
  re.form = NA
))

newdat_sbp$lcl <- apply(boot_o3_sbp$t, 2, quantile, 0.025, na.rm = T)
newdat_sbp$ucl <- apply(boot_o3_sbp$t, 2, quantile, 0.975, na.rm = T)
newdat_sbp$lcl_temp <- apply(boot_o3_temp_sbp$t, 2, quantile, 0.025, na.rm = T)
newdat_sbp$ucl_temp <- apply(boot_o3_temp_sbp$t, 2, quantile, 0.975, na.rm = T)
newdat_sbp$lcl_temp_pm10 <- apply(boot_o3_temp_pm10_sbp$t, 2, quantile, 0.025, na.rm = T)
newdat_sbp$ucl_temp_pm10 <- apply(boot_o3_temp_pm10_sbp$t, 2, quantile, 0.975, na.rm = T)

p2sbp <- 
  ggplot(newdat_sbp, 
         aes(x = o3_24h*sd(df_1$o3_24h, na.rm = T) + mean(df_1$o3_24h, na.rm = T))) +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = .25, fill = 'darkorange') + 
  geom_ribbon(aes(ymin = lcl_temp, ymax = ucl_temp), alpha = .25, fill = 'darkgreen') + 
  geom_ribbon(aes(ymin = lcl_temp_pm10, ymax = ucl_temp_pm10), alpha = .25, fill = 'darkblue') + 
  geom_line(aes(y = fit), colour = 'darkorange', linetype = 'dashed') +
  geom_line(aes(y = fit_temp), colour = 'darkgreen') +
  geom_line(aes(y = fit_temp_pm10), colour = 'darkblue', linetype = 'dotted') +
  scale_x_continuous(expand = c(0,0), breaks = 2:7*10) +
  coord_cartesian(ylim = c(-1, 2)) +
  labs(x = 'O3, 24h mean [\u00b5g/m\u00b3]',
       y = 'SBP difference [mmHg]') +
  theme_classic()

## DBP -------------------------------------------------------------------------
fit_dbp_o3_spline <-
  lme4::lmer(
    formula = dbp ~ ns(o3_24h, 5) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_2
  )

fit_dbp_o3_temp_spline <-
  lme4::lmer(
    formula = dbp ~ ns(o3_24h, 5) + ns(temp_24h, 5) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_2
  )

fit_dbp_o3_temp_pm10_spline <-
  lme4::lmer(
    formula = dbp ~ ns(o3_24h, 5) + ns(temp_24h, 5) + ns(pm10_24h, 5) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_2
  )

boot_o3_dbp <- lme4::bootMer(
  fit_dbp_o3_spline,
  FUN = boot_fun,
  nsim = 100,
  type = 'parametric',
  use.u = T
)

boot_o3_temp_dbp <- lme4::bootMer(
  fit_dbp_o3_temp_spline,
  FUN = boot_fun,
  nsim = 100,
  type = 'parametric',
  use.u = T
)

boot_o3_temp_pm10_dbp <- lme4::bootMer(
  fit_dbp_o3_temp_pm10_spline,
  FUN = boot_fun,
  nsim = 100,
  type = 'parametric',
  use.u = T
)

newdat_dbp <- newdat

newdat_dbp$fit <- predict(
  fit_dbp_o3_spline,
  newdata = newdat,
  re.form = NA
) - as.numeric(predict(
  fit_dbp_o3_spline,
  newdata = refdat,
  re.form = NA
))

newdat_dbp$fit_temp <- predict(
  fit_dbp_o3_temp_spline,
  newdata = newdat,
  re.form = NA
) - as.numeric(predict(
  fit_dbp_o3_temp_spline,
  newdata = refdat,
  re.form = NA
))

newdat_dbp$fit_temp_pm10 <- predict(
  fit_dbp_o3_temp_pm10_spline,
  newdata = newdat,
  re.form = NA
) - as.numeric(predict(
  fit_dbp_o3_temp_pm10_spline,
  newdata = refdat,
  re.form = NA
))

newdat_dbp$lcl <- apply(boot_o3_dbp$t, 2, quantile, 0.025, na.rm = T)
newdat_dbp$ucl <- apply(boot_o3_dbp$t, 2, quantile, 0.975, na.rm = T)
newdat_dbp$lcl_temp <- apply(boot_o3_temp_dbp$t, 2, quantile, 0.025, na.rm = T)
newdat_dbp$ucl_temp <- apply(boot_o3_temp_dbp$t, 2, quantile, 0.975, na.rm = T)
newdat_dbp$lcl_temp_pm10 <- apply(boot_o3_temp_pm10_dbp$t, 2, quantile, 0.025, na.rm = T)
newdat_dbp$ucl_temp_pm10 <- apply(boot_o3_temp_pm10_dbp$t, 2, quantile, 0.975, na.rm = T)

p2dbp <- 
  ggplot(newdat_dbp, 
         aes(x = o3_24h*sd(df_1$o3_24h, na.rm = T) + mean(df_1$o3_24h, na.rm = T))) +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = .25, fill = 'darkorange') + 
  geom_ribbon(aes(ymin = lcl_temp, ymax = ucl_temp), alpha = .25, fill = 'darkgreen') + 
  geom_ribbon(aes(ymin = lcl_temp_pm10, ymax = ucl_temp_pm10), alpha = .25, fill = 'darkblue') +
  geom_line(aes(y = fit), colour = 'darkorange', linetype = 'dashed') +
  geom_line(aes(y = fit_temp), colour = 'darkgreen') +
  geom_line(aes(y = fit_temp_pm10), colour = 'darkblue', linetype = 'dotted') +
  scale_x_continuous(expand = c(0,0), breaks = 2:7*10) +
  coord_cartesian(ylim = c(-1, 2)) +
  labs(x = 'O3, 24h mean [\u00b5g/m\u00b3]',
       y = 'DBP difference [mmHg]') +
  theme_classic()

## PP -------------------------------------------------------------------------
fit_pp_o3_spline <-
  lme4::lmer(
    formula = pp ~ ns(o3_24h, 5) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_2
  )

fit_pp_o3_temp_spline <-
  lme4::lmer(
    formula = pp ~ ns(o3_24h, 5) + ns(temp_24h, 5) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_2
  )

fit_pp_o3_temp_pm10_spline <-
  lme4::lmer(
    formula = pp ~ ns(o3_24h, 5) + ns(temp_24h, 5) + ns(pm10_24h, 5) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_2
  )

boot_o3_pp <- lme4::bootMer(
  fit_pp_o3_spline,
  FUN = boot_fun,
  nsim = 100,
  type = 'parametric',
  use.u = T
)

boot_o3_temp_pp <- lme4::bootMer(
  fit_pp_o3_temp_spline,
  FUN = boot_fun,
  nsim = 100,
  type = 'parametric',
  use.u = T
)

boot_o3_temp_pm10_pp <- lme4::bootMer(
  fit_pp_o3_temp_pm10_spline,
  FUN = boot_fun,
  nsim = 100,
  type = 'parametric',
  use.u = T
)

newdat_pp <- newdat

newdat_pp$fit <- predict(
  fit_pp_o3_spline,
  newdata = newdat,
  re.form = NA
) - as.numeric(predict(
  fit_pp_o3_spline,
  newdata = refdat,
  re.form = NA
))

newdat_pp$fit_temp <- predict(
  fit_pp_o3_temp_spline,
  newdata = newdat,
  re.form = NA
) - as.numeric(predict(
  fit_pp_o3_temp_spline,
  newdata = refdat,
  re.form = NA
))

newdat_pp$fit_temp_pm10 <- predict(
  fit_pp_o3_temp_pm10_spline,
  newdata = newdat,
  re.form = NA
) - as.numeric(predict(
  fit_pp_o3_temp_pm10_spline,
  newdata = refdat,
  re.form = NA
))

newdat_pp$lcl <- apply(boot_o3_pp$t, 2, quantile, 0.025, na.rm = T)
newdat_pp$ucl <- apply(boot_o3_pp$t, 2, quantile, 0.975, na.rm = T)
newdat_pp$lcl_temp <- apply(boot_o3_temp_pp$t, 2, quantile, 0.025, na.rm = T)
newdat_pp$ucl_temp <- apply(boot_o3_temp_pp$t, 2, quantile, 0.975, na.rm = T)
newdat_pp$lcl_temp_pm10 <- apply(boot_o3_temp_pm10_pp$t, 2, quantile, 0.025, na.rm = T)
newdat_pp$ucl_temp_pm10 <- apply(boot_o3_temp_pm10_pp$t, 2, quantile, 0.975, na.rm = T)

p2pp <- 
  ggplot(newdat_pp, 
         aes(x = o3_24h*sd(df_1$o3_24h, na.rm = T) + mean(df_1$o3_24h, na.rm = T))) +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = .25, fill = 'darkorange') + 
  geom_ribbon(aes(ymin = lcl_temp, ymax = ucl_temp), alpha = .25, fill = 'darkgreen') + 
  geom_ribbon(aes(ymin = lcl_temp_pm10, ymax = ucl_temp_pm10), alpha = .25, fill = 'darkblue') + 
  geom_line(aes(y = fit), colour = 'darkorange', linetype = 'dashed') +
  geom_line(aes(y = fit_temp), colour = 'darkgreen') +
  geom_line(aes(y = fit_temp_pm10), colour = 'darkblue', linetype = 'dotted') +
  scale_x_continuous(expand = c(0,0), breaks = 2:7*10) +
  coord_cartesian(ylim = c(-1, 2)) +
  labs(x = 'O3, 24h mean [\u00b5g/m\u00b3]',
       y = 'PP difference [mmHg]') +
  theme_classic()

## HR --------------------------------------------------------------------------
fit_hr_o3_spline <-
  lme4::lmer(
    formula = pulse ~ ns(o3_24h, 5) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_2
  )

fit_hr_o3_temp_spline <-
  lme4::lmer(
    formula = pulse ~ ns(o3_24h, 5) + ns(temp_24h, 5) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_2
  )

fit_hr_o3_temp_pm10_spline <-
  lme4::lmer(
    formula = pulse ~ ns(o3_24h, 5) + ns(temp_24h, 5) + ns(pm10_24h, 5) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_2
  )

boot_o3_hr <- lme4::bootMer(
  fit_hr_o3_spline,
  FUN = boot_fun,
  nsim = 100,
  type = 'parametric',
  use.u = T
)

boot_o3_temp_hr <- lme4::bootMer(
  fit_hr_o3_temp_spline,
  FUN = boot_fun,
  nsim = 100,
  type = 'parametric',
  use.u = T
)

boot_o3_temp_pm10_hr <- lme4::bootMer(
  fit_hr_o3_temp_pm10_spline,
  FUN = boot_fun,
  nsim = 100,
  type = 'parametric',
  use.u = T
)

newdat_hr <- newdat

newdat_hr$fit <- predict(
  fit_hr_o3_spline,
  newdata = newdat,
  re.form = NA
) - as.numeric(predict(
  fit_hr_o3_spline,
  newdata = refdat,
  re.form = NA
))

newdat_hr$fit_temp <- predict(
  fit_hr_o3_temp_spline,
  newdata = newdat,
  re.form = NA
) - as.numeric(predict(
  fit_hr_o3_temp_spline,
  newdata = refdat,
  re.form = NA
))

newdat_hr$fit_temp_pm10 <- predict(
  fit_hr_o3_temp_pm10_spline,
  newdata = newdat,
  re.form = NA
) - as.numeric(predict(
  fit_hr_o3_temp_pm10_spline,
  newdata = refdat,
  re.form = NA
))

newdat_hr$lcl <- apply(boot_o3_hr$t, 2, quantile, 0.025, na.rm = T)
newdat_hr$ucl <- apply(boot_o3_hr$t, 2, quantile, 0.975, na.rm = T)
newdat_hr$lcl_temp <- apply(boot_o3_temp_hr$t, 2, quantile, 0.025, na.rm = T)
newdat_hr$ucl_temp <- apply(boot_o3_temp_hr$t, 2, quantile, 0.975, na.rm = T)
newdat_hr$lcl_temp_pm10 <- apply(boot_o3_temp_pm10_hr$t, 2, quantile, 0.025, na.rm = T)
newdat_hr$ucl_temp_pm10 <- apply(boot_o3_temp_pm10_hr$t, 2, quantile, 0.975, na.rm = T)

p2hr <- 
  ggplot(newdat_hr, 
         aes(x = o3_24h*sd(df_1$o3_24h, na.rm = T) + mean(df_1$o3_24h, na.rm = T))) +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = .25, fill = 'darkorange') + 
  geom_ribbon(aes(ymin = lcl_temp, ymax = ucl_temp), alpha = .25, fill = 'darkgreen') + 
  geom_ribbon(aes(ymin = lcl_temp_pm10, ymax = ucl_temp_pm10), alpha = .25, fill = 'darkblue') + 
  geom_line(aes(y = fit), colour = 'darkorange', linetype = 'dashed') +
  geom_line(aes(y = fit_temp), colour = 'darkgreen') +
  geom_line(aes(y = fit_temp_pm10), colour = 'darkblue', linetype = 'dotted') +
  scale_x_continuous(expand = c(0,0), breaks = 2:7*10) +
  coord_cartesian(ylim = c(-1, 2)) +
  labs(x = 'O3, 24h mean [\u00b5g/m\u00b3]',
       y = 'HR difference [bpm]') +
  theme_classic()

# 3 Interaction temp-O3 --------------------------------------------------------
df_3 <- df_2 %>% 
  filter(!is.na(o3_24h) & !is.na(temp_24h)) %>%
  mutate(temp_24h = factor(ntile(temp_24h, 3)))

newdat <- expand.grid(o3_24h = seq(quantile(df_3$o3_24h, .05, na.rm = T),
                                   quantile(df_3$o3_24h, .95, na.rm = T),
                                   length.out = 100),
                      temp_24h = factor(c(1,2,3))) %>% 
  mutate(pm10_24h = 0,
         sex = 'FEMALE',
         age = 55,
         diab = 'no',
         whr = .9,
         smo = 'NEVER',
         alc = factor('2', levels = levels(df_2$alc)),
         phy = factor('0', levels = levels(df_2$alc)),
         ses = .28,
         year = factor('3', levels = levels(df_2$year)),
         month = factor('Jan', levels = levels(df_2$month)),
         wday = factor('Mon', levels = levels(df_2$wday)),
         site = 'got')

refdat <- newdat
refdat$o3_24h <- 0
refdat$temp_24h <- c(rep('1', 100),
                     rep('2', 100),
                     rep('3', 100)) %>% factor()

## SBP -------------------------------------------------------------------------
fit_sbp_o3_temp_inter <-
  lme4::lmer(
    formula = sbp ~ ns(o3_24h, 5)*temp_24h + ns(pm10_24h, 5) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

boot_int_sbp <- lme4::bootMer(
  fit_sbp_o3_temp_inter,
  FUN = boot_fun,
  nsim = 100,
  type = 'parametric',
  use.u = T
)

newdat_sbp <- newdat

newdat_sbp$fit <- predict(
  fit_sbp_o3_temp_inter,
  newdata = newdat,
  re.form = NA
) - as.numeric(predict(
  fit_sbp_o3_temp_inter,
  newdata = refdat,
  re.form = NA
))

newdat_sbp$lcl <- apply(boot_int_sbp$t, 2, quantile, 0.025, na.rm = T)
newdat_sbp$ucl <- apply(boot_int_sbp$t, 2, quantile, 0.975, na.rm = T)

p3sbp <- 
  ggplot(newdat_sbp %>% 
           mutate(temp_24h =factor(temp_24h,
                                   labels = c('Coldest tertile (<-2\u00b0C)',
                                              'Middle tertile (-2 to 5\u00b0C)',
                                              'Warmest tertile (>5 \u00b0C)'))),
         aes(x = o3_24h*sd(df_1$o3_24h, na.rm = T) + mean(df_1$o3_24h, na.rm = T),
             group = temp_24h)) +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = temp_24h), alpha = .1) + 
  geom_line(aes(y = fit, 
                colour = temp_24h)) +
  scale_x_continuous(expand = c(0,0), breaks = 2:7*10) +
  scale_colour_manual(values = c('darkblue', 'black', 'darkred')) +
  scale_fill_manual(values = c('darkblue', 'black', 'darkred')) +
  coord_cartesian(ylim = c(-1, 2)) +
  labs(x = 'O3, 24h mean [\u00b5g/m\u00b3]',
       y = 'SBP difference [mmHg]',
       colour = 'Temperature',
       fill = 'Temperature') +
  theme_classic()

## DBP -------------------------------------------------------------------------
fit_dbp_o3_temp_inter <-
  lme4::lmer(
    formula = dbp ~ ns(o3_24h, 5)*temp_24h + ns(pm10_24h) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

boot_int_dbp <- lme4::bootMer(
  fit_dbp_o3_temp_inter,
  FUN = boot_fun,
  nsim = 100,
  type = 'parametric',
  use.u = T
)

newdat_dbp <- newdat

newdat_dbp$fit <- predict(
  fit_dbp_o3_temp_inter,
  newdata = newdat,
  re.form = NA
) - as.numeric(predict(
  fit_dbp_o3_temp_inter,
  newdata = refdat,
  re.form = NA
))

newdat_dbp$lcl <- apply(boot_int_dbp$t, 2, quantile, 0.025, na.rm = T)
newdat_dbp$ucl <- apply(boot_int_dbp$t, 2, quantile, 0.975, na.rm = T)

p3dbp <- 
  ggplot(newdat_dbp %>% 
           mutate(temp_24h = factor(temp_24h)),#,
         #labels = c('Coldest tertile (<-2\u00b0C)',
         #           'Middle tertile (-2 to 5\u00b0C)',
         #           'Warmest tertile (>5 \u00b0C)'))),
         aes(x = o3_24h*sd(df_1$o3_24h, na.rm = T) + mean(df_1$o3_24h, na.rm = T),
             group = temp_24h)) +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = temp_24h), alpha = .1) + 
  geom_line(aes(y = fit, 
                colour = temp_24h)) +
  scale_x_continuous(expand = c(0,0), breaks = 2:7*10) +
  scale_colour_manual(values = c('darkblue', 'black', 'darkred')) +
  scale_fill_manual(values = c('darkblue', 'black', 'darkred')) +
  coord_cartesian(ylim = c(-1, 2)) +
  labs(x = 'O3, 24h mean [\u00b5g/m\u00b3]',
       y = 'DBP difference [mmHg]',
       colour = 'Temperature',
       fill = 'Temperature') +
  theme_classic()

## PP -------------------------------------------------------------------------
fit_pp_o3_temp_inter <-
  lme4::lmer(
    formula = pp ~ ns(o3_24h, 5)*temp_24h + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

boot_int_pp <- lme4::bootMer(
  fit_pp_o3_temp_inter,
  FUN = boot_fun,
  nsim = 100,
  type = 'parametric',
  use.u = T
)

newdat_pp <- newdat

newdat_pp$fit <- predict(
  fit_pp_o3_temp_inter,
  newdata = newdat,
  re.form = NA
) - as.numeric(predict(
  fit_pp_o3_temp_inter,
  newdata = refdat,
  re.form = NA
))

newdat_pp$lcl <- apply(boot_int_pp$t, 2, quantile, 0.025, na.rm = T)
newdat_pp$ucl <- apply(boot_int_pp$t, 2, quantile, 0.975, na.rm = T)

p3pp <- 
  ggplot(newdat_pp %>% 
           mutate(temp_24h = factor(temp_24h,
                                    labels = c('Coldest tertile (<-2\u00b0C)',
                                               'Middle tertile (-2 to 5\u00b0C)',
                                               'Warmest tertile (>5 \u00b0C)'))),
         aes(x = o3_24h*sd(df_1$o3_24h, na.rm = T) + mean(df_1$o3_24h, na.rm = T),
             group = temp_24h)) +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = temp_24h), alpha = .1) + 
  geom_line(aes(y = fit, 
                colour = temp_24h)) +
  scale_x_continuous(expand = c(0,0), breaks = 2:7*10) +
  scale_colour_manual(values = c('darkblue', 'black', 'darkred')) +
  scale_fill_manual(values = c('darkblue', 'black', 'darkred')) +
  coord_cartesian(ylim = c(-1, 2)) +
  labs(x = 'O3 concentration, 24h mean [\u00b5g/m\u00b3]',
       y = 'Estimated PP difference [mmHg]',
       colour = 'Apparent temperature',
       fill = 'Apparent temperature') +
  theme_classic()

## HR --------------------------------------------------------------------------
fit_hr_o3_temp_inter <-
  lme4::lmer(
    formula = pulse ~ ns(o3_24h, 5)*temp_24h + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + (1|site),
    data = df_3
  )

boot_int_hr <- lme4::bootMer(
  fit_hr_o3_temp_inter,
  FUN = boot_fun,
  nsim = 100,
  type = 'parametric',
  use.u = T
)

newdat_hr <- newdat

newdat_hr$fit <- predict(
  fit_hr_o3_temp_inter,
  newdata = newdat,
  re.form = NA
) - as.numeric(predict(
  fit_hr_o3_temp_inter,
  newdata = refdat,
  re.form = NA
))

newdat_hr$lcl <- apply(boot_int_hr$t, 2, quantile, 0.025, na.rm = T)
newdat_hr$ucl <- apply(boot_int_hr$t, 2, quantile, 0.975, na.rm = T)

p3hr <- 
  ggplot(newdat_hr %>% 
           mutate(temp_24h =factor(temp_24h,
                                   labels = c('Coldest tertile (<-2\u00b0C)',
                                              'Middle tertile (-2 to 5\u00b0C)',
                                              'Warmest tertile (>5 \u00b0C)'))),
         aes(x = o3_24h*sd(df_1$o3_24h, na.rm = T) + mean(df_1$o3_24h, na.rm = T),
             group = temp_24h)) +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = temp_24h), alpha = .1) + 
  geom_line(aes(y = fit, 
                colour = temp_24h)) +
  scale_x_continuous(expand = c(0,0), breaks = 2:7*10) +
  scale_colour_manual(values = c('darkblue', 'black', 'darkred')) +
  scale_fill_manual(values = c('darkblue', 'black', 'darkred')) +
  coord_cartesian(ylim = c(-1, 2)) +
  labs(x = 'O3, 24h mean [\u00b5g/m\u00b3]',
       y = 'HR difference [bpm]',
       colour = 'Temperature',
       fill = 'Temperature') +
  theme_classic()


## Plot ------------------------------------------------------------------------
cowplot::plot_grid(
  p1sbp,
  p1dbp,
  p1pp,
  p1hr,
  p2sbp,
  p2dbp,
  p2pp,
  p2hr,
  p3sbp + theme(legend.position = 'none'),
  p3dbp + theme(legend.position = 'none'),
  p3pp + theme(legend.position = 'none'),
  p3hr + theme(legend.position = 'none'),
  nrow = 3,
  align = 'hv',
  labels = c('A', '', '', '', 'B', '', '', '', 'C', '', '', '')
)
ggsave(here::here('figures', paste0(Sys.Date(), '_fig1_5df.svg')), width = 14, height = 9)
ggsave(here::here('figures', paste0(Sys.Date(), '_fig1_5df.png')), width = 14, height = 9)
