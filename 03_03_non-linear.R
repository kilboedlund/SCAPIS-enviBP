library('mgcv')

df_3 <- df_2 %>% 
  mutate(o3_24h = df_1$o3_24h,
         temp_24h = df_1$temp_24h,
         site = factor(site))

gam_sbp <-
  gam(sbp ~ te(o3_24h, temp_24h, k = 3) +
        s(pm10_24h) + s(no2_24h) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + s(site, bs = 're'),
    data = df_3,
    method = 'REML')

gam_dbp <-
  gam(dbp ~ te(o3_24h, temp_24h, k = 3) +
        s(pm10_24h) + s(no2_24h) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + s(site, bs = 're'),
      data = df_3,
      method = 'REML')

gam_pp <-
  gam(pp ~ te(o3_24h, temp_24h, k = 3) +
        s(pm10_24h) + s(no2_24h) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + s(site, bs = 're'),
      data = df_3,
      method = 'REML')

gam_hr <-
  gam(pulse ~ te(o3_24h, temp_24h, k = 3) +
        s(pm10_24h) + s(no2_24h) + sex + age + diab + whr + smo + alc + phy + ses + year + month + wday + s(site, bs = 're'),
      data = df_3,
      method = 'REML')

png(here::here('figures', paste0(Sys.Date(), '_gam.png')), width = 2400, height = 2400)
par(mfrow = c(2,2), cex = 2.5, mar = c(2,2,2,2))
vis.gam(gam_sbp, view = c('o3_24h', 'temp_24h'), theta = 40, phi = 30, too.far = .05, n.grid = 50,
        xlab = 'O3 (24-h) [\u00b5g/m3]',
        ylab = 'Temperature (24-h) [\u00b0C]',
        zlab = 'SBP [mmHg]',
        ticktype = 'detailed',
        cex.lab = 1.5)
vis.gam(gam_dbp, view = c('o3_24h', 'temp_24h'), theta = 40, phi = 30, too.far = .05, n.grid = 50,
        xlab = 'O3 (24-h) [\u00b5g/m3]',
        ylab = 'Temperature (24-h) [\u00b0C]',
        zlab = 'DBP [mmHg]',
        ticktype = 'detailed',
        cex.lab = 1.5)
vis.gam(gam_pp, view = c('o3_24h', 'temp_24h'), theta = 40, phi = 30, too.far = .05, n.grid = 50,
        xlab = 'O3 (24-h) [\u00b5g/m3]',
        ylab = 'Temperature (24-h) [\u00b0C]',
        zlab = 'PP [mmHg]',
        ticktype = 'detailed',
        cex.lab = 1.5)
vis.gam(gam_hr, view = c('o3_24h', 'temp_24h'), theta = 40, phi = 30, too.far = .05, n.grid = 50,
        xlab = 'O3 (24-h) [\u00b5g/m3]',
        ylab = 'Temperature (24-h) [\u00b0C]',
        zlab = 'HR [bpm]',
        ticktype = 'detailed',
        cex.lab = 1.5)
dev.off()