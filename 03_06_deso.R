library('tidyverse')
library('sf')
library('mgcv')
library('gratia')
library('broom')
source(here::here('03_00_analysis dataset preparation.R'))
source(here::here('00_00_functions.R'))

deso_sf <- st_read(here::here('data expo', 'DeSO_2018.gpkg')) %>% 
  st_transform(3006)

stations_sf <- st_as_sf(
  data.frame(
    id = c(8577, 8773, 8781, 71420, 52350, 98230),
    long = c(11.97024, 13.00196, 18.05781, 11.9924, 12.9843, 18.0549),
    lat = c(57.70902, 55.60639, 59.31601, 57.7156, 55.5715, 59.3417),
    type = c('o3', 'o3', 'o3', 'temp', 'temp', 'temp'),
    site = c('got', 'mal', 'sth', 'got', 'mal', 'sth')
  ),
  coords = c('long', 'lat'),
  crs = 4326,
  remove = F
) %>% 
  st_transform(3006)

deso_sf$dist_o3 <- apply(st_distance(st_centroid(deso_sf), stations_sf %>% filter(type == 'o3')), 1, min)
deso_sf$dist_temp <- apply(st_distance(st_centroid(deso_sf), stations_sf %>% filter(type == 'temp')), 1, min)

df_3 <- df_2 %>%
  ungroup %>% 
  filter(!is.na(o3_24h) & !is.na(temp_24h) & !is.na(pm10_24h) & !is.na(no2_24h),
         !is.na(sbp) & !is.na(dbp) & !is.na(hr),
         !is.na(sex) & !is.na(age) & !is.na(diab) & !is.na(whr) & !is.na(smo) & !is.na(alc) & !is.na(phy) & !is.na(ses)) %>%
  left_join(
    readRDS('/safe/data/Research projects/SCAPIS/Originaldata/SCAPIS_master_20230605.rds') %>% 
      select(id = scapis_id, date = anthropometrycollectiondate, contains('deso')) %>% 
      pivot_longer(cols = -c(date, id), names_to = 'year', values_to = 'deso', names_pattern = 'deso_code(.*)_as01') %>% 
      filter(year == str_sub(date, 1, 4)) %>% 
      select(id, deso), 
    by = 'id') %>% 
  left_join(
    deso_sf %>% select(deso = desokod, dist_o3, dist_temp) %>% st_drop_geometry(),
    by = 'deso'
  )

deso_sf <- deso_sf %>% 
  left_join(
    df_3 %>% group_by(deso) %>% count() %>% rename('n_SCAPIS' = 'n'),
    by = c('desokod' = 'deso')
  )

# 1 SBP ------------------------------------------------------------------------
fit_sbp_m3_lto_deso1o <-
  gam(
    formula = log(sbp) ~ o3_24h + temp_24h + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    subset = dist_o3 <= 1000
  )
#summary(fit_sbp_m3_lto_deso1o)
#appraise(fit_sbp_m3_lto_deso1o)
#draw(fit_sbp_m3_lto_deso1o)

fit_sbp_m3_lto_deso1t <-
  gam(
    formula = log(sbp) ~ o3_24h + temp_24h + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    subset = dist_temp <= 1000
  )
#summary(fit_sbp_m3_lto_deso1t)
#appraise(fit_sbp_m3_lto_deso1t)
#draw(fit_sbp_m3_lto_deso1t)

fit_sbp_m3_lto_deso5o <-
  gam(
    formula = log(sbp) ~ o3_24h + temp_24h + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    subset = dist_o3 <= 5000
  )
#summary(fit_sbp_m3_lto_deso5o)
#appraise(fit_sbp_m3_lto_deso5o)
#draw(fit_sbp_m3_lto_deso5o)

fit_sbp_m3_lto_deso5t <-
  gam(
    formula = log(sbp) ~ o3_24h + temp_24h + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    subset = dist_temp <= 5000
  )
#summary(fit_sbp_m3_lto_deso5t)
#appraise(fit_sbp_m3_lto_deso5t)
#draw(fit_sbp_m3_lto_deso5t)

fit_sbp_m3_lto_deso10o <-
  gam(
    formula = log(sbp) ~ o3_24h + temp_24h + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    subset = dist_o3 <= 10000
  )
#summary(fit_sbp_m3_lto_deso10o)
#appraise(fit_sbp_m3_lto_deso10o)
#draw(fit_sbp_m3_lto_deso10o)

fit_sbp_m3_lto_deso10t <-
  gam(
    formula = log(sbp) ~ o3_24h + temp_24h + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    subset = dist_temp <= 10000
  )
#summary(fit_sbp_m3_lto_deso10t)
#appraise(fit_sbp_m3_lto_deso10t)
#draw(fit_sbp_m3_lto_deso10t)

# 2 DBP ------------------------------------------------------------------------
fit_dbp_m3_lto_deso1o <-
  gam(
    formula = log(dbp) ~ o3_24h + temp_24h + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    subset = dist_o3 <= 1000
  )

#summary(fit_dbp_m3_lto_deso1o)
#appraise(fit_dbp_m3_lto_deso1o)
#draw(fit_dbp_m3_lto_deso1o)

fit_dbp_m3_lto_deso1t <-
  gam(
    formula = log(dbp) ~ o3_24h + temp_24h + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    subset = dist_temp <= 1000
  )
#summary(fit_dbp_m3_lto_deso1t)
#appraise(fit_dbp_m3_lto_deso1t)
#draw(fit_dbp_m3_lto_deso1t)

fit_dbp_m3_lto_deso5o <-
  gam(
    formula = log(dbp) ~ o3_24h + temp_24h + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    subset = dist_o3 <= 5000
  )
#summary(fit_dbp_m3_lto_deso5o)
#appraise(fit_dbp_m3_lto_deso5o)
#draw(fit_dbp_m3_lto_deso5o)

fit_dbp_m3_lto_deso5t <-
  gam(
    formula = log(dbp) ~ o3_24h + temp_24h + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    subset = dist_temp <= 5000
  )
#summary(fit_dbp_m3_lto_deso5t)
#appraise(fit_dbp_m3_lto_deso5t)
#draw(fit_dbp_m3_lto_deso5t)

fit_dbp_m3_lto_deso10o <-
  gam(
    formula = log(dbp) ~ o3_24h + temp_24h + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    subset = dist_o3 <= 10000
  )
#summary(fit_dbp_m3_lto_deso10o)
#appraise(fit_dbp_m3_lto_deso10o)
#draw(fit_dbp_m3_lto_deso10o)

fit_dbp_m3_lto_deso10t <-
  gam(
    formula = log(dbp) ~ o3_24h + temp_24h + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    subset = dist_temp <= 10000
  )
#summary(fit_dbp_m3_lto_deso10t)
#appraise(fit_dbp_m3_lto_deso10t)
#draw(fit_dbp_m3_lto_deso10t)

# 3 PP -------------------------------------------------------------------------
fit_pp_m3_lto_deso1o <-
  gam(
    formula = log(pp) ~ o3_24h + temp_24h + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    subset = dist_o3 <= 1000
  )
#summary(fit_pp_m3_lto_deso1o)
#appraise(fit_pp_m3_lto_deso1o)
#draw(fit_pp_m3_lto_deso1o)

fit_pp_m3_lto_deso1t <-
  gam(
    formula = log(pp) ~ o3_24h + temp_24h + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    subset = dist_temp <= 1000
  )
#summary(fit_pp_m3_lto_deso1t)
#appraise(fit_pp_m3_lto_deso1t)
#draw(fit_pp_m3_lto_deso1t)

fit_pp_m3_lto_deso5o <-
  gam(
    formula = log(pp) ~ o3_24h + temp_24h + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    subset = dist_o3 <= 5000
  )
#summary(fit_pp_m3_lto_deso5o)
#appraise(fit_pp_m3_lto_deso5o)
#draw(fit_pp_m3_lto_deso5o)

fit_pp_m3_lto_deso5t <-
  gam(
    formula = log(pp) ~ o3_24h + temp_24h + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    subset = dist_temp <= 5000
  )
#summary(fit_pp_m3_lto_deso5t)
#appraise(fit_pp_m3_lto_deso5t)
#draw(fit_pp_m3_lto_deso5t)

fit_pp_m3_lto_deso10o <-
  gam(
    formula = log(pp) ~ o3_24h + temp_24h + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    subset = dist_o3 <= 10000
  )
#summary(fit_pp_m3_lto_deso10o)
#appraise(fit_pp_m3_lto_deso10o)
#draw(fit_pp_m3_lto_deso10o)

fit_pp_m3_lto_deso10t <-
  gam(
    formula = log(pp) ~ o3_24h + temp_24h + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    subset = dist_temp <= 10000
  )
#summary(fit_pp_m3_lto_deso10t)
#appraise(fit_pp_m3_lto_deso10t)
#draw(fit_pp_m3_lto_deso10t)

# 4 HR -------------------------------------------------------------------------
fit_hr_m3_lto_deso1o <-
  gam(
    formula = log(hr) ~ o3_24h + temp_24h + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    subset = dist_o3 <= 1000
  )
#summary(fit_hr_m3_lto_deso1o)
#appraise(fit_hr_m3_lto_deso1o)
#draw(fit_hr_m3_lto_deso1o)

fit_hr_m3_lto_deso1t <-
  gam(
    formula = log(hr) ~ o3_24h + temp_24h + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    subset = dist_temp <= 1000
  )
#summary(fit_hr_m3_lto_deso1t)
#appraise(fit_hr_m3_lto_deso1t)
#draw(fit_hr_m3_lto_deso1t)

fit_hr_m3_lto_deso5o <-
  gam(
    formula = log(hr) ~ o3_24h + temp_24h + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    subset = dist_o3 <= 5000
  )
#summary(fit_hr_m3_lto_deso5o)
#appraise(fit_hr_m3_lto_deso5o)
#draw(fit_hr_m3_lto_deso5o)

fit_hr_m3_lto_deso5t <-
  gam(
    formula = log(hr) ~ o3_24h + temp_24h + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    subset = dist_temp <= 5000
  )
#summary(fit_hr_m3_lto_deso5t)
#appraise(fit_hr_m3_lto_deso5t)
#draw(fit_hr_m3_lto_deso5t)

fit_hr_m3_lto_deso10o <-
  gam(
    formula = log(hr) ~ o3_24h + temp_24h + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    subset = dist_o3 <= 10000
  )
#summary(fit_hr_m3_lto_deso10o)
#appraise(fit_hr_m3_lto_deso10o)
#draw(fit_hr_m3_lto_deso10o)

fit_hr_m3_lto_deso10t <-
  gam(
    formula = log(hr) ~ o3_24h + temp_24h + 
      s(pm10_24h, bs = 'cs', k = 5) + s(no2_24h, bs = 'cs', k = 5) + 
      sex + age + diab + whr + smo + alc + phy + ses + 
      year + s(yday, bs = 'cc', k = 7) + s(wday, bs = 're') + s(site, bs = 're'),
    data = df_3,
    subset = dist_temp <= 10000
  )
#summary(fit_hr_m3_lto_deso10t)
#appraise(fit_hr_m3_lto_deso10t)
#draw(fit_hr_m3_lto_deso10t)

tableS3 <- 
  bind_rows(
    tableS1_helper('sbp', 'o3_24h', fit_sbp_m3_lto_deso1o) %>% mutate(dist = '1km'),
    tableS1_helper('sbp', 'o3_24h', fit_sbp_m3_lto_deso5o) %>% mutate(dist = '5km'),
    tableS1_helper('sbp', 'o3_24h', fit_sbp_m3_lto_deso10o) %>% mutate(dist = '10km'),
    tableS1_helper('sbp', 'temp_24h', fit_sbp_m3_lto_deso1t) %>% mutate(dist = '1km'),
    tableS1_helper('sbp', 'temp_24h', fit_sbp_m3_lto_deso5t) %>% mutate(dist = '5km'),
    tableS1_helper('sbp', 'temp_24h', fit_sbp_m3_lto_deso10t) %>% mutate(dist = '10km'),
    tableS1_helper('dbp', 'o3_24h', fit_dbp_m3_lto_deso1o) %>% mutate(dist = '1km'),
    tableS1_helper('dbp', 'o3_24h', fit_dbp_m3_lto_deso5o) %>% mutate(dist = '5km'),
    tableS1_helper('dbp', 'o3_24h', fit_dbp_m3_lto_deso10o) %>% mutate(dist = '10km'),
    tableS1_helper('dbp', 'temp_24h', fit_dbp_m3_lto_deso1t) %>% mutate(dist = '1km'),
    tableS1_helper('dbp', 'temp_24h', fit_dbp_m3_lto_deso5t) %>% mutate(dist = '5km'),
    tableS1_helper('dbp', 'temp_24h', fit_dbp_m3_lto_deso10t) %>% mutate(dist = '10km'),
    tableS1_helper('pp', 'o3_24h', fit_pp_m3_lto_deso1o) %>% mutate(dist = '1km'),
    tableS1_helper('pp', 'o3_24h', fit_pp_m3_lto_deso5o) %>% mutate(dist = '5km'),
    tableS1_helper('pp', 'o3_24h', fit_pp_m3_lto_deso10o) %>% mutate(dist = '10km'),
    tableS1_helper('pp', 'temp_24h', fit_pp_m3_lto_deso1t) %>% mutate(dist = '1km'),
    tableS1_helper('pp', 'temp_24h', fit_pp_m3_lto_deso5t) %>% mutate(dist = '5km'),
    tableS1_helper('pp', 'temp_24h', fit_pp_m3_lto_deso10t) %>% mutate(dist = '10km'),
    tableS1_helper('hr', 'o3_24h', fit_hr_m3_lto_deso1o) %>% mutate(dist = '1km'),
    tableS1_helper('hr', 'o3_24h', fit_hr_m3_lto_deso5o) %>% mutate(dist = '5km'),
    tableS1_helper('hr', 'o3_24h', fit_hr_m3_lto_deso10o) %>% mutate(dist = '10km'),
    tableS1_helper('hr', 'temp_24h', fit_hr_m3_lto_deso1t) %>% mutate(dist = '1km'),
    tableS1_helper('hr', 'temp_24h', fit_hr_m3_lto_deso5t) %>% mutate(dist = '5km'),
    tableS1_helper('hr', 'temp_24h', fit_hr_m3_lto_deso10t) %>% mutate(dist = '10km')
  ) %>% 
  pivot_wider(id_cols = c('outcome', 'term'), names_from = 'dist', values_from = 'f')

writexl::write_xlsx(tableS3, here::here(paste0(Sys.Date(), '_tableS3.xlsx')))

cowplot::plot_grid(
  ggplot(deso_sf %>% filter(!is.na(n_SCAPIS) & n_SCAPIS >= 5)) + 
    geom_sf(aes(fill = n_SCAPIS), colour = 'black') + 
    scale_fill_gradient(low = 'white', high = 'grey50') + 
    geom_sf(data = stations_sf %>% filter(site == 'got'), aes(colour = type), size = 2) + 
    geom_sf(data = ne_countries(scale = 10, country = 'sweden', return = 'sf'), alpha = 0, colour = 'black', linewidth = 2) + 
    geom_sf(data = st_buffer(stations_sf %>% filter(site == 'got'), dist = 1000), aes(colour = type), fill = NA, linewidth = 1) + 
    geom_sf(data = st_buffer(stations_sf %>% filter(site == 'got'), dist = 5000), aes(colour = type), fill = NA, linewidth = 1) + 
    geom_sf(data = st_buffer(stations_sf %>% filter(site == 'got'), dist = 10000), aes(colour = type), fill = NA, linewidth = 1) + 
    coord_sf(xlim = c(11.6, 12.3), ylim = c(57.5, 57.85), crs = 4326, expand = F) + 
    theme_minimal() +
    theme(legend.position = 'none',
          panel.border = element_rect(colour = 'black', fill = NA)),
  ggplot(deso_sf %>% filter(!is.na(n_SCAPIS) & n_SCAPIS >= 5)) + 
    geom_sf(aes(fill = n_SCAPIS), colour = 'black') + 
    scale_fill_gradient(low = 'white', high = 'grey50') + 
    geom_sf(data = stations_sf %>% filter(site == 'mal'), aes(colour = type), size = 2) + 
    geom_sf(data = ne_countries(scale = 10, country = 'sweden', return = 'sf'), alpha = 0, colour = 'black', linewidth = 2) + 
    geom_sf(data = st_buffer(stations_sf %>% filter(site == 'mal'), dist = 1000), aes(colour = type), fill = NA, linewidth = 1) + 
    geom_sf(data = st_buffer(stations_sf %>% filter(site == 'mal'), dist = 5000), aes(colour = type), fill = NA, linewidth = 1) + 
    geom_sf(data = st_buffer(stations_sf %>% filter(site == 'mal'), dist = 10000), aes(colour = type), fill = NA, linewidth = 1) + 
    coord_sf(xlim = c(12.8, 13.2), ylim = c(55.48, 55.7), crs = 4326, expand = F) + 
    theme_minimal() +
    theme(legend.position = 'none',
          panel.border = element_rect(colour = 'black', fill = NA)),
  ggplot(deso_sf %>% filter(!is.na(n_SCAPIS) & n_SCAPIS >= 5)) + 
    geom_sf(aes(fill = n_SCAPIS), colour = 'black') + 
    scale_fill_gradient(low = 'white', high = 'grey50') + 
    geom_sf(data = stations_sf %>% filter(site == 'sth'), aes(colour = type), size = 2) + 
    geom_sf(data = ne_countries(scale = 10, country = 'sweden', return = 'sf'), alpha = 0, colour = 'black', linewidth = 2) + 
    geom_sf(data = st_buffer(stations_sf %>% filter(site == 'sth'), dist = 1000), aes(colour = type), fill = NA, linewidth = 1) + 
    geom_sf(data = st_buffer(stations_sf %>% filter(site == 'sth'), dist = 5000), aes(colour = type), fill = NA, linewidth = 1) + 
    geom_sf(data = st_buffer(stations_sf %>% filter(site == 'sth'), dist = 10000), aes(colour = type), fill = NA, linewidth = 1) + 
    coord_sf(xlim = c(17.4, 18.8), ylim = c(58.9, 59.6), crs = 4326, expand = F) + 
    theme_minimal() +
    theme(legend.position = 'none',
          panel.border = element_rect(colour = 'black', fill = NA)),
  labels = c('Gothenburg', 'Malm\u00f6', 'Stockholm'),
  rel_widths = c(.3,.3,.3),
  nrow = 1
)
ggsave(here::here('figures', paste0(Sys.Date(), '_map.png')), width = 10, height = 5)
