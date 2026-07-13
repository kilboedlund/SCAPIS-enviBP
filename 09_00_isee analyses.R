library('splines')

q05 <- function(x, na.rm = F) {
  quantile(x, .05, na.rm = na.rm) %>% 
    unname
}
q95 <- function(x, na.rm = F) {
  quantile(x, .95, na.rm = na.rm) %>% 
    unname
}


df_2 <- df_1 %>% 
  left_join(
    df_expo_2 %>% select(-hour),
    by = c('date', 'site')
  ) %>% 
  left_join(df_ndvi, by = 'id') %>% 
  mutate(across(c('ndvi', contains('mean'), 'whr'),
                ~(.x - mean(.x, na.rm = T)) / IQR(.x, na.rm = T)),
         across(c('year', 'age'),
                ~ .x - min(.x, na.rm = T)),
         across('year',
                as.factor))

with(df_2 %>% select(o3_mean24h, temp_mean24h), cor(o3_mean24h, temp_mean24h, use = 'complete.obs'))

lm(sbp ~ temp_mean24h + o3_mean24h + site + year + age + sex + smo + whr + diab + ses, 
   data = df_2) %>% summary

lm(sbp ~ temp_mean24h + o3_mean24h + temp_mean24h:sex + o3_mean24h:sex + site + year + age + sex + smo + whr + diab + ses, 
   data = df_2) %>% summary

lm(sbp ~ temp_mean24h + o3_mean24h + temp_mean24h:ses + o3_mean24h:ses + site + year + age + sex + smo + whr + diab + ses, 
   data = df_2) %>% summary

lm(pp ~ temp_mean24h + o3_mean24h + site + year + age + sex + smo + whr + diab + ses, 
   data = df_2) %>% summary

lm(pp ~ temp_mean24h:sex + o3_mean24h:sex + site + year + age + sex + smo + whr + diab + ses, 
   data = df_2) %>% summary

lm(pulse ~ temp_mean24h + o3_mean24h + site + year + age + sex + smo + whr + diab + ses, 
   data = df_2) %>% summary

lm(pulse ~ temp_mean24h + o3_mean24h + temp_mean24h:sex + o3_mean24h:sex + site + year + age + sex + smo + whr + diab + ses, 
   data = df_2) %>% summary

lm(pulse ~ temp_mean24h + o3_mean24h + temp_mean24h:ses + o3_mean24h:ses + site + year + age + sex + smo + whr + diab + ses, 
   data = df_2) %>% summary

lm(pulse ~ temp_mean24h*ndvi + o3_mean24h*ndvi + site + year + age + sex + smo + whr + diab + ses, 
   data = df_2) %>% summary

pred <- bind_cols(
  expand.grid(
    temp_mean24h = seq(q05(df_2$temp_mean24h, na.rm = T), q95(df_2$temp_mean24h, na.rm = T), length.out = 10), 
    o3_mean24h = seq(q05(df_2$o3_mean24h, na.rm = T), q95(df_2$o3_mean24h, na.rm = T), length.out = 10)),
  predict(
    lm(sbp ~ ns(temp_mean24h, 3) + ns(o3_mean24h, 3) + site + year + age + sex + smo + whr + diab + ses, 
       data = df_2),
    expand.grid(
      temp_mean24h = seq(q05(df_2$temp_mean24h, na.rm = T), q95(df_2$temp_mean24h, na.rm = T), length.out = 10), 
      o3_mean24h = seq(q05(df_2$o3_mean24h, na.rm = T), q95(df_2$o3_mean24h, na.rm = T), length.out = 10), 
      site = 'got',
      year = mean(df_2$year, na.rm = T), 
      age = mean(df_2$age, na.rm = T), 
      sex = 'FEMALE', 
      smo = 'NEVER',
      whr = 0,
      diab = 'NORMOGLYCEMIA',
      ses = 0
    ),
    se.fit = T
  ) %>% 
    as.data.frame()
)

ggplot(pred,
       aes(x = o3_mean24h*sd(df_expo_2$o3_mean24h, na.rm = T) + mean(df_expo_2$o3_mean24h, na.rm = T), 
                y = fit, 
                colour = temp_mean24h, 
                group = temp_mean24h)) +
  geom_line()

ggplot(pred,
       aes(x = temp_mean24h*sd(df_expo_2$temp_mean24h, na.rm = T) + mean(df_expo_2$temp_mean24h, na.rm = T), 
           y = fit, 
           colour = o3_mean24h, 
           group = o3_mean24h)) +
  geom_line()


