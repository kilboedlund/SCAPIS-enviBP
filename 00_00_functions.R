rel_smooth <- function(model, term) {
  sm <- gratia::get_smooth(model, paste0('s(', term, ')'))
  var <- sm$term
  
  x <- model.frame(model)[[var]]
  values <- c(gratia::evenly(df_3[[term]], lower = quantile(df_3[[term]], .05), upper = quantile(df_3[[term]], .95)), 0)
  
  newdata <- rlang::exec(gratia::data_slice, object = model, 
                         !!!rlang::set_names(list(values), var))
  
  i <- gratia::smooth_coef_indices(sm)
  X <- gratia::lp_matrix(model, data = newdata)[, i, drop = F]
  
  ref <- which.min(abs(newdata[[var]]))
  L <- sweep(X, 2, X[ref, ], FUN = '-')
  
  b <- coef(model)[i]
  V <- vcov(model, unconditional = T)[i, i, drop = F]
  
  data.frame(term = term, 
             value = newdata[[var]],
             beta = drop(L %*% b),
             se = sqrt(rowSums((L %*% V) * L))) %>% 
    mutate(est = exp(beta) - 1,
           lcl = exp(beta - 1.96 * se) - 1,
           ucl = exp(beta + 1.96 * se) - 1) %>% 
    arrange(value) %>% 
    return()
}

table3_helper <- function(outc, expo, base_model, spline_model,
                          inter_model, tertile_model_same, tertile_model_other) {
  bind_rows(
  base_model %>% 
    tidy(conf.int = T, parametric = T) %>% 
    filter(str_detect(term, expo)) %>% 
    mutate(outcome = outc,
           p_lin = tidy(spline_model, parametric = F) %>% filter(str_detect(term, expo)) %>% pull(p.value),
           p_int = tidy(inter_model, parametric = T) %>% filter(str_detect(term, ':')) %>% pull(p.value),
           across(c(estimate, conf.low, conf.high), \(x) exp(10 * x / sd(df_1[[expo]], na.rm = T)) - 1)),
  tertile_model_same %>% 
  tidy(conf.int = T, parametric = T) %>%  
    filter(str_detect(term, expo)) %>% 
    mutate(outcome = outc,
           across(c(estimate, conf.low, conf.high), \(x) exp(10 * x / sd(df_1[[expo]], na.rm = T)) - 1)),
  tertile_model_other %>% 
  tidy(conf.int = T, parametric = T) %>%  
    filter(str_detect(term, expo)) %>% 
    mutate(outcome = outc,
           across(c(estimate, conf.low, conf.high), \(x) exp(10 * x / sd(df_1[[expo]], na.rm = T)) - 1))
  )
}

table3_format <- function(x) {
  x <- x %>% 
    mutate(f = paste0(sprintf('%.2f', estimate * 100), ' (',
                      sprintf('%.2f', conf.low * 100), ', ',
                      sprintf('%.2f', conf.high * 100), ')')) %>% 
    select(p_lin, p_int, f, outcome)
  
  matrix(nrow = 3, ncol = 4,
         c(paste0(x$outcome[[1]], '_main'),
           paste0(x$outcome[[1]], '_same'),
           paste0(x$outcome[[1]], '_other'),
           x$f[[1]], 
           x$f[[2]],
           x$f[[5]],
           sprintf('%.3f', x$p_lin[[1]]),
           x$f[[3]],
           x$f[[6]],
           sprintf('%.3f', x$p_int[[1]]),
           x$f[[4]],
           x$f[[7]]))
}

tableS1_helper <- function(outc, expo, base_model) {
  base_model %>% 
    tidy(conf.int = T, parametric = T) %>% 
    filter(str_detect(term, expo)) %>% 
    mutate(outcome = outc,
           across(c(estimate, conf.low, conf.high), \(x) exp(10 * x / sd(df_1[[expo]], na.rm = T)) - 1)) %>% 
    mutate(f = paste0(sprintf('%.2f', estimate * 100), ' (',
                      sprintf('%.2f', conf.low * 100), ', ',
                      sprintf('%.2f', conf.high * 100), ')')) %>% 
    select(term, outcome, f)
}

tableS3_helper <- function(outc, expo, base_model) {
  base_model %>% 
    tidy(conf.int = T, parametric = T) %>% 
    filter(str_detect(term, expo)) %>% 
    mutate(outcome = outc,
           across(c(estimate, conf.low, conf.high), \(x) exp(10 * x / sd(df_1[[expo]], na.rm = T)) - 1)) %>% 
    mutate(f = paste0(sprintf('%.2f', estimate * 100), ' (',
                      sprintf('%.2f', conf.low * 100), ', ',
                      sprintf('%.2f', conf.high * 100), ')')) %>% 
    select(term, outcome, expo, f)
}


sd_table <- df_1 %>% 
  summarise(across(c(o3_24h, temp_24h, o3, temp_ap, o3_168h, temp_168h), \(x) sd(x, na.rm = T))) %>% 
  pivot_longer(cols = everything(), names_to = 'term', values_to = 'sd')

season_helper <- function(fit, outcome) {
  tidy(fit, parametric = T, conf.int = T) %>% 
    filter(str_detect(term, 'o3|temp')) %>%
    mutate(season = str_extract(term, 'C|W'),
           outcome = outcome,
           term = str_remove_all(term, 'season|C|W|:')) %>% 
    left_join(sd_table, by = 'term') %>% 
    mutate(across(c(estimate, conf.low, conf.high),
                  \(x) exp(10 * x / sd) - 1),
           f = paste0(sprintf('%.2f', estimate * 100), ' (',
                      sprintf('%.2f', conf.low * 100), ', ',
                      sprintf('%.2f', conf.high * 100), ')')) %>% 
    select(outcome, term, season, f)
}

effect_modific_helper <- function(fit, fit_trend, outcome) {
  tidy(fit, conf.int = T, parametric = T) %>% 
    filter(str_detect(term, 'o3|temp')) %>%
    separate('term', into = c('term', 'effect'), sep = ':') %>% 
    mutate(termx = term,
           term = if_else(str_detect(term, 'o3|temp'), term, effect),
           effect = if_else(str_detect(effect, 'o3|temp'), termx, effect)) %>% 
    select(-termx) %>% 
    left_join(sd_table, by = 'term') %>% 
    mutate(outcome = outcome,
           across(c(estimate, conf.low, conf.high),
                  \(x) exp(10 * x / sd) - 1),
           formatted = paste0(sprintf('%.2f', estimate * 100), ' (',
                              sprintf('%.2f', conf.low * 100), ', ',
                              sprintf('%.2f', conf.high * 100), ')')) %>% 
    pivot_wider(id_cols = c('term', 'outcome'), names_from = 'effect', values_from = 'formatted') %>% 
    mutate(p_int = anova(fit_trend)$pTerms.table %>% 
             as.data.frame %>% 
             rownames_to_column('term') %>% 
             filter(str_detect(term, ':')) %>% 
             pull(`p-value`),
           p_int = sprintf('%.3f', p_int))
}

plot_gam_helper <- function(fit, zrange, ztext) {
  n <- 100
  
  pred_grid <- crossing(o3_24h = gratia::evenly(df_1$o3_24h, n),
                        temp_24h = gratia::evenly(df_1$temp_24h, n),
                        slice(df_3, 1) %>% select(-c(o3_24h, temp_24h)))
  
  pred_grid$log.sbp <- predict(
    object = fit,
    newdata = pred_grid,
    type = 'response'
  )
  pred_grid$sbp_pred <- exp(pred_grid$log.sbp)
  
  plot_data <- pred_grid %>% 
    select(o3_24h, temp_24h, sbp_pred)
  
  kde <- MASS::kde2d(
    x = df_3$temp_24h,
    y = df_3$o3_24h,
    n = n,
    lims = c(min(df_1$temp_24h, na.rm = T), max(df_1$temp_24h, na.rm = T), 
             0, max(df_1$o3_24h, na.rm = T))
  )
  
  plot_data$sbp_masked <- ifelse(
    as.vector(kde$z) > 0.0001047902,
    plot_data$sbp_pred,
    NA_real_
  )
  
  z_matrix <- matrix(
    plot_data$sbp_masked,
    nrow = n,
    ncol = n,
    byrow = T
  )
  
  return(plot_ly(
    x = gratia::evenly(df_1$temp_24h, n),
    y = -gratia::evenly(df_1$o3_24h, n),
    z = z_matrix,
    type = 'surface',
    connectgaps = F,
    colorscale = 'inferno',
    cmin = zrange[1],
    cmax = zrange[2],
    showscale = F,
    contours = list(
      y = list(
        show = T,
        color = 'black'
      ),
      x = list(
        show = T,
        color = 'black'
      )
    )
  ) %>% 
    layout(scene = 
             list(
               xaxis = list(
                 title = list(
                   text = '<b>Temperature, 24h mean [\u00b0C]</b>',
                   font = list(size = 24)
                 ),
                 tickvals = -2:4 * 5,
                 tickmode = 'array',
                 tickfont = list(size = 14),
                 range = c(-10, 20),
                 gridcolor = 'grey90'
               ),
               yaxis = list(
                 title = list(
                   text = '<b>O3, 24h mean [\u00b5g/m\u00b3]</b>',
                   font = list(size = 24)
                 ),
                 range = c(-100, 0),
                 tickvals = -5:-1 * 20,
                 ticktext = 5:1 * 20,
                 tickmode = 'array',
                 tickfont = list(size = 14),
                 gridcolor = 'grey90'
               ),
               zaxis = list(
                 title = list(
                   text = paste0('<b>', ztext, '</b>'),
                   font = list(size = 24)
                 ),
                 range = zrange,
                 tickvals = 40:150,
                 tickmode = 'array',
                 tickfont = list(size = 14),
                 gridcolor = 'grid90'
               ),
               camera = list(
                 eye = list(
                   x = -1.0,
                   y = -1.5,
                   z = 1.2),
                 center = list(
                   x = 0,
                   y = 0,
                   z = -.2
                 )
               ),
               aspectmode = 'manual',
               aspectratio = 
                 list(
                   x = 1.0,
                   y = 1.0,
                   z = 0.8)
             )
    )
  )
}
