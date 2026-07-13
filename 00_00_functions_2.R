#lm_SVBP <- function(df, outc, t = 24, int = F, spline_df, model, stratum = '.', filter = 'T') {
#  df <- df %>% filter(!!rlang::parse_expr(filter))
#  x1 <- if_else(spline_df >1, 'ns(', '')
#  x2 <- if_else(spline_df >1, str_c(', ', spline_df, ')'), '')
#  x_int <- if_else(int, ' * ', ' + ')
#  if (stratum != 'site') {
#    formula <- as.formula(
#      str_c(outc, ' ~ ', x1, 'o3_mean', t, 'h', x2, x_int, 'ns(temp_mean', t, 'h)', ' + ', str_c(model, collapse = ' + '), ' + (1|site)')
#    )
#    print(formula)
#    fit <- lme4::lmer(formula, 
#                      df %>% 
#                        select(outc, site, str_c(c(expo1, expo2), '_', m, t, 'h'), all_of(model)))
#  }
#  else {
#    formula <- as.formula(
#      str_c(outc, ' ~ ', x1, 'o3_mean', t, 'h', x2, x_int, 'ns(temp_mean', t, 'h)', ' + ', str_c(model, collapse = ' + '))
#    )
#    print(formula)
#    fit <- lm(formula, 
#              df %>% 
#                select(outc, site, str_c(c(expo1, expo2), '_', m, t, 'h'), all_of(model)))
#  }
#  
#  return(fit)
#}

boot_fun <- function(fit) {
  predict(fit,
          newdata = newdat,
          re.form = NA) -
    as.numeric(
      predict(fit,
              newdata = refdat,
              re.form = NA)
      )
}
