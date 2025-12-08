## Import functions ----------------------------------------------------------------------------------------------------

temp_data <- function(id, skip = 0) {
  require('readr')
  return(
    readr::read_delim(
      here::here('data temp', paste0('smhi-opendata_1_', id, '_201301_201812.csv')),
      skip = skip,
      delim = ';',
      col_select = 1:4,
      col_types = 'Dtdc',
      locale = locale(date_names = 'sv', decimal_mark = '.'))
  )
}

air_data <- function(id, pollutant, skip = 0) {
  require('readr')
  return(
    readr::read_delim(
      here::here('data air', paste0(id, '_', pollutant, '.csv')),
      skip = skip,
      delim = ';',
      locale = locale(date_names = 'sv', decimal_mark = '.'))
  )
}

humid_data <- function(id, skip = 0) {
  require('readr')
  return(
    readr::read_delim(
      here::here('data humid', paste0('smhi-opendata_6_', id, '_201301_201812.csv')),
      skip = skip,
      delim = ';',
      col_select = 1:4,
      col_types = 'Dtdc',
      locale = locale(date_names = 'sv', decimal_mark = '.'))
  )
}


wind_data <- function(id, skip = 0) {
  require('readr')
  return(
    readr::read_delim(
      here::here('data wind', paste0('smhi-opendata_4_', id, '_201301_201812.csv')),
      skip = skip,
      delim = ';',
      col_select = 1:6,
      col_types = 'Dtdcdc',
      locale = locale(date_names = 'sv', decimal_mark = '.'))
  )
}

wind_data(71420, 14)

# Transformers ---------------------------------------------------------------------------------------------------------
#normal distribution to lognormal distribution (for data simulation)
to_lognorm_rank_preserve <- function(x) {
  m <- mean(x)
  s <- sd(x)

  sigma_log <- sqrt(log(1 + (s^2 / m^2)))
  mu_log    <- log(m) - sigma_log^2 / 2

  # generate lognormal values
  x_ln <- rlnorm(length(x), meanlog = mu_log, sdlog = sigma_log)

  # reorder so that ranks match original
  return(
    x_ln[order(order(x))]
  )
}