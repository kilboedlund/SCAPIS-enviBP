## Import functions ----------------------------------------------------------------------------------------------------

temp_data <- function(id, skip = 0, dew = '1') {
  require('readr')
  return(
    readr::read_delim(
      here::here('data temp', paste0('smhi-opendata_', dew, '_', id, '_201301_201812.csv')),
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

iqr <- function(x) {
  if (anyNA(x)) return(NA_real_)
  return(unname(diff(quantile(x, c(.25, .75)))))
}

cv <- function(x, na.rm = F) {
  return(sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm))
}