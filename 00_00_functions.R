temp_data <- function(id, skip) {
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

air_data <- function(id, pollutant, skip) {
  require('readr')
  return(
    readr::read_delim(
      here::here('data air', paste0(id, '_', pollutant, '.csv')),
      skip = skip,
      delim = ';',
      locale = locale(date_names = 'sv', decimal_mark = '.'))
  )
}

