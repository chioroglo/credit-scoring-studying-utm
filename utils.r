parse_decimal_or_na <- function(string) {
  numeric_value <- as.numeric(gsub("[^0-9.]", "", string))
  if (is.na(numeric_value)) {
    return(NA)
  }
  return(numeric_value)
}

parse_credit_history_age_column_to_amounth_of_months <- function(string) {

  pattern_years_months <- "(\\d+) years and (\\d+) months"

  years <- str_extract(string,pattern_years_months) %>%
    str_replace(pattern_years_months,"\\1")
  months <- str_extract(string,pattern_years_months) %>%
    str_replace(pattern_years_months,"\\2")
  return(as.numeric(years) * 12 + as.numeric(months))
}

is_convertible_to_numeric = function(string) {
  return (!is.na(as.numeric(string)))
}