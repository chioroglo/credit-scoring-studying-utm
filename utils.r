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

replace_outliers_with_mean_numeric = function(numeric_array) {
  
  numeric_array[is.na(numeric_array)] <- 0
  numeric_array <- sapply(numeric_array,as.numeric)

  summary <- summary(numeric_array)
  q1 <- summary[2]
  q3 <- summary[5]
  interquartile_range <- abs(q1 - q3)
  median_numeric_array <- median(numeric_array)
  print(interquartile_range)
  print(summary(numeric_array))

  numeric_array[numeric_array < (q1 - 1.5 * interquartile_range)] <- median_numeric_array
  numeric_array[numeric_array > (q3 + 1.5 * interquartile_range)] <- median_numeric_array
  
  return(numeric_array)
}

initialize_environment = function() {
  library(nnet)
  library(tibble)
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(DescTools)
  library(scales)
  library(plotly)
  library(networkD3)
  library(highcharter)
  setwd("../CreditScoringStudying")
}