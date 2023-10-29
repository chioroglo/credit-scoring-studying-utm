parse_decimal_or_na <- function(string) {
  numeric_value <- as.numeric(gsub("[^0-9.]", "", string))
  if (is.na(numeric_value)) {
    return("NA")
  }
  return(numeric_value)
}

replace_outliers_with_mode <- function(data, group_column, column_name, quantile_value) {
  q_value <- quantile(data[[column_name]], quantile_value, na.rm = TRUE)
  
  data <- data %>%
    group_by({{ group_column }}) %>%
    mutate({{ column_name }} := ifelse({{ column_name }} > q_value, Mode({{ column_name }}), {{ column_name }})) %>%
    ungroup()
  
  return(data)
}