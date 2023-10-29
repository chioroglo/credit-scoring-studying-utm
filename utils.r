parse_decimal_or_na <- function(string) {
  numeric_value <- as.numeric(gsub("[^0-9.]", "", string))
  if (is.na(numeric_value)) {
    return("NA")
  }
  return(numeric_value)
}