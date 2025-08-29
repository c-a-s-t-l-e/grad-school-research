# Packages
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)

# Function to decode a single VIN
decode_vin_single <- function(vin, year, base_url = "https://vpic.nhtsa.dot.gov/api//vehicles/DecodeVin/") {
  message("Decoding VIN: ", vin, " (Year: ", year, ") ...")
  
  url <- paste0(base_url, vin, "?format=json&modelyear=", year)
  response <- GET(url)
  
  if (status_code(response) != 200) {
    warning("API request failed for VIN: ", vin, " with year: ", year)
    return(NULL)
  }
  
  # Parse JSON
  res_text <- content(response, as = "text", encoding = "UTF-8")
  res_json <- fromJSON(res_text, flatten = TRUE)
  
  if (!is.null(res_json$Results)) {
    df_long <- as.data.frame(res_json$Results, stringsAsFactors = FALSE)
    df_long$VIN <- vin
    df_long$Year <- year
    message("Successfully decoded VIN: ", vin)
    return(df_long)
  } else {
    warning("No results returned for VIN: ", vin)
    return(data.frame(VIN = vin, Year = year))
  }
}

# Function to decode multiple VINs and return wide format
decode_vin_dataframe <- function(vin_vector, year_vector) {
  if (length(vin_vector) != length(year_vector)) {
    stop("VIN vector and Year vector must have the same length")
  }
  
  all_results <- mapply(decode_vin_single, vin_vector, year_vector, SIMPLIFY = FALSE)
  
  # Combine long-format results
  df_long <- bind_rows(all_results)
  
  # Flatten to wide format: one row per VIN
  df_wide <- df_long %>%
    select(VIN, Year, Variable, Value) %>%
    pivot_wider(names_from = Variable, values_from = Value)
  
  message("All VINs processed and flattened!")
  return(df_wide)
}

# Example
decoded_df <- decode_vin_dataframe(df$VIN, df$`Model Year`)