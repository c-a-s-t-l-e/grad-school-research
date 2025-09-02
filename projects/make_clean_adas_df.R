library(tidyverse)
library(janitor)
library(lubridate)

# load the data
adas_data <- readr::read_csv("./ADAS_level_2.csv")

# standardize the names
names(adas_data) <- janitor::make_clean_names(names(adas_data))

# make a collision_category column from the contact area columns
adas_collision_df <- adas_data %>% 
  mutate(
    collision_category = case_when(
      
      # --- Rear-end collisions ---
      (sv_contact_area_rear == "Y" | sv_contact_area_rear_left == "Y" | sv_contact_area_rear_right == "Y") &
        (cp_contact_area_front == "Y" | cp_contact_area_front_left == "Y" | cp_contact_area_front_right == "Y") ~ "SV rear-ended by CP",
      
      (cp_contact_area_rear == "Y" | cp_contact_area_rear_left == "Y" | cp_contact_area_rear_right == "Y") &
        (sv_contact_area_front == "Y" | sv_contact_area_front_left == "Y" | sv_contact_area_front_right == "Y") ~ "CP rear-ended by SV",
      
      # --- Head-on collision ---
      (sv_contact_area_front == "Y" | sv_contact_area_front_left == "Y" | sv_contact_area_front_right == "Y") &
        (cp_contact_area_front == "Y" | cp_contact_area_front_left == "Y" | cp_contact_area_front_right == "Y") ~ "Head-on collision",
      
      # --- T-bone / Side impacts ---
      # SV struck on left side by CP
      (sv_contact_area_left == "Y" | sv_contact_area_front_left == "Y" | sv_contact_area_rear_left == "Y") &
        (cp_contact_area_right == "Y" | cp_contact_area_front_right == "Y" | cp_contact_area_rear_right == "Y") ~ "Side-impact SV left",
      
      # SV struck on right side by CP
      (sv_contact_area_right == "Y" | sv_contact_area_front_right == "Y" | sv_contact_area_rear_right == "Y") &
        (cp_contact_area_left == "Y" | cp_contact_area_front_left == "Y" | cp_contact_area_rear_left == "Y") ~ "Side-impact SV right",
      
      # CP struck on left side by SV
      (cp_contact_area_left == "Y" | cp_contact_area_front_left == "Y" | cp_contact_area_rear_left == "Y") &
        (sv_contact_area_right == "Y" | sv_contact_area_front_right == "Y" | sv_contact_area_rear_right == "Y") ~ "Side-impact CP left",
      
      # CP struck on right side by SV
      (cp_contact_area_right == "Y" | cp_contact_area_front_right == "Y" | cp_contact_area_rear_right == "Y") &
        (sv_contact_area_left == "Y" | sv_contact_area_front_left == "Y" | sv_contact_area_rear_left == "Y") ~ "Side-impact CP right",
      
      # --- Top / rollover / unusual ---
      (sv_contact_area_top == "Y" | sv_contact_area_bottom == "Y" | 
         cp_contact_area_top == "Y" | cp_contact_area_bottom == "Y") ~ "Top / rollover / unusual impact",
      
      # --- Catch-all ---
      TRUE ~ "Other / unknown"
    )
  )

# make a weather column from the other prior weather columns using pivot_longer
adas_collision_df <- adas_collision_df %>%
  mutate(row_id = row_number()) %>%             
  pivot_longer(
    cols = starts_with("weather_"),             
    names_to = "weather",
    values_to = "occurred"
  ) %>%
  filter(occurred == "Y") %>%
  mutate(weather = str_remove(weather, "^weather_")) %>%  # remove "weather_" prefix
  group_by(row_id) %>%
  summarise(weather = paste(weather, collapse = ", "), .groups = "drop") %>%
  right_join(adas_collision_df %>% mutate(row_id = row_number()), by = "row_id") %>%
  #mutate(weather = ifelse(is.na(weather), "Clear", weather)) %>%  
  select(-row_id)

# make mileage category column from Mileage column
adas_collision_df$mileage_category <- cut(
  adas_collision_df$mileage,
  breaks = c(-Inf, 30000, 60000, 100000, 150000, Inf),
  labels = c("Low", "Low Medium", "High Medium", "High", "Very High")
)

# remove missing value rows that are in mileage
adas_collision_df <- adas_collision_df %>% filter(!is.na(mileage))

# make the time columns
adas_collision_df <- adas_collision_df %>% 
  mutate(date = lubridate::my(incident_date),
         month = lubridate::month(date), # make Month column
         year = lubridate::year(date), # make Year column
         hour = lubridate::hour(round_date(as.POSIXct(adas_collision_df$incident_time_24_00, format="%H:%M:%S"), unit = "hour"))# make Hour column
  ) %>% 
  relocate(year, month, hour, .before = city) %>% 
  select(-c(incident_date, incident_time_24_00, date))

adas_clean <- adas_collision_df %>% 
  filter(report_version == 1) %>% 
  select(
    year,
    month,
    hour,
    city,
    state,
    make,
    model,
    model_year,
    vin,
    mileage_category,
    weather,
    roadway_type,
    roadway_surface,
    lighting,
    sv_pre_crash_movement,
    cp_pre_crash_movement,
    collision_category,
    highest_injury_severity_alleged
  )

adas_clean$city <- replace(adas_clean$city, is.na(adas_clean$city), "Unknown")
adas_clean$cp_pre_crash_movement <- replace(adas_clean$cp_pre_crash_movement, is.na(adas_clean$cp_pre_crash_movement), "Unknown")
adas_clean$sv_pre_crash_movement <- replace(adas_clean$sv_pre_crash_movement, is.na(adas_clean$sv_pre_crash_movement), "Unknown")


adas_clean$weather <- str_to_title(adas_clean$weather)

abbreviations <- c("SV", "CP")

title_case_with_exceptions <- function(x, exceptions = abbreviations) {
  # Split by space
  words <- str_split(x, " ")
  
  # Apply to each entry
  sapply(words, function(w) {
    sapply(w, function(word) {
      if (toupper(word) %in% exceptions) {
        toupper(word)            # preserve abbreviation
      } else {
        str_to_title(word)       # title-case normally
      }
    }) %>% paste(collapse = " ")
  })
}

# Apply to your column
adas_clean$collision_category <- title_case_with_exceptions(adas_clean$collision_category)

adas_clean$collision_category <- str_to_title(adas_clean$collision_category)

adas_clean <- adas_clean %>% mutate(across(everything(), ~ as.factor(.))) %>% drop_na()