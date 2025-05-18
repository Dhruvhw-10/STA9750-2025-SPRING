# 02_data_cleaning.R

# =============================
# 🧹 Data Cleaning & Transformation
# =============================

# Load libraries
library(tidyverse)
library(lubridate)
library(janitor)

# Load hourly ridership data (filtered from 2020–2023)
mta_hourly <- readRDS("data/cleaned/mta_hourly_filtered_2020_2023.rds")

# =============================
# 1. Parse datetime and derive temporal fields
# =============================
mta_hourly <- mta_hourly |> 
  filter(!is.na(transit_timestamp) & str_detect(transit_timestamp, "^\\d{4}-\\d{2}-\\d{2}")) |> 
  mutate(
    transit_timestamp = gsub("\\.000", "", transit_timestamp),
    transit_timestamp = ymd_hms(transit_timestamp, quiet = TRUE),  # suppress warning
    year = year(transit_timestamp),
    month = month(transit_timestamp, label = TRUE),
    day = day(transit_timestamp),
    hour = hour(transit_timestamp),
    weekday = wday(transit_timestamp, label = TRUE),
    date = as_date(transit_timestamp)
  )

# =============================
# 2. Define COVID-era categories
# =============================
mta_hourly <- mta_hourly |> 
  mutate(
    covid_era = case_when(
      year == 2019 ~ "Pre-COVID",
      year %in% c(2020, 2021) ~ "Core COVID",
      year %in% c(2022, 2023) ~ "WFH Era",
      TRUE ~ "Unknown"
    )
  )

# =============================
# 3. Add weekday/weekend classification
# =============================
mta_hourly <- mta_hourly |>  
  mutate(day_type = if_else(weekday %in% c("Sat", "Sun"), "Weekend", "Weekday"))

# =============================
# 4. Filter to reasonable values
# =============================
mta_hourly <- mta_hourly |> 
  filter(ridership >= 0, transfers >= 0)

# =============================
# 5. Save cleaned version
# =============================
write_csv(mta_hourly, "data/cleaned/mta_hourly_cleaned.csv")
saveRDS(mta_hourly, "data/cleaned/mta_hourly_cleaned.rds")

message("\n✅ Hourly ridership data cleaned and ready for aggregation!")
